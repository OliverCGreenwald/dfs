from position import Positions
from database import Database
from roster import *
import pulp



class OptimizationProblem(object):
    """Optimization problem definition"""

    def __init__(self, db, roster_set=None, constraint_fns=None):
        super(OptimizationProblem, self).__init__()
        if roster_set is None:
            roster_set = RosterSet()
        self.roster_set = roster_set
        self.db = db
        
        # Establish optimization problem.
        self.prob = pulp.LpProblem("DK Optimization", pulp.LpMaximize)

        # Create basic and necessary integer variables.
        self.player_vars = {pid: pulp.LpVariable(name="player-%s-%s-%s-%s" % 
            (db.position(pid), db.team(pid), db.name(pid), pid), cat='Binary')
            for pid in db.pids()}
        self.team_vars = {team: pulp.LpVariable(name="team-%s" % team, cat='Binary')
            for team in db.teams()}
        self.matchup_vars = {matchup: pulp.LpVariable(name="matchup-%s" % '-'.join(matchup), 
            cat='Binary') for matchup in db.matchups()}

        # List of constraints to be called during refresh.
        if constraint_fns == None:
            constraint_fns = {}
        self.constraint_fns = constraint_fns


    def __str__(self):
        return str(self.prob)


    def _refresh(self):
        """Refresh problem definition, variables and constraints."""
        self.__init__(self.db, self.roster_set, self.constraint_fns)
        self.add_objective()
        for fns in self.constraint_fns.keys():
            fns(*self.constraint_fns[fns])


    def _solve(self):
        """Solves the problem once with current roster set."""
        self.prob.solve()
        if self.prob.status <= 0:
            raise Exception("Infeasible Solution.")
        return {pid for pid, variable 
            in self.player_vars.iteritems()
            if variable.varValue}


    def add_objective(self):
        """Objective function."""
        objective = sum([self.player_vars[pid] * projections 
            for pid, projections in zip(self.db.df.ID, self.db.df.Projection_dfn)])
        self.prob += objective, "maximize expectied fantasy points"


    def add_feasibility_constraint(self, num_players=9, salary_cap=50000,
                                   min_participating_teams=2,
                                   min_participating_matchups=2):
        """Basic contraints for valid lineups."""
        # Add function to be called on refresh.
        self.constraint_fns[self.add_feasibility_constraint] = [num_players, salary_cap]

        self.prob += (sum(self.player_vars.values()) == num_players,
            "%s players required" %num_players)

        for position in Positions.all():
            # Number of active players for position.
            active_in_position = sum(self.player_vars[pid] for pid 
                in self.db.pid_positions(position))
            required_in_position = Positions.num_required(position)
        
            # Bounds on the active player per position.
            if Positions.is_flex(position):
                self.prob += (active_in_position >= required_in_position,
                    "%s requires at LEAST %s players" %(position, required_in_position))
                self.prob += (active_in_position <= required_in_position+1,
                    "%s requires at MOST %s players" %(position, required_in_position+1))
            else:
                self.prob += (active_in_position == required_in_position,
                    "%s requires at MOST %s players" %(position, required_in_position))

        # The total money spent on active players must be <= than salary cap.
        self.prob += (salary_cap >= sum(self.player_vars[pid] * salary
            for pid, salary in zip(self.db.df.ID, self.db.df.Salary)),
            "Must have salary below %s" %salary_cap)

        # Must have a minimum of two teams on a roster.
        for team in self.db.teams():
            self.prob += (self.team_vars[team] <= sum(self.player_vars[pid]
                for pid in self.db.pid_teams(team)),
                "Team %s count mustn't exceed active players on team" % team)
        self.prob += (sum(self.team_vars.values()) >= min_participating_teams,
            "Must have %s participating teams" % min_participating_teams) 

        # Must have a minimum of two matchups in a roster.
        for matchup in self.db.matchups():
            self.prob += (self.matchup_vars[matchup] <= sum(self.player_vars[pid]
                for team in matchup for pid in self.db.pid_teams(team)),
                "Matchup %s count mustn't exceed active players in matchup" % '-'.join(matchup))
        self.prob += (sum(self.matchup_vars.values()) >= min_participating_matchups,
            "Must have %s participating matchups" % min_participating_matchups)


    def add_overlap_constraint(self, overlap_ceiling=4):
        """Overlap contraint prevents similar rosters."""
        # Add function to be called on refresh.
        self.constraint_fns[self.add_overlap_constraint] = [overlap_ceiling]

        for i, roster in enumerate(self.roster_set.rosters):
            overlap = 0.0
            for pid in roster.pids:
                overlap += self.player_vars[pid]
            self.prob += (overlap <= overlap_ceiling,
                "Roster %d overlap must not exceed %s" %(i, overlap_ceiling))


    def add_qb_stack_constraint(self, num_stacks=1, num_wrs=0, num_rbs=0, num_tes=0):
        """QB and other positions same team constraint"""
        self.constraint_fns[self.add_qb_stack_constraint] = [num_stacks,
            num_wrs, num_rbs, num_tes]

        pos_map = {Positions.WR:num_wrs,
                   Positions.RB:num_rbs,
                   Positions.TE:num_tes}
        for pos in (Positions.WR, Positions.RB, Positions.TE):
            if pos_map[pos] <= 0:
                continue
            qb_stack_vars = {team: pulp.LpVariable(name="QB-%s-count-%s" %(pos,team), cat='Binary')
                for team in self.db.teams()}
            for team in self.db.teams():
                sum_qb_team = sum(self.player_vars[pid] for pid in self.db.pid_teams(team)
                    if self.db.position(pid) in {Positions.QB})
                sum_pos_team = sum(self.player_vars[pid] for pid in self.db.pid_teams(team)
                    if self.db.position(pid) in {pos})
                self.prob += (qb_stack_vars[team]*(9+pos_map[pos]) <= 9*sum_qb_team + sum_pos_team,
                    "Team %s QB %s Stack *%s must be <= # Active 9*QB + %s on team."
                    %(team, pos, 9+pos_map[pos], pos))
            self.prob += (sum(qb_stack_vars.values()) >= num_stacks,
                "Must have at least %s Stack of %s QB+%s on same team." 
                %(num_stacks, pos_map[pos], pos))


    def add_opp_dst_constraint(self, no_qb=True, no_wr=True, no_te=True, no_rb=True):
        """Does not allow offensive positions to be with opponents defense."""
        return


    def solve(self, roster_set_size, verbose=True):
        """Iteratively solves problem to fill the roster set."""
        self.roster_set = RosterSet()
        for i in range(roster_set_size):
            # Print to std out current roster problem is solving.
            if verbose:
                print "Working on roster: %d" %(i+1)

            # Get next optimal roster.
            self.roster_set.add(self._solve())
            self._refresh()




if __name__ == '__main__':
    csv_name = 'data/2016-09-18.csv'
    db = Database(csv_path = csv_name)

    op = OptimizationProblem(db)
    op.add_objective()
    op.add_feasibility_constraint()
    op.add_overlap_constraint()
    op.add_qb_stack_constraint(num_wrs=1, num_tes=0, num_rbs=0)

    op.solve(3)
    print op.roster_set.to_string(db)


