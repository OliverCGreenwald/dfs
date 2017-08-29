from position import Positions
from database import Database
from roster import *
import pulp


SALARY_CAP = 50000
MIN_PARTICIPATING_TEAMS = 2
MIN_PARTICIPATING_MATCHUPS = 2


class OptimizationProblem(object):
    """Optimization problem definition"""

    def __init__(self, db, roster_set=None):
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


    def add_feasibility_constraint(self, num_players=9, salary_cap=50000):
        """Basic contraints for valid lineups."""
        self.prob += (sum(self.player_vars.values()) == num_players,
            "%s players required" % num_players)

        for position in Positions.all():
            # Number of active players for position.
            active_in_position = sum(self.player_vars[pid] for pid 
                in self.db.pid_positions(position))
            required_in_position = Positions.num_required(position)
        
            # Bounds on the active player per position.
            if Positions.is_flex(position):
                self.prob += (active_in_position >= required_in_position,
                    "%s requires at LEAST %s players" % (position, required_in_position))
                self.prob += (active_in_position <= required_in_position+1,
                    "%s requires at MOST %s players" % (position, required_in_position+1))
            else:
                self.prob += (active_in_position == required_in_position,
                    "%s requires at MOST %s players" % (position, required_in_position))

        # The total money spent on active players must be <= than salary cap.
        self.prob += (sum(salary_cap >= self.player_vars[pid] * salary
            for pid, salary in zip(self.db.df.ID, self.db.df.Salary)),
            "Must have salary below %s" % salary_cap)


    def add_overlap_constraint(self, overlap_ceiling=4):
        """Overlap contraint prevents similar rosters."""
        for i, roster in enumerate(self.roster_set.rosters):
            overlap = 0.0
            for pid in roster.pids:
                overlap += player_vars[pid]
            self.prob += (overlap <= overlap_ceiling,
                "Roster %d overlap must not exceed %s" % (i, overlap_ceiling))


    def solve(self, roster_set_size):
        """Iteratively solves problem to fill the roster set."""
        self.roster_set = RosterSet()
        for _ in range(roster_set_size):
            self.roster_set.add(self._solve())



if __name__ == '__main__':
    csv_name = 'data/2016-09-18.csv'
    db = Database(csv_path = csv_name)

    op = OptimizationProblem(db)
    op.add_objective()
    op.add_feasibility_constraint()
    op.add_overlap_constraint()
    op.solve(1)
    print op.roster_set.to_string(db)


