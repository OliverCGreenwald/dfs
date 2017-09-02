from roster import *
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import cPickle as pickle




class Evaluation(object):
    """Evaluation class for a generated roseter set."""

    def __init__(self, db, payout_df= None, payout_path=None, 
                 standings_df= None, standings_path=None,
                 roster_set=None):
        super(Evaluation, self).__init__()
        if roster_set == None:
            roster_set = RosterSet()
        self.roster_set = roster_set

        self.db = db

        # Dataframes to store evaluation metrics.
        self.payout = payout_df if payout_df is not None else pandas.read_csv(payout_path)
        self.standings = standings_df if standings_df is not None else pandas.read_csv(standings_path)
        
        # Map roster to actual fantasy points earned.
        self.actual_fp = {}
        for roster in self.roster_set.rosters:
            actual_roster_fp = 0
            for pid in roster.pids:
                actual_roster_fp += self.db.actual(pid)
            self.actual_fp[roster] = actual_roster_fp

        # Map roster standings.
        self.roster_standings = {}
        for roster in self.roster_set.rosters:
            rankings = list(self.standings[self.standings.Points > 
                self.actual_fp[roster]].Rank)
            rank = 1 if len(rankings) == 0 else rankings[-1]-1
            self.roster_standings[roster] = rank

        # TODO: Map payout.
        self.roster_payouts = {}



    def money_earned(self):
        """Payout for current roster set."""
        return 


    def percent_itm(self):
        """Percentage of rosters in the money."""
        return


    def average_standing(self):
        """Returns the average roster standing."""
        return np.mean(self.roster_standings.values())


    def plot_standings(self):
        bins = list(self.payout.Place_lo)
        plt.xscale('log')
        plt.hist(self.roster_standings.values(),bins=bins) 
        plt.show()


if __name__ == '__main__':
    csv_name = '_data/2016-3/records.csv'
    db = Database(csv_path = csv_name)

    with open('_experiments/formulation_1/2016-3.pickle', 'rb') as input:
        roster_set = pickle.load(input)

    standings_path = '_data/2016-3/contest_standings.csv'
    payout_path = '_data/2016-3/payout_structure.csv'

    ev = Evaluation(db=db,
                    payout_path=payout_path,
                    standings_path=standings_path,
                    roster_set=roster_set)
    print ev.roster_standings
    print ev.average_standing()
    ev.plot_standings()




