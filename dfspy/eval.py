from roster import *
import pandas as pd

class Evaluation(object):
    """Evaluation class for a generated roseter set."""

    def __init__(self, payout_df= None, payout_path=None, 
                 standings_df= None, standings_path=None,
                 roster_set=None):
        super(Evaluation, self).__init__()
        if roster_set == None:
            roster_set = RosterSet()
        self.roster_set = roster_set
        self.payout = payout_df if payout_df is not None else pandas.read_csv(payout_path)
        self.standings = standings_df if standings_df is not None else pandas.read_csv(standings_path)


    def money_earned(self):
        """Payout for current roster set."""
        return 

    def percent_itm(self):
        """Percentage of rosters in the money."""
        return

    def average_standing(self):
        """Returns the average roster standing."""
        return 