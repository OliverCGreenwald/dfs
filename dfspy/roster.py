from database import *


class Roster(object):
    """Holds a single roster."""

    def __init__(self, pids):
        super(Roster, self).__init__()
        self.pids = pids

    def to_string(self, db):
        strings = ["Roster:"]
        actual_roster_fp = 0
        for pid in self.pids:
            string = "%s %s %s" % (db.position(pid), 
                db.team(pid), db.name(pid))
            strings.append(string)
            actual_roster_fp += db.actual(pid)
        strings.append(str(actual_roster_fp))
        return '|'.join(strings)



class RosterSet(object):
    """Holds sequence of rosters."""

    def __init__(self, rosters=None):
        super(RosterSet, self).__init__()
        rosters = rosters or []
        self.rosters = rosters

    def add(self, pids):
        self.rosters.append(Roster(pids))

    def to_string(self, db):
        return '\n'.join([
          roster.to_string(db)
          for roster in self.rosters
        ])



if __name__=='__main__':
    
    csv_name = 'data/2016-09-18.csv'
    db = Database(csv_path = csv_name)
    
    roster1 = Roster([7395497,7395498])
    roster2 = Roster([7395553,7395608])

    roster_set = RosterSet([roster1, roster2])


    print roster_set.to_string(db)

