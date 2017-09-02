from optimizer import *
from database import *
from roster import *
import cPickle as pickle


def formulation_1(db, roster_set_size=150):
    op = OptimizationProblem(db)
    op.add_objective()
    op.add_feasibility_constraint()
    op.add_overlap_constraint()
    op.add_qb_stack_constraint(num_wrs=1, num_tes=0, num_rbs=0)
    op.add_opp_dst_constraint(no_qb=True, no_wr=True, 
                              no_rb=True, no_te=True)
    op.solve(roster_set_size)
    return op.roster_set, str(op)



if __name__ == '__main__':

    csv_name = '_data/2016-3/records.csv'
    db = Database(csv_path=csv_name)
    roster_set, summary = formulation_1(db, 150)

    with open('_experiments/formulation_1/2016-3.pickle', 'wb') as output:
        pickle.dump(roster_set, output, protocol=pickle.HIGHEST_PROTOCOL)
    with open("_experiments/formulation_1/readme.txt", "w") as output:
        output.write(summary)
