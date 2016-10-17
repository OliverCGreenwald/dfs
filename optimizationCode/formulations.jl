module formulations 

# To install DataFrames, simply run Pkg.add("DataFrames")
using DataFrames

#=
GLPK is an open-source solver, and additionally Cbc is an open-source solver. This code uses GLPK
because we found that it was slightly faster than Cbc in practice. For those that want to build
very sophisticated models, they can buy Gurobi. To install GLPKMathProgInterface, simply run
Pkg.add("GLPKMathProgInterface")
=#
#using GLPKMathProgInterface
using Gurobi

# Once again, to install run Pkg.add("JuMP")
using JuMP

export one_lineup_no_stacking, one_lineup_Type_1, one_lineup_Type_2, one_lineup_Type_3,
one_lineup_Type_4, one_lineup_Type_5, one_lineup_Type_6
############################  Lineup Generator Functions  ############################

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Only Feasibility Constraints 
function one_lineup_no_stacking(num_lineups, offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())
    # Variable for skaters in lineup.
    @defVar(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for goalie in lineup.
    @defVar(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @addConstraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @addConstraint(m, sum{offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @addConstraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @addConstraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @addConstraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # between 1 and 2 TE (Because of FLEX player)
    @addConstraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @addConstraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @defVar(m, used_team[i=1:num_teams], Bin)
    @addConstraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @addConstraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # Overlap Constraint
    @addConstraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @addConstraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    # Objective
    @setObjective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})


    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
function one_lineup_Type_1(num_lineups, offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @defVar(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @defVar(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @addConstraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @addConstraint(m, sum{offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @addConstraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @addConstraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @addConstraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # between 1 and 2 TE (Because of FLEX player)
    @addConstraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @addConstraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @defVar(m, used_team[i=1:num_teams], Bin)
    @addConstraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @addConstraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @addConstraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Overlap Constraint
    @addConstraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @addConstraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    # Objective
    @setObjective(m, Max, sum{offensive_players[i,:Projection]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection]*defenses_lineup[i], i=1:num_defenses})


    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
function one_lineup_Type_2(num_lineups, offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @defVar(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @defVar(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @addConstraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @addConstraint(m, sum{offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @addConstraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @addConstraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @addConstraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # between 1 and 2 TE (Because of FLEX player)
    @addConstraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @addConstraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @defVar(m, used_team[i=1:num_teams], Bin)
    @addConstraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @addConstraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @addConstraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @defVar(m, QBWR_stack[i=1:num_pairs], Bin)
    @addConstraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @addConstraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Overlap Constraint
    @addConstraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @addConstraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    # Objective
    @setObjective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})


    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end


# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - QB-oppWR
function one_lineup_Type_3(num_lineups, offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @variable(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @variable(m, defenses_lineup[i=1:num_defenses], Bin)

    # #=
    # DraftKings Fantasy Contests require the following lineup:
    #     - 1xQB
    #     - 2xRB
    #     - 3xWR 
    #     - 1xTE
    #     - 1xFLEX (RB/WR/TE)
    #     - 1xDST
    # Whose salaries sum to less than $55,000
    # =#

    # One Defense constraint
    @constraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @constraint(m, sum{offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @constraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @constraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @constraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # between 1 and 2 TE (Because of FLEX player)
    @constraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @constraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @constraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 2 different teams represented between the 8 offensive players (constructed w/o defenses_lineup b/c will effectively never have an entire offense from the same team so DK rule will be satisfied)
    @variable(m, used_team[i=1:num_teams], Bin)
    @constraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @constraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @constraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @defVar(m, QBWR_stack[i=1:num_pairs], Bin)
    @constraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @constraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)
    

    # Must have a QB/opp-WR Pair
    @defVar(m, QBoppWR_stack[i=1:num_pairs_QBoppWR], Bin)
    @addConstraint(m, constr[i=1:num_pairs_QBoppWR], 10*QBoppWR_stack[i] <= sum{team_pairs_QBoppWR[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @addConstraint(m, sum{QBoppWR_stack[i], i=1:num_pairs_QBoppWR} >= 1)

    # Overlap Constraint
    @constraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @addConstraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    # Objective
    @objective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})


    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end


# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - no TE for flex
function one_lineup_Type_4(num_lineups, offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @defVar(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @defVar(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @addConstraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @addConstraint(m, sum{offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @addConstraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @addConstraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @addConstraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @addConstraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@addConstraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@addConstraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @addConstraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @defVar(m, used_team[i=1:num_teams], Bin)
    @addConstraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @addConstraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @addConstraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @defVar(m, QBWR_stack[i=1:num_pairs], Bin)
    @addConstraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @addConstraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Overlap Constraint
    @addConstraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @addConstraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    # Objective
    @setObjective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})


    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end


# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - no TE or RB for flex (must be WR)
function one_lineup_Type_5(num_lineups, offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @defVar(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @defVar(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @addConstraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @addConstraint(m, sum{offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @addConstraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # 2 RB (no RB FLEX player)
    @addConstraint(m, 2 == sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    # @addConstraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    # @addConstraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @addConstraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @addConstraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@addConstraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@addConstraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)

    # Financial Constraint
    @addConstraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @defVar(m, used_team[i=1:num_teams], Bin)
    @addConstraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @addConstraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @addConstraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @defVar(m, QBWR_stack[i=1:num_pairs], Bin)
    @addConstraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @addConstraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Overlap Constraint
    @addConstraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @addConstraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    # Objective
    @setObjective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})


    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Feasibility Constraints 
# - Defense constraint (Defense can't be playing any offensive players)
# - QB-WR Stack (If you have a QB then also include a WR from the same team)
# - no TE for flex
# - Must have a Value RB (RB worth less than 5000)
function one_lineup_Type_6(num_lineups, offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure, team_pairs_QBoppWR, num_pairs_QBoppWR, cheapRunningBack)
    #m = Model(solver=GLPKSolverMIP())
    m = Model(solver=GurobiSolver())

    # Variable for Offensive_Players in lineup.
    @defVar(m, offensive_players_lineup[i=1:num_offensive_players], Bin)

    # Variable for Defense in lineup.
    @defVar(m, defenses_lineup[i=1:num_defenses], Bin)

    #=
    DraftKings Fantasy Contests require the following lineup:
        - 1xQB
        - 2xRB
        - 3xWR 
        - 1xTE
        - 1xFLEX (RB/WR/TE)
        - 1xDST
    Whose salaries sum to less than $55,000
    =#

    # One Defense constraint
    @addConstraint(m, sum{defenses_lineup[i], i=1:num_defenses} == 1)

    # Eight Offensive_Players constraint
    @addConstraint(m, sum{offensive_players_lineup[i], i=1:num_offensive_players} == 8)

    # One QB constraint
    @addConstraint(m, sum{quarterBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} == 1)

    # between 2 and 3 RB (Because of FLEX player)
    @addConstraint(m, 2<=sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{runningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 3)

    # between 3 and 4 WR (Because of FLEX player)
    @addConstraint(m, 3 <= sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    @addConstraint(m, sum{wideReciever[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 4)

    # 1 TE (no TE FLEX player)
    @addConstraint(m, 1 == sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@addConstraint(m, 1 <= sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players})
    #@addConstraint(m, sum{tightEnd[i]*offensive_players_lineup[i], i=1:num_offensive_players} <= 2)
    
    # Must have 1 Value RB 
    @addConstraint(m, sum{cheapRunningBack[i]*offensive_players_lineup[i], i=1:num_offensive_players} >= 1)

    # Financial Constraint
    @addConstraint(m, sum{offensive_players[i,:Salary]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Salary]*defenses_lineup[i], i=1:num_defenses} <= 50000)

    # at least 3 different teams for the 8 skaters constraints
    @defVar(m, used_team[i=1:num_teams], Bin)
    @addConstraint(m, constr[i=1:num_teams], used_team[i] <= sum{offensive_players_teams[t, i]*offensive_players_lineup[t], t=1:num_offensive_players})
    @addConstraint(m, sum{used_team[i], i=1:num_teams} >= 2)

    # No Defenses going against Offensive_Players constraint
    @addConstraint(m, constr[i=1:num_defenses], 6*defenses_lineup[i] + sum{defenses_opponents[k, i]*offensive_players_lineup[k], k=1:num_offensive_players}<=6)

    # Must have a QB/WR Pair
    # QB is weighted 9 and WR is weighted 1 so in order to have a sum >= 10 there must be 
    # at least a QB/WR Pair
    @defVar(m, QBWR_stack[i=1:num_pairs], Bin)
    @addConstraint(m, constr[i=1:num_pairs], 10*QBWR_stack[i] <= sum{team_pairs[k,i]*offensive_players_lineup[k], k=1:num_offensive_players})
    @addConstraint(m, sum{QBWR_stack[i], i=1:num_pairs} >= 1)

    # Overlap Constraint
    @addConstraint(m, constr[i=1:size(lineups)[2]], sum{lineups[j,i]*offensive_players_lineup[j], j=1:num_offensive_players} + sum{lineups[num_offensive_players+j,i]*defenses_lineup[j], j=1:num_defenses} <= num_overlap)

    # Exposure Constraint
    @addConstraint(m, constr[j=1:num_offensive_players], sum{lineups[j,i], i=1:size(lineups)[2]} + offensive_players_lineup[j] <= num_lineups * exposure)

    # Objective
    @setObjective(m, Max, sum{offensive_players[i,:Projection_dfn]*offensive_players_lineup[i], i=1:num_offensive_players} + sum{defenses[i,:Projection_dfn]*defenses_lineup[i], i=1:num_defenses})


    # Solve the integer programming problem
    println("Solving Problem...")
    @printf("\n")
    status = solve(m);


    # Puts the output of one lineup into a format that will be used later
    if status==:Optimal
        offensive_players_lineup_copy = Array(Int64, 0)
        for i=1:num_offensive_players
            if getvalue(offensive_players_lineup[i]) >= 0.9 && getvalue(offensive_players_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        for i=1:num_defenses
            if getvalue(defenses_lineup[i]) >= 0.9 && getvalue(defenses_lineup[i]) <= 1.1
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(1,1))
            else
                offensive_players_lineup_copy = vcat(offensive_players_lineup_copy, fill(0,1))
            end
        end
        return(offensive_players_lineup_copy)
    end
end

end
