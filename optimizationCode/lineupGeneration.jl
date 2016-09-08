#=
This code is our (Michael Chiang and Alan Du) adaptation of the techniques used 
in the paper, Winning Daily Fantasy Hockey Contests Using Integer Programming by 
Hunter, Vielma, and Zaman. 

=#

# To install DataFrames, simply run Pkg.add("DataFrames")
using DataFrames

#=
GLPK is an open-source solver, and additionally Cbc is an open-source solver. This code uses GLPK
because we found that it was slightly faster than Cbc in practice. For those that want to build
very sophisticated models, they can buy Gurobi. To install GLPKMathProgInterface, simply run
Pkg.add("GLPKMathProgInterface")
=#
using GLPKMathProgInterface

# Once again, to install run Pkg.add("JuMP")
using JuMP

############################  Setting Variables  ############################

#=
Variables for solving the problem (change these)
=#
# num_lineups is the total number of lineups
num_lineups = 150

# num_overlap is the maximum overlap of players between the lineups that you create
num_overlap = 3

# exposure is a number from 0-1 that gives the total % of lineups that a single player can be in
exposure = 0.34

# path_offensive_players is a string that gives the path to the csv file with the offensive_players information
#TESTING PATH
#path_offensive_players = "../testingLineups/data_warehouse/offensive_players.csv"
#PRODUCTION PATH
path_offensive_players = "data_warehouse/offensive_players.csv"

# path_defense is a string that gives the path to the csv file with the defenses information
#TESTING PATH
#path_defenses = "../testingLineups/data_warehouse/defenses.csv"
#PRODUCTION PATH
path_defenses = "data_warehouse/defenses.csv"

# path_to_output is a string that gives the path to the csv file that will give the outputted results
#TESTING PATH
#path_to_output= "../testingLineups/output.csv"
#PRODUCTION PATH
path_to_output= "output.csv"

############################  Lineup Generator Functions  ############################

# This is a function that creates one lineup using the No Stacking formulation from the paper
# - Only Feasibility Constraints 
function one_lineup_no_stacking(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure)
    m = Model(solver=GLPKSolverMIP())

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
function one_lineup_Type_1(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure)
    m = Model(solver=GLPKSolverMIP())

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
function one_lineup_Type_2(offensive_players, defenses, lineups, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure)
    m = Model(solver=GLPKSolverMIP())

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


############################  Setting Formation  ############################

#=
formulation is the type of formulation that you would like to use. 
    Available Options: 
        - one_lineup_no_stacking
        - one_lineup_Type_1
        - one_lineup_Type_2
=#
formulation = one_lineup_Type_2

############################  Setting Formation  ############################

function create_lineups(num_lineups, num_overlap, path_offensive_players, path_defenses, formulation, path_to_output)
    #=
    num_lineups is an integer that is the number of lineups (Line 28)
    num_overlap is an integer that gives the overlap between each lineup (Line 31)
    path_offensive_players is a string that gives the path to the Offensive_Players csv file (Line 34)
    path_defenses is a string that gives the path to the Defenses csv file (Line 37)
    formulation is the type of formulation you would like to use (Line 48)
    path_to_output is a string where the final csv file with your lineups will be (Line 40)
    =#


    # Load information for offensive_players table
    offensive_players = readtable(path_offensive_players)

    # Load information for defenses table
    defenses = readtable(path_defenses)

    # Number of offensive_players
    num_offensive_players = size(offensive_players)[1]

    # Number of defenses
    num_defenses = size(defenses)[1]

    # quarterBack stores the information on which players are quarterBack
    quarterBack = Array(Int64, 0)

    # runningBack stores the information on which players are runningBack
    runningBack = Array(Int64, 0)

    # wideReciever stores the information on which players are wideReciever
    wideReciever = Array(Int64, 0)

    # tightEnd stores the information on which players are tightEnd
    tightEnd = Array(Int64, 0)


    #=
    Process the position information in the skaters file to populate the 
    skill_positions (QB, RB, WR, TE) with the corresponding correct information
    =#
    for i =1:num_offensive_players
        if offensive_players[i,:Position] == "QB" 
            quarterBack=vcat(quarterBack,fill(1,1))
            runningBack=vcat(runningBack,fill(0,1))
            wideReciever=vcat(wideReciever,fill(0,1))
            tightEnd=vcat(tightEnd,fill(0,1))
        elseif offensive_players[i,:Position] == "RB"
            quarterBack=vcat(quarterBack,fill(0,1))
            runningBack=vcat(runningBack,fill(1,1))
            wideReciever=vcat(wideReciever,fill(0,1))
            tightEnd=vcat(tightEnd,fill(0,1))
        elseif offensive_players[i,:Position] == "WR"
            quarterBack=vcat(quarterBack,fill(0,1))
            runningBack=vcat(runningBack,fill(0,1))
            wideReciever=vcat(wideReciever,fill(1,1))
            tightEnd=vcat(tightEnd,fill(0,1))
        else
            quarterBack=vcat(quarterBack,fill(0,1))
            runningBack=vcat(runningBack,fill(0,1))
            wideReciever=vcat(wideReciever,fill(0,1))
            tightEnd=vcat(tightEnd,fill(1,1))
        end
    end


    # Create team indicators from the information in the offensive_players file
    teams = unique(offensive_players[:Team])

    # Total number of teams
    num_teams = size(teams)[1]

    # player_info stores information on which team each player is on
    player_info = zeros(Int, size(teams)[1])

    # Populate player_info with the corresponding information
    for j=1:size(teams)[1]
        if offensive_players[1, :Team] == teams[j]
            player_info[j] =1
        end
    end
    offensive_players_teams = player_info'

    for i=2:num_offensive_players
        player_info = zeros(Int, size(teams)[1])
        for j=1:size(teams)[1]
            if offensive_players[i, :Team] == teams[j]
                player_info[j] =1
            end
        end
        offensive_players_teams = vcat(offensive_players_teams, player_info')
    end



    # Create defense identifiers so you know who they are playing
    opponents = defenses[:Opponent]
    defenses_teams = defenses[:Team]
    defenses_opponents=[]
    for num = 1:size(teams)[1]
        if opponents[1] == teams[num]
            defenses_opponents = offensive_players_teams[:, num]
        end
    end
    for num = 2:size(opponents)[1]
        for num_2 = 1:size(teams)[1]
            if opponents[num] == teams[num_2]
                defenses_opponents = hcat(defenses_opponents, offensive_players_teams[:,num_2])
            end
        end
    end

     # Create WR/QB Lines to know which QB-WR Pairs are on the same team
    pair_info = zeros(Int, num_offensive_players)
    for num=1:size(offensive_players)[1]
        if offensive_players[:Team][num] == teams[1]
            if offensive_players[:Position][num] == "QB"
                pair_info[num] = 9
            elseif offensive_players[:Position][num] == "WR"
                pair_info[num] = 1
            end
        end
    end
    team_pairs = hcat(pair_info)

    #Weighting so that we can force a QB to exist in the stack of QB/WR's 
    for num2 = 2:size(teams)[1]
        pair_info = zeros(Int, num_offensive_players)
        for num=1:size(offensive_players)[1]
            if offensive_players[:Team][num] == teams[num2]
                if offensive_players[:Position][num] == "QB"
                    pair_info[num] = 9
                elseif offensive_players[:Position][num] == "WR"
                    pair_info[num] = 1
                end
            end
        end
        team_pairs = hcat(team_pairs, pair_info)
    end
    num_pairs = size(team_pairs)[2]
    

    # Lineups using formulation as the stacking type
    the_lineup= formulation(offensive_players, defenses, hcat(zeros(Int, num_offensive_players + num_defenses), zeros(Int, num_offensive_players + num_defenses)), num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure)
    the_lineup2 = formulation(offensive_players, defenses, hcat(the_lineup, zeros(Int, num_offensive_players + num_defenses)), num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure)
    tracer = hcat(the_lineup, the_lineup2)
    for i=1:(num_lineups-2)
        try
            thelineup=formulation(offensive_players, defenses, tracer, num_overlap, num_offensive_players, num_defenses, quarterBack, runningBack, wideReciever, tightEnd, num_teams, offensive_players_teams, defenses_opponents, team_pairs, num_pairs, exposure)
            tracer = hcat(tracer,thelineup)
        catch
            break
        end
    end


    # Create the output csv file
    # Write File in the following order:
    # Names of the QB, RB1, RB2, WR1, WR2, WR3, TE, FLEX (RB/WR/TE), and DST
    lineup2 = ""
    for j = 1:size(tracer)[2]
        lineup = ["" "" "" "" "" "" "" "" ""]
        for i =1:num_offensive_players
            if tracer[i,j] == 1
                if quarterBack[i]==1
                    lineup[1] = string(offensive_players[i,1], " ", offensive_players[i,2])
                elseif runningBack[i] == 1
                    if lineup[2] == ""
                        lineup[2] = string(offensive_players[i,1], " ", offensive_players[i,2])
                    elseif lineup[3] == ""
                        lineup[3] = string(offensive_players[i,1], " ", offensive_players[i,2])
                    elseif lineup[8] == ""
                        lineup[8] = string(offensive_players[i,1], " ", offensive_players[i,2])
                    end
                elseif wideReciever[i]==1
                    if lineup[4] == ""
                        lineup[4] = string(offensive_players[i,1], " ", offensive_players[i,2])
                    elseif lineup[5] ==""
                        lineup[5] = string(offensive_players[i,1], " ", offensive_players[i,2])
                    elseif lineup[6] == ""
                        lineup[6] = string(offensive_players[i,1], " ", offensive_players[i,2])
                    elseif lineup[8] == ""
                        lineup[8] = string(offensive_players[i,1], " ", offensive_players[i,2])
                    end
                elseif tightEnd[i]==1
                    if lineup[7] == ""
                        lineup[7] = string(offensive_players[i,1], " ", offensive_players[i,2])
                    elseif lineup[8] ==""
                        lineup[8] = string(offensive_players[i,1], " ", offensive_players[i,2])
                    end
                end
            end
        end
        for i =1:num_defenses
            if tracer[num_offensive_players+i,j] == 1
                lineup[9] = string(defenses[i,1])
            end
        end
        for name in lineup
            lineup2 = string(lineup2, name, ",")
        end
        lineup2 = chop(lineup2)
        lineup2 = string(lineup2, """

        """)
    end
    outfile = open(path_to_output, "w")
    write(outfile, lineup2)
    close(outfile)
end




# Running the code
create_lineups(num_lineups, num_overlap, path_offensive_players, path_defenses, formulation, path_to_output)
