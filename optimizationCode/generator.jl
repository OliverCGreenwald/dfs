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
#using GLPKMathProgInterface
using Gurobi

# Once again, to install run Pkg.add("JuMP")
using JuMP

include("formulations.jl")

############################  Setting Variables  ############################

#=
Variables for solving the problem (change these)
=#
# num_lineups is the total number of lineups
num_lineups = 150

# num_overlap is the maximum overlap of players between the lineups that you create
num_overlap = 4

# exposure is a number from 0-1 that gives the total % of lineups that a single player can be in
exposure = 1

#############  Setting Variables Related to Path  #############
#= week sets which week of data we're looking at 
    Available Options: 
        - "LIVE" (Current week)
        - 1 (1-6)
=#
week = 1

############################  Setting Formation  ############################

#=
formulation is the type of formulation that you would like to use. 
    Available Options: 
        - one_lineup_no_stacking
        - one_lineup_Type_1
        - one_lineup_Type_2
        - one_lineup_Type_3
        - one_lineup_Type_4
        - one_lineup_Type_5
        - one_lineup_Type_6
=#
formulation = formulations.one_lineup_Type_4

############################  Setting Projections Source  ############################

#=
projections_source tells which Projections we're using for this generation
    Available Options: 
        - "Projection" <- From rotogrinders 
        - "Projection_dfn"
        - "Projection_fc"
        - "Projection_dfn_perturbed"
=#
projections_source = "Projection_dfn"  

############################  Create Paths to data  ############################

if (week == "LIVE") 
    path_offensive_players = "data_warehouse/offensive_players.csv"
    path_defenses = "data_warehouse/defenses.csv"
    path_to_output = "output.csv"
else
    println(projections_source[11:end])
    path_offensive_players = string("data_warehouse/2016_cleaned_input/wk", week, "/offensive_players.csv")
    path_defenses = string("data_warehouse/2016_cleaned_input/wk", week, "/defenses.csv")
    path_to_output = string("../resultsAnalysis/data_warehouse/testing_lineups/week", week, projections_source[11:end])
end


########### Running the code ###########

formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, path_to_output, projections_source)

# Varying num_lineups
# for i=1:9
#     formulations.create_lineups(num_lineups, i, exposure, path_offensive_players, path_defenses, formulation, string(path_to_output, "_formulation6_overlap_", i, "_exposure_", exposure, ".csv"), projections_source)
# end

# # Varying exposure (need to change code first)
# for i=1:9
#     formulations.create_lineups(num_lineups, num_overlap, 0.1*i, path_offensive_players, path_defenses, formulation, string(path_to_output, "_formulation4_overlap_", num_overlap, "_exposure_0.", i, ".csv"), projections_source)
# end


