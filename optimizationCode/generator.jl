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
num_overlap = 6

# exposure is a number from 0-1 that gives the total % of lineups that a single player can be in
exposure = 0.5

#############  Setting Variables Related to Path  #############
#= week sets which week of data we're looking at 
    Available Options: 
        - "LIVE" (Current week)
        - 1 (1-7)
=#
# <<<<<<< Updated upstream
# week = 5
# =======
week = "LIVE"
# >>>>>>> Stashed changes

############################  Setting Formation  ############################

#=
formulation is the type of formulation that you would like to use. 
    Available Options: 
        - 0 <- one_lineup_no_stacking
        - 1 <- one_lineup_Type_1
        - 2 <- one_lineup_Type_2
        - 3 <- one_lineup_Type_3
        - 4 <- one_lineup_Type_4
        - 5 <- one_lineup_Type_5
        - 6 <- one_lineup_Type_6 
        - 7 <- one_lineup_Type_7 
        - 8 <- one_lineup_Type_8 In Progress
=#
formulation_type = 7


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
    path_offensive_players = string("data_warehouse/2016_cleaned_input/wk", week, "/offensive_players.csv")
    path_defenses = string("data_warehouse/2016_cleaned_input/wk", week, "/defenses.csv")
    path_to_output = string("../resultsAnalysis/data_warehouse/testing_lineups/week", week, projections_source[11:end])
end

if (formulation_type == 1) 
    formulation = formulations.one_lineup_Type_1
elseif (formulation_type == 2) 
    formulation = formulations.one_lineup_Type_2
elseif (formulation_type == 3)
    formulation = formulations.one_lineup_Type_3
elseif (formulation_type == 4) 
    formulation = formulations.one_lineup_Type_4
elseif (formulation_type == 5)
    formulation = formulations.one_lineup_Type_5
elseif (formulation_type == 6)
    formulation = formulations.one_lineup_Type_6
elseif (formulation_type == 7) 
    formulation = formulations.one_lineup_Type_7  
elseif (formulation_type == 8) 
    formulation = formulations.one_lineup_Type_8  
else
    formulation = one_lineup_no_stacking 
end

########### Running the code ###########

formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, path_to_output, projections_source)

# Varying num_lineups
# for i=1:9
#     formulations.create_lineups(num_lineups, i, exposure, path_offensive_players, path_defenses, formulation, string(path_to_output, "_formulation", formulation_type, "_overlap_", i, "_exposure_", exposure, ".csv"), projections_source)
# end

# # Varying exposure (need to change code first)
# for i=1:9
#     formulations.create_lineups(num_lineups, num_overlap, 0.1*i, path_offensive_players, path_defenses, formulation, string(path_to_output, "_formulation", formulation_type, "_overlap_", num_overlap, "_exposure_0.", i, ".csv"), projections_source)
# end

# Do all Posibilities for a week.
# for overlap_var =1:9
#     for exposure_var =1:9
#         formulations.create_lineups(num_lineups, overlap_var, exposure_var*0.1, path_offensive_players, path_defenses, formulation, string(path_to_output, "_formulation", formulation_type, "_overlap_", overlap_var, "_exposure_0.", exposure_var, ".csv"), projections_source)
#     end
#     formulations.create_lineups(num_lineups, overlap_var, 1, path_offensive_players, path_defenses, formulation, string(path_to_output, "_formulation", formulation_type, "_overlap_", overlap_var, "_exposure_1", ".csv"), projections_source)
# end
