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

# Running
# exec '/Applications/Julia-0.4.6.app/Contents/Resources/julia/bin/julia'
# include("generator.jl")

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
exposure = 0.4

#Only used for Formulation 14
exposure_defense = 0.25
exposure_wr = 0.25
exposure_rb = 0.75
exposure_te = 0.75
exposure_qb = 0.5
exposure_valuewr = 0.1
#############  Setting Variables Related to Path  #############
#= week sets which week of data we're looking at 
    Available Options:
        - live = true or false (if live false, set week to any week 1-16 (if true then doesn't matter))
        - thu_mon = true or false (if false, then sunday players only; if true, all players Thu - Mon)
=#
live = true
week = 16
model=""
###########################  Setting Formation  ############################

use_Freq_Ind = false


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
        - 9 <- one_lineup_Type_9
        - 10 <- one_lineup_Type_10
        - 11 <- one_lineup_Type_11
        - 12 <- one_lineup_Type_12
        - 13 <- one_lineup_Type_13
        - 14 <- one_lineup_Type_14 (form 4 with player exposure constraints)
        - 14 <- one_lineup_Type_15 (form 14 + must have 1 value wr + 2700 <= DST Salary <= 3100)
=#
formulation_type = 15


############################  Setting Projections Source  ############################

#=
projections_source tells which Projections we're using for this generation
    Available Options: 
        - "Projection" <- From rotogrinders 
        - "Projection_dfn"
        - "Projection_fc"
        - "Projection_dfn_perturbed"
        - "Projection_reg"
        - "Projection_reg_split"
        - "Actual" (historical)
=#
projections_source = "Projection_dfn"

############################  Create Paths to data  ############################

path_offensive_players = string("data_warehouse/2016_cleaned_input/wk", week, "/offensive_players.csv")
path_defenses = string("data_warehouse/2016_cleaned_input/wk", week, "/defenses.csv")
path_to_output = string("../resultsAnalysis/data_warehouse/testing_lineups/week", week, projections_source[11:end])

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
elseif (formulation_type == 9) 
    formulation = formulations.one_lineup_Type_9  
elseif (formulation_type == 10) 
    formulation = formulations.one_lineup_Type_10  
elseif (formulation_type == 11) 
    formulation = formulations.one_lineup_Type_11
elseif (formulation_type == 12) 
    formulation = formulations.one_lineup_Type_12
elseif (formulation_type == 13) 
    formulation = formulations.one_lineup_Type_13
elseif (formulation_type == 14) 
    formulation = formulations.one_lineup_Type_14
elseif (formulation_type == 15) 
    formulation = formulations.one_lineup_Type_15
else
    formulation = formulations.one_lineup_no_stacking 
end

########### Running the code ###########


# Normal (Live) Output
formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, path_to_output, projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)

# use this for testing model1 thu-mon
# if (model=="model1")
#     if (formulation_type == 14)
#         formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/model1/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_defexp_", exposure_defense, "_wrexp_", exposure_wr, "_rbexp_", exposure_rb, "_teexp_", exposure_te,"_qbexp_", exposure_qb, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
#     elseif (formulation_type == 15)
#         formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/model1/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_defexp_", exposure_defense, "_wrexp_", exposure_wr, "_rbexp_", exposure_rb, "_teexp_", exposure_te,"_qbexp_", exposure_qb, "_valuewrexp_", exposure_valuewr, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
#     else
#         formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/model1/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_exposure_", exposure, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
#     end
# else
#     if (formulation_type == 14)
#         formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/testing_alan/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_defexp_", exposure_defense, "_wrexp_", exposure_wr, "_rbexp_", exposure_rb, "_teexp_", exposure_te,"_qbexp_", exposure_qb, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
#     elseif (formulation_type == 15)
#         formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/testing_alan/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_defexp_", exposure_defense, "_wrexp_", exposure_wr, "_rbexp_", exposure_rb, "_teexp_", exposure_te,"_qbexp_", exposure_qb, "_valuewrexp_", exposure_valuewr, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
#     else
#         formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/testing_alan/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_exposure_", exposure, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
#     end
# end

print(path_offensive_players)

# Output to testing_alan
# if (formulation_type == 14)
#     formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/testing_alan/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_defexp_", exposure_defense, "_wrexp_", exposure_wr, "_rbexp_", exposure_rb, "_teexp_", exposure_te,"_qbexp_", exposure_qb, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# elseif (formulation_type == 15)
#     formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/testing_alan/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_defexp_", exposure_defense, "_wrexp_", exposure_wr, "_rbexp_", exposure_rb, "_teexp_", exposure_te,"_qbexp_", exposure_qb, "_valuewrexp_", exposure_valuewr, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# else
#     formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/testing_alan/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_exposure_", exposure, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# end

# Output to includes_thu-mon
# if (formulation_type == 14)
#     formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_defexp_", exposure_defense, "_wrexp_", exposure_wr, "_rbexp_", exposure_rb, "_teexp_", exposure_te,"_qbexp_", exposure_qb, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# elseif (formulation_type == 15)
#     formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_defexp_", exposure_defense, "_wrexp_", exposure_wr, "_rbexp_", exposure_rb, "_teexp_", exposure_te,"_qbexp_", exposure_qb, "_valuewrexp_", exposure_valuewr, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# else
#     formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/includes_thu-mon/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_exposure_", exposure, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# end

# Output to includes_sun-mon
# if (formulation_type == 14)
#     formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/includes_sun-mon/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_defexp_", exposure_defense, "_wrexp_", exposure_wr, "_rbexp_", exposure_rb, "_teexp_", exposure_te,"_qbexp_", exposure_qb, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# elseif (formulation_type == 15)
#     formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/includes_sun-mon/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_defexp_", exposure_defense, "_wrexp_", exposure_wr, "_rbexp_", exposure_rb, "_teexp_", exposure_te,"_qbexp_", exposure_qb, "_valuewrexp_", exposure_valuewr, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# else
#     formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string("../resultsAnalysis/data_warehouse/testing_lineups/includes_sun-mon/week", week, projections_source[11:end], "_formulation", formulation_type, "_overlap_", num_overlap, "_exposure_", exposure, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# end

# Output with num_lineups (deprecated)
# formulations.create_lineups(num_lineups, num_overlap, exposure, path_offensive_players, path_defenses, formulation, string(path_to_output, "_formulation", formulation_type, "_overlap_", num_overlap, "_exposure_", exposure, "_numlineups_", num_lineups,".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)



# Varying num_lineups
# for i=1:9
#     formulations.create_lineups(num_lineups, i, exposure, path_offensive_players, path_defenses, formulation, string(path_to_output, "_formulation", formulation_type, "_overlap_", i, "_exposure_", exposure, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# end

# # Varying exposure (need to change code first)
# for i=1:9
#     formulations.create_lineups(num_lineups, num_overlap, 0.1*i, path_offensive_players, path_defenses, formulation, string(path_to_output, "_formulation", formulation_type, "_overlap_", num_overlap, "_exposure_0.", i, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# end

# Do all Possibilities for a week.
# for overlap_var =1:9
#     for exposure_var =1:9
#         formulations.create_lineups(num_lineups, overlap_var, exposure_var*0.1, path_offensive_players, path_defenses, formulation, string(path_to_output, "_formulation", formulation_type, "_overlap_", overlap_var, "_exposure_0.", exposure_var, ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
#     end
#     formulations.create_lineups(num_lineups, overlap_var, 1, path_offensive_players, path_defenses, formulation, string(path_to_output, "_formulation", formulation_type, "_overlap_", overlap_var, "_exposure_1", ".csv"), projections_source, use_Freq_Ind, exposure_defense, exposure_wr, exposure_rb, exposure_te, exposure_qb, exposure_valuewr)
# end
