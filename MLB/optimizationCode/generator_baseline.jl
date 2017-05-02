# This code solves for multiple baseball lineups

include("data_cleaning.jl")
include("formulations.jl")  #this code has all the different formualations


################################################################################################################
# Choose Formulation

#FORMULATION:  formulation is the type of formulation that you would like to use. 
formulation = formulations.formulation2_covar
# formulation_feasibility
# formulation0_covar
# formulation1_covar - no stacking
# formulation2_covar - stacking

#########################################################################
# Running the code

num_lineups = 150; 

contest_date_array = ["2017-04-25","2017-04-26","2017-04-28","2017-04-30","2017-04-21","2017-04-19","2017-04-18"]
contest_name_array = ["\$33.00entry_MLB\$400KTUESDAYSUPERFastball[\$50Kto1st]",
                 "\$55.00entry_DK5YearAnniversary\$55Special[\$50KTop]",
                 "\$55.00entry_DKFiveYearAnniversary\$55",
                 "\$50.00entry_MLB\$275KSundayNiftyFifty",
                 "\$33.00entry_MLB\$300KFastball",
                 "\$33.00entry_MLB\$300KFastball(Early)",
                 "\$40.00entry_MLB\$250KMEDIUMHOMERUN" ]


for contest_info_index in 1:size(contest_name_array)[1]
    for stack in 2:4 #stack
        for overlap in 5:6 
            for lambda in 1:3 # time line
                num_overlap = overlap

                stack_size = stack

                # Covariance term 
                lambda_var = 0.001 * lambda

                # Exposure Constraints
                exposure = 0.6

                contest_date = contest_date_array[contest_info_index]
                contest_name = contest_name_array[contest_info_index]

               contest_directory_path = string("../data_warehouse/", contest_date, "/", contest_name, "/");

               #path to the csv file with the players information (pitchers and hitters);
               path_pitchers = string(contest_directory_path, "pitchers.csv"); 
               path_hitters = string(contest_directory_path, "hitters.csv"); 
               path_covar_matrix = string(contest_directory_path, "covariance_mat_test.csv"); 
               # path_to_output is a string  that gives the path to the csv file that will give the outputted results
               path_to_output= string(contest_directory_path, "lineups/",
                                      string(formulation), "_stacksize_", stack_size,"_overlap_", num_overlap,"_lineups_", num_lineups,"_lambda_", lambda_var,"_exposure_", exposure,"_test.csv"); 


                start_time = time_ns()

                data_cleaning.create_lineups(num_lineups, num_overlap, stack_size,formulation, path_pitchers,path_hitters, path_covar_matrix, lambda_var, exposure,  path_to_output);


                println("##############################")
                println("###### Finished Lineups ######")
                println("##############################")

                println("\nCalculated DraftKings baseball lineups.\n\tNumber of lineups = ", num_lineups, " \n\tStack size = ",stack_size,
                "\n\tOverlap = ", num_overlap,"\n" )
                end_time = time_ns()
                println("Took ", (end_time - start_time)/60e10, " minutes to calculate ", num_lineups, " lineups")

                println("Saving data to file ",path_to_output)
            end
        end
    end
end

