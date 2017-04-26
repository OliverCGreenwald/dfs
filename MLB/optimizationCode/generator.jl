# This code solves for multiple baseball lineups

include("data_cleaning.jl")
include("formulations.jl")  #this code has all the different formualations


################################################################################################################
# Contest information

contest_date = "2017-04-21";
contest_name = "\$33.00entry_MLB\$300KFastball";


################################################################################################################

#INPUT PARAMS
# num_lineups is the total number of lineups
num_lineups = 10; 

# num_overlap is the maximum overlap of players between the lineups 
num_overlap = 6

#number of hitters in the stack (number of consecutive hitters in the hitting order)
stack_size = 5; 

#FORMULATION:  formulation is the type of formulation that you would like to use. 
formulation = formulations.baseball_formulation

# Covariance term 
lambda_var = 0.1

################################################################################################################
contest_directory_path = string("../data_warehouse/", contest_date, "/", contest_name, "/");

#path to the csv file with the players information (pitchers and hitters);
path_pitchers = string(contest_directory_path, "pitchers.csv"); 
path_hitters = string(contest_directory_path, "hitters.csv"); 

# path_to_output is a string  that gives the path to the csv file that will give the outputted results
path_to_output= string(contest_directory_path,
                       string(formulation), "_stacksize_", stack_size,"_overlap_", num_overlap,"_lineups_", num_lineups,".csv"); 

# path_to_output_proj is a string  that gives the path to the csv file that will give the outputted results with projected lineup points
#path_to_output_proj = string("proj_baseball_", string(formulation), "_stacksize_", stack_size,"_overlap_", num_overlap,"_lineups_", num_lineups,".csv"); 

# path_to_output_actual is a string  that gives the path to the csv file that will give the outputted results with projected and actual lineup points
#path_to_output_actual = string("actual_baseball_", string(formulation), "_stacksize_", stack_size,"_overlap_", num_overlap,"_lineups_", num_lineups,".csv"); 


#########################################################################
# Running the code
println("Calculating DraftKings baseball linueps.\n ", num_lineups, " lineups\n","Formulation  ",formulation,
"\nOverlap = ", num_overlap,"\nStack size = ", stack_size)


tic()
data_cleaning.create_lineups(num_lineups, num_overlap, stack_size,formulation, path_pitchers,path_hitters,  path_to_output);
telapsed = toq();

println("##############################")
println("###### Finished Lineups ######")
println("##############################")

println("\nCalculated DraftKings baseball lineups.\n\tNumber of lineups = ", num_lineups, " \n\tStack size = ",stack_size,
"\n\tOverlap = ", num_overlap,"\n" )

println("Took ", telapsed/60.0, " minutes to calculate ", num_lineups, " lineups")

println("Saving data to file ",path_to_output)

#save the projected and actual points for the lineups
#lineup_points_proj(path_to_output,path_hitters,path_pitchers,path_to_output_proj);
#lineup_points_actual(path_to_output,path_hitters,path_pitchers,path_to_output_actual);

