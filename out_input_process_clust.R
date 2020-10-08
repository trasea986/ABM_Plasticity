#script for creating the data frame from the output files
#you need to have the PopVar_Names object from the "create_input_list.R" to quickyly get the run names in to the dataframe

library(tidyverse)
library(cowplot)

time_test_start = Sys.time()
print(time_test_start)

#go to input folder, where the output files from CDMetaPop are created

input_files = list.files(pattern = "PatchVars*", 
                         full.names = TRUE, 
                         recursive = TRUE, 
                         include.dirs = TRUE) %>% 
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=gsub(".csv","",basename(x))))

file_names <- as.data.frame(input_files$filename)

file_names <- rename(file_names, Name = 'input_files$filename')

file_names <- unique(file_names)

file_names$Name <- as.character(file_names$Name)

#now to add the prefix needed for the PopVars file, and then combine the two

#go back to the directory where the PopVars file will be held

file_names$prefix <- c('patchvars/')

file_names$suffix <- c('.csv')

PopVar_Names <- file_names %>%
  unite(Final, prefix, Name, suffix, sep = "")

print('PopVar_Names has been created')

#create list of files, pulling in individuals output file
#because of R memory issues only pulling in every 10 years
#if simulating for much longer I may need to bring in dispersal or het/hom independently then combine dfs in R

#original "ind.0|ind0"
#changed to every 20 due to memory issues

output_files = list.files(pattern = "ind10|ind30|ind50|ind70|ind90|ind110|ind130|ind150|ind170|ind190", 
                          full.names = TRUE, 
                          recursive = TRUE, 
                          include.dirs = TRUE) %>% 
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=gsub(".csv","",file.path(x))))

print('output files imported')

data_df <- separate(data = output_files, col = filename, into = c('dot', 'fecun', 'evo', 'stray','space','runinfo','batchmc','year'), sep = "/")

#breakout the year from ind
data_df <- separate(data = data_df, col = year, into = c('ind', 'year'), sep = "d")

#old code break the evolution state
#data_df <- separate(data = data_df, col = evo, into = c('evo', 'junk1', 'junk2'), sep = "_")

#make space into the number of bad patches
data_df <- separate(data = data_df, col = space, into = c('junk1', 'bad_patch'), sep = "_")

#make space into the number of bad patches
data_df <- separate(data = data_df, col = stray, into = c('stray', 'junk2'), sep = "_")

#split the run. right now this isn't important because each run was only a single replicate, but this will help with adding scenario names and averaging if replicates are done

data_df <- separate(data = data_df, col = batchmc, into = c('run', 'rep'), sep = "m")
data_df <- separate(data = data_df, col = run, into = c('junk3', 'run'), sep = "n")
data_df <- separate(data = data_df, col = rep, into = c('junk4', 'rep'), sep = "n")
data_df <- separate(data = data_df, col = fecun, into = c('fecun', 'junk5'), sep = "n")
#now to remove columns of junk: dot, junk1, junk2, ind. Doing in two steps in case that is useful later

junk <- c('dot', 'junk1', 'junk2', 'junk3', 'junk4', 'junk5', 'ind')
data_df <- data_df[ , !(names(data_df) %in% junk)]

print('columns fixed')


#next step is to replace the run value with each. going to add row numbers for join and split up name to the different factors. Note PopVar_Names comes from the create_inpute_list.R
PopVar_Names2 <- separate(data = PopVar_Names, col = Final, into = c('junk', 'run'), sep = "/")

PopVar_Names2 <- separate(data = PopVar_Names2, col = run, into = c('run', 'junk'), sep = ".cs")

PopVar_Names2 <- separate(data = PopVar_Names2, col = run, into = c('junk','name1', 'name2', 'mort', 'value'), sep = "_")

#now to bring the names back together. again, this may change if I am smarter about the naming convention

PopVar_Names2$run_name<- with(PopVar_Names2, paste0(name1, name2))

#and delete junk. again, doing in two steps for now, but may change later if I need to track this less

junk2 <- c('junk', 'name1', 'name2')
PopVar_Names2 <- PopVar_Names2[ , !(names(PopVar_Names2) %in% junk2)]

#so now we need to bind the names from PopVar_Names2 to the data_df by joining row_num to run. also note CMP starts at 0 for run.
PopVar_Names2$run <- seq.int(nrow(PopVar_Names2))
PopVar_Names2$run <- PopVar_Names2$run - 1
PopVar_Names2$run <- as.character(PopVar_Names2$run)
#data_df$run <- as.factor(data_df$run)

print('PopVar_Names2 complete')

data_df_final <- left_join(data_df, PopVar_Names2, by = 'run')

print('data joined')

#removed making csv, ran out of memory on Marvin
#write.table(data_df_final, "all_inds.csv", row.names = FALSE)
#print('all individuals written to file')

#saveRDS(data_df_final, file = "data_ind.rds")
#print('RDS file saved')


#exit <- function() {
#  .Internal(.invokeRestart(list(NULL, NULL), NULL))
#}

end_time_final <- Sys.time()
print(end_time_final)
total_time <- end_time_final - time_test_start
print(total_time)

print("this is the last message")
#exit()
#print("you should not see this")