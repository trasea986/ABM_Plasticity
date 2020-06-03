#script for creating the data frame from the output files
#you need to have the PopVar_Names object from the "create_input_list.R" to quickyly get the run names in to the dataframe


library(tidyverse)
library(cowplot)

#go to input folder, where the output files from CDMetaPop are created

setwd("D:/OneDrive/GEM3_PostDoc/Agent-Based-Models/Theoretical_Paper/inputs")

#create list of files, pulling in individuals output file
#because of R memory issues only pulling in every 10 years
#if simulating for much longer I may need to bring in dispersal or het/hom independently then combine dfs in R

output_files = list.files(pattern = "ind.0|ind0", 
                         full.names = TRUE, 
                         recursive = TRUE, 
                         include.dirs = TRUE) %>% 
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=gsub(".csv","",file.path(x))))


#create year column from file directory

data_df <- separate(data = output_files, col = filename, into = c('dot', 'disp', 'evo', 'run','year'), sep = "/")

#before doing more column manipulation, going to remove folders not of interest

data_df <- subset(data_df, disp != 'subset_test')

#breakout the year from ind
data_df <- separate(data = data_df, col = year, into = c('ind', 'year'), sep = "d")

#break the evolution state
data_df <- separate(data = data_df, col = evo, into = c('evo', 'junk1', 'junk2'), sep = "_")

#split the run. right now this isn't important because each run was only a single replicate, but this will help with adding scenario names and averaging if replicates are done

data_df <- separate(data = data_df, col = run, into = c('run', 'rep'), sep = "m")
data_df <- separate(data = data_df, col = run, into = c('junk3', 'run'), sep = "n")
data_df <- separate(data = data_df, col = rep, into = c('junk4', 'rep'), sep = "n")
#now to remove columns of junk: dot, junk1, junk2, ind. Doing in two steps in case that is useful later

junk <- c('dot', 'junk1', 'junk2', 'junk3', 'junk4', 'ind')
data_df <- data_df[ , !(names(data_df) %in% junk)]




#next step is to replace the run value with each. going to add row numbers for join and split up name to the different factors. Note PopVar_Names comes from the create_inpute_list.R
PopVar_Names2 <- separate(data = PopVar_Names, col = Final, into = c('junk', 'run'), sep = "/")

PopVar_Names2 <- separate(data = PopVar_Names2, col = run, into = c('run', 'junk'), sep = ".cs")

#splitting run is a bit more complicated. Might need to change my naming convention in the future so that the same number of "_" is used for each run name. Manually changing for now...

PopVar_Names2[25, 1] = 'PatchVars_pan_mix_eggmort_20'
PopVar_Names2[26, 1] = 'PatchVars_pan_mix_eggmort_60'
PopVar_Names2[27, 1] = 'PatchVars_pan_mix_movemort_20'
PopVar_Names2[28, 1] = 'PatchVars_pan_mix_movemort_60'

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

data_df_final <- left_join(data_df, PopVar_Names2, by = 'run')
