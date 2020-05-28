#script to read in the various PatchVars input files created for CDMetaPOP.
#use the exported list of file names to population the PopVars input.

library(tidyverse)

#set working directory to where PatchVars are kept

setwd("D:/OneDrive/GEM3_PostDoc/Agent-Based-Models/Theoretical_Paper/inputs/subset_test/patchvars")

input_files = list.files(pattern = "PatchVars*", 
                         full.names = TRUE, 
                         recursive = TRUE, 
                         include.dirs = TRUE) %>% 
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=gsub(".csv","",basename(x))))

file_names <- as.data.frame(input_files$filename)

file_names <- unique(file_names)

#go back to the directory where the PopVars file will be held

setwd("D:/OneDrive/GEM3_PostDoc/Agent-Based-Models/Theoretical_Paper/inputs/subset_test")

write.csv(file_names, "PatchVar_List.csv", row.names = FALSE)
