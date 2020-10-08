#script to read in the various PatchVars input files created for CDMetaPOP.
#use the exported list of file names to population the PopVars input.

library(tidyverse)

#set working directory to where PatchVars are kept

setwd("D:/OneDrive/GEM3_PostDoc/Agent-Based-Models/Theoretical_Paper/outputs")

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



write.csv(PopVar_Names, "PatchVar_List.csv", row.names = FALSE)
