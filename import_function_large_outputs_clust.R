#this is a new script for importing data and calculating the population level statistics after running into some memory issues on the cluster. the idea is to use the existing code, but as a function pointed at a vector of years to input.

#setwd('../inputs') useful bit if you are planning on running just in the parent directory

library(tidyverse)
library(cowplot)
library(future.apply)

time_test_start = Sys.time()
print(time_test_start)

plan(multisession)

#first create patch variable list

#go to input folder, where the output files from CDMetaPop are created


years_to_analyze <- list(10, 50, 110, 160)



#print years function

#this below pulled in many years besides 10?

summary_inds <- function(year) {
  #create patch file
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
  
  print('Patch list created')
  
  
  output_files = list.files(pattern = paste("ind",year,".csv", sep=''), 
                            full.names = TRUE, 
                            recursive = TRUE, 
                            include.dirs = TRUE) %>% 
    map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=paste(dirname(normalizePath(x)),basename(x),sep="/")))
  
  print("Import complete")
  
  #remove columns we don't care about to save space
  
  output_files$ID <- NULL
  output_files$sex <- NULL
  output_files$size <- NULL
  output_files$mature <- NULL
  output_files$newmature <- NULL
  output_files$layeggs <- NULL
  output_files$capture <- NULL
  output_files$infection <- NULL
  output_files$Hindex <- NULL
  output_files$Species <- NULL
  output_files$recapture <- NULL
  output_files$SubPatchID <- NULL
  output_files$L5A0 <- NULL
  output_files$L5A1 <- NULL
  output_files$L6A0 <- NULL
  output_files$L6A1 <- NULL
  output_files$L7A0 <- NULL
  output_files$L7A1 <- NULL
  output_files$L8A0 <- NULL
  output_files$L8A1 <- NULL
  output_files$L9A0 <- NULL
  output_files$L9A1 <- NULL
  #/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/inputs/low_fecun/hom/low_stray/space_24
  #D:/OneDrive/GEM3_PostDoc/Agent-Based-Models/Theoretical_Paper/src/ABM_plasticity_theory/function_test/high_fecun/hom/high_stray/space_16/first_output1599683501/batchrun26mcrun7/ind10.csv
  #102 for testing on local machine
  data_df <- separate(data=output_files, col = filename, into = c('junk','filename'), sep =48)
  
  data_df$junk <- NULL
  
  data_df <- separate(data = data_df, col = filename, into = c('fecun', 'evo', 'stray','space','runinfo','batchmc','year'), sep = "/")
  
  #breakout the year from ind
  data_df <- separate(data = data_df, col = year, into = c('ind', 'year'), sep = "d")
  
  data_df$ind <- NULL
  
  data_df$year <- substr(data_df$year,1,nchar(data_df$year)-4)
  
  #make space into the number of bad patches
  data_df <- separate(data = data_df, col = space, into = c('junk1', 'bad_patch'), sep = "_")
  
  data_df$junk1 <- NULL
  
  #make space into the number of bad patches
  data_df <- separate(data = data_df, col = stray, into = c('stray', 'junk2'), sep = "_")
  
  data_df$junk2 <- NULL
  
  #split the run. right now this isn't important because each run was only a single replicate, but this will help with adding scenario names and averaging if replicates are done
  
  data_df <- separate(data = data_df, col = batchmc, into = c('run', 'rep'), sep = "m")
  data_df <- separate(data = data_df, col = run, into = c('junk3', 'run'), sep = "n")
  data_df <- separate(data = data_df, col = rep, into = c('junk4', 'rep'), sep = "n")
  data_df <- separate(data = data_df, col = fecun, into = c('fecun', 'junk5'), sep = "n")
  #now to remove columns of junk: junk1, junk2, ind. Doing in two steps in case that is useful later

  data_df$junk3 <- NULL
  data_df$junk4 <- NULL
  data_df$junk5 <- NULL
  
  
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
  
  print("Reformat complete")
  
  data_df_final <- left_join(data_df, PopVar_Names2, by = 'run')
  
  data_df_final$L0A0 <-  as.numeric(data_df_final$L0A0)
  data_df_final$L0A1 <- as.numeric(data_df_final$L0A1)
  data_df_final$L1A0 <-  as.numeric(data_df_final$L1A0)
  data_df_final$L1A1 <- as.numeric(data_df_final$L1A1)
  data_df_final$L2A0 <-  as.numeric(data_df_final$L2A0)
  data_df_final$L3A1 <- as.numeric(data_df_final$L3A1)
  data_df_final$L3A0 <-  as.numeric(data_df_final$L3A0)
  data_df_final$L2A1 <- as.numeric(data_df_final$L2A1)
  data_df_final$L2A4 <-  as.numeric(data_df_final$L4A0)
  data_df_final$L2A4 <- as.numeric(data_df_final$L4A1)
  data_df_final$PatchID <- as.factor(data_df_final$PatchID)
  data_df_final$XCOORD <- as.numeric(data_df_final$XCOORD)
  data_df_final$YCOORD <- as.numeric(data_df_final$YCOORD)
  data_df_final$CDist <- as.numeric(data_df_final$CDist)
  data_df_final$stray <- as.factor(data_df_final$stray)
  data_df_final$evo <- as.factor(data_df_final$evo)
  data_df_final$run_name <- as.factor(data_df_final$run_name)
  data_df_final$rep <- as.factor(data_df_final$rep)
  data_df_final$year <- as.numeric(data_df_final$year)
  data_df_final$mort <- as.factor(data_df_final$mort)
  data_df_final$value <- as.factor(data_df_final$value)
  data_df_final$fecun <- as.factor(data_df_final$fecun)
  data_df_final$bad_patch <- as.numeric(data_df_final$bad_patch)
  
  population_df <- data_df_final %>%
    group_by(fecun, stray, evo, run_name, rep, mort, value, year, bad_patch) %>%
    tally()
  
  data_L1A0 <- data_df_final %>%
    group_by(fecun, stray, evo, run_name, rep, mort, value, year, bad_patch) %>%
    count(L1A0, name = "L1A0_count")
  
  data_L1A0_spread <- data_L1A0 %>%
    spread(L1A0, L1A0_count)
  
  #renaming last couple of columns for when merging
  colnames(data_L1A0_spread) <- c("fecun", "stray", "evo", "run_name", "rep", "mort", "value", "year", "bad_patch", "0.x", "1.x", "2.x")
  
  
  data_L1A1 <- data_df_final %>%
    group_by(fecun, stray, evo, run_name, rep, mort, value, year, bad_patch) %>%
    count(L1A1, name = "L1A1_count")
  
  data_L1A1_spread <- data_L1A1 %>%
    spread(L1A1, L1A1_count)
  
  colnames(data_L1A1_spread) <- c("fecun", "stray", "evo", "run_name", "rep", "mort", "value", "year", "bad_patch", "0.y", "1.y", "2.y")
  
  data_df_L1 <- left_join(x=data_L1A0_spread, y=data_L1A1_spread)
  
  
  
  #next sum together across
  
  data_df_L1$L1A0 <- data_df_L1$'0.x' + data_df_L1$'0.y'
  #this will throw an eroor if all switches flipped
  data_df_L1$L1A1 <- data_df_L1$'1.x' + data_df_L1$'1.y'
  data_df_L1$L1A2 <- data_df_L1$'2.x' + data_df_L1$'2.y'
  
  #remove old rows
  data_df_L1$'0.x' <- NULL
  data_df_L1$'0.y' <- NULL
  data_df_L1$'1.x' <- NULL
  data_df_L1$'1.y' <- NULL
  data_df_L1$'2.x' <- NULL
  data_df_L1$'2.y' <- NULL
  
  #it might be of interest to track the presence of the plastic region, by combining 1s and 2s
  
  data_df_pl <- data_df_L1
  data_df_pl$pl_region <- data_df_pl$L1A1 + data_df_pl$L1A2
  
  #gathering the columns of alleles
  data_df_L1_gathered <- gather(data_df_L1, key = "Allele", counts, L1A0:L1A2)
  
  #move from count to the proportion data
  data_df_prop <- merge(x=data_df_L1_gathered, y=population_df)
  data_df_prop$prop <- data_df_prop$counts / (2*data_df_prop$n)
  
  
  #now to do the same thing with a neutral locus for comparison
  
  data_df_L2 <- data_df_final %>%
    group_by(fecun, stray, evo, run_name, rep, mort, value, year, bad_patch) %>%
    summarise_at(vars(L2A0:L2A1), sum, na.rm = TRUE)
  
  #gathering the columns of alleles
  data_df_L2_gathered <- gather(data_df_L2, key = "Allele", counts, L2A0:L2A1)
  
  #move from count to the proportion data
  data_df_neut <- merge(x=data_df_L2_gathered, y=population_df)
  data_df_neut$prop <- data_df_neut$counts / (2*data_df_neut$n)
  
  
  data_df_prop_final <- rbind(data_df_prop, data_df_neut)
  
  #now need to take this and caculate mean n and mean proportion (+sd) over all replicates of the same dataset. this will also shrink output file size
  
  data_df_prop_final <- data_df_prop_final %>%
    group_by(fecun, stray, evo, run_name, mort, value, year, bad_patch, Allele) %>%
    summarise(prop_mean = mean(prop), 
           prop_sd =sd(prop),
           count_mean = mean(n),
           count_sd = sd(n))
  data_df_prop_final

  
  write.table(data_df_prop_final[1,], file = paste("popprop",year,"csv",sep="."), sep = ",", col.names = TRUE, row.names = FALSE, append = TRUE)
  
  write.table(data_df_prop_final[-1,], file = paste("popprop",year,"csv",sep="."), sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)

}



setwd('../inputs/low_fecun/het/high_stray/space_10')
#setwd("D:/OneDrive/GEM3_PostDoc/Agent-Based-Models/Theoretical_Paper/src/ABM_plasticity_theory/function_test/high_fecun/hom/high_stray/space_16")
lapply(years_to_analyze, summary_inds)
#setwd("D:/OneDrive/GEM3_PostDoc/Agent-Based-Models/Theoretical_Paper/src/ABM_plasticity_theory/function_test/low_fecun/het/low_stray/space_10")
#lapply(years_to_analyze, summary_inds)
setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
print('One done')

setwd('../inputs/low_fecun/het/high_stray/space_16')
future_lapply(years_to_analyze, summary_inds)
setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
print('Two done')
# 
# setwd('../inputs/low_fecun/het/high_stray/space_24')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Three done')
# 
# setwd('../inputs/low_fecun/het/low_stray/space_10')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Four done')
# 
# setwd('../inputs/low_fecun/het/low_stray/space_16')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Five done')
# 
# setwd('../inputs/low_fecun/het/low_stray/space_24')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Six done')
# 
# setwd('../inputs/low_fecun/hom/high_stray/space_10')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Seven done')
# 
# setwd('../inputs/low_fecun/hom/high_stray/space_24')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Eight done')
# 
# setwd('../inputs/low_fecun/hom/low_stray/space_10')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Nine done')
# 
# setwd('../inputs/low_fecun/hom/low_stray/space_16')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Ten done')
# 
# setwd('../inputs/low_fecun/hom/low_stray/space_24')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Eleven done')
# 
# setwd('../inputs/low_fecun/hom/high_stray/space_16')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Twelve done')
# 
# setwd('../inputs/high_fecun/hom/high_stray/space_10')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Thirteen done')
# 
# setwd('../inputs/high_fecun/hom/high_stray/space_16')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Fourteen done')
# 
# setwd('../inputs/high_fecun/hom/high_stray/space_24')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Fifteen done')
# 
# setwd('../inputs/high_fecun/hom/low_stray/space_10')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Sixteen done')
# 
# setwd('../inputs/high_fecun/hom/low_stray/space_16')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Seventeen done')
# 
# setwd('../inputs/high_fecun/hom/low_stray/space_24')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Eighteen done')
# 
# setwd('../inputs/high_fecun/het/high_stray/space_10')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Nineteen done')
# 
# setwd('../inputs/high_fecun/het/high_stray/space_16')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Twenty done')
# 
# setwd('../inputs/high_fecun/het/high_stray/space_24')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Twenty one done')
# 
# setwd('../inputs/high_fecun/het/low_stray/space_10')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Twenty two done')
# 
# setwd('../inputs/high_fecun/het/low_stray/space_16')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Twenty three done')
# 
# setwd('../inputs/high_fecun/het/low_stray/space_24')
# lapply(years_to_analyze, summary_inds)
#setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/src')
# print('Twenty four done')

#now to calculat change
#setwd("D:/OneDrive/GEM3_PostDoc/Agent-Based-Models/Theoretical_Paper/src/ABM_plasticity_theory/function_test")
setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/inputs')
pop_files = list.files(pattern = paste("popprop", sep=''), 
                          full.names = TRUE, 
                          recursive = TRUE, 
                          include.dirs = TRUE) %>% 
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")))

print('Imported summary files')

pop_files$Allele <- as.factor(pop_files$Allele)

data_df_prop_L1A0 <- subset(pop_files, Allele == 'L1A0')

data_df_prop_L1A0$Allele <- NULL
data_df_prop_L1A0$year <- as.numeric(data_df_prop_L1A0$year)
data_df_prop_L1A0$prop_mean <- as.numeric(data_df_prop_L1A0$prop_mean)
data_df_prop_L1A0$count_mean <- as.numeric(data_df_prop_L1A0$count_mean)

#then, calculate the change by looking at minimum and maximum year

data_df_change <- data_df_prop_L1A0 %>%
  group_by(fecun, stray, evo, run_name, mort, value, bad_patch) %>%
  filter(year == max(year) | year == min(year)) %>%
  mutate(count_change = max(count_mean) - min(count_mean),
         prop_change = max(prop_mean) - min(prop_mean)) %>%
  slice(which.max(year))

print('Calculated change across min/max years')

write.table(data_df_change, file = paste("change","csv",sep="."), sep = ",", col.names = TRUE, row.names = FALSE, append = TRUE)

data_df_prop_L2A0 <- subset(pop_files, Allele == 'L2A0')

data_df_prop_L2A0$Allele <- NULL
data_df_prop_L2A0$year <- as.numeric(data_df_prop_L2A0$year)
data_df_prop_L2A0$prop_mean <- as.numeric(data_df_prop_L2A0$prop_mean)
data_df_prop_L2A0$count_mean <- as.numeric(data_df_prop_L2A0$count_mean)

#then, calculate the change by looking at minimum and maximum year

data_df_change2 <- data_df_prop_L2A0 %>%
  group_by(fecun, stray, evo, run_name, mort, value, bad_patch) %>%
  filter(year == max(year) | year == min(year)) %>%
  mutate(count_change = max(count_mean) - min(count_mean),
         prop_change = max(prop_mean) - min(prop_mean)) %>%
  slice(which.max(year))

print('Calculated change across min/max years with neutral allele')

write.table(data_df_change2, file = paste("change_neutral","csv",sep="."), sep = ",", col.names = TRUE, row.names = FALSE, append = TRUE)


print('Wrote table of changes')

#print('made plots')


time_test_end = Sys.time()
print(time_test_end)