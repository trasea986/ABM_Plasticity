#now to calculat change
#setwd("D:/OneDrive/GEM3_PostDoc/Agent-Based-Models/Theoretical_Paper/src/ABM_plasticity_theory/function_test")

setwd('/mnt/ceph/seaborn/ABM_GEM3/Theory_Paper/inputs')
pop_files = list.files(pattern = paste("popprop", sep=''), 
                          full.names = TRUE, 
                          recursive = TRUE, 
                          include.dirs = TRUE) %>% 
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")))

print('Imported summary files')

write.table(pop_files, file = paste("all_pop","csv",sep="."), sep = ",", col.names = TRUE, row.names = FALSE, append = FALSE)