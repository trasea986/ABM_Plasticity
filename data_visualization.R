#visualization script for data frames created through output_import_process.R

library(tidyverse)
library(cowplot)

#data_df_final <- readRDS('data_ind.rds')

#convert characters to their correct columns
#note that the genetics really should be a factor, but giving it a number makes downstream calulations easier
data_df_final$L0A0 <-  as.numeric(data_df_final$L0A0)
data_df_final$L0A1 <- as.numeric(data_df_final$L0A1)
data_df_final$L1A0 <-  as.numeric(data_df_final$L1A0)
data_df_final$L1A1 <- as.numeric(data_df_final$L1A1)
data_df_final$PatchID <- as.factor(data_df_final$PatchID)
data_df_final$XCOORD <- as.numeric(data_df_final$XCOORD)
data_df_final$YCOORD <- as.numeric(data_df_final$YCOORD)
data_df_final$CDist <- as.numeric(data_df_final$CDist)
data_df_final$disp <- as.factor(data_df_final$disp)
data_df_final$evo <- as.factor(data_df_final$evo)
data_df_final$run_name <- as.factor(data_df_final$run_name)
data_df_final$rep <- as.factor(data_df_final$rep)
data_df_final$year <- as.numeric(data_df_final$year)
data_df_final$mort <- as.factor(data_df_final$mort)
data_df_final$value <- as.factor(data_df_final$value)

summary(data_df_final)

#going to first see what the total of individuals
population_df <- data_df_final %>%
  group_by(disp, evo, run_name, mort, value, year) %>%
  tally()

#this is a lot much going on, going to split dataframe by mortality type for playing with graphs
population_egg_df <- subset(population_df, mort == 'eggmort')
population_move_df <- subset(population_df, mort == 'movemort')

#going back to full figure for now
ggplot(data=population_df, aes(x=year, y=n, linetype = evo, color = value)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  #geom_point(size = 2) +
  labs(x = "Time (years)", y = "Population Size") +
  facet_wrap(run_name~mort) +
  scale_color_brewer(palette = "Dark2")+
  ylim(0, 18000) +
  theme_bw(base_size = 14) +
  geom_line(linetype= "solid", color = "black", size = 1.25, aes(x=year), y=(15000))


#can also subset to a specific run name and graph

population_dend_df <- subset(population_df, run_name == 'dendbad')

ggplot(data=population_dend_df, aes(x=year, y=n, linetype = evo, color = value)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  geom_point(size = 2) +
  labs(x = "Time (years)", y = "Population Size") +
  facet_wrap(~mort) +
  scale_color_brewer(palette = "Dark2")+
  ylim(0, 16000) +
  theme_bw(base_size = 14) +
  geom_line(linetype= "solid", color = "black", size = 1.25, aes(x=year), y=(15000)) +
  geom_line(linetype= "solid", color = "black", size = 1.25, aes(x=year), y=(0))




#going to make seperate dataframes here for each locus and graph proportion with the plastic allele and whether it is turned on or not
#sum gives the total number of alleles in the population
#note that this is with cdevolvans = 1

data_L1A0 <- data_df_final %>%
  group_by(disp, evo, run_name, mort, value, year) %>%
  count(L1A0, name = "L1A0_count")

data_L1A0_spread <- data_L1A0 %>%
  spread(L1A0, L1A0_count)

#renaming last couple of columns for when merging
colnames(data_L1A0_spread) <- c("disp", "evo", "run_name", "mort", "value", "year", "0.x", "1.x", "2.x")


data_L1A1 <- data_df_final %>%
  group_by(disp, evo, run_name, mort, value, year) %>%
  count(L1A1, name = "L1A1_count")

data_L1A1_spread <- data_L1A1 %>%
  spread(L1A1, L1A1_count)

colnames(data_L1A1_spread) <- c("disp", "evo", "run_name", "mort", "value", "year", "0.y", "1.y", "2.y")

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


#graph

ggplot(data=data_df_L1_gathered, aes(x = year, y = counts, color=Allele, linetype = evo, shape= value)) +
  #geom_point(size=4, alpha=0.55) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25)+
  facet_wrap(run_name~mort) +
  labs(x = "Time (years)", y = "Raw Plastic Allele Count", color="Allele", fill="Allele", shape="Value", linetype = "Dominance") +
  scale_color_brewer(palette = "Dark2")+
  theme_classic(base_size=14)



#now to calculate proportion in the population
data_df_prop <- merge(x=data_df_L1_gathered, y=population_df)
data_df_prop$prop <- data_df_prop$counts / (2*data_df_prop$n)

ggplot(data=data_df_prop, aes(x = year, y = prop, color=Allele, linetype = evo, shape= value)) +
  #geom_point(size=4, alpha=0.55) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25)+
  facet_wrap(run_name~mort) +
  labs(x = "Time (years)", y = "Allele Proportion", color="Allele", fill="Allele", shape="Value", linetype = "Dominance") +
  scale_color_brewer(palette = "Dark2")+
  theme_classic(base_size=14)




#one problem here is to actually see trends. need to add models. starting with one example and then going back to the big graph
df_prop_dend <- subset(data_df_prop, run_name == 'dendbad')

ggplot(data=df_prop_dend, aes(x = year, y = prop, color=Allele, linetype = evo, shape= value)) +
  #geom_point(size=2, alpha=0.55) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  facet_wrap(value~mort) +
  labs(x = "Time (years)", y = "Allele Proportion", color="Allele", fill="Allele", shape="Value", linetype = "Dominance") +
  scale_color_brewer(palette = "Dark2")+
  theme_classic(base_size=14)

df_prop_pan <- subset(data_df_prop, run_name == 'panmix')

ggplot(data=df_prop_pan, aes(x = year, y = prop, color=Allele, linetype = evo, shape= value)) +
  #geom_point(size=2, alpha=0.55) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  facet_wrap(value~mort) +
  labs(x = "Time (years)", y = "Allele Proportion", color="Allele", fill="Allele", shape="Value", linetype = "Dominance") +
  scale_color_brewer(palette = "Dark2")+
  theme_classic(base_size=14)



#going to just look at egg mort...
data_df_prop_egg <- subset(data_df_prop, mort == 'eggmort')

ggplot(data=data_df_prop_egg, aes(x = year, y = prop, color=Allele, linetype = evo, shape= value)) +
  #geom_point(size=4, alpha=0.55) +
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  facet_wrap(~run_name) +
  labs(x = "Time (years)", y = "Allele Proportion", color="Allele", fill="Allele", shape="Value", linetype = "Dominance") +
  scale_color_brewer(palette = "Dark2")+
  theme_classic(base_size=14)

#dropping to one value
data_df_prop_egg_60 <- subset(data_df_prop_egg, value == '60')

ggplot(data=data_df_prop_egg_60, aes(x = year, y = prop, color=Allele, linetype = evo)) +
  #geom_point()+
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  facet_wrap(~run_name) +
  labs(x = "Time (years)", y = "Allele Proportion", color="Allele", fill="Allele", shape="Value", linetype = "Dominance") +
  scale_color_brewer(palette = "Dark2")+
  theme_classic(base_size=14)+
  geom_line(linetype= "solid", color = "black", size = 1.25, aes(x=year), y=(0))
  

#just looking at plastic region, by doing inverse of L1A0

data_df_prop_egg_L1A0 <- subset(data_df_prop_egg, Allele == 'L1A0')

ggplot(data=data_df_prop_egg_L1A0, aes(x = year, y = prop, color=value, linetype = evo)) +
  #geom_point()+
  geom_smooth(method = "lm", se = FALSE, size = 1.25) +
  facet_wrap(~run_name) +
  labs(x = "Time (years)", y = "Allele Proportion", color="Value", fill="Value", shape="Value", linetype = "Dominance") +
  scale_color_brewer(palette = "Dark2")+
  theme_classic(base_size=14)+
  ylim(0.2, 0.3)+
  geom_line(linetype= "solid", color = "black", size = 1.25, aes(x=year), y=(0))






#create new df with calculated change in proportion of non-plastic region??
data_df_prop_change <- subset(data_df_prop, year == '10')
data_df_prop_change <- rbind(subset(data_df_prop, year == '50'), subset(data_df_prop, year == '10'))
data_df_prop_change <- subset(data_df_prop_change, Allele == 'L1A0')


#now that we have only that region, we need to remove the count and n column so when it spreads those differences do not sep. the 0 from 90

data_df_prop_change <- select(data_df_prop_change, -c('counts', 'n'))

data_df_prop_change2 <- data_df_prop_change %>%
  group_by(disp, evo, run_name, mort, value) %>%
  spread(year, prop)

#one challenge is pops that don't make it to the end, so going to set all of those to zero as a quick work-around

data_df_prop_change2[is.na(data_df_prop_change2)] <- 0
data_df_prop_change2$difference <- data_df_prop_change2$'50' - data_df_prop_change2$'10'

ggplot(data_df_prop_change2, aes(x=(difference))) +
  geom_histogram(col="black", fill="green", alpha = .2) +
  facet_wrap(~value) +
  theme_bw()

ggplot(data_df_prop_change2, aes(x=(difference))) +
  geom_histogram(col="black", fill="green", alpha = .2) +
  facet_wrap(~evo) +
  theme_bw()

#now to look at pop and genetic counts spatially

data_df_population_space <- data_df_final %>%
  group_by(XCOORD, YCOORD, disp, evo, run_name, mort, value, year) %>%
  tally()

data_df_population_space_end <- subset(data_df_population_space, year == '90')

data_df_population_space_end_het <- subset(data_df_population_space_end, evo == 'heterzyg')

ggplot(data_df_population_space_end_het, aes(x = XCOORD, y = YCOORD)) + 
  geom_point(aes(color = value, size = n), alpha = 0.2) +
  facet_wrap(mort~run_name) +
  scale_color_manual(values = c("blue", "red")) +
  scale_size(range = c(0.5, 20))+
  theme_bw()

data_df_population_space_end_het_step <- subset(data_df_population_space_end, run_name == 'stepclust')

ggplot(data_df_population_space_end_het_step, aes(x = XCOORD, y = YCOORD)) + 
  geom_point(aes(color = evo, size = n), alpha = 0.1) +
  facet_wrap(value~mort) +
  scale_color_manual(values = c("blue", "red")) +
  scale_size(range = c(1, 30))+
  xlim(-50, 1700)+
  theme_classic()

data_df_population_space_end_het_stepa <- subset(data_df_population_space_end, run_name == 'stepalt')

ggplot(data_df_population_space_end_het_stepa, aes(x = XCOORD, y = YCOORD)) + 
  geom_point(aes(color = evo, size = n), alpha = 0.1) +
  facet_wrap(value~mort) +
  scale_color_manual(values = c("blue", "red")) +
  scale_size(range = c(0.5, 20))+
  xlim(-50, 1700)+
  theme_classic()
