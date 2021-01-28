#for use after combining and summarizing outputs on the cluster (done when running lots of models and running out of memory)

library(tidyverse)
library(cowplot)

all_pop <- read.csv("D:/OneDrive/GEM3_PostDoc/Agent-Based-Models/Theoretical_Paper/outputs/all_pop.csv")

summary(all_pop)

all_pop$stray <- as.factor(all_pop$stray)
all_pop$evo <- as.factor(all_pop$evo)
all_pop$fecun <- as.factor(all_pop$fecun)
all_pop$Allele <- as.factor(all_pop$Allele)
all_pop$bad_patch <- as.factor(all_pop$bad_patch)
all_pop$mort <- NULL
all_pop$value <- as.factor(all_pop$value)
all_pop$run_name <- as.factor(all_pop$run_name)

#note that some outputs are old/misplaced because of med fec, so removing here
all_pop <- all_pop %>%
  filter(run_name != "ListNA")

#checking to make sure all are n = 5
count_check <- all_pop %>%
  group_by(year, count_mean, fecun, value, run_name, bad_patch, stray, evo) %>%
  tally()

summary(count_check)

pop_fig <- ggplot(data=all_pop, aes(x=year, y=count_mean, linetype = fecun, color = value)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.25) +
  #geom_point(size = 1) +
  labs(x = "Time (years)", y = "Population Size") +
  facet_wrap(run_name~bad_patch) +
  scale_color_brewer(palette = "Dark2")+
  theme_bw(base_size = 16)
pop_fig

#there is a lot going on here. going to focus on dend, panmixia and step by themselves
dend_pop <- all_pop %>%
  filter(run_name == "dendbad" | run_name == "dendgood" | run_name == "dendsep")

pan_pop <- all_pop %>%
  filter(run_name == "panmix")

step_pop <- all_pop %>%
  filter(run_name == "stepalt" | run_name == "stepclust")

dend_fig <- ggplot(data=dend_pop, aes(x=year, y=count_mean, linetype = fecun, color = value, shape = run_name)) +
  geom_line() +
  geom_point(size = 2.5) +
  labs(x = "Time (years)", y = "Population Size") +
  facet_wrap(evo~bad_patch) +
  scale_color_brewer(palette = "Dark2")+
  ggtitle("Dend") +
  theme_bw(base_size = 16)
dend_fig



pan_fig <- ggplot(data=pan_pop, aes(x=year, y=count_mean, linetype = fecun, color = value, shape = stray)) +
  geom_line() +
  geom_point(size = 3) +
  labs(x = "Time (years)", y = "Population Size") +
  facet_wrap(evo~bad_patch) +
  scale_color_brewer(palette = "Dark2")+
  ggtitle("Pan") +
  theme_bw(base_size = 16)
pan_fig



step_fig <- ggplot(data=step_pop, aes(x=year, y=count_mean, linetype = fecun, color = value, shape = run_name)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.25) +
  geom_point(size = 2.5) +
  labs(x = "Time (years)", y = "Population Size") +
  facet_wrap(evo~bad_patch) +
  scale_color_brewer(palette = "Dark2")+
  ggtitle("Step") +
  theme_bw(base_size = 16)
step_fig

#stray and name means really almost ned another axis, or something besides shape color and ine type... really not sure. One option is simply to not worry, and do smooth?

step_fig <- ggplot(data=step_pop, aes(x=year, y=count_mean, color = value)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.25) +
  geom_point(size = 2.5) +
  labs(x = "Time (years)", y = "Population Size") +
  facet_wrap(run_name~bad_patch) +
  scale_color_brewer(palette = "Dark2")+
  ggtitle("Step") +
  theme_bw(base_size = 16)
step_fig

#anyways, proportion of the two allele types is really what matters.
#let's lump everything together

#first, split the allele name so the number is second, so that I can facet by locus and color by allele
all_pop <- separate(data=all_pop, col = Allele, into = c('locus','allele'), sep = "A")
evo_prop <- all_pop %>%
  filter(allele == 0)

evo_prop$locus <- factor(evo_prop$locus)

prop <- ggplot(data=evo_prop, aes(x=year, y=prop_mean, color = locus)) +
  geom_smooth(method = "loess", se = TRUE, size = 1.25) +
  labs(x = "Time (years)", y = "Population Size") +
  scale_color_brewer(palette = "Dark2")+
  ggtitle("Prop All pop") +
  theme_bw(base_size = 16)
prop

evo_prop_diff <- evo_prop %>%
  select(year, prop_mean, fecun, value, run_name, bad_patch, stray, evo, locus)%>%
  spread(locus, prop_mean) %>%
  mutate(diff = L2 - L1)

prop_dif <- ggplot(data=evo_prop_diff, aes(x=year, y=diff)) +
  geom_smooth(method = "loess", se = TRUE, size = 1.25) +
  facet_wrap(~fecun) +
  labs(x = "Time (years)", y = "Neutral - Not Plastic Region Frequency") +
  scale_color_brewer(palette = "Dark2")+
  theme_bw(base_size = 16)
prop_dif
