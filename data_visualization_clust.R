#setting this up to rerun on the cluster after the output input is done. needs to be run in R because the resulting df from the input process is too large to export on the cluster

library(tidyverse)
library(cowplot)

data_df_final$L0A0 <-  as.numeric(data_df_final$L0A0)
data_df_final$L0A1 <- as.numeric(data_df_final$L0A1)
data_df_final$L1A0 <-  as.numeric(data_df_final$L1A0)
data_df_final$L1A1 <- as.numeric(data_df_final$L1A1)
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
  group_by(stray, evo, run_name, rep, mort, value, year, bad_patch) %>%
  tally()

pop_fig <- ggplot(data=population_df, aes(x=year, y=n, linetype = evo, color = value)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.25) +
  geom_point(size = 1) +
  labs(x = "Time (years)", y = "Population Size") +
  facet_wrap(run_name~bad_patch) +
  scale_color_brewer(palette = "Dark2")+
  ylim(0, 30000) +
  theme_bw(base_size = 16)
  #geom_line(linetype= "solid", color = "black", size = 1.25, aes(x=year), y=(15000))
pop_fig


ggsave(file = "pop_fig.pdf", last_plot(), width = 20, height = 20)