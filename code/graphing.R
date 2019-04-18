library(tidyverse)
library(ggplot2)

data <- read_csv('data/score_frequencies.csv') %>%
  select(-X1)


mass_graph <- ggplot(mass_table) +
  geom_line(aes(norm_rank, MAUI), color = 'blue') +
  #geom_line(aes(norm_rank, UI), color = 'red') +
  facet_grid(rows = TypeItem, cols = sample_size)