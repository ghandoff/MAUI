library(tidyverse)
library(ggplot2)

score_freq <- read_csv('data/score_frequencies.csv') %>%
  select(-X1)

mass_graph <- ggplot(score_freq) +
  geom_line(aes(norm_rank, MAUI), color = 'blue') +
  geom_line(aes(norm_rank, UI), color = 'red') +
  facet_grid(rows = vars(TypeItem), cols = vars(sample_size))

p_scores <- read_csv('data/participant_scores.csv') %>%
  select(-X1)

p_UI_hist <- ggplot(p_scores, aes(sum_UI95)) +
  geom_histogram() +
  facet_grid(rows = vars(TypeItem), cols = vars(sample_size))

p_MAUI_hist <- ggplot(p_scores, aes(sum_MAUI)) +
  geom_histogram() +
  facet_grid(rows = vars(TypeItem), cols = vars(sample_size))

p_topMAUI_hist <- ggplot(p_scores, aes(top5_MAUI)) +
  geom_histogram() +
  facet_grid(rows = vars(TypeItem), cols = vars(sample_size))