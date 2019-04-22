library(tidyverse)
library(ggplot2)
library(foreach)

#' cdfs for entire sample
score_freq <- read_csv('data/score_frequencies.csv') %>%
  select(-X1)

total_mass <- score_freq %>%
  group_by(TypeItem, sample_size) %>%
  summarise(item_mass = max(cum_mass))

score_freq <- score_freq %>%
  left_join(total_mass) %>%
  mutate(mass_weight = mass/item_mass)

mass_graph <- ggplot(score_freq) +
  facet_grid(rows = vars(TypeItem), cols = vars(sample_size)) +
  geom_line(aes(norm_rank, MAUI), color = 'blue') +
  geom_line(aes(norm_rank, UI), color = 'red') +
  geom_hline(yintercept = .95)

response_trees <- ggplot(score_freq, aes(x = factor(sample_size), y = norm_rank, weight = mass_weight)) +
  geom_violin() +
  facet_grid(rows = vars(TypeItem))

MAUI_trees <- ggplot(score_freq, aes(x = factor(sample_size), y = MAUI, weight = mass_weight)) +
  geom_violin() +
  facet_grid(rows = vars(TypeItem))

UI_trees <- ggplot(score_freq, aes(x = factor(sample_size), y = UI, weight = mass_weight)) +
  geom_violin() +
  facet_grid(rows = vars(TypeItem))

#'histograms for all participants
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

#' histograms for only target sample
#' including overlaying normal distributions
target_scores <- read_csv('data/target_participant_scores.csv') %>%
  select(-X1)

target_grid <- function(meas) {
  val <- enquo(meas)
  ts <- target_scores %>%
    select(TypeItem, sample_size, !!val) %>%
    summarise(max = max(!!val),
              min = min(!!val))
  seq(ts$min, ts$max, length = 100)
}

target_dens_UI <- target_scores %>%
  group_by(TypeItem, sample_size) %>%
  summarise(mean = mean(sum_UI95),
            sd = sd(sum_UI95))

norm_curve <- function(item, size, frame, measure) {
  val <- enquo(measure)
  df <- frame %>%
    filter(TypeItem == item & sample_size == size)
  dnorm(target_grid(!!val), df$mean, df$sd)
}

target_curve_UI <- foreach(i=seq(100, 1000, by=100), .combine = 'rbind') %do% {
  foreach(j=c('F1', 'F2', 'F3', 'I1', 'I2', 'I3', 'U1', 'U2', 'U3'), .combine = 'rbind') %do%
  {data.frame(TypeItem = j,
              sample_size = i,
              sum_UI95 = target_grid(sum_UI95),
              norm_curve = norm_curve(j, i, target_dens_UI, sum_UI95))
              }
  
}

target_UI_hist <- ggplot(target_scores, aes(sum_UI95)) +
  facet_grid(rows = vars(TypeItem), cols = vars(sample_size)) +
  geom_density() +
  geom_line(aes(y = norm_curve), data = target_curve_UI, colour = "red")

target_dens_MAUI <- target_scores %>%
  group_by(TypeItem, sample_size) %>%
  summarise(mean = mean(sum_MAUI),
            sd = sd(sum_MAUI))

target_curve_MAUI <- foreach(i=seq(100, 1000, by=100), .combine = 'rbind') %do% {
  foreach(j=c('F1', 'F2', 'F3', 'I1', 'I2', 'I3', 'U1', 'U2', 'U3'), .combine = 'rbind') %do%
  {data.frame(TypeItem = j,
              sample_size = i,
              sum_MAUI = target_grid(sum_MAUI),
              norm_curve = norm_curve(j, i, target_dens_MAUI, sum_MAUI))
  }
  
}

target_MAUI_hist <- ggplot(target_scores, aes(sum_MAUI)) +
  facet_grid(rows = vars(TypeItem), cols = vars(sample_size)) +
  geom_density() +
  geom_line(aes(y = norm_curve), data = target_curve_MAUI, colour = "red")

target_dens_top5 <- target_scores %>%
  group_by(TypeItem, sample_size) %>%
  summarise(mean = mean(top5_MAUI),
            sd = sd(top5_MAUI))

target_curve_top5 <- foreach(i=seq(100, 1000, by=100), .combine = 'rbind') %do% {
  foreach(j=c('F1', 'F2', 'F3', 'I1', 'I2', 'I3', 'U1', 'U2', 'U3'), .combine = 'rbind') %do%
  {data.frame(TypeItem = j,
              sample_size = i,
              top5_MAUI = target_grid(top5_MAUI),
              norm_curve = norm_curve(j, i, target_dens_top5, top5_MAUI))
  }
  
}

target_top5_hist <- ggplot(target_scores, aes(top5_MAUI)) +
  facet_grid(rows = vars(TypeItem), cols = vars(sample_size)) +
  geom_density() +
  geom_line(aes(y = norm_curve), data = target_curve_top5, colour = "red")