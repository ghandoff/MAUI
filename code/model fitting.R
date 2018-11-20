library(MASS)
library(foreach)
library(tidyverse)
library(readxl)

#source('code/model fitting functions.R')

##### 
# Set up/load files
item_score_frequencies <- read_csv('data/dissertation score frequencies.csv')
participant_scores <- read_csv('data/dissertation participant scores.csv')
participant_ids <- read_csv('data/bootstrapped participant ids.csv')
target_ids <- filter(participant_ids, sample_size == 100)
target_scores <- filter(participant_scores, partID %in% target_ids$partID)

items <- unique(item_score_frequencies$TypeItem) #item names from frame
samples <- unique(item_score_frequencies$sample_size) #sample sizes from frame

#####
# makes calcs for and graphs density plots
item_densities <- foreach(i=items, .combine='rbind') %:%
  foreach(j=samples, .combine='rbind') %do% {
    hold <- item_score_frequencies %>%
      filter(TypeItem == i & sample_size == j)
    max_rank <- nrow(hold)
    #ans <- max(cumsum)
    
    densities <- hold %>%
      mutate(rank = rank(-count)) %>%
      mutate(UI95 = case_when(pct_giving <= .05 ~ 1,
                              TRUE ~ 0)) %>%
      mutate(norm_rank = (rank-0.5)/max_rank,
             norm_mass = cumsum/max(cumsum))
  }

MAUI_item_density_plot <- ggplot(data = item_densities) +
  geom_line(mapping = aes(x=norm_rank, y=MAUI)) +
  facet_grid(sample_size ~ TypeItem)

UI95_item_density_plot <- ggplot(data = item_densities) +
  geom_line(mapping = aes(x=norm_mass, y=UI95)) +
  facet_grid(sample_size ~ TypeItem)

MAUI_step_plot <- ggplot(data = item_densities) +
  geom_step(mapping = aes(x=norm_mass, y=MAUI), direction='vh') +
  facet_grid(sample_size ~ TypeItem)

UI95_step_plot <- ggplot(data = item_densities) +
  geom_step(mapping = aes(x=norm_mass, y=UI95)) +
  facet_grid(sample_size ~ TypeItem)

MAUI_item_hist <- qplot(MAUI, data=item_densities, weight=mass, geom="histogram") +
  facet_grid(sample_size ~ TypeItem)

# UI95_item_hist is basically identical to the step plot, as it only takes 2 values

#####
# makes calcs for and graphs participant score histograms

#' normal curve generation
MAUI_grid <- with(participant_scores, seq(min(top5_MAUI), max(top5_MAUI), length = 100))

MAUI_normdens <- participant_scores %>%
  select(one_of(c('top5_MAUI', 'TypeItem', 'sample_size'))) %>%
  group_by_at(vars(one_of('TypeItem', 'sample_size'))) %>%
  do(data.frame(top5_MAUI = MAUI_grid,  density = dnorm(MAUI_grid, mean(.$top5_MAUI), sd(.$top5_MAUI))))

MAUI_participant_hist <- ggplot(data = participant_scores, aes(top5_MAUI)) +
  geom_density() +
  geom_line(data=MAUI_normdens, aes(y=density), colour='red') +
  facet_grid(sample_size ~ TypeItem)

UI95_participant_hist <- ggplot(data = participant_scores, aes(sum_UI95)) +
  geom_histogram(binwidth=2) +
  facet_grid(sample_size ~ TypeItem)

joint_partipant_hist <- ggplot(data=participant_scores) +
  geom_histogram(aes(sum_MAUI), binwidth=2) +

#####
# target sample vizualizations
MAUI_target_hist <- ggplot(data = target_scores, aes(sum_MAUI)) +
  geom_histogram() +
  facet_grid(sample_size ~ TypeItem)

UI95_target_hist <- ggplot(data = target_scores, aes(sum_UI95)) +
  geom_histogram() +
  facet_grid(sample_size ~ TypeItem)

target_scores_summary <- target_scores %>%
  group_by(TypeItem, sample_size) %>%
  summarise(sum_MAUI_mean = mean(sum_MAUI),
            sum_MAUI_var = var(sum_MAUI),
            avg_MAUI_mean = mean(avg_MAUI),
            avg_MAUI_var = var(avg_MAUI),
            sum_UI95_mean = mean(sum_UI95),
            sum_UI95_var = var(sum_UI95),
            avg_UI95_mean = mean(avg_UI95),
            avg_UI95_var = var(avg_UI95))




curve_formula <- formula(MAUI ~ ((d*(norm_rank)^g)/((d*(norm_rank)^g)+(1-norm_rank)^g)))


#' SAVE SMPL_RANKS HERE FOR GRAPHING
#' ggplot(data = smpl_ranks, aes(x = MAUI, y = mass)) + geom_col() + xlim(0,1)
#' ggplot(data = smpl_ranks, aes(x = 1-pct_giving, y = mass)) + geom_col() + geom_vline(aes(xintercept = .95)) + xlim(0,1)
#' 
#FIT GAMMA AND DELTA HERE
fit <- nls(curve_formula, smpl_calcs, start = list(d = .5, g = .6))

s <- filter(all_responses, partID == target_sample$partID[1] & TypeItem == 'U1')
new <- smpl_itemcount 
