library(tidyverse)
library(readxl)
library(openxlsx)

set.seed(1729)

#' Everything relative to project folder '#
source('code/bootstrapping subsample functions.R')

all_responses <- read_csv('data/Garrett Dissertation Data Answers Only.csv')

#' part_flexbyitem is a P x item frame of flex scores
#' crucially, NA means that the item was not completed by the P
part_flexbyitem <- std_to_flextable(all_responses, 'TypeItem', 'partID', 'FlexCat')

#' column of ID numbers in the target sample
holdout_nums <- part_flexbyitem %>%
  na.omit() %>% #' ensures target sample did all 9 items
  sample_n(100, replace = FALSE) %>%
  select(partID)
#' P x item frame of those remaining
remains_flexbyitem <- part_flexbyitem %>%
  filter(!partID %in% holdout_nums$partID)

N <- 100
focal_item <- 'U1'
curve_formula <- formula(MAUI ~ ((d*(norm_rank)^g)/((d*(norm_rank)^g)+(1-norm_rank)^g)))

smpl <- sample_ids(remains_flexbyitem, focal_item, N) #outputs bootstrap partIDs
boot_list <- bind_rows(holdout_nums, smpl) #participant IDs of holdout sample & resamples


boot_items <- sample_responses(all_responses, boot_list$partID, focal_item) #outputs all responses in entire bootstrap sample
boot_itemcount <- sort_count(boot_items) #outputs counts of bootstrap sample responses
boot_ranks <- ranks(boot_itemcount, N) #outputs MAUI rank table of bootstrap sample responses
boot_calcs <- item_calcs(boot_ranks) #outputs MAUI rank table with 0 and 1 points

boot_itemscores <- boot_itemcount %>%
  left_join(select(boot_ranks, -cumsum, -mass))
boot_scores <- boot_items %>%
  left_join(boot_itemscores, by = 'Std') %>%
  mutate(UI95 = ifelse(pct_giving <= .05, 1, 0)) %>%
  group_by(partID) %>%
  summarise(sum_MAUI = sum(MAUI),
            avg_MAUI = mean(MAUI),
            sum_UI95 = sum(UI95),
            avg_UI95 = mean(UI95))

#' SAVE SMPL_RANKS HERE FOR GRAPHING
#' ggplot(data = smpl_ranks, aes(x = MAUI, y = mass)) + geom_col() + xlim(0,1)
#' ggplot(data = smpl_ranks, aes(x = 1-pct_giving, y = mass)) + geom_col() + geom_vline(aes(xintercept = .95)) + xlim(0,1)
#' 
#FIT GAMMA AND DELTA HERE
fit <- nls(curve_formula, smpl_calcs, start = list(d = .5, g = .6))

s <- filter(all_responses, partID == holdout_nums$partID[1] & TypeItem == 'U1')
new <- smpl_itemcount 



















