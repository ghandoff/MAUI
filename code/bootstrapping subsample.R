library(openxlsx)
library(foreach)
library(tidyverse)
library(readxl)

set.seed(1729)

source('code/bootstrapping subsample functions.R')

##### 
# Set up/load files
all_responses <- read_csv('data/Garrett Dissertation Data Answers Only.csv')
all_responses$Std <- str_replace_all(all_responses$Std, "[^[:alnum:]]", " ") %>% #gets rid of non alphanumerics
  tolower() #' turns everything to lowercase
#' part_flexbyitem is a P x item frame of flex scores
#' crucially, NA means that the item was not completed by the P
part_flexbyitem <- std_to_flextable(all_responses, 'TypeItem', 'partID', 'FlexCat')

#####
# Randomization for target & bootstrap samples
# Also sets up dfs with bootstrap participants & their responses
#' column of ID numbers in the target sample
target_sample <- part_flexbyitem %>%
  na.omit() %>% #' ensures target sample did all 9 items
  sample_n(100, replace = FALSE) %>%
  select(partID)
#' P x item frame of those remaining
remains_flexbyitem <- part_flexbyitem %>%
  filter(!partID %in% target_sample$partID)
#' creates entire list of bootstrapped partIDs
#' note that not all of them have done all items
#' This line should be changed if we don't want each resample to be fully random
boot_parts <- foreach(i = seq(100, 800, by=100), .combine='rbind') %do% boot_nums(remains_flexbyitem, i)

#####
# helper formulas, probably can be removed later
items <- names(part_flexbyitem)[-1] #item names from frame
N <- 100
curve_formula <- formula(MAUI ~ ((d*(norm_rank)^g)/((d*(norm_rank)^g)+(1-norm_rank)^g)))

#####
# scoring of responses, probably will be turned into a function later
#' creates response tables
ps_for_scoring <- bind_rows(target_sample, filter(boot_parts, n==0)) #participant IDs of holdout sample & resamples, n==0 is just target sample
boot_responses <- all_responses %>%
  filter(partID %in% ps_for_scoring$partID) #'outputs all responses for all items on ps_for_scoring
boot_response_scores <- foreach(i = items, .combine='rbind') %do% sort_count(boot_responses, i) #' returns response count for standardized responses in a single df
boot_ranks <- foreach(i = items, .combine='rbind') %do% ranks(boot_response_scores, 100, i) #outputs MAUI rank table of bootstrap sample responses
#boot_calcs <- item_calcs(boot_ranks) #outputs MAUI rank table with 0 and 1 points

boot_response_scores <- foreach(i = items, .combine='rbind') %do% 
  append_scores(boot_response_scores, boot_ranks, i) #' appends scores

boot_participant_scores <- foreach(i = items, .combine='rbind') %do% 
  p_score(boot_responses, boot_response_scores, i) #' calculates participant scores


#####



#' SAVE SMPL_RANKS HERE FOR GRAPHING
#' ggplot(data = smpl_ranks, aes(x = MAUI, y = mass)) + geom_col() + xlim(0,1)
#' ggplot(data = smpl_ranks, aes(x = 1-pct_giving, y = mass)) + geom_col() + geom_vline(aes(xintercept = .95)) + xlim(0,1)
#' 
#FIT GAMMA AND DELTA HERE
fit <- nls(curve_formula, smpl_calcs, start = list(d = .5, g = .6))

s <- filter(all_responses, partID == target_sample$partID[1] & TypeItem == 'U1')
new <- smpl_itemcount 



















