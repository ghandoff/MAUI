library(foreach)
library(tidyverse)
library(readxl)

source('code/model fitting functions.R')

##### 
# Set up/load files
item_scores <- read_csv('data/dissertation item scores.csv')
participant_scores <- read_csv('data/dissertation participant scores.csv')

items <- unique(item_scores$TypeItem) #item names from frame
item_densities <- foreach(i=items, .combine='rbind') %do% sort_count(item_scores, i)



curve_formula <- formula(MAUI ~ ((d*(norm_rank)^g)/((d*(norm_rank)^g)+(1-norm_rank)^g)))


#' SAVE SMPL_RANKS HERE FOR GRAPHING
#' ggplot(data = smpl_ranks, aes(x = MAUI, y = mass)) + geom_col() + xlim(0,1)
#' ggplot(data = smpl_ranks, aes(x = 1-pct_giving, y = mass)) + geom_col() + geom_vline(aes(xintercept = .95)) + xlim(0,1)
#' 
#FIT GAMMA AND DELTA HERE
fit <- nls(curve_formula, smpl_calcs, start = list(d = .5, g = .6))

s <- filter(all_responses, partID == target_sample$partID[1] & TypeItem == 'U1')
new <- smpl_itemcount 
