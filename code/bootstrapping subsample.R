library(openxlsx)
library(doParallel)
library(foreach)
library(tidyverse)
library(readxl)

set.seed(1729) #' 91 works too, but is less interesting.

source('code/bootstrapping subsample functions.R')

##### 
# Set up/load files
all_responses <- read_csv('data/Garrett Dissertation Data Answers Only.csv')
all_responses$Std <- str_replace_all(all_responses$Std, "[^[:alnum:]]", " ") %>% #gets rid of non alphanumerics
  tolower() #' turns everything to lowercase
#' part_fluency_byitem is a P x item frame of fluency scores
#' crucially, NA means that the item was not completed by the P
part_fluency_byitem <- std_to_fluency_table(all_responses, 'TypeItem', 'partID', 'Std')

#####
# Randomization for target & bootstrap samples
# Also sets up dfs with bootstrap participants & their responses
#' column of ID numbers in the target sample
target_sample <- part_fluency_byitem %>%
  na.omit() %>% #' ensures target sample did all 9 items
  sample_n(100, replace = FALSE) %>%
  select(partID)
#' P x item frame of those remaining
remains_fluency_byitem <- part_fluency_byitem %>%
  filter(!partID %in% target_sample$partID)
#' creates entire list of bootstrapped partIDs
#' note that not all of them have done all items
#' This line should be changed if we don't want each resample to be fully random
boot_parts <- foreach(i = seq(100, 800, by=100), .combine='rbind') %do% boot_nums(remains_fluency_byitem, i)
#' establishing participant list for scoring for loop
target_parts <- mutate(target_sample, n = 100)
remain_parts <- select(remains_fluency_byitem, partID) %>% setdiff(target_sample) %>% mutate(n = 1000)
boot_parts <- bind_rows(target_parts, boot_parts, remain_parts)

items <- names(part_fluency_byitem)[-1] #item names from frame

#####
# scoring functionality!

#' custom function to combine results from for loop
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

#' score_frames returns 2 lists
#' list 1 is a list of 10 response-level tibbles, 1 for each resample N
#' list 2 is a list of 10 participant-level tibbles, 1 for each resample N
score_frames <- foreach(i=seq(100, 1000, by=100), .combine='comb', .multicombine=TRUE,
                        .init=list(list(), list())) %dopar% {
                          
                          ps_for_scoring <- bind_rows(target_sample, filter(boot_parts, n==i | n==100)) #participant IDs of holdout sample & resamples
                          boot_responses <- all_responses %>%
                            filter(partID %in% ps_for_scoring$partID) #'outputs all responses for all items on ps_for_scoring
                          boot_response_scores <- foreach(j = items, .combine='rbind') %do%
                            sort_count(boot_responses, j) #' returns response count for standardized responses in a single df
                          boot_ranks <- foreach(k = items, .combine='rbind') %do%
                            ranks(boot_response_scores, i, k) %>% #outputs MAUI rank table of bootstrap sample responses
                            item_calcs() #outputs MAUI rank table with 0 and 1 points
                          
                          boot_response_scores <- foreach(l = items, .combine='rbind') %do% 
                            append_scores(boot_response_scores, boot_ranks, l) %>% #' appends scores
                            mutate(sample_size = as.integer(i)) #' adds indicator of bootstrap size; not important for public use tool
                          
                          boot_response_freq <- boot_ranks %>%
                            mutate(sample_size = as.integer(i)) #' adds indicator of bootstrap size; not important for public use tool
                          
                          boot_participant_scores <- foreach(m = items, .combine='rbind') %do% 
                            p_score(boot_responses, boot_response_scores, m) %>% #' calculates participant scores
                            mutate(sample_size = as.integer(i)) #' adds indicator of bootstrap size; not important for public use tool
                          
                          list(boot_response_freq, boot_participant_scores)
                        }

#' score_frames is a list of 2 tibbles
#' [[1]] is all the item scores, [[2]] is all the participant scores
score_frames[[1]] <- bind_rows(score_frames[[1]])
score_frames[[2]] <- bind_rows(score_frames[[2]])
names(score_frames) <- c('item_scores', 'participant_scores')