library(openxlsx)
library(doParallel)
library(foreach)
library(tidyverse)
library(readxl)

#'This codelet operates on a single item

#' input file
#' first column must be named 'participant' and holds participant IDs
#' secon column must be named 'response' and holds the standardized responses
raw <- read_csv('data/test data.csv')

n <- length(unique(raw$participant)) #' calculates number of participants

UI_thresh <- .95 #' the 'old UI' threshhold, default at 95%
top_x <- 5 #' how many of the highest MAUI scores to use for the top X score

#' freq_table holds standardized responses and the frequency they occur in the item (i.e. the size of the fruit)
freq_table <- raw %>%
  group_by(response) %>%
  summarise(frequency = n())

#' mass_table has a bunch of intermediate calculations, and also the scores
#' 'frequency' is how often that response occurs in the item (fruit size)
#' 'count' is how many responses are of that frequency (i.e. how many fruits are on that level)
#' 'mass' is frequency*count (i.e. the total mass of fruit on that level)
#' 'cum_mass' is the mass of responses on that level and below
#' MAUI is calculated from the cum_mass of the level below + half of the current level mass
#' UI is calculated in the standard way from n and UI_thresh
mass_table <- freq_table %>%
  group_by(frequency) %>%
  summarise(count = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(mass = frequency*count) %>%
  mutate(cum_mass = cumsum(mass)) %>%
  mutate(MAUI = (cum_mass - mass/2)/max(cum_mass),
         UI = 1 - frequency/n)

#' appends MAUI and UI to freq_table
freq_table <- freq_table %>%
  left_join(select(mass_table, -count, -mass, -cum_mass))

#' p_response_scores is the original data with MAUI and UI scores appended to each response
p_response_scores <- raw %>%
  left_join(select(freq_table, -frequency))

#' calculates the top X score to append below
top_x_scores <- p_response_scores %>%
  select(-UI) %>%
  arrange(participant, desc(MAUI)) %>%
  group_by(participant) %>%
  slice(seq_len(top_x)) %>%
  summarise(top_x_MAUI = sum(MAUI))

#' p_scores is a summary for each participant
#' current reports fluency, the sum of all MAUI scores and the sum of all UI scores
#' But very adaptable for new scores!
p_scores <- p_response_scores %>%
  mutate(UI = if_else(UI >= UI_thresh, 1, 0)) %>%
  group_by(participant) %>%
  summarise(fluency = n(),
            MAUI_sum = sum(MAUI),
            UI_sum = sum(UI)) %>%
  left_join(top_x_scores)