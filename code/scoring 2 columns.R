library(openxlsx)
library(doParallel)
library(foreach)
library(tidyverse)
library(readxl)

raw <- read_csv('data/test data.csv')

n <- length(unique(raw$participant))

UI_thresh <- .95

freq_table <- raw %>%
  group_by(response) %>%
  summarise(frequency = n())

mass_table <- freq_table %>%
  group_by(frequency) %>%
  summarise(count = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(mass = frequency*count) %>%
  mutate(cum_mass = cumsum(mass)) %>%
  mutate(MAUI = (cum_mass - mass/2)/max(cum_mass),
         UI = 1 - frequency/n)
  
freq_table <- freq_table %>%
  left_join(select(mass_table, -count, -mass, -cum_mass))

p_response_scores <- raw %>%
  left_join(select(freq_table, -frequency))

p_scores <- p_response_scores %>%
  mutate(UI = if_else(UI >= UI_thresh, 1, 0)) %>%
  group_by(participant) %>%
  summarise(responses = n(),
            MAUI_sum = sum(MAUI),
            UI_sum = sum(UI))