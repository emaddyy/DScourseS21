# ECON 5253
# Ethan Maddy
# 3/9/2021
# PS5

# Question 3

library(xml2)
library(tidyverse)
library(polite)
library(rvest)


ghi <- read_html("https://en.wikipedia.org/wiki/Global_Hunger_Index") 

#  mw-content-text > div.mw-parser-output > table.wikitable.centered.sortable.jquery-tablesorter

ghi_rank <- 
  ghi %>%
  html_nodes("div.mw-parser-output > table.wikitable.centered.sortable.jquery-tablesorter") # %>%
# '[[' (1) %>%
  # html_table()
ghi_rank


# Question 4

# FRED (Oklahoma Unemployment Rate since 1990)
library(tidyverse)
library(fredr)

OK_UNRATE <- fredr(
  series_id = "OKUR",
  observation_start = as.Date("1990-02-17"), # my birthday
  observation_end = as.Date("2020-02-17") # right before the pandemic 
)

plot(OK_UNRATE$value)
view(OK_UNRATE)
