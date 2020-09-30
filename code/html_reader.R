library(tidyverse)
library(RSelenium)
library(XML)
library(rvest)
library(htmltools)
library(lubridate)
library(magrittr)
source("/Users/James/James/Reproducibility-in-Cricket/code/html_reader_functions.R")

# Load in the old data, the DLS table, and the player stat table
load("/Users/James/James/Reproducibility-in-Cricket/data_files/odi_dls.Rdata")
load("/Users/James/James/Reproducibility-in-Cricket/data_files/player_list.Rdata")
load("/Users/James/James/Reproducibility-in-Cricket/data_files/cmsac_data.Rdata")

# We read in by year, and later add it to the dataset we already have. This is for html that you have 
# scraped on your machine only, it does nothing otherise
data2020 <- extract_data_commentary_scorecards(2020, "/Users/James/James/Reproducibility-in-Cricket")

# Fairly easy to add in your newly acquired data after you scrape it, just replace the year.
cmsac_data %>% 
	dplyr::filter(year!=2020) %>% 
	bind_rows(data2020)
