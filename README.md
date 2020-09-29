# Reproducibility-in-Cricket

## Package Overview

waRne is contains the play-by-play data from T20 and ODI cricket matches, and in the future will contain Test play-by-play data as well. We have tested this out by scraping from the 2010 season to the 2019/2020 season on ESPNcricinfo.com, up to the September T20I series between Australia and England. Rather than leave the scraping code and force cricket enthusiasts to scrape the data themselves, we have attached a data set containing our scraped data, and the code for scraping more. Below is a guide for loading in the package, the data, and using the scraping functions. 

We grabbed the play-by-play data and the personal webpages for all the players in our data set. Combining these datasets together is done by joining on the common player id in each. For example Steve Smith is represented by the numeric id 267192 in both data sets. 

Included is a copy of the ODI ball-by-ball DLS table, found at http://icc-live.s3.amazonaws.com/cms/media/about_docs/542b8685a3693-06%5D%20Duckworth%20Lewis_2014.pdf

## Installation And Loading Data

library(devtools)
install_github(jrt5/waRne)

To install the 3 data sets, (better figure that out)

## Adding New Data To The Existing Data Set

It's simple, left join

## Scraping New Data

To scrape new data you will need to download docker, available free at https://www.docker.com/. This is enable you to load webpages in containers, which allow for button selection and scrolling. This will be necessary as the commentary webpage requires scrolling and clicking of dropdown menus to load all the data for a given game. The various functions have been condensed into just a few easy to use scrapers, which will create the necessary folders for the data. All that is required is you keep an eye on the scraper, since cricinfo is not completely consistent in the way it structures links, it may be impossible to scrape certain series using the tools provided. However the scraping functions have been commented thoroughly in case modifications need to be made.

We have chosen to ignore games that have abnormal, so included are functions to drop the rows in the master spreadsheet that are problematic. In our experience, these are rarely games in T20I, ODI, or in the Big Bash or IPL T20 leagues, which is where our interest in this data lies.

The code for scraping new data for a season is in the run.scraper.R file, and just replace the current season 20192020 with 2020 when it begins

There is the potential of a broken link affecting the scraper, causing it to stop. In this case, we simply leave the commentary and scorecard links on the master as NA values. If these specific links are important to you, it may be necessary to click each tournament within this broken tournament link on the season archive, and manually add the scorecard and commentary links into the master.csv

<img width="1280" alt="example_of_broken_link" src="https://user-images.githubusercontent.com/44284779/93940508-6de84080-fce1-11ea-8ab2-a55710969989.png">

Alternatively it could be in a format that the scraper wasn't built to handle. In our experience these are minor tours, not the international level games. Usually we remove the row from the master dataset, and drop the tournament.

<img width="1278" alt="Screen Shot 2020-09-23 at 12 30 34 PM" src="https://user-images.githubusercontent.com/44284779/94060304-997a3200-fd98-11ea-9a2e-65af28fd599d.png">
