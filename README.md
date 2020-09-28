# Reproducibility-in-Cricket

## Package Overview

waRne is contains the play-by-play data from T20 and ODI cricket matches, and in the future will contain Test play-by-play data as well. We have tested this out by scraping from the 2010 season to the 2019/2020 season on ESPNcricinfo.com, up to the September T20I series between Australia and England. Rather than leave the scraping code and force all cricket enthusiasts to scrape the data themselves, we have attached a dataset containing our scraped data, and the code for scraping more. Below is a guide for loading in the package, the data, and using the scraping functions. 

We grabbed the play-by-play data and the personal webpages for all the players in our data set. Combining these datasets together is done by joining on the common player id in each. For example Steve Smith is represented by the numeric id 267192 in both data sets. 

Included is a copy of the ODI ball-by-ball DLS table, found at http://icc-live.s3.amazonaws.com/cms/media/about_docs/542b8685a3693-06%5D%20Duckworth%20Lewis_2014.pdf

## Installation And Loading Data

library(devtools)
install_github(jrt5/waRne)

To install the 3 data sets, (better figure that out)

There is the potential of a broken link affecting the scraper, causing it to stop. In this case, we simply leave the commentary and scorecard links on the master as NA values. If these specific links are important to you, it may be necessary to click each tournament within this broken tournament link on the season archive, and manually add the scorecard and commentary links into the master.csv

<img width="1280" alt="example_of_broken_link" src="https://user-images.githubusercontent.com/44284779/93940508-6de84080-fce1-11ea-8ab2-a55710969989.png">

Alternatively it could be in a format that the scraper wasn't built to handle. In our experience these are minor tours, not the international level games. Usually we remove the row from the master dataset, and drop the tournament.

<img width="1278" alt="Screen Shot 2020-09-23 at 12 30 34 PM" src="https://user-images.githubusercontent.com/44284779/94060304-997a3200-fd98-11ea-9a2e-65af28fd599d.png">
