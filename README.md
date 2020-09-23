# Reproducibility-in-Cricket

There is the potential of a broken link affecting the scraper, causing it to stop. In this case, we simply leave the commentary and scorecard links on the master as NA values. If these specific links are important to you, it may be necessary to click each tournament within this broken tournament link on the season archive, and manually add the scorecard and commentary links into the master.csv

<img width="1280" alt="example_of_broken_link" src="https://user-images.githubusercontent.com/44284779/93940508-6de84080-fce1-11ea-8ab2-a55710969989.png">

Alternatively it could be in a format that the scraper wasn't built to handle. In our experience these are minor tours, not the international level games. Usually we remove the row from the master dataset, and drop the tournament.

<img width="1278" alt="Screen Shot 2020-09-23 at 12 30 34 PM" src="https://user-images.githubusercontent.com/44284779/94060304-997a3200-fd98-11ea-9a2e-65af28fd599d.png">
