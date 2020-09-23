library(tidyverse)
library(RSelenium)
library(XML)
library(rvest)
library(htmltools)
library(lubridate)
library(magrittr)
source("/Users/James/James/Reproducibility-in-Cricket/code/scraping_functions.R")

# You will need to create an image to do this in docker
# system('docker run -d -p 4445:4444 --shm-size 2g selenium/standalone-chrome')
# So if it does break, its a good idea to clear your container. the scraper will start a fresh one
system('docker kill $(docker ps -q)')
system('docker run -d -p 4445:4444 --shm-size 2g selenium/standalone-chrome')
# Run this loop, it catches a few errors that are common and can be ignored. If the tournament has
# a broken link, remove_master underneth willt take the year and row number as an argument and 
# remove it for you
complete = 0
while(complete!=1){
	commentary <- try(grab_tournament_links(2018, "/Users/James/James/Reproducibility-in-Cricket"))
	if(class(commentary)=="try-error"){
		error_type <- attr(commentary, "condition")
		print(class(error_type))
		print(error_type$message)
	}
	if(error_type$message == "HTTP error 504."){
		print("504 error, taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "HTTP error 500."){
		print("500 error, taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "HTTP error 501."){
		print("501 error, taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "HTTP error 502."){
		print("502 error, taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "HTTP error 503."){
		print("503 error, taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "object 'starting_date' not found"){
		print("I think it failed to get the results page loaded, 
          taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "\t Summary: UnknownError\n \t Detail: An unknown server-side error occurred while processing the command.\n \t class: org.openqa.selenium.WebDriverException\n\t Further Details: run errorDetails method"){
		print("I think it failed to get the results page loaded, 
          taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else{
		complete <- 1
	}
}

# We leave this commented out when not in use to avoid accidentally removing a good row
# remove_master_row(20202021, "/Users/James/James/Reproducibility-in-Cricket", 10)

# Now we scrape the season for the new data
complete = 0
while(complete!=1){
	commentary <- try(collect_commentary_html_new_new_format(2020, "mac"))
	if(class(commentary)=="try-error"){
		error_type <- attr(commentary, "condition")
		print(class(error_type))
		print(error_type$message)
	}
	if(error_type$message == "HTTP error 504."){
		print("504 error, taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "HTTP error 500."){
		print("500 error, taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "HTTP error 501."){
		print("501 error, taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "HTTP error 502."){
		print("502 error, taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "HTTP error 503."){
		print("503 error, taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "object 'starting_date' not found"){
		print("I think it failed to get the results page loaded, 
          taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else if(error_type$message == "\t Summary: UnknownError\n \t Detail: An unknown server-side error occurred while processing the command.\n \t class: org.openqa.selenium.WebDriverException\n\t Further Details: run errorDetails method"){
		print("I think it failed to get the results page loaded, 
          taking a 2-4 minute break before trying again")
		Sys.sleep(sample(120:240, 1))
		complete <- 0
	}else{
		complete <- 1
	}
}
# Leave this commented as well, use to remove a problematic row you don't need
# remove_commentary_row(2020, "/Users/James/James/Reproducibility-in-Cricket", 212)





