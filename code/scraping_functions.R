# This function will alter the input year to the correct format for the url in ESPNcricinfo
get_season <- function(input_year, input_link){
	
	print(paste("Scraping the", input_year, "data"))
	
	# year is a 4 or 8 length numeric value to indicate either a single year, or a season that crossed two
	# years (ie 20102011)
	
	# There is a specific way of formatting season that span 1 or 2 years, and this function will alter
	# the input, say 2019 or 20192020 to 2019 or 2019%2F20. 
	
	if(floor(log10(input_year))+1==4){
		
		year <- as.character(input_year)
		
		half_season <- as.character(input_year+1)
		half_season <- paste0(str_sub(half_season, 1, 1), "F", str_sub(half_season, 3, 4), sep = "")
		
		not_year <- paste(year, "%", half_season, sep = "")
		
		links <- 
			input_link %>%
			html_nodes(".season-links") %>%
			html_nodes("a") %>%
			html_attr(name = "href") %>% 
			as_tibble() %>% 
			rename("links" = value) %>%
			dplyr::filter(grepl(year, links)) %>%
			dplyr::filter(!grepl(not_year, links)) %>%
			pull()
		
	} else if(floor(log10(input_year))+1==8){
		
		first_year <- str_sub(input_year, 1, 4)
		second_year <- paste0(str_sub(input_year, 5, 5), "F", str_sub(input_year, 7, 8), sep = "")
		year <- paste(first_year, "%", second_year, sep= "")
		
		links <- 
			input_link %>%
			html_nodes(".season-links") %>%
			html_nodes("a") %>%
			html_attr(name = "href") %>% 
			as_tibble() %>% 
			rename("links" = value) %>%
			dplyr::filter(grepl(year, links)) %>%
			pull()
		
		Sys.sleep(sample(5:15, 1))
		
	} else{
		
		print("Problem with input values, please enter either a single year (i.e. 2019), or a season spanning two seasons (i.e. 20192020)")
	}
	
	return(links)
	
}

# The 2 inputs are:
# year_input - just the season. For example 2019, or 20182019. Must be input as a numeric input, 4 or 8
# characters in length
# root_path - You need to select a root path for the data to be saved on your computer. For example,
# C:/Cricket on a pc, or Users/Name/Name/Cricket on a mac. It will automtically create the necessary 
# folders, beginning with a Data folder to hold each season

grab_tournament_links <- function(year_input, root_path){
	# system('docker kill $(docker ps -q)')
	# tourney_list_site is the season archive page for cricinfo
	tourney_list_site <- "http://www.espncricinfo.com/ci/engine/series/index.html"
	# basic prefix
	web_prefix <- "http://www.espncricinfo.com"
	tourney_list_site_data <-
		read_html(tourney_list_site)
	# These are the suffixes for upcoming (fixture) and complete (results) matches
	results_suffix <- "?view=results"
	fixtures_suffix <- "?view=fixtures"
	
	data_path <- paste(root_path, "/", "Data", sep = "")
	master_path <- paste(data_path, "/", year_input, "/master.csv", sep = "")
	year_path <- paste(data_path, "/", year_input, sep = "")
	if(!dir.exists(root_path)){
		dir.create(root_path)
	}
	if(!dir.exists(data_path)){
		dir.create(data_path)
	}
	if(!dir.exists(year_path)){
		dir.create(year_path)
	}
	# Create the master spreadsheet for the year if it doesn't exist. If it does just read it in
	if(!file.exists(master_path)){
		year_link<- get_season(year_input, 
													 tourney_list_site_data)
		full_link <- paste(web_prefix, year_link, sep = "")
		# The get season function just changes the input year from a numeric to the strange format they
		# use on cric info. It then reads the season archive page, finds the correct link, and outputs
		# the end of that link which gets pasted to the prefix in full_link. Then I read it into 
		# season_link
		season_link <- read_html(full_link)
		Sys.sleep(sample(5:15, 1))
		
		# this is the tournament_type_column in the master
		tournament_types <- 
			season_link %>% 
			html_nodes(".match-section-head") %>% 
			html_text() %>% 
			as_tibble() %>% 
			rename(tournament_type = value)
		
		master_spreadsheet <- tibble()
		# Ok so now I'll grab the links to each tournament and make the first edition of the master 
		# spreadsheet. This will have the tournament type (Test, ODI, etc.), the name each tournament
		# within that type, and the root link for that tournament I'll read after to get the scorecard 
		# and commentary links for each match in that tournament
		for(i in 1:nrow(tournament_types)){
			tournament_name <- 
				season_link %>% 
				html_nodes(".series-summary-wrap") %>% 
				extract(i) %>% 
				html_nodes(".brief-summary") %>% 
				html_nodes(".series-info") %>% 
				html_nodes(".teams") %>% 
				html_text() %>% 
				as_tibble() %>% 
				rename(tournament_name = value) %>% 
				mutate(tournament_link = season_link %>% 
							 	html_nodes(".series-summary-wrap") %>% 
							 	extract(i) %>% 
							 	html_nodes(".brief-summary") %>% 
							 	html_nodes(".series-info") %>% 
							 	html_nodes(".teams") %>% 
							 	html_nodes("a") %>% 
							 	html_attr("href") %>% 
							 	as_tibble() %>% 
							 	pull(), 
							 tournament_type = tournament_types$tournament_type[i])
			master_spreadsheet <- 
				master_spreadsheet %>% 
				bind_rows(tournament_name)
		}
	}else{
		master_spreadsheet <- read_csv(master_path)
	}
	# Ok now that I have my links, reading the actual tournament links 1 by 1 requires navigating around
	# a webpage that changes based on some buttons without altering the url. Docker is a free program
	# that you will need to install to scrape this website
	
	total_tournaments <- 
		master_spreadsheet %>% 
		distinct(tournament_name, tournament_type) %>% 
		nrow()
	# We'll scrape from the bottom up to the top of the spreadsheet
	for(i in nrow(master_spreadsheet):1){
		this_row <- 
			master_spreadsheet %>% 
			slice(i)
		
		# So we will read in the date when possible, however there are cases when it doesnt make sense. 
		# The default date when they don't know seems to be in 1970, which will be recorded but should 
		# not be taken seriously 
		if(!"scorecard_link" %in% colnames(master_spreadsheet) & 
			 !"commentary_link" %in% colnames(master_spreadsheet) & 
			 !"status" %in% colnames(master_spreadsheet) & 
			 !"date" %in% colnames(master_spreadsheet)){
			print(paste(i, "/", total_tournaments, " remaining: ",
									this_row$tournament_name, ", a ", this_row$tournament_type, 
									" tournament in ", year_input, " is being scraped", sep = ""))
			print(this_row$tournament_link)
			
			# The default is to load the "fixtures" page, which has upcoming tournaments. Results has to be
			# loaded separately
			this_tournament_upcoming <- 
				read_html(paste(this_row$tournament_link, fixtures_suffix, sep = ""))
			
			# So Rselenium and docker work well to gather html from websites that change depending on a 
			# button without altering their url. Lots of breaks gives the program time to load the data 
			# properly, and viewing the webpage is optional to check that it is working correctly. The
			# view commands are commented out, but someone nw to this may be interested in seeing how 
			# window size affects what the page loads, or to double check that it's working
			remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
																			 port = 4445L,
																			 browserName = "chrome")
			# The longer the break, the less likely to return an empty webpage I have found. Can probably
			# be lowered, but wehave found 5 second breaks to be fine
			Sys.sleep(5)
			remDr$open(silent = TRUE)
			Sys.sleep(5)
			remDr$navigate(paste(this_row$tournament_link, fixtures_suffix, sep = "")) 
			remDr$maxWindowSize()
			Sys.sleep(5)
			# remDr$screenshot(display = TRUE)
			results_tab <- 
				paste("//div/div/div/div/div/div/div/div/span[contains(text(),", 
							"'", 
							"Results", 
							"'", 
							")]", 
							sep = "")
			element <- remDr$findElement(using = "xpath", results_tab)
			Sys.sleep(5)
			element$clickElement()
			Sys.sleep(5)
			# remDr$screenshot(display = TRUE)
			page_source<-remDr$getPageSource()
			this_tournament_complete <- read_html(page_source[[1]])
			remDr$close()
			
			# Now that we have the code for both upcoming and complete matches in this tournament, 
			# let's grab all the information we need and save it. We want the scorecard, commentary, 
			# date and status (compete or no) for each tournament, which we already have name, type and
			# link for
			
			this_tournament_data_upcoming <- 
				this_tournament_upcoming %>% 
				html_nodes(".match-cta-container") %>% 
				html_nodes("a") %>% 
				html_attr("href") %>% 
				as_tibble() %>% 
				dplyr::filter(grepl("/game/", value)) %>% 
				mutate(value = paste(web_prefix, value, sep = "")) %>% 
				mutate(scorecard_link = str_replace(value, "game", "scorecard"), 
							 commentary_link = str_replace(value, "game", "commentary"), 
							 status = "upcoming", 
							 tournament_name = this_row$tournament_name, 
							 tournament_link = this_row$tournament_link, 
							 tournament_type = this_row$tournament_type) %>% 
				dplyr::select(-value)
			
			# There's a good chance the tournament has been played through. If so the number of rows will be 0
			if(nrow(this_tournament_data_upcoming)>0){
				this_tournament_data_upcoming <- 
					this_tournament_data_upcoming %>% 
					bind_cols(this_tournament_upcoming %>% 
											html_nodes(".small.mb-0.match-description") %>% 
											html_text() %>% 
											as_tibble() %>% 
											mutate(month = map(value, extract_month)) %>% 
											unnest(month) %>%
											mutate(year = map(value, extract_year)) %>% 
											unnest(year) %>% 
											mutate(isolate_day = word(word(value, 
																										 2, 
																										 sep = starting_month_shorthand), 
																								1, 
																								sep = as.character(year)), 
														 isolate_day = case_when(starting_month_shorthand != ending_month_shorthand ~
														 													str_remove(isolate_day, ending_month_shorthand), 
														 												starting_month_shorthand == ending_month_shorthand ~
														 													isolate_day), 
														 starting_day = case_when(grepl("-", isolate_day)==TRUE~
														 												 	word(isolate_day, 1, sep = "\\-"), 
														 												 grepl("-", isolate_day)==FALSE~
														 												 	isolate_day), 
														 ending_day = case_when(grepl("-", isolate_day)==TRUE~
														 											 	word(isolate_day, 2, sep = "\\-"), 
														 											 grepl("-", isolate_day)==FALSE~
														 											 	isolate_day), 
														 starting_date = paste(str_trim(year), "-", 
														 											str_trim(starting_month), "-", 
														 											str_trim(starting_day), sep = ""), 
														 ending_date = paste(str_trim(year), "-", 
														 										str_trim(ending_month), "-", 
														 										str_trim(ending_day), sep = "")) %>% 
											dplyr::select(-c(value, isolate_day, starting_month_shorthand, 
																			 ending_month_shorthand, starting_month, ending_month, 
																			 year, starting_day, ending_day)))
			}
			# We do the same as above for complete games, and then bind them together and join them into the
			# master.csv
			this_tournament_data_complete <- 
				this_tournament_complete %>% 
				html_nodes(".match-cta-container") %>% 
				html_nodes("a") %>% 
				html_attr("href") %>% 
				as_tibble() %>% 
				dplyr::filter(grepl("/game/", value)) %>% 
				mutate(value = paste(web_prefix, value, sep = "")) %>% 
				mutate(scorecard_link = str_replace(value, "game", "scorecard"), 
							 commentary_link = str_replace(value, "game", "commentary"), 
							 status = "complete", 
							 tournament_name = this_row$tournament_name, 
							 tournament_link = this_row$tournament_link, 
							 tournament_type = this_row$tournament_type) %>% 
				dplyr::select(-value)
			if(nrow(this_tournament_data_complete)>0){
				this_tournament_data_complete <- 
					this_tournament_data_complete %>% 
					bind_cols(this_tournament_complete %>% 
											html_nodes(".small.mb-0.match-description") %>% 
											html_text() %>% 
											as_tibble() %>% 
											mutate(month = map(value, extract_month)) %>% 
											unnest(month) %>%
											mutate(year = map(value, extract_year)) %>% 
											unnest(year) %>% 
											mutate(isolate_day = word(word(value, 
																										 2, 
																										 sep = starting_month_shorthand), 
																								1, 
																								sep = as.character(year)), 
														 isolate_day = case_when(starting_month_shorthand != ending_month_shorthand ~
														 													str_remove(isolate_day, ending_month_shorthand), 
														 												starting_month_shorthand == ending_month_shorthand ~
														 													isolate_day), 
														 starting_day = case_when(grepl("-", isolate_day)==TRUE~
														 												 	word(isolate_day, 1, sep = "\\-"), 
														 												 grepl("-", isolate_day)==FALSE~
														 												 	isolate_day), 
														 ending_day = case_when(grepl("-", isolate_day)==TRUE~
														 											 	word(isolate_day, 2, sep = "\\-"), 
														 											 grepl("-", isolate_day)==FALSE~
														 											 	isolate_day), 
														 starting_date = paste(str_trim(year), "-", 
														 											str_trim(starting_month), "-", 
														 											str_trim(starting_day), sep = ""), 
														 ending_date = paste(str_trim(year), "-", 
														 										str_trim(ending_month), "-", 
														 										str_trim(ending_day), sep = "")) %>% 
											dplyr::select(-c(value, isolate_day, starting_month_shorthand, 
																			 ending_month_shorthand, starting_month, ending_month, 
																			 year, starting_day, ending_day)))
			}
			# Bind upcoming and complete tournaments together
			this_tournament <- 
				this_tournament_data_upcoming %>% 
				bind_rows(this_tournament_data_complete) %>% 
				mutate(starting_date = as.Date(starting_date), 
							 ending_date = as.Date(ending_date))
			
			# Left join into the master by name, type and link
			master_spreadsheet <- 
				master_spreadsheet %>% 
				left_join(this_tournament, by = c("tournament_name" = "tournament_name", 
																					"tournament_type" = "tournament_type", 
																					"tournament_link" = "tournament_link"))
			write_csv(master_spreadsheet, master_path)
		}else{
			# If the master has the columns specified above, we must have at least one successfully 
			# gathered tournament in our master. Now when we left join the new columns will form a column.x and
			# column.y, which we will coalesce into a single column. Otherwie nothing changes from above
			if(is.na(this_row$scorecard_link)){
				# If somehting broke the loop, it will continue from where it left off
				print(paste(i, "/", total_tournaments, " remaining: ",
										this_row$tournament_name, ", a ", this_row$tournament_type, 
										" tournament in ", year_input, " is being scraped", sep = ""))
				print(this_row$tournament_link)
				
				this_tournament_upcoming <- 
					read_html(paste(this_row$tournament_link, fixtures_suffix, sep = ""))
				
				remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
																				 port = 4445L,
																				 browserName = "chrome")
				Sys.sleep(5)
				# For each tournament I will assume there are 4 rounds. I will also grab the course info, it has the yards and par for each hole 
				# which isn't necessary but why not I'm already here
				remDr$open(silent = TRUE)
				Sys.sleep(5)
				remDr$navigate(paste(this_row$tournament_link, fixtures_suffix, sep = "")) 
				remDr$maxWindowSize()
				Sys.sleep(5)
				# remDr$screenshot(display = TRUE)
				results_tab <- 
					paste("//div/div/div/div/div/div/div/div/span[contains(text(),", 
								"'", 
								"Results", 
								"'", 
								")]", 
								sep = "")
				element <- remDr$findElement(using = "xpath", results_tab)
				Sys.sleep(5)
				element$clickElement()
				Sys.sleep(5)
				# remDr$screenshot(display = TRUE)
				page_source<-remDr$getPageSource()
				this_tournament_complete <- read_html(page_source[[1]])
				remDr$close()
				
				this_tournament_data_upcoming <- 
					this_tournament_upcoming %>% 
					html_nodes(".match-cta-container") %>% 
					html_nodes("a") %>% 
					html_attr("href") %>% 
					as_tibble() %>% 
					dplyr::filter(grepl("/game/", value)) %>% 
					mutate(value = paste(web_prefix, value, sep = "")) %>% 
					mutate(scorecard_link = str_replace(value, "game", "scorecard"), 
								 commentary_link = str_replace(value, "game", "commentary"), 
								 status = "upcoming", 
								 tournament_name = this_row$tournament_name, 
								 tournament_link = this_row$tournament_link, 
								 tournament_type = this_row$tournament_type) %>% 
					dplyr::select(-value)
				if(nrow(this_tournament_data_upcoming)>0){
					this_tournament_data_upcoming <- 
						this_tournament_data_upcoming %>% 
						bind_cols(this_tournament_upcoming %>% 
												html_nodes(".small.mb-0.match-description") %>% 
												html_text() %>% 
												as_tibble() %>% 
												mutate(month = map(value, extract_month)) %>% 
												unnest(month) %>%
												mutate(year = map(value, extract_year)) %>% 
												unnest(year) %>% 
												mutate(isolate_day = word(word(value, 
																											 2, 
																											 sep = starting_month_shorthand), 
																									1, 
																									sep = as.character(year)), 
															 isolate_day = case_when(starting_month_shorthand != ending_month_shorthand ~
															 													str_remove(isolate_day, ending_month_shorthand), 
															 												starting_month_shorthand == ending_month_shorthand ~
															 													isolate_day), 
															 starting_day = case_when(grepl("-", isolate_day)==TRUE~
															 												 	word(isolate_day, 1, sep = "\\-"), 
															 												 grepl("-", isolate_day)==FALSE~
															 												 	isolate_day), 
															 ending_day = case_when(grepl("-", isolate_day)==TRUE~
															 											 	word(isolate_day, 2, sep = "\\-"), 
															 											 grepl("-", isolate_day)==FALSE~
															 											 	isolate_day), 
															 starting_date = paste(str_trim(year), "-", 
															 											str_trim(starting_month), "-", 
															 											str_trim(starting_day), sep = ""), 
															 ending_date = paste(str_trim(year), "-", 
															 										str_trim(ending_month), "-", 
															 										str_trim(ending_day), sep = "")) %>% 
												dplyr::select(-c(value, isolate_day, starting_month_shorthand, 
																				 ending_month_shorthand, starting_month, ending_month, 
																				 year, starting_day, ending_day)))
				}
				this_tournament_data_complete <- 
					this_tournament_complete %>% 
					html_nodes(".match-cta-container") %>% 
					html_nodes("a") %>% 
					html_attr("href") %>% 
					as_tibble() %>% 
					dplyr::filter(grepl("/game/", value)) %>% 
					mutate(value = paste(web_prefix, value, sep = "")) %>% 
					mutate(scorecard_link = str_replace(value, "game", "scorecard"), 
								 commentary_link = str_replace(value, "game", "commentary"), 
								 status = "complete", 
								 tournament_name = this_row$tournament_name, 
								 tournament_link = this_row$tournament_link, 
								 tournament_type = this_row$tournament_type) %>% 
					dplyr::select(-value)
				if(nrow(this_tournament_data_complete)>0){
					this_tournament_data_complete <- 
						this_tournament_data_complete %>% 
						bind_cols(this_tournament_complete %>% 
												html_nodes(".small.mb-0.match-description") %>% 
												html_text() %>% 
												as_tibble() %>% 
												mutate(month = map(value, extract_month)) %>% 
												unnest(month) %>%
												mutate(year = map(value, extract_year)) %>% 
												unnest(year) %>% 
												mutate(isolate_day = word(word(value, 
																											 2, 
																											 sep = starting_month_shorthand), 
																									1, 
																									sep = as.character(year)), 
															 isolate_day = case_when(starting_month_shorthand != ending_month_shorthand ~
															 													str_remove(isolate_day, ending_month_shorthand), 
															 												starting_month_shorthand == ending_month_shorthand ~
															 													isolate_day), 
															 starting_day = case_when(grepl("-", isolate_day)==TRUE~
															 												 	word(isolate_day, 1, sep = "\\-"), 
															 												 grepl("-", isolate_day)==FALSE~
															 												 	isolate_day), 
															 ending_day = case_when(grepl("-", isolate_day)==TRUE~
															 											 	word(isolate_day, 2, sep = "\\-"), 
															 											 grepl("-", isolate_day)==FALSE~
															 											 	isolate_day), 
															 starting_date = paste(str_trim(year), "-", 
															 											str_trim(starting_month), "-", 
															 											str_trim(starting_day), sep = ""), 
															 ending_date = paste(str_trim(year), "-", 
															 										str_trim(ending_month), "-", 
															 										str_trim(ending_day), sep = "")) %>% 
												dplyr::select(-c(value, isolate_day, starting_month_shorthand, 
																				 ending_month_shorthand, starting_month, ending_month, 
																				 year, starting_day, ending_day)))
				}
				
				this_tournament <- 
					this_tournament_data_upcoming %>% 
					bind_rows(this_tournament_data_complete) %>% 
					mutate(starting_date = as.Date(starting_date), 
								 ending_date = as.Date(ending_date))
				
				master_spreadsheet <- 
					master_spreadsheet %>% 
					left_join(this_tournament, by = c("tournament_name" = "tournament_name", 
																						"tournament_type" = "tournament_type", 
																						"tournament_link" = "tournament_link")) %>% 
					mutate(starting_date = coalesce(starting_date.x, starting_date.y), 
								 ending_date = coalesce(ending_date.x, ending_date.y), 
								 status = coalesce(status.x, status.y), 
								 scorecard_link = coalesce(scorecard_link.x, scorecard_link.y), 
								 commentary_link = coalesce(commentary_link.x, commentary_link.y)) %>% 
					dplyr::select(-c(starting_date.x, starting_date.y, ending_date.x, ending_date.y, 
													 status.x, status.y, scorecard_link.x, scorecard_link.y, 
													 commentary_link.x, commentary_link.y))
				
				write_csv(master_spreadsheet, master_path)
			}else{
				
				# print(paste(i, "/", total_tournaments, sep = ""))
				# write_csv(master_spreadsheet, master_path)
			}
		}
	}
}

# The fomatting is inconsistent, sometimes there's a space on both sides of the month, sometimes only
# one side has a space. This will find the month and return the month value
extract_month <- function(this_date){
	# This first one is for the month with a space to the left only
	if(!is.na(min(str_locate(this_date, " Jan"))) | !is.na(min(str_locate(this_date, " January")))){
		starting_month = "01"
		starting_month_shorthand <- " Jan"
		if(!is.na(min(str_locate(this_date, " Feb"))) | !is.na(min(str_locate(this_date, " February")))){
			ending_month = "02"
			ending_month_shorthand <- " Feb"
		}else{
			ending_month = "01"
			ending_month_shorthand <- " Jan"
		}
	}else if(!is.na(min(str_locate(this_date, " Feb"))) | !is.na(min(str_locate(this_date, " February")))){
		starting_month = "02"
		starting_month_shorthand <- " Feb"
		if(!is.na(min(str_locate(this_date, " Mar"))) | !is.na(min(str_locate(this_date, " March")))){
			ending_month = "03"
			ending_month_shorthand <- " Mar"
		}else{
			ending_month = "02"
			ending_month_shorthand <- " Feb"
		}
	}else if(!is.na(min(str_locate(this_date, " Mar"))) | !is.na(min(str_locate(this_date, " March")))){
		starting_month = "03"
		starting_month_shorthand <- " Mar"
		if(!is.na(min(str_locate(this_date, " Apr"))) | !is.na(min(str_locate(this_date, " April")))){
			ending_month = "04"
			ending_month_shorthand <- " Apr"
		}else{
			ending_month = "03"
			ending_month_shorthand <- " Mar"
		}
	}else if(!is.na(min(str_locate(this_date, " Apr"))) | !is.na(min(str_locate(this_date, " April")))){
		starting_month = "04"
		starting_month_shorthand <- " Apr"
		if(!is.na(min(str_locate(this_date, " May"))) | !is.na(min(str_locate(this_date, " May")))){
			ending_month = "05"
			ending_month_shorthand <- " May"
		}else{
			ending_month = "04"
			ending_month_shorthand <- " Apr"
		}
	}else if(!is.na(min(str_locate(this_date, " May"))) | !is.na(min(str_locate(this_date, " May")))){
		starting_month = "05"
		starting_month_shorthand <- " May"
		if(!is.na(min(str_locate(this_date, " Jun"))) | !is.na(min(str_locate(this_date, " June")))){
			ending_month = "06"
			ending_month_shorthand <- " Jun"
		}else{
			ending_month = "05"
			ending_month_shorthand <- " May"
		}
	}else if(!is.na(min(str_locate(this_date, " Jun"))) | !is.na(min(str_locate(this_date, " June")))){
		starting_month = "06"
		starting_month_shorthand <- " Jun"
		if(!is.na(min(str_locate(this_date, " Jul"))) | !is.na(min(str_locate(this_date, " July")))){
			ending_month = "07"
			ending_month_shorthand <- " Jul"
		}else{
			ending_month = "06"
			ending_month_shorthand <- " Jun"
		}
	}else if(!is.na(min(str_locate(this_date, " July"))) | !is.na(min(str_locate(this_date, " Jul")))){
		starting_month = "07"
		starting_month_shorthand <- " Jul"
		if(!is.na(min(str_locate(this_date, " Aug"))) | !is.na(min(str_locate(this_date, " August")))){
			ending_month = "08"
			ending_month_shorthand <- " Aug"
		}else{
			ending_month = "07"
			ending_month_shorthand <- " Jul"
		}
	}else if(!is.na(min(str_locate(this_date, " August"))) | !is.na(min(str_locate(this_date, " Aug")))){
		starting_month = "08"
		starting_month_shorthand <- " Aug"
		if(!is.na(min(str_locate(this_date, " Sep"))) | !is.na(min(str_locate(this_date, " September")))){
			ending_month = "09"
			ending_month_shorthand <- " Sep"
		}else{
			ending_month = "08"
			ending_month_shorthand <- " Aug"
		}
	}else if(!is.na(min(str_locate(this_date, " September"))) | !is.na(min(str_locate(this_date, " Sep")))){
		starting_month = "09"
		starting_month_shorthand <- " Sep"
		if(!is.na(min(str_locate(this_date, " Oct"))) | !is.na(min(str_locate(this_date, " October")))){
			ending_month = "10"
			ending_month_shorthand <- " Oct"
		}else{
			ending_month = "09"
			ending_month_shorthand <- " Sep"
		}
	}else if(!is.na(min(str_locate(this_date, " October"))) | !is.na(min(str_locate(this_date, " Oct")))){
		starting_month = "10"
		starting_month_shorthand <- " Oct"
		if(!is.na(min(str_locate(this_date, " Nov"))) | !is.na(min(str_locate(this_date, " November")))){
			ending_month = "11"
			ending_month_shorthand <- " Nov"
		}else{
			ending_month = "10"
			ending_month_shorthand <- " Oct"
		}
	}else if(!is.na(min(str_locate(this_date, " November"))) | !is.na(min(str_locate(this_date, " Nov")))){
		starting_month = "11"
		starting_month_shorthand <- " Nov"
		if(!is.na(min(str_locate(this_date, " December"))) | !is.na(min(str_locate(this_date, " Dec")))){
			ending_month = "12"
			ending_month_shorthand <- " Dec"
		}else{
			ending_month = "11"
			ending_month_shorthand <- " Nov"
		}
	}else if(!is.na(min(str_locate(this_date, " December"))) | !is.na(min(str_locate(this_date, " Dec")))){
		starting_month = "12"
		starting_month_shorthand <- " Dec"
		if(!is.na(min(str_locate(this_date, " January"))) | !is.na(min(str_locate(this_date, " Jan")))){
			ending_month = "01"
			ending_month_shorthand <- " Jan"
		}else{
			ending_month = "12"
			ending_month_shorthand <- " Dec"
		}
	}
	# Now to the right
	if(!is.na(min(str_locate(this_date, "Jan "))) | !is.na(min(str_locate(this_date, "January ")))){
		starting_month = "01"
		starting_month_shorthand <- "Jan "
		if(!is.na(min(str_locate(this_date, "Feb "))) | !is.na(min(str_locate(this_date, "February ")))){
			ending_month = "02"
			ending_month_shorthand <- "Feb "
		}else{
			ending_month = "01"
			ending_month_shorthand <- "Jan "
		}
	}else if(!is.na(min(str_locate(this_date, "Feb "))) | !is.na(min(str_locate(this_date, "February ")))){
		starting_month = "02"
		starting_month_shorthand <- "Feb "
		if(!is.na(min(str_locate(this_date, "Mar "))) | !is.na(min(str_locate(this_date, "March ")))){
			ending_month = "03"
			ending_month_shorthand <- "Mar "
		}else{
			ending_month = "02"
			ending_month_shorthand <- "Feb "
		}
	}else if(!is.na(min(str_locate(this_date, "Mar "))) | !is.na(min(str_locate(this_date, "March ")))){
		starting_month = "03"
		starting_month_shorthand <- "Mar "
		if(!is.na(min(str_locate(this_date, "Apr "))) | !is.na(min(str_locate(this_date, "April ")))){
			ending_month = "04"
			ending_month_shorthand <- "Apr "
		}else{
			ending_month = "03"
			ending_month_shorthand <- "Mar "
		}
	}else if(!is.na(min(str_locate(this_date, "Apr "))) | !is.na(min(str_locate(this_date, "April ")))){
		starting_month = "04"
		starting_month_shorthand <- "Apr "
		if(!is.na(min(str_locate(this_date, "May "))) | !is.na(min(str_locate(this_date, "May ")))){
			ending_month = "05"
			ending_month_shorthand <- "May "
		}else{
			ending_month = "04"
			ending_month_shorthand <- "Apr "
		}
	}else if(!is.na(min(str_locate(this_date, "May "))) | !is.na(min(str_locate(this_date, "May ")))){
		starting_month = "05"
		starting_month_shorthand <- "May "
		if(!is.na(min(str_locate(this_date, "Jun "))) | !is.na(min(str_locate(this_date, "June ")))){
			ending_month = "06"
			ending_month_shorthand <- "Jun "
		}else{
			ending_month = "05"
			ending_month_shorthand <- "May "
		}
	}else if(!is.na(min(str_locate(this_date, "Jun "))) | !is.na(min(str_locate(this_date, "June ")))){
		starting_month = "06"
		starting_month_shorthand <- "Jun "
		if(!is.na(min(str_locate(this_date, "Jul "))) | !is.na(min(str_locate(this_date, "July ")))){
			ending_month = "07"
			ending_month_shorthand <- "Jul "
		}else{
			ending_month = "06"
			ending_month_shorthand <- "Jun "
		}
	}else if(!is.na(min(str_locate(this_date, "July "))) | !is.na(min(str_locate(this_date, "Jul ")))){
		starting_month = "07"
		starting_month_shorthand <- "Jul "
		if(!is.na(min(str_locate(this_date, "Aug "))) | !is.na(min(str_locate(this_date, "August ")))){
			ending_month = "08"
			ending_month_shorthand <- "Aug "
		}else{
			ending_month = "07"
			ending_month_shorthand <- "Jul "
		}
	}else if(!is.na(min(str_locate(this_date, "August "))) | !is.na(min(str_locate(this_date, "Aug ")))){
		starting_month = "08"
		starting_month_shorthand <- "Aug "
		if(!is.na(min(str_locate(this_date, "Sep "))) | !is.na(min(str_locate(this_date, "September ")))){
			ending_month = "09"
			ending_month_shorthand <- "Sep "
		}else{
			ending_month = "08"
			ending_month_shorthand <- "Aug "
		}
	}else if(!is.na(min(str_locate(this_date, "September "))) | !is.na(min(str_locate(this_date, "Sep ")))){
		starting_month = "09"
		starting_month_shorthand <- "Sep "
		if(!is.na(min(str_locate(this_date, "Oct "))) | !is.na(min(str_locate(this_date, "October ")))){
			ending_month = "10"
			ending_month_shorthand <- "Oct "
		}else{
			ending_month = "09"
			ending_month_shorthand <- "Sep "
		}
	}else if(!is.na(min(str_locate(this_date, "October "))) | !is.na(min(str_locate(this_date, "Oct ")))){
		starting_month = "10"
		starting_month_shorthand <- "Oct "
		if(!is.na(min(str_locate(this_date, "Nov "))) | !is.na(min(str_locate(this_date, "November ")))){
			ending_month = "11"
			ending_month_shorthand <- "Nov "
		}else{
			ending_month = "10"
			ending_month_shorthand <- "Oct "
		}
	}else if(!is.na(min(str_locate(this_date, "November "))) | !is.na(min(str_locate(this_date, "Nov ")))){
		starting_month = "11"
		starting_month_shorthand <- "Nov "
		if(!is.na(min(str_locate(this_date, "December "))) | !is.na(min(str_locate(this_date, "Dec ")))){
			ending_month = "12"
			ending_month_shorthand <- "Dec "
		}else{
			ending_month = "11"
			ending_month_shorthand <- "Nov "
		}
	}else if(!is.na(min(str_locate(this_date, "December "))) | !is.na(min(str_locate(this_date, "Dec ")))){
		starting_month = "12"
		starting_month_shorthand <- "Dec "
		if(!is.na(min(str_locate(this_date, "January "))) | !is.na(min(str_locate(this_date, "Jan ")))){
			ending_month = "01"
			ending_month_shorthand <- "Jan "
		}else{
			ending_month = "12"
			ending_month_shorthand <- "Dec "
		}
	}
	# And both sides
	if(!is.na(min(str_locate(this_date, " Jan "))) | !is.na(min(str_locate(this_date, " January ")))){
		starting_month = "01"
		starting_month_shorthand <- " Jan "
		if(!is.na(min(str_locate(this_date, " Feb "))) | !is.na(min(str_locate(this_date, " February ")))){
			ending_month = "02"
			ending_month_shorthand <- " Feb "
		}else{
			ending_month = "01"
			ending_month_shorthand <- " Jan "
		}
	}else if(!is.na(min(str_locate(this_date, " Feb "))) | !is.na(min(str_locate(this_date, " February ")))){
		starting_month = "02"
		starting_month_shorthand <- " Feb " 
		if(!is.na(min(str_locate(this_date, " Mar "))) | !is.na(min(str_locate(this_date, " March ")))){
			ending_month = "03"
			ending_month_shorthand <- " Mar "
		}else{
			ending_month = "02"
			ending_month_shorthand <- " Feb "
		}
	}else if(!is.na(min(str_locate(this_date, " Mar "))) | !is.na(min(str_locate(this_date, " March ")))){
		starting_month = "03"
		starting_month_shorthand <- " Mar "
		if(!is.na(min(str_locate(this_date, " Apr "))) | !is.na(min(str_locate(this_date, " April ")))){
			ending_month = "04"
			ending_month_shorthand <- " Apr "
		}else{
			ending_month = "03"
			ending_month_shorthand <- " Mar "
		}
	}else if(!is.na(min(str_locate(this_date, " Apr "))) | !is.na(min(str_locate(this_date, " April ")))){
		starting_month = "04"
		starting_month_shorthand <- " Apr "
		if(!is.na(min(str_locate(this_date, " May "))) | !is.na(min(str_locate(this_date, " May ")))){
			ending_month = "05"
			ending_month_shorthand <- " May "
		}else{
			ending_month = "04"
			ending_month_shorthand <- " Apr "
		}
	}else if(!is.na(min(str_locate(this_date, " May "))) | !is.na(min(str_locate(this_date, " May ")))){
		starting_month = "05"
		starting_month_shorthand <- " May "
		if(!is.na(min(str_locate(this_date, " Jun "))) | !is.na(min(str_locate(this_date, " June ")))){
			ending_month = "06"
			ending_month_shorthand <- " Jun "
		}else{
			ending_month = "05"
			ending_month_shorthand <- " May "
		}
	}else if(!is.na(min(str_locate(this_date, " Jun "))) | !is.na(min(str_locate(this_date, " June ")))){
		starting_month = "06"
		starting_month_shorthand <- " Jun "
		if(!is.na(min(str_locate(this_date, " Jul "))) | !is.na(min(str_locate(this_date, " July ")))){
			ending_month = "07"
			ending_month_shorthand <- " Jul "
		}else{
			ending_month = "06"
			ending_month_shorthand <- " Jun "
		}
	}else if(!is.na(min(str_locate(this_date, " July "))) | !is.na(min(str_locate(this_date, " Jul ")))){
		starting_month = "07"
		starting_month_shorthand <- " Jul "
		if(!is.na(min(str_locate(this_date, " Aug "))) | !is.na(min(str_locate(this_date, " August ")))){
			ending_month = "08"
			ending_month_shorthand <- " Aug "
		}else{
			ending_month = "07"
			ending_month_shorthand <- " Jul "
		}
	}else if(!is.na(min(str_locate(this_date, " August "))) | !is.na(min(str_locate(this_date, " Aug ")))){
		starting_month = "08"
		starting_month_shorthand <- " Aug "
		if(!is.na(min(str_locate(this_date, " Sep "))) | !is.na(min(str_locate(this_date, " September ")))){
			ending_month = "09"
			ending_month_shorthand <- " Sep "
		}else{
			ending_month = "08"
			ending_month_shorthand <- " Aug "
		}
	}else if(!is.na(min(str_locate(this_date, " September "))) | !is.na(min(str_locate(this_date, " Sep ")))){
		starting_month = "09"
		starting_month_shorthand <- " Sep "
		if(!is.na(min(str_locate(this_date, " Oct "))) | !is.na(min(str_locate(this_date, " October ")))){
			ending_month = "10"
			ending_month_shorthand <- " Oct "
		}else{
			ending_month = "09"
			ending_month_shorthand <- " Sep "
		}
	}else if(!is.na(min(str_locate(this_date, " October "))) | !is.na(min(str_locate(this_date, " Oct ")))){
		starting_month = "10"
		starting_month_shorthand <- " Oct "
		if(!is.na(min(str_locate(this_date, " Nov "))) | !is.na(min(str_locate(this_date, " November ")))){
			ending_month = "11"
			ending_month_shorthand <- " Nov "
		}else{
			ending_month = "10"
			ending_month_shorthand <- " Oct "
		}
	}else if(!is.na(min(str_locate(this_date, " November "))) | !is.na(min(str_locate(this_date, " Nov ")))){
		starting_month = "11"
		starting_month_shorthand <- " Nov "
		if(!is.na(min(str_locate(this_date, " December "))) | !is.na(min(str_locate(this_date, " Dec ")))){
			ending_month = "12"
			ending_month_shorthand <- " Dec "
		}else{
			ending_month = "11"
			ending_month_shorthand <- " Nov "
		}
	}else if(!is.na(min(str_locate(this_date, " December "))) | !is.na(min(str_locate(this_date, " Dec ")))){
		starting_month = "12"
		starting_month_shorthand <- " Dec "
		if(!is.na(min(str_locate(this_date, " January "))) | !is.na(min(str_locate(this_date, " Jan ")))){
			ending_month = "01"
			ending_month_shorthand <- " Jan "
		}else{
			ending_month = "12"
			ending_month_shorthand <- " Dec "
		}
	}
	out <- tibble(starting_month = starting_month) %>% 
		mutate(ending_month = ending_month, 
					 starting_month_shorthand = starting_month_shorthand, 
					 ending_month_shorthand = ending_month_shorthand)
	return(out)
}

# This function will take the input dates and extract the year. It assumes that no tournaments
# take place over 2 years (i.e December 31 and Jan 1 are dates in a single tournament).
# It outputs the year, and the date object without the year in it. If a tournament did take place
# over 2 years, it would probably output an error, but this hasn't happened yet
extract_year <- function(this_date){
	if(!is.na(min(str_locate(this_date, "2021")))){
		year <- 2021
		this_date <- str_remove(this_date, "2021")
	}else if(!is.na(min(str_locate(this_date, "2020")))){
		year <- 2020
		this_date <- str_remove(this_date, "2020")
	}else if(!is.na(min(str_locate(this_date, "2019")))){
		year <- 2019
		this_date <- str_remove(this_date, "2019")
	}else if(!is.na(min(str_locate(this_date, "2018")))){
		year <- 2018
		this_date <- str_remove(this_date, "2018")
	}else if(!is.na(min(str_locate(this_date, "2017")))){
		year <- 2017
		this_date <- str_remove(this_date, "2017")
	}else if(!is.na(min(str_locate(this_date, "2016")))){
		year <- 2016
		this_date <- str_remove(this_date, "2016")
	}else if(!is.na(min(str_locate(this_date, "2015")))){
		year <- 2015
		this_date <- str_remove(this_date, "2015")
	}else if(!is.na(min(str_locate(this_date, "2014")))){
		year <- 2014
		this_date <- str_remove(this_date, "2014")
	}else if(!is.na(min(str_locate(this_date, "2013")))){
		year <- 2013
		this_date <- str_remove(this_date, "2013")
	}else if(!is.na(min(str_locate(this_date, "2012")))){
		year <- 2012
		this_date <- str_remove(this_date, "2012")
	}else if(!is.na(min(str_locate(this_date, "2011")))){
		year <- 2011
		this_date <- str_remove(this_date, "2011")
	}else if(!is.na(min(str_locate(this_date, "2010")))){
		year <- 2010
		this_date <- str_remove(this_date, "2010")
	}else if(!is.na(min(str_locate(this_date, "2009")))){
		year <- 2009
		this_date <- str_remove(this_date, "2009")
	}else if(!is.na(min(str_locate(this_date, "2008")))){
		year <- 2008
		this_date <- str_remove(this_date, "2008")
	}else if(!is.na(min(str_locate(this_date, "2007")))){
		year <- 2007
		this_date <- str_remove(this_date, "2007")
	}else if(!is.na(min(str_locate(this_date, "2006")))){
		year <- 2006
		this_date <- str_remove(this_date, "2006")
	}else if(!is.na(min(str_locate(this_date, "2005")))){
		year <- 2005
		this_date <- str_remove(this_date, "2005")
	}else if(!is.na(min(str_locate(this_date, "2004")))){
		year <- 2004
		this_date <- str_remove(this_date, "2004")
	}else if(!is.na(min(str_locate(this_date, "2003")))){
		year <- 2003
		this_date <- str_remove(this_date, "2003")
	}else if(!is.na(min(str_locate(this_date, "2002")))){
		year <- 2002
		this_date <- str_remove(this_date, "2002")
	}else if(!is.na(min(str_locate(this_date, "2001")))){
		year <- 2001
		this_date <- str_remove(this_date, "2001")
	}else if(!is.na(min(str_locate(this_date, "2000")))){
		year <- 2000
		this_date <- str_remove(this_date, "2000")
	}else if(!is.na(min(str_locate(this_date, "1999")))){
		year <- 1999
		this_date <- str_remove(this_date, "1999")
	}else if(!is.na(min(str_locate(this_date, "1998")))){
		year <- 1998
		this_date <- str_remove(this_date, "1998")
	}else if(!is.na(min(str_locate(this_date, "1997")))){
		year <- 1997
		this_date <- str_remove(this_date, "1997")
	}else if(!is.na(min(str_locate(this_date, "1996")))){
		year <- 1996
		this_date <- str_remove(this_date, "1996")
	}else if(!is.na(min(str_locate(this_date, "1995")))){
		year <- 1995
		this_date <- str_remove(this_date, "1995")
	}else if(!is.na(min(str_locate(this_date, "1994")))){
		year <- 1994
		this_date <- str_remove(this_date, "1994")
	}else if(!is.na(min(str_locate(this_date, "1993")))){
		year <- 1993
		this_date <- str_remove(this_date, "1993")
	}else if(!is.na(min(str_locate(this_date, "1992")))){
		year <- 1992
		this_date <- str_remove(this_date, "1992")
	}else if(!is.na(min(str_locate(this_date, "1991")))){
		year <- 1991
		this_date <- str_remove(this_date, "1991")
	}else if(!is.na(min(str_locate(this_date, "1990")))){
		year <- 1990
		this_date <- str_remove(this_date, "1990")
	}else if(!is.na(min(str_locate(this_date, "1989")))){
		year <- 1989
		this_date <- str_remove(this_date, "1989")
	}else if(!is.na(min(str_locate(this_date, "1988")))){
		year <- 1988
		this_date <- str_remove(this_date, "1988")
	}else if(!is.na(min(str_locate(this_date, "1987")))){
		year <- 1987
		this_date <- str_remove(this_date, "1987")
	}else if(!is.na(min(str_locate(this_date, "1986")))){
		year <- 1986
		this_date <- str_remove(this_date, "1986")
	}else if(!is.na(min(str_locate(this_date, "1985")))){
		year <- 1985
		this_date <- str_remove(this_date, "1985")
	}else if(!is.na(min(str_locate(this_date, "1984")))){
		year <- 1984
		this_date <- str_remove(this_date, "1984")
	}else if(!is.na(min(str_locate(this_date, "1983")))){
		year <- 1983
		this_date <- str_remove(this_date, "1983")
	}else if(!is.na(min(str_locate(this_date, "1982")))){
		year <- 1982
		this_date <- str_remove(this_date, "1982")
	}else if(!is.na(min(str_locate(this_date, "1981")))){
		year <- 1981
		this_date <- str_remove(this_date, "1981")
	}else if(!is.na(min(str_locate(this_date, "1980")))){
		year <- 1980
		this_date <- str_remove(this_date, "1980")
	}else if(!is.na(min(str_locate(this_date, "1979")))){
		year <- 1979
		this_date <- str_remove(this_date, "1979")
	}else if(!is.na(min(str_locate(this_date, "1978")))){
		year <- 1978
		this_date <- str_remove(this_date, "1978")
	}else if(!is.na(min(str_locate(this_date, "1977")))){
		year <- 1977
		this_date <- str_remove(this_date, "1977")
	}else if(!is.na(min(str_locate(this_date, "1976")))){
		year <- 1976
		this_date <- str_remove(this_date, "1976")
	}else if(!is.na(min(str_locate(this_date, "1975")))){
		year <- 1975
		this_date <- str_remove(this_date, "1975")
	}else if(!is.na(min(str_locate(this_date, "1974")))){
		year <- 1974
		this_date <- str_remove(this_date, "1974")
	}else if(!is.na(min(str_locate(this_date, "1973")))){
		year <- 1973
		this_date <- str_remove(this_date, "1973")
	}else if(!is.na(min(str_locate(this_date, "1972")))){
		year <- 1972
		this_date <- str_remove(this_date, "1972")
	}else if(!is.na(min(str_locate(this_date, "1971")))){
		year <- 1971
		this_date <- str_remove(this_date, "1971")
	}else if(!is.na(min(str_locate(this_date, "1970")))){
		year <- 1970
		this_date <- str_remove(this_date, "1970")
	}
	
	out <- list(year = year, 
							date_minus_year = this_date)
	return(year)
}

# Webscraping is a bit messy, so occasionally it's easier to just drop a bad link from the scraping
# process altogether. These functions will drop them for you
remove_commentary_row <- function(year_input, root_path, row_to_drop){
	
	data_path <- paste(root_path, "/Data", sep = "")
	year_path <- paste(data_path, "/", year_input, sep = "")
	year_master_spreadsheet_path <- paste(year_path, "/master.csv", sep = "")
	year_master_commentary_spreadsheet_path <- paste(year_path, "/master_commentary.csv", sep = "")
	
	master <- 
		read_csv(year_master_commentary_spreadsheet_path) %>% 
		slice(-row_to_drop)
	write_csv(master, year_master_commentary_spreadsheet_path)
	
}
remove_master_row <- function(year_input, root_path, row_to_drop){
	
	data_path <- paste(root_path, "/Data", sep = "")
	year_path <- paste(data_path, "/", year_input, sep = "")
	year_master_spreadsheet_path <- paste(year_path, "/master.csv", sep = "")

	master <- 
		read_csv(year_master_spreadsheet_path) %>% 
		slice(-row_to_drop)
	write_csv(master, year_master_spreadsheet_path)
	
}


# So just use the same root as before, to the folder that will contain all the code, and data, and
# whatever else you may eventially put in here in your cricket analysis
collect_commentary_html <- function(year_input, root_path){
	# system('docker kill $(docker ps -q)')
	# Set up the appropriate paths first
	
	data_path <- paste(root_path, "/", "Data", sep = "")
	year_path <- paste(data_path, "/", "year_input", sep = "")
	year_master_spreadsheet_path <- paste(year_path, "/master.csv", sep = "")
	year_master_commentary_spreadsheet_path <- paste(year_path, "/master_commentary.csv", sep = "")
	
	# Firstly, if there is no commentary spreadsheet I need to create it and save it with
	# appropriate columns (tests would need 4 innings, I don't think I'll ever use them but they're there)
	
	if(!file.exists(year_master_commentary_spreadsheet_path)){
		master_commentary_spreadsheet <- read_csv(year_master_spreadsheet_path)
		
		master_commentary_spreadsheet$inning_1_commentary <- NA
		master_commentary_spreadsheet$inning_2_commentary <- NA
		master_commentary_spreadsheet$inning_3_commentary <- NA
		master_commentary_spreadsheet$inning_4_commentary <- NA
		master_commentary_spreadsheet$commentary_scraped <- NA
		master_commentary_spreadsheet$scorecard_scraped <- NA
		master_commentary_spreadsheet$scorecard_path <- NA
		
		# I also organize these by the order I am interested, and I stop the function when I reach 
		# the last tournaent type. Since I didn't both putting in a Test scraping ability to this funtion, 
		# all tests formats are listed last, followed by Youth  and minor tours since those tournaments have 
		# frequently missing or poor quality commentary. Reorganize by interest as needed
		master_commentary_spreadsheet <- 
			master_commentary_spreadsheet %>%
			arrange(factor(tournament_type, c("Twenty20 Internationals",
																				"Twenty20",
																				"One-Day Internationals",
																				"Women's Twenty20 Internationals",
																				"Woman's One-Day Internationals",
																				"Other Twenty20 matches",
																				"Other one-day_limited-overs matches",
																				"Tests",
																				"First-class",
																				"List A",
																				"Woman's Tests",
																				"Other matches",
																				"tour",
																				"minor tour",
																				"Youth One-Day Internationals",
																				"Youth Tests",
																				"Youth Twenty20 Internationals"
			))) %>% 
			distinct()
		
		write_csv(master_commentary_spreadsheet, year_master_commentary_spreadsheet_path)
	}else{
		master_commentary_spreadsheet <- read_csv(year_master_commentary_spreadsheet_path)
	}
	
	where_we_left_off <- 
		master_commentary_spreadsheet %>% 
		mutate(row_n = row_number()) %>% 
		dplyr::filter(is.na(commentary_scraped)) %>% 
		slice(1) %>% 
		pull(row_n)
	
	# After a game is scraped the NA becomes a "yes", so you can pick up where you left off
	
	for(i in where_we_left_off:nrow(master_commentary_spreadsheet)){
		print(paste(i, "/", nrow(master_commentary_spreadsheet)))

		this_row <- 
			master_commentary_spreadsheet %>% 
			slice(i)
		
		# So to be safe you only want to grab matches that have been played. 
		# A game than was played at an earlier date is guaranteed to be over. 
		if(this_row$ending_date<Sys.Date()){
			
			# How many times do I need to scroll down the page depends on the tournament type. This is overkill, 
			# you don't need to scroll this many times, but this function was not built to be fast, but to
			# crawl through the links and grab the full commentary
			if(this_row$tournament_type %in% c("Twenty20", "Twenty20 Internationals", 
																				 "Women's Twenty20 Internationals", 
																				 "Other Twenty20 matches", "Youth Twenty20 Internationals")){
				scroll_number <-  20*1.5
			}else if(this_row$tournament_type %in% c("One-Day Internationals", "Youth One-Day Internationals", 
																							 "Other one-day_limited-overs matches", "List A", 
																							 "Women's One-Day Internationals")){
				scroll_number <- 50*1.5
			}else{
				scroll_number <- 100*1.5
			} 
			# SOme weird problems with scraped links is there is repeat of the base url, get rid of it and
			# replace with the correct version
			if(str_detect(master_commentary_spreadsheet$commentary_link[i], "http://www.espncricinfo.comhttps://www.espncricinfo.com")){
				master_commentary_spreadsheet$commentary_link[i] <- str_replace(master_commentary_spreadsheet$commentary_link[i], 
																																				"http://www.espncricinfo.comhttps://www.espncricinfo.com", "http://www.espncricinfo.com")
				master_commentary_spreadsheet$commentary_link[i] <- str_sub(master_commentary_spreadsheet$commentary_link[i],
																																		1, str_length(master_commentary_spreadsheet$commentary_link[i])-1)
				write_csv(master_commentary_spreadsheet, year_master_commentary_spreadsheet_path)
			}
			# Setting up paths for commentary html to be saved to
			type_path <- paste(year_path, "/", master_commentary_spreadsheet$tournament_type[i], 
												 sep = "")
			name_path <- paste(type_path, "/", master_commentary_spreadsheet$tournament_name[i], 
												 sep = "")
			if(!dir.exists(type_path)){
				dir.create(type_path, showWarnings = FALSE)
			}
			if(!dir.exists(name_path)){
				dir.create(name_path, showWarnings = FALSE)
			}
			# Just letting you know what game we're scraping, and some more paths. 
			game_name <- 
				master_commentary_spreadsheet %>%
				slice(i) %>%
				dplyr::select(scorecard_link) %>%
				pull() 
			game_name <- word(word(game_name, 2, sep = "scorecard/"), 2, sep = "\\/")
			commentary_path_1 <- paste(name_path, "/", game_name, 
																 "_inning_", 1, "_commentary.html", sep = "")
			commentary_path_2 <- paste(name_path, "/", game_name, 
																 "_inning_", 2, "_commentary.html", sep = "")
			scorecard_path <- paste(name_path, "/", game_name, 
															"_scorecard.html", sep = "")
			print(paste("Grabbing the commentary and scorecard html for ",
									master_commentary_spreadsheet$tournament_name[i], 
									", ", game_name, ", 2 innings", sep = ""))
			print(master_commentary_spreadsheet$commentary_link[i])
			
			# If we haven't gotten the scorecard yet, grab it. No need for docker
			if(!file.exists(scorecard_path)){
				this_scorecard <- 
					read_html(this_row$scorecard_link)
				write_xml(this_scorecard, scorecard_path)
				Sys.sleep(sample(5:15, 1))
			}
			master_commentary_spreadsheet$scorecard_path[i] <- scorecard_path
			master_commentary_spreadsheet$scorecard_scraped[i] <- "yes"
			write_csv(master_commentary_spreadsheet, year_master_commentary_spreadsheet_path)
			# Check if the first commentary file already exists
			if(is.na(this_row$commentary_scraped)){
				if(!file.exists(commentary_path_1)){
					print("Scraping inning 1")
					# Open docker
					# Read in commentary page
					remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
																					 port = 4444L,
																					 browserName = "chrome")
					Sys.sleep(5)
					remDr$open(silent = TRUE)
					Sys.sleep(5)
					remDr$navigate(this_row$commentary_link) 
					remDr$setWindowSize(width = 1260, height = 600)
					webElem <- remDr$findElement("css", "body")
					Sys.sleep(1)
					webElem$sendKeysToElement(list(key = "end"))
					# remDr$screenshot(display = TRUE)
					pre_scroll <- 
						read_html(remDr$getPageSource()[[1]])
					
					# So there are a few different formats we have found on the website telling the reader the
					# game was abandoned, no result, or simply missing comentary. We check for these, and if not
					# then we proceed with the scraping. We've added to this over time as new formts appear, and 
					# will continue to do so
					pre_scroll_empty <- 
						pre_scroll %>% 
						html_nodes(".box-shadow-none") %>% 
						html_text() %>%
						as_tibble() %>% 
						dplyr::filter(value == "No content available") %>% 
						nrow() > 0
					
					pre_scroll_abandoned <- 
						pre_scroll %>% 
						html_nodes(".event-full-width") %>% 
						html_text() %>% 
						as_tibble() %>% 
						mutate(value = tolower(value)) %>% 
						dplyr::filter(grepl("abandon", value)) %>% 
						nrow()>0
					
					pre_scroll_abandoned2 <- 
						pre_scroll %>% 
						html_nodes(".event") %>% 
						html_text() %>% 
						as_tibble() %>% 
						mutate(value = tolower(value)) %>% 
						dplyr::filter(grepl("abandon", value)) %>% 
						nrow()>0
					
					pre_scroll_no_result <- 
						pre_scroll %>% 
						html_nodes(".event-full-width") %>% 
						html_text() %>% 
						as_tibble() %>% 
						mutate(value = tolower(value)) %>% 
						dplyr::filter(grepl("no result", value)) %>% 
						nrow()>0
					
					if(pre_scroll_empty == TRUE){
						print("This commentary doesn't exist")
					}else if(pre_scroll_abandoned==TRUE){
						print("This match was abandoned")
					}else if(pre_scroll_abandoned2==TRUE){
						print("This match was abandoned")
					}else if(pre_scroll_no_result==TRUE){
						print("This match has no result")
					}else{
						# The buttons are named by the team abbreviation + "Innings". This is to 
						# help locate the button at the top for each innings
						inning_buttons_names <- 
							pre_scroll %>% 
							html_nodes(".team-name") %>% 
							html_text() %>% 
							as_tibble()
						
						# This is the button path, its specific to this game. This part would need to be altered to
						# scrape test format cricket commentary
						inning1_button_name <- 
							paste("//div/div/div[contains(text(),", 
										"'", 
										inning_buttons_names %>% 
											slice(1) %>% 
											pull(),
										" Innings'", 
										")]", 
										sep = "")
						
						inning2_button_name <- 
							paste("//div/div/div[contains(text(),", 
										"'", 
										inning_buttons_names %>% 
											slice(2) %>% 
											pull(),
										" Innings'", 
										")]", 
										sep = "")
						
						# This is the general path to the dropdown menu, which is general to all commentary
						inning_button_name <- 
							paste("//div/div/div/div[contains(text(),", 
										"'", 
										"Innings",
										"'", 
										")]", 
										sep = "")
						# I will need to click outside of the dropdown menu to scroll, all games seem 
						# to end with this phrase. I may add more possible phrases if this doesn't work, 
						# but that hasn't been a problem yet
						webpage_body <- 
							"//span[contains(text(),  'end of over')]"
						# Click dropdown
						element <- remDr$findElement(using = "xpath", inning_button_name)
						element$clickElement()
						Sys.sleep(5)
						# Then, select the first inning from the dropdown
						element <- remDr$findElement(using = "xpath", inning1_button_name)
						element$clickElement()
						Sys.sleep(5)
						# remDr$screenshot(display = TRUE)
						# Click body of webpage outside dropdown menu
						element <- remDr$findElement(using = "xpath", webpage_body)
						element$clickElement()
						Sys.sleep(5)
						# remDr$screenshot(display = TRUE)
						webElem <- remDr$findElement("css", "body")
						# Scroll to bottom, this is more page downs than necessary, probably by a lot
						print("Scrolling down")
						for(k in 1:scroll_number){
							webElem$sendKeysToElement(list(key = "end"))
							Sys.sleep(2)
							print(k)
						}
						# remDr$screenshot(display = TRUE)
						# Save the html to the path specified earlier. 
						page_source<-remDr$getPageSource()
						inning1_html <- read_html(page_source[[1]])
						write_xml(inning1_html, commentary_path_1)
						master_commentary_spreadsheet$inning_1_commentary[i] <- commentary_path_1
						write_csv(master_commentary_spreadsheet, year_master_commentary_spreadsheet_path)
						
					}
					# close the window
					remDr$close()
				}else{
					# It's possible it has been scraped already, so we skip if that's the case to save time
					print("Inning 1 has already been scraped")
					master_commentary_spreadsheet$inning_1_commentary[i] <- commentary_path_1
					write_csv(master_commentary_spreadsheet, year_master_commentary_spreadsheet_path)
				}
				# Do it all over again for the second inning. 
				if(!file.exists(commentary_path_2)){
					print("Scraping inning 2")
					
					remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
																					 port = 4444L,
																					 browserName = "chrome")
					Sys.sleep(5)
					remDr$open(silent = TRUE)
					Sys.sleep(5)
					remDr$navigate(this_row$commentary_link) 
					remDr$setWindowSize(width = 1260, height = 600)
					webElem <- remDr$findElement("css", "body")
					Sys.sleep(1)
					webElem$sendKeysToElement(list(key = "end"))
					remDr$screenshot(display = TRUE)
					pre_scroll <- 
						read_html(remDr$getPageSource()[[1]])
					
					pre_scroll_empty <- 
						pre_scroll %>% 
						html_nodes(".box-shadow-none") %>% 
						html_text() %>%
						as_tibble() %>% 
						dplyr::filter(value == "No content available") %>% 
						nrow() > 0
					
					pre_scroll_abandoned <- 
						pre_scroll %>% 
						html_nodes(".event-full-width") %>% 
						html_text() %>% 
						as_tibble() %>% 
						mutate(value = tolower(value)) %>% 
						dplyr::filter(grepl("abandon", value)) %>% 
						nrow()>0
					
					pre_scroll_abandoned2 <- 
						pre_scroll %>% 
						html_nodes(".event") %>% 
						html_text() %>% 
						as_tibble() %>% 
						mutate(value = tolower(value)) %>% 
						dplyr::filter(grepl("abandon", value)) %>% 
						nrow()>0
					
					pre_scroll_no_result <- 
						pre_scroll %>% 
						html_nodes(".event-full-width") %>% 
						html_text() %>% 
						as_tibble() %>% 
						mutate(value = tolower(value)) %>% 
						dplyr::filter(grepl("no result", value)) %>% 
						nrow()>0
					
					if(pre_scroll_empty == TRUE){
						print("This commentary doesn't exist")
					}else if(pre_scroll_abandoned==TRUE){
						print("This match was abandoned")
					}else if(pre_scroll_abandoned2==TRUE){
						print("This match was abandoned")
					}else if(pre_scroll_no_result==TRUE){
						print("This match has no result")
					}else{
						inning_buttons_names <- 
							pre_scroll %>% 
							html_nodes(".team-name") %>% 
							html_text() %>% 
							as_tibble()
						
						inning1_button_name <- 
							paste("//div/div/div[contains(text(),", 
										"'", 
										inning_buttons_names %>% 
											slice(1) %>% 
											pull(),
										" Innings'", 
										")]", 
										sep = "")
						
						inning2_button_name <- 
							paste("//div/div/div[contains(text(),", 
										"'", 
										inning_buttons_names %>% 
											slice(2) %>% 
											pull(),
										" Innings'", 
										")]", 
										sep = "")
						
						inning_button_name <- 
							paste("//div/div/div/div[contains(text(),", 
										"'", 
										"Innings",
										"'", 
										")]", 
										sep = "")
						
						webpage_body <- 
							"//span[contains(text(),  'end of over')]"
						
						element <- remDr$findElement(using = "xpath", inning_button_name)
						element$clickElement()
						Sys.sleep(5)
						element <- remDr$findElement(using = "xpath", inning2_button_name)
						element$clickElement()
						Sys.sleep(5)
						# remDr$screenshot(display = TRUE)
						element <- remDr$findElement(using = "xpath", webpage_body)
						element$clickElement()
						Sys.sleep(5)
						# remDr$screenshot(display = TRUE)
						webElem <- remDr$findElement("css", "body")
						
						print("Scrolling down")
						for(k in 1:scroll_number){
							webElem$sendKeysToElement(list(key = "end"))
							Sys.sleep(2)
							print(k)
						}
						remDr$screenshot(display = TRUE)
						
						page_source<-remDr$getPageSource()
						inning2_html <- read_html(page_source[[1]])
						write_xml(inning2_html, file = commentary_path_2)
						master_commentary_spreadsheet$inning_2_commentary[i] <- commentary_path_2
						write_csv(master_commentary_spreadsheet, year_master_commentary_spreadsheet_path)
						
					}
					remDr$close()
				}else{
					print("Inning 2 has already been scraped")
					master_commentary_spreadsheet$inning_2_commentary[i] <- commentary_path_2
					write_csv(master_commentary_spreadsheet, year_master_commentary_spreadsheet_path)
				}
				master_commentary_spreadsheet$commentary_scraped[i] <- "yes"
				write_csv(master_commentary_spreadsheet, year_master_commentary_spreadsheet_path)
			}else{
				# If the scraper breaks, or when scraping tournaments from the current year, it will pass
				# over games you have already scraped. 
				game_name <- 
					master_commentary_spreadsheet %>%
					slice(i) %>%
					dplyr::select(scorecard_link) %>%
					pull() 
				print(paste(master_commentary_spreadsheet$tournament_name[i], 
										", ", game_name, ", both innings have already been scraped ", sep = ""))
			}
		}else{
			# Games that have not yet been played
			game_name <- 
				master_commentary_spreadsheet %>%
				slice(i) %>%
				dplyr::select(scorecard_link) %>%
				pull() 
			print(paste(master_commentary_spreadsheet$tournament_name[i], 
									", ", game_name, ", will be available on ", this_row$ending_date, sep = ""))
		}
		
	}
}




