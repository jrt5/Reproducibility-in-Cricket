# Grabbing the data from the html is fairly straightforward, and by saving the html in the original 
# scraper we are able to go back and make changes to the scraper over time 

extract_data_commentary_scorecards <- function(year_input, root_path){
	
	# define your file path
	data_path <- paste(root_path, "/", "data", sep = "")
	player_path <- paste(data_path, "_files/player_list.Rdata", sep = "")
	
	full_commentary_csv <- 
		read_csv(paste(data_path, "/", year_input, "/master_commentary.csv", sep = "")) %>% 
		mutate(match_id = 0, 
					 rescraping_required = "NA", 
					 row_n = row_number())
	write_csv(full_commentary_csv, paste(data_path, "/", year_input, "/master_commentary.csv", sep = ""))
	
	commentary <- 
		full_commentary_csv %>%
		dplyr::filter(tournament_type %in% c("One-Day Internationals", 
																				 "Twenty20 Internationals", "Twenty20")) %>%
		dplyr::select(tournament_type, tournament_name, inning_1_commentary, inning_2_commentary, 
									scorecard_path, scorecard_link, commentary_link) 
	
	out <- tibble()
	
	# Ok read through each row, but ignore ones you didn't scrape
	for(i in 1:nrow(commentary)){
		
		scorecard_link_check <- 
			commentary$scorecard_link[i]
		commentary_link_check <- 
			commentary$commentary_link[i]
		tournament_name_check <- 
			commentary$tournament_name[i]
		tournament_type_check <- 
			commentary$tournament_type[i]
		
		match_id_num <- as.numeric(word(word(scorecard_link_check, 2, sep = "scorecard/"), 1, sep = "\\/"))
		
		which_row_to_add_info <- 
			full_commentary_csv %>% 
			dplyr::filter(scorecard_link == scorecard_link_check, 
										commentary_link == commentary_link_check, 
										tournament_type %in% c("One-Day Internationals", 
																					 "Twenty20 Internationals", "Twenty20"), 
										# commentary_available == TRUE, 
										tournament_name == tournament_name_check, 
										tournament_type == tournament_type_check) %>% 
			pull(row_n)
		
		print(i)
		print(commentary$scorecard_link[i])
		
		score_path <- 
			commentary$scorecard_path[i]
		inn1_commentary <- 
			commentary$inning_1_commentary[i]
		inn2_commentary <- 
			commentary$inning_2_commentary[i]

		if(!is.na(score_path)){
			scorecard <- 
				xml2::read_html(score_path) 
		}
		if(!is.na(inn1_commentary)){
			inning1_commentary <- 
				xml2::read_html(inn1_commentary) 
			inning1_scorecard <- 
				scorecard %>% 
				html_nodes(".card.content-block") %>% 
				extract(1)
		}
		if(!is.na(inn2_commentary)){
			inning2_commentary <- 
				xml2::read_html(inn2_commentary) 
			inning2_scorecard <- 
				scorecard %>% 
				html_nodes(".card.content-block") %>% 
				extract(2)
		}
		
		if(!is.na(inn1_commentary)){

			# connecting the batsmen and bowlers by their id is tricky. The scorecard has hyperlinks to their 
			# names, we take advantage of that to better connect players 
			
			batsmen_who_batted <- 
				inning1_scorecard %>% 
				html_nodes(".table.batsman") %>% 
				html_nodes(".batsman-cell") %>% 
				html_text() %>% 
				as_tibble() %>% 
				mutate(value = 
							 	str_trim(value), 
							 value = 
							 	case_when(str_detect(value, ",")==TRUE~str_remove(value, ","), 
							 						str_detect(value, ",")==FALSE~value), 
							 value = 
							 	case_when(str_detect(value, "\\(c\\)")==TRUE~str_remove(value, "\\(c\\)"), 
							 						str_detect(value, "\\(c\\)")==FALSE~value), 
							 value = 
							 	case_when(str_detect(value, "†")==TRUE~str_remove(value, "†"), 
							 						str_detect(value, "†")==FALSE~value), 
							 value = 
							 	str_trim(value), 
							 batting_order = row_number(), 
							 batsman_links = 
							 	inning1_scorecard %>% 
							 	html_nodes(".table.batsman") %>% 
							 	html_nodes(".batsman-cell") %>% 
							 	html_nodes("a") %>% 
							 	html_attr("href") %>% 
							 	as_tibble() %>% 
							 	pull()) %>% 
				rename(batsman_name = value) %>% 
				dplyr::select(-batting_order) %>% 
				mutate(batsman_id = as.numeric(word(word(batsman_links, 
																								 2, 
																								 sep = "player\\/"), 
																						1, 
																						sep = "\\.html")))
			
			bowler_names <- 
				inning1_scorecard %>% 
				html_nodes(".table.bowler") %>% 
				html_nodes(".text-nowrap") %>% 
				html_text() %>% 
				as_tibble() %>% 
				rename(bowler_name = value) %>% 
				mutate(bowler_links = 
							 	inning1_scorecard %>% 
							 	html_nodes(".table.bowler") %>%
							 	html_nodes("a") %>% 
							 	html_attr("href") %>% 
							 	as_tibble() %>% 
							 	pull()) %>% 
				mutate(bowler_id = as.numeric(word(word(bowler_links, 
																								2, 
																								sep = "player\\/"), 
																					 1, 
																					 sep = "\\.html")))
			
			# Sav this player link to another dataset to scrape their links, which you can reconnect 
			# to the data later
			
			if(!file.exists(player_path)){
				player_list <- 
					tibble()
			}else{
				load(player_path)
			}
			
			inning_players_data <- 
				batsmen_who_batted %>% 
				dplyr::select(batsman_name, batsman_links, batsman_id) %>% 
				rename(player_name = batsman_name, 
							 player_links = batsman_links, 
							 player_id = batsman_id) %>% 
				bind_rows(bowler_names %>% 
										rename(player_name = bowler_name, 
													 player_links = bowler_links, 
													 player_id = bowler_id))
			
			player_list <- 
				player_list %>% 
				bind_rows(inning_players_data) %>% 
				distinct(player_name, player_id, player_links)
			
			save(player_list, file = player_path)

			# This is the inning summary
			
			inning_sum <-
				inning1_scorecard %>% 
				html_nodes("tfoot") %>% 
				html_nodes(".thead-light") %>% 
				html_nodes(".text-right") %>% 
				extract(1) %>% 
				html_text() %>% 
				as_tibble() %>% 
				bind_cols(inning1_scorecard %>% 
										html_nodes("tfoot") %>% 
										html_nodes(".thead-light") %>% 
										html_nodes(".text-left") %>% 
										html_text() %>% 
										as_tibble()) %>% 
				mutate(summary = paste(value, value1, sep = " ")) %>% 
				pull(summary)
			
			# match summary
			match_sum <- 
				inning1_commentary %>% 
				html_nodes(".match-header") %>% 
				html_nodes(".summary") %>% 
				html_text() 
			
			# First inning play by play data, each variable is self explanatory
			out1 <- 
				tibble(over = 
							 	inning1_commentary %>% 
							 	html_nodes(".match-comment-over") %>% 
							 	html_text() %>% 
							 	as.numeric()) %>%
				bind_cols(inning1_commentary %>% 
										html_nodes(".match-comment-run") %>% 
										html_text() %>% 
										as_tibble() %>%
										mutate(value = if_else(value == "•", "0", value), 
													 text = str_remove(value, "[1234567890]"), 
													 nums = suppressWarnings(parse_number(value)),
													 nums = if_else(is.na(nums), 0, nums),
													 # nums = if_else(text %in% c("w", "lb", "nb", "b"), 0, as.numeric(nums)),
													 extra_runs = if_else(text %in% c("w", "lb", "nb", "b"), as.numeric(nums), 0) %>%
													 	as.numeric(), 
													 extra_runs = if_else(is.na(extra_runs), 0, extra_runs),
													 earned_runs = if_else(nchar(text)==0, nums, 0) %>%
													 	as.numeric(),
													 extra = case_when(text == "w" & nums != 0 ~ "wide", 
													 									text == "lb" ~ "leg_bye", 
													 									text == "nb" ~ "no_ball", 
													 									text == "b" ~ "bye"), 
													 wicket = if_else(value == "w" & nums == 0, TRUE, FALSE)
													 # , 
													 # leg_bye = if_else(text == "lb", TRUE, FALSE, missing = FALSE), 
													 # wide = if_else(text == "w", TRUE, FALSE, missing = FALSE), 
													 # bye = if_else(text == "b", TRUE, FALSE, missing = FALSE), 
													 # no_ball = if_else(text == "nb", TRUE, FALSE, missing = FALSE)
										) %>%
										dplyr::select(-c(value, nums, text))) %>%
				mutate(team = 
							 	inning1_commentary %>% 
							 	html_nodes(".team-name") %>% 
							 	html_nodes(".font-weight-bold") %>% 
							 	html_attr("title") %>% 
							 	as_tibble() %>% 
							 	slice(1) %>% 
							 	pull(), 
							 inning = 1, 
							 # "home/away" = "away", 
							 target = 0, 
							 play_description = 
							 	inning1_commentary %>% 
							 	html_nodes(".d-flex.match-comment-padder.align-items-center") %>% 
							 	html_nodes(".match-comment-wrapper") %>%
							 	# html_nodes(".match-comment-short-text") %>%
							 	html_text() %>% 
							 	as_tibble() %>% 
							 	mutate(value = str_remove(str_replace(str_remove(value, "\\\n"), "\\\n", ". "), "\\\n")) %>% 
							 	pull(), 
							 batsman = word(word(play_description,1,sep = "\\,"), 2, sep = " to "), #name of batsman
							 bowler = word(word(play_description,1,sep = "\\,"), 1, sep = " to "), #name of bowler
							 wicket_description = add_dismissal_description(inning1_commentary), 
							 year = year_input,
							 tournament_type = commentary$tournament_type[i], 
							 tournament_name = commentary$tournament_name[i], 
							 match_id = match_id_num, 
							 scorecard_link = scorecard_link_check, 
							 match_summary = match_sum, 
							 inning_summary = inning_sum
							 # wicket = get_wicket(temp1), #yes or no, if a wicket is lost on this play
							 # runs = get_earned_runs(temp1), #runs earned by play
							 # extra_runs = , #runs from an extra 
							 # extra_type = , #type of extra
							 # score = , #team score at this point
							 # over = , #what over we are on
							 # nb = , #yes or no, no ball
							 # wb = , #yes or no, wide ball
							 # lb = , #yes or no, leg bye
							 # dismissal = , #caught, bowled, run out, stumped, leg before wicket, hit wicket, NA (not out)
							 # 
							 #   )
				) %>%
				arrange((over)) %>% 
				mutate(over_number = floor(over), 
							 ball_in_over = 10*(over-over_number), 
							 over_number = ceiling(over), 
							 required_run_rate = 0, 
							 balls_remaining = if_else(tournament_type == "One-Day International", 300-(6*over_number-abs(7-ball_in_over)), 120-(6*over_number-abs(7-ball_in_over)))) 
				
			# when you flip it to go from over 0.1 to the end, the wickets and extras are backwards, so this 
			# flips them back into the right order
			for(z in 1:(nrow(out1)-1)){
				if(i != nrow(out1)){
					if(out1$over[z]==out1$over[z+1]){
						check_em <- 
							out1 %>% 
							slice(z:(z+1))
						
						if(check_em$wicket[1]==TRUE & check_em$wicket[2]==FALSE){
							out1[z,] = check_em[2,]
							out1[z+1,] = check_em[1,]
						}
						if(is.na(check_em$extra[1]) & !is.na(check_em$wicket[2])){
							out1[z,] = check_em[2,]
							out1[z+1,] = check_em[1,]
						}
					}
				}
			}
			
			# Let's add a few more variables like rpo, a running total of runs, etc
			out1 <- 
				out1 %>% 
				mutate(inning_runs = 
							 	earned_runs+extra_runs, 
							 runs = 
							 	sapply(1:length(inning_runs), 
							 				 function(x) sum(inning_runs[1:x])),
							 rpo = 
							 	as.numeric(word(as.character(over), 1, sep = "\\.")) %>%
							 	as_tibble() %>%
							 	rename(overs_faced = value) %>%
							 	mutate(over_fraction = as.numeric(word(as.character(over), 2, sep = "\\."))/6, 
							 				 rpo = runs/(overs_faced+over_fraction)) %>% 
							 	pull(rpo), 
							 total = max(runs)) %>% 
				dplyr::select(-inning_runs) %>% 
				mutate(wickets_taken = sapply(1:length(wicket), 
																			function(x)sum(wicket[1:x]==TRUE)), 
							 duckworth_lewis = 
							 	inning1_commentary %>% 
							 	html_nodes(".match-header") %>% 
							 	html_nodes(".summary") %>% 
							 	html_text() %>% 
							 	as_tibble() %>% 
							 	dplyr::filter(grepl("D/L method", value)) %>% 
							 	nrow()>0)
			
			# Back to the most difficult aspect of this scraping project, connecting batsmen and bowler name
			# to the dataset. They are typially addressed by last name, which makes it simple to identify
			# from the list of batsmen and bowlers above. The difficulty comes from multiple players with the 
			# same last name in a match, ? values instead of player names, misspelled names. And those are 
			# just the problems I have found so far. This will be continuously updated over time as they
			# are addressed. ? has not yet been fixed
			
			bowler_data_inning_1 <- 
				out1 %>% 
				dplyr::select(bowler, play_description) %>% 
				mutate(scorecard_name = "")
			
			bowler_list_inning_1 <- tibble()
			
			for(b in 1:nrow(bowler_data_inning_1)){
				
				this_bowler <- 
					bowler_names %>% 
					dplyr::filter(grepl(str_trim(bowler_data_inning_1 %>% 
																			 	slice(b) %>% 
																			 	pull(bowler)), 
															bowler_name)) %>% 
					mutate(play_description = 
								 	bowler_data_inning_1 %>% 
								 	slice(b) %>% 
								 	pull(play_description))
				
				bowler_list_inning_1 <- 
					bowler_list_inning_1 %>% 
					bind_rows(this_bowler)
				
			}
			batting_data_inning_1 <- 
				out1 %>% 
				dplyr::select(batsman, play_description) %>% 
				mutate(scorecard_name = "")
			
			batting_list_inning_1 <- tibble()
			
			for(b in 1:nrow(batting_data_inning_1)){
				
				this_bowler <- 
					batsmen_who_batted %>% 
					dplyr::filter(grepl(str_trim(batting_data_inning_1 %>% 
																			 	slice(b) %>% 
																			 	pull(batsman)), 
															batsman_name)) %>% 
					mutate(play_description = 
								 	batting_data_inning_1 %>% 
								 	slice(b) %>% 
								 	pull(play_description))
				
				batting_list_inning_1 <- 
					batting_list_inning_1 %>% 
					bind_rows(this_bowler)
				
			}
			
			batting_order <- 
				batting_list_inning_1 %>% 
				distinct(batsman_links) %>% 
				mutate(batting_order = row_number())
			out1 <- 
				out1 %>% 
				left_join(batting_list_inning_1, by = c("play_description" = "play_description")) %>% 
				left_join(bowler_list_inning_1, by = c("play_description" = "play_description")) %>% 
				left_join(batting_order, by = c("batsman_links" = "batsman_links")) %>% 
				distinct()
			
			# Because of the scrolling required to access all the data in the html, sometimes the webite
			# was not fully loaded while scraping. This is a check, if it needs rescraping it will not
			# include over 0.1, which means variables like runs, rpo, wickets, etc. will be wrong
			min_over <- 
				min(out1$over)
			
			if((min_over>0.1)){
				needs_to_be_rescraped_1 = "yes"
			}else{
				needs_to_be_rescraped_1 = "no"
			}
			
		}else{
			out1 <- tibble()
			needs_to_be_rescraped_1 = "no"
		}
		if(!is.na(inn2_commentary)){
			
			batsmen_who_batted <- 
				inning2_scorecard %>% 
				html_nodes(".table.batsman") %>% 
				html_nodes(".batsman-cell") %>% 
				html_text() %>% 
				as_tibble() %>% 
				mutate(value = 
							 	str_trim(value), 
							 value = 
							 	case_when(str_detect(value, ",")==TRUE~str_remove(value, ","), 
							 						str_detect(value, ",")==FALSE~value), 
							 value = 
							 	case_when(str_detect(value, "(c)")==TRUE~str_remove(value, "(c)"), 
							 						str_detect(value, "(c)")==FALSE~value), 
							 value = 
							 	case_when(str_detect(value, "†")==TRUE~str_remove(value, "†"), 
							 						str_detect(value, "†")==FALSE~value), 
							 value = 
							 	str_trim(value), 
							 batting_order = row_number(), 
							 batsman_links = 
							 	inning2_scorecard %>% 
							 	html_nodes(".table.batsman") %>% 
							 	html_nodes(".batsman-cell") %>% 
							 	html_nodes("a") %>% 
							 	html_attr("href") %>% 
							 	as_tibble() %>% 
							 	pull()) %>% 
				rename(batsman_name = value) %>% 
				dplyr::select(-batting_order) %>% 
				mutate(batsman_id = as.numeric(word(word(batsman_links, 
																								 2, 
																								 sep = "player\\/"), 
																						1, 
																						sep = "\\.html")))
			
			bowler_names <- 
				inning2_scorecard %>% 
				html_nodes(".table.bowler") %>% 
				html_nodes(".text-nowrap") %>% 
				html_text() %>% 
				as_tibble() %>% 
				rename(bowler_name = value) %>% 
				mutate(bowler_links = 
							 	inning2_scorecard %>% 
							 	html_nodes(".table.bowler") %>%
							 	html_nodes("a") %>% 
							 	html_attr("href") %>% 
							 	as_tibble() %>% 
							 	pull()) %>% 
				mutate(bowler_id = as.numeric(word(word(bowler_links, 
																								2, 
																								sep = "player\\/"), 
																					 1, 
																					 sep = "\\.html")))
			
			if(!file.exists(player_path)){
				player_list <- 
					tibble()
			}else{
				load(player_path)
			}
			
			inning_players_data <- 
				batsmen_who_batted %>% 
				dplyr::select(batsman_name, batsman_links, batsman_id) %>% 
				rename(player_name = batsman_name, 
							 player_links = batsman_links, 
							 player_id = batsman_id) %>% 
				bind_rows(bowler_names %>% 
										rename(player_name = bowler_name, 
													 player_links = bowler_links, 
													 player_id = bowler_id))
			
			player_list <- 
				player_list %>% 
				bind_rows(inning_players_data) %>% 
				distinct(player_name, player_id, player_links)
			
			save(player_list, file = player_path)
			
			inning_sum <-
				inning2_scorecard %>% 
				html_nodes("tfoot") %>% 
				html_nodes(".thead-light") %>% 
				html_nodes(".text-right") %>% 
				extract(1) %>% 
				html_text() %>% 
				as_tibble() %>% 
				bind_cols(inning1_scorecard %>% 
										html_nodes("tfoot") %>% 
										html_nodes(".thead-light") %>% 
										html_nodes(".text-left") %>% 
										html_text() %>% 
										as_tibble()) %>% 
				mutate(summary = paste(value, value1, sep = " ")) %>% 
				pull(summary)
			
			match_sum <- 
				inning2_commentary %>% 
				html_nodes(".match-header") %>% 
				html_nodes(".summary") %>% 
				html_text() 
			
			out2 <- 
				tibble(over = inning2_commentary %>% 
							 	html_nodes(".match-comment-over") %>% 
							 	html_text() %>% 
							 	as.numeric()) %>%
				bind_cols(inning2_commentary %>% 
										html_nodes(".match-comment-run") %>% 
										html_text() %>% 
										as_tibble() %>%
										mutate(value = if_else(value == "•", "0", value), 
													 text = str_remove(value, "[1234567890]"), 
													 nums = suppressWarnings(parse_number(value)),
													 nums = if_else(is.na(nums), 0, nums),
													 # nums = if_else(text %in% c("w", "lb", "nb", "b"), 0, as.numeric(nums)),
													 extra_runs = if_else(text %in% c("w", "lb", "nb", "b"), as.numeric(nums), 0) %>%
													 	as.numeric(), 
													 extra_runs = if_else(is.na(extra_runs), 0, extra_runs),
													 earned_runs = if_else(nchar(text)==0, nums, 0) %>%
													 	as.numeric(),
													 extra = case_when(text == "w" & nums != 0 ~ "wide", 
													 									text == "lb" ~ "leg_bye", 
													 									text == "nb" ~ "no_ball", 
													 									text == "b" ~ "bye"), 
													 wicket = if_else(value == "w" & nums == 0, TRUE, FALSE)
													 # , 
													 # leg_bye = if_else(text == "lb", TRUE, FALSE, missing = FALSE), 
													 # wide = if_else(text == "w", TRUE, FALSE, missing = FALSE), 
													 # bye = if_else(text == "b", TRUE, FALSE, missing = FALSE), 
													 # no_ball = if_else(text == "nb", TRUE, FALSE, missing = FALSE)
										) %>%
										dplyr::select(-c(value, nums, text))) %>%
				mutate(team = 
							 	inning2_commentary %>% 
							 	html_nodes(".team-name") %>% 
							 	html_nodes(".font-weight-bold") %>% 
							 	html_attr("title") %>% 
							 	as_tibble() %>% 
							 	slice(2) %>% 
							 	pull(), 
							 inning = 2, 
							 # "home/away" = "away", 
							 target = unique(out1$total), 
							 play_description = 
							 	inning2_commentary %>% 
							 	html_nodes(".d-flex.match-comment-padder.align-items-center") %>% 
							 	html_nodes(".match-comment-wrapper") %>%
							 	# html_nodes(".match-comment-short-text") %>%
							 	html_text() %>% 
							 	as_tibble() %>% 
							 	mutate(value = str_remove(str_replace(str_remove(value, "\\\n"), "\\\n", ". "), "\\\n")) %>% 
							 	pull(),
							 batsman = word(word(play_description,1,sep = "\\,"), 2, sep = " to "), #name of batsman
							 bowler = word(word(play_description,1,sep = "\\,"), 1, sep = " to "), #name of bowler
							 wicket_description = add_dismissal_description(inning2_commentary), 
							 year = year_input,
							 tournament_type = commentary$tournament_type[i], 
							 tournament_name = commentary$tournament_name[i], 
							 match_id = match_id_num, 
							 scorecard_link = scorecard_link_check, 
							 match_summary = match_sum, 
							 inning_summary = inning_sum
							 # wicket = get_wicket(temp1), #yes or no, if a wicket is lost on this play
							 # runs = get_earned_runs(temp1), #runs earned by play
							 # extra_runs = , #runs from an extra 
							 # extra_type = , #type of extra
							 # score = , #team score at this point
							 # over = , #what over we are on
							 # nb = , #yes or no, no ball
							 # wb = , #yes or no, wide ball
							 # lb = , #yes or no, leg bye
							 # dismissal = , #caught, bowled, run out, stumped, leg before wicket, hit wicket, NA (not out)
							 # 
							 #   )
				) %>%
				arrange((over)) %>% 
				mutate(over_number = floor(over), 
							 ball_in_over = 10*(over-over_number), 
							 over_number = ceiling(over), 
							 balls_remaining = if_else(tournament_type == "One-Day International", 300-(6*over_number-abs(7-ball_in_over)), 120-(6*over_number-abs(7-ball_in_over))), 
							 required_run_rate = target/balls_remaining*6) 
			
			for(z in 1:(nrow(out2)-1)){
				if(i != nrow(out2)){
					if(out2$over[z]==out2$over[z+1]){
						check_em <- 
							out2 %>% 
							slice(z:(z+1))
						
						if(check_em$wicket[1]==TRUE & check_em$wicket[2]==FALSE){
							out2[z,] = check_em[2,]
							out2[z+1,] = check_em[1,]
						}
						if(is.na(check_em$extra[1]) & !is.na(check_em$wicket[2])){
							out2[z,] = check_em[2,]
							out2[z+1,] = check_em[1,]
						}
					}
				}
			}
			
			out2 <- 
				out2 %>% 
				mutate(inning_runs = 
							 	earned_runs+extra_runs, 
							 runs = 
							 	sapply(1:length(inning_runs), 
							 				 function(x) sum(inning_runs[1:x])),
							 rpo = 
							 	as.numeric(word(as.character(over), 1, sep = "\\.")) %>%
							 	as_tibble() %>%
							 	rename(overs_faced = value) %>%
							 	mutate(over_fraction = as.numeric(word(as.character(over), 2, sep = "\\."))/6, 
							 				 rpo = runs/(overs_faced+over_fraction)) %>% 
							 	pull(rpo), 
							 total = max(runs)) %>% 
				dplyr::select(-inning_runs) %>% 
				mutate(wickets_taken = sapply(1:length(wicket), 
																			function(x)sum(wicket[1:x]==TRUE)), 
							 duckworth_lewis = 
							 	inning2_commentary %>% 
							 	html_nodes(".match-header") %>% 
							 	html_nodes(".summary") %>% 
							 	html_text() %>% 
							 	as_tibble() %>% 
							 	dplyr::filter(grepl("D/L method", value)) %>% 
							 	nrow()>0)
			
			bowler_data_inning_2 <- 
				out2 %>% 
				dplyr::select(bowler, play_description) %>% 
				mutate(scorecard_name = "")
			
			bowler_list_inning_2 <- tibble()
			
			for(b in 1:nrow(bowler_data_inning_2)){
				
				this_bowler <- 
					bowler_names %>% 
					dplyr::filter(grepl(str_trim(bowler_data_inning_2 %>% 
																			 	slice(b) %>% 
																			 	pull(bowler)), 
															bowler_name)) %>% 
					mutate(play_description = 
								 	bowler_data_inning_2 %>% 
								 	slice(b) %>% 
								 	pull(play_description))
				
				bowler_list_inning_2 <- 
					bowler_list_inning_2 %>% 
					bind_rows(this_bowler)
				
			}
			batting_data_inning_2 <- 
				out2 %>% 
				dplyr::select(batsman, play_description) %>% 
				mutate(scorecard_name = "")
			
			batting_list_inning_2 <- tibble()
			
			for(b in 1:nrow(batting_data_inning_2)){
				
				this_bowler <- 
					batsmen_who_batted %>% 
					dplyr::filter(grepl(str_trim(batting_data_inning_2 %>% 
																			 	slice(b) %>% 
																			 	pull(batsman)), 
															batsman_name)) %>% 
					mutate(play_description = 
								 	batting_data_inning_2 %>% 
								 	slice(b) %>% 
								 	pull(play_description))
				
				batting_list_inning_2 <- 
					batting_list_inning_2 %>% 
					bind_rows(this_bowler)
				
			}
			
			batting_order <- 
				batting_list_inning_2 %>% 
				distinct(batsman_links) %>% 
				mutate(batting_order = row_number())
			out2 <- 
				out2 %>% 
				left_join(batting_list_inning_2, by = c("play_description" = "play_description")) %>% 
				left_join(bowler_list_inning_2, by = c("play_description" = "play_description")) %>% 
				left_join(batting_order, by = c("batsman_links" = "batsman_links")) %>% 
				distinct()
			
			min_over <- 
				min(out2$over)
			
			if((min_over>0.1)){
				needs_to_be_rescraped_2 = "yes"
			}else{
				needs_to_be_rescraped_2 = "no"
			}
			
			out2_result <- max(out2$runs)
			
			if(out2_result>mean(out2$target)){
				out2 <- 
					out2 %>% 
					mutate(batting_team_result = "win")
				out1 <- 
					out1 %>% 
					mutate(batting_team_result = "lose")
			}else if(out2_result<mean(out2$target)){
				out2 <- 
					out2 %>% 
					mutate(batting_team_result = "lose")
				out1 <- 
					out1 %>% 
					mutate(batting_team_result = "win")
			}else{
				out2 <- 
					out2 %>% 
					mutate(batting_team_result = "super-over")
				out1 <- 
					out1 %>% 
					mutate(batting_team_result = "super-over")
			}
			
		}else{
			out2 <- tibble()
			needs_to_be_rescraped_2 = "no"
		}
		
		if(needs_to_be_rescraped_2 =="yes" | needs_to_be_rescraped_1=="yes"){
			full_commentary_csv$rescraping_required[which_row_to_add_info] <- "yes"
			full_commentary_csv$match_id[which_row_to_add_info] <- match_id_num
			write_csv(full_commentary_csv, paste(data_path, "/", year_input, "/master_commentary.csv", sep = ""))
		}else{
			full_commentary_csv$rescraping_required[which_row_to_add_info] <- "no"
			full_commentary_csv$match_id[which_row_to_add_info] <- match_id_num
			write_csv(full_commentary_csv, paste(data_path, "/", year_input, "/master_commentary.csv", sep = ""))
		}
		
		if(nrow(out1)>0 & nrow(out2)>0){
			out <- 
				out %>%
				bind_rows(out1, out2) %>% 
				group_by(batsman_id, inning) %>% 
				mutate(batsman_balls_faced_this_match = sapply(1:length(batsman_id), 
																											 function(x) sum(batsman_id[1:x]==batsman_id[x]))) %>% 
				group_by(bowler_id, inning) %>% 
				mutate(bowler_balls_delivered_this_match = sapply(1:length(bowler_id), 
																													function(x) sum(bowler_id[1:x]==bowler_id[x]))) %>% 
				dplyr::select(-c(bowler, batsman))
		}
		
	}
	return(out)
}


add_dismissal_description <- function(dat){
	
	out2 <- c()
	
	wicket <- 
		dat %>% 
		html_nodes(".match-comment-run") %>% 
		html_text() %>% 
		as_tibble() %>%
		mutate(value = if_else(value == "•", "0", value), 
					 text = str_remove(value, "[1234567890]"), 
					 nums = suppressWarnings(parse_number(value)),
					 nums = if_else(is.na(nums), 0, nums),
					 # nums = if_else(text %in% c("w", "lb", "nb", "b"), 0, as.numeric(nums)),
					 extra_runs = if_else(text %in% c("w", "lb", "nb", "b"), as.numeric(nums), 0) %>%
					 	as.numeric(), 
					 extra_runs = if_else(is.na(extra_runs), 0, extra_runs),
					 earned_runs = if_else(nchar(text)==0, nums, 0) %>%
					 	as.numeric(),
					 extra = case_when(text == "w" & nums != 0 ~ "wide", 
					 									text == "lb" ~ "leg_bye", 
					 									text == "nb" ~ "no_ball", 
					 									text == "b" ~ "bye"), 
					 wicket = if_else(value == "w" & nums == 0, TRUE, FALSE)
					 # , 
					 # leg_bye = if_else(text == "lb", TRUE, FALSE, missing = FALSE), 
					 # wide = if_else(text == "w", TRUE, FALSE, missing = FALSE), 
					 # bye = if_else(text == "b", TRUE, FALSE, missing = FALSE), 
					 # no_ball = if_else(text == "nb", TRUE, FALSE, missing = FALSE)
		) %>%
		dplyr::select(-c(value, nums, text, extra, earned_runs, extra_runs)) %>% 
		pull()
	
	temp <- 
		dat %>% 
		html_nodes(".d-flex.match-comment-padder.align-items-center") %>%
		html_nodes(".match-comment-wicket") %>% 
		html_text() %>% 
		as_tibble() %>% 
		pull()
	
	counter = 1
	for(k in 1:length(wicket)){
		# print(out2)
		if(wicket[k]!=TRUE){
			out2 <- c(out2, NA)
		}else{
			out2 = c(out2, temp[counter])
			counter = counter + 1
		}
	}
	
	return(out2)
	
}
