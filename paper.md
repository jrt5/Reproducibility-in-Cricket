---
title: "waRne - Putting cricket analytics in a spin"
authors: Robert(1), James (2)
output: 
  html_document:
    toc: true
    toc_float: true
    keep_md: true
bibliography: references.bib

---





## Abstract

The importance of reproducibility, and the related issue of open access to data, has received a lot of recent attention. Momentum on these issues is gathering in the sports analytics community. While cricket is the second largest commercial sport in the world, unlike other popular international sports, there has been no mechanism for the public to access comprehensive statistics on players and teams. Expert commentary currently relies heavily on data that isn't made readily accessible and this produces an unnecessary barrier for the development of an inclusive sports analytics community.

## Introduction

Data access is a key enabler for any analytics community. Most major sports have easy access to match statistics, for example [nbastatR](https://github.com/abresler/nbastatR) for NBA, [Lahman](https://cran.r-project.org/web/packages/Lahman/Lahman.pdf) for baseball, [deuce](https://github.com/skoval/deuce) for tennis and [nflfastR](https://github.com/mrcaseb/nflfastR) for NFL. Through access to sports data and reproducible findings and metrics fans clubs and researchers are better able to understand the game, predict match outcomes and rate players. For example the way teams tackled 4th down decisions through [@romer2006firms]

## What is waRne

## Why cricket needs reproducibility

maybe include stuff around cricviz interesting tweets but fans can recreate or investigate further.

James and I entered a reproducibility competition and the winning entry provided a statistic sort of a gateway into the importance of their ''project'' was was specifically in 2019 Australian Big Bash T20 season. The statistic given was in the 2019 big bash season teams that batted first won only $43$% of games. Thankfully we can try to reproduce that statistic now through the use of waRne

Wikipedia tells us that there were the following:

In the 2018-19 Big Bash League there were 59 matches played. Of which we had 3 DLS results.

| Games                             | Result                        |
|-----------------------------------|-------------------------------|
| 21 DEC 2018 - THUNDER VS STARS    | THUNDER WON BY 15 RUNS (D/L)  |
| 2 FEB 2019 - THUNDER VS SIXERS    | SIXERS WON BY 9 WICKETS (D/L) |
| 8 JAN 2019 HEAT VS THUNDER        | HEAT BY 15 RUNS (D/L)         |
| 17th January 2019 Thunder vs Heat | no result                     |

: Games under consideration in 2018-19 Big Bash


```
##           
##             0  1
##   20102011  8 11
##   20112012 10 18
##   20122013 17 13
##   20132014 13 18
##   20142015 15 17
##   20152016 19 15
##   20162017 19 11
##   20172018 18 20
##   20182019 33 22
##   20192020 20 39
```

\`\`\`

So here we can see that we in the big bash season 2018-19 we have a current winning percentage batting first of 22/(22+33) =0.4 but this doesn't include the duckworth lewis games or the washed out completely game.

If we were to add in the duckworth lewis games we get 24/(24+34) which is equal to 0.414. Which isn't the result.

We can also clearly see that in the 20192020 big bash season the team batting first wins 39/(39+20) or 0.66 so what could be going on? Did Quantium mean only games played in the actual year 2019?

In that case from the 2018-2019 season we would remove the following games

| Date                 | Game                    | Result                   | Winning Team |
|----------------------|-------------------------|--------------------------|--------------|
| 19 December 2018     | Heat Vs Strickers       | Strickers 5 Wickets      | Chased       |
| 20th December 2018   | Scorchers Vs Renegades  | Renegades by 4 wickets   | Chased       |
| 21 December 2018     | Thunder vs Stars        | Thunder by 15 runs (D/L) | Batted First |
| 22 December 2018     | Sixers vs Scorchers     | Sixers by 17 runs        | Batted First |
| 22 December          | Hurricanes vs Heat      | Hurricanes by 15 runs    | Batted First |
| 23 December 2018     | Strikers vs Renegades   | Renegades by 5 wickets   | Chased       |
| 24 December 2018     | Stars vs Hurricanes     | Hurricanes by 6 wickets  | Chased       |
| 24th December 2018   | Thunder vs sixers       | Thunder by 21 runs       | Batted First |
| 26th December 2018   | Strickers vs scorchers  | Scorchers by 7 wickets   | Chased       |
| 27th December        | Sixers vs Stars         | stars by 5 wickets       | Chased       |
| 28th December        | thunder vs hurricanes   | hurricanes by 7 wickets  | Chased       |
| 29th december        | sixers vs renegades     | sixers by 33 runs        | Batted First |
| 30th decemeber 2018  | scorchers vs hurricanes | hurricanes by 6 wickets  | Chased       |
| 31st decemember 2018 | strickers vs thunder    | strickers by 20 runs     | Batted First |

: Games in 2018 of the 2018-19 Big Bash Season

i.e. we remove 14 games here of which 6 teams won batting first 16/(22+33-14)

Then from here we have to add on games in the 2019-2020 big bash season that were played in 2019

+--------------------+-------------------------+---------------------------------------+--------------------------+
| Date               | Teams                   | Result                                | Winning Team             |
+====================+=========================+=======================================+==========================+
| 17 December        | Thunder vs Heat         | Thunder by 29 runs                    | Batted First             |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 18th December      | Scorchers vs Sixers     | Sixers by 8 wickets                   | Chased                   |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 19th december      | Renegades vs thunder    | thunder by 6 wickets                  | chased                   |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 20th decemeber     | hurricanes vs sixers    | hurricanes by 25 runs                 | batted first             |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 20th decemember    | stars vs heat           | stars by 22 runs                      | batting first            |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 21st decemember    | strickers vs thunder    | no result                             | no result                |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 21st december      | scorchers vs renegades  | scorchers by 11 runs                  | batting first            |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 22 deceember       | stars vs hurricanes     | stars by 52 runs                      | batting first            |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 22 decemember      | heat vs sixers          | heat by 48 runs                       | batting first            |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 23 dec             | strickers vs scorchers  | strickers by 15 (DLS)                 | batting first            |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 24th dec           | renegades vs hurricanes | hurricanes by 7 wickets               | chased                   |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 26 decemember      | sixers vs scorchers     | sixers by 48 runs                     | batting first            |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 27 dec             | strickers vs stars      | strikers by 5 runs                    | batting first            |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 28th december      | thunder vs sixers       | sixers won super over (batting first) | sixers by superover      |
|                    |                         |                                       |                          |
|                    |                         |                                       | or sixers batting second |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 29th dec           | strikers vs renegades   | strikers by 18 runs                   | batting first            |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 30th dec           | hurricanes vs stars     | stars by 4 runs (DLS)                 | batting first            |
+--------------------+-------------------------+---------------------------------------+--------------------------+
| 31st december 2019 | thunder vs strikers     | thunder by 3 runs                     | batting first            |
+--------------------+-------------------------+---------------------------------------+--------------------------+

: Games played in 2019 of the 2019-20 Big Bash

12 batting first and 3 batting second

(16+12)/(12+3+22+33-14)

So over the past 2 years (not including DLS) we have

(22+39)/(20+39+33+22)

i.e. \>50% of teams win chasing so what does that mean?

## Easier engagement of fans

To the surprise of many, its hard to engage cricket fans into the analytics behind the game. Reasons for this are generally centred around reproducibility and explainability to fans.

Without an easy accessible medium how can crickets version of an analytics community grow.

something something look towards how EPA has changed the way fans are engaged in NFL analytics - so maybe the change in run rate can be like EPA?


```
## # A tibble: 831,409 x 50
##    match_id inning  over extra_runs earned_runs  runs wicket wickets_taken
##       <int>  <int> <dbl>      <int>       <int> <int> <lgl>          <int>
##  1   439151      2   0.1          0           0     0 FALSE              0
##  2   439150      2   0.1          0           0     0 FALSE              0
##  3   439149      2   0.1          0           0     0 FALSE              0
##  4   439148      2   0.1          0           0     0 FALSE              0
##  5   452152      2   0.1          0           1     1 FALSE              0
##  6   452151      2   0.1          1           0     1 FALSE              0
##  7   452151      2   0.1          0           0     1 FALSE              0
##  8   452150      2   0.1          0           0     0 FALSE              0
##  9   452149      2   0.1          0           0     0 FALSE              0
## 10   452148      2   0.1          4           0     4 FALSE              0
## # … with 831,399 more rows, and 42 more variables: scorecard_link <chr>,
## #   batsman_name <chr>, bowler_name <chr>, extra <chr>, team <chr>,
## #   target <int>, play_description <chr>, batsman <chr>, bowler <chr>,
## #   wicket_description <chr>, year <int>, tournament_type <chr>,
## #   tournament_name <chr>, inning_total <chr>, rpo <dbl>, total <int>,
## #   duckworth_lewis <lgl>, match_summary <chr>, batsman_links <chr>,
## #   batsman_id <int>, bowler_links <chr>, bowler_id <int>, batting_order <int>,
## #   batting_team_result <chr>, bucketed_over <int>, ball_in_over <dbl>,
## #   starting_required_run_rate <dbl>, balls_remaining <int>, resources <dbl>,
## #   runs_required <int>, previous_runs_required <int>,
## #   previous_dls_resources_remain <dbl>, r0 <dbl>, r1 <dbl>, r1_over_r0 <dbl>,
## #   r0_over_r1 <dbl>, binary_result <int>, required_run_rate <dbl>,
## #   previous_required_run_rate <dbl>, change_in_requireed_run_rate <dbl>,
## #   batsman_balls_faced_this_match <int>,
## #   bowler_balls_delivererd_this_match <int>
```

## Cool cricket examples

-   Dhoni spin/pace/yorkr

    -   shows off joins to player information and extracting information from play by play

-   espncricinfo articles but instead of ipl do same tables for big bash

-   recreate stats from espncricinfo like this [page](https://www.espncricinfo.com/series/19297/statistics/1187665/new-zealand-vs-england-1st-t20i-england-in-new-zealand-2019-20)

+------------------------------+--------------------------------------------------------------------------+
| Things to do                 | person                                                                   |
+==============================+==========================================================================+
| clean scrapers/csv for waRne | James                                                                    |
+------------------------------+--------------------------------------------------------------------------+
| data dictionary              | Robert                                                                   |
+------------------------------+--------------------------------------------------------------------------+
| examples                     | Robert - start (assume same csv being used) - cool cricket example stuff |
+------------------------------+--------------------------------------------------------------------------+

## Dhoni Dilemma

With this new dataset, we allow fans to put on their analyst thats and to be able to ask themselves, what exactly makes a good closer and what exactly is the Dhoni Dilemma.
