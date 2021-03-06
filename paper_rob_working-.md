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

The importance of reproducibility, and the related issue of open access to data, has received a lot of recent attention.\^[why this not work] Momentum on these issues is gathering in the sports analytics community. While cricket is the second largest commercial sport in the world, unlike other popular international sports, there has been no mechanism for the public to access comprehensive statistics on players and teams. Expert commentary currently relies heavily on data that isn't made readily accessible and this produces an unnecessary barrier for the development of an inclusive sports analytics community.

## Introduction

Data access is a key enabler for any analytics community. Most major sports have easy access to match statistics, for example [nbastatR](https://github.com/abresler/nbastatR) for NBA, [Lahman](https://cran.r-project.org/web/packages/Lahman/Lahman.pdf) for baseball, [deuce](https://github.com/skoval/deuce) for tennis and [nflfastR](https://github.com/mrcaseb/nflfastR) for NFL. Through access to sports data and reproducible findings and metrics fans clubs and researchers are better able to understand the game, predict match outcomes and rate players. For example the way teams tackled 4th down decisions in the NFL has changed since [@romer2006firms] seminial work. As teams have changed their 4th down decision making this allows follow up research such as [@yam2019lost] which looked at more granular data to see if this happens in practice.

By making data more accessible and more advanced metrics more accessible fans data journalism in sports has grown in recent years. For example in [@horowitz2017nflscrapr] has enabled EPA to enter popular discussion among fans.

Cricket is the second largest sport in the world. However, unfortunetaly there is no easy accessible way to access ball by ball data nor aggregated statistics of teams and players. Data while available on sites like [espncricinfo](https://www.espncricinfo.com/) are not in an easy to use form. For example, each match is listed on different webpages so hours upon hours of time would be required to copy and paste a single season, not to mention the added difficulty of linking players between games and competitions in different countries. Hence, there are significant logistical barriers for prospective fans and analysts studying the game, which stagnats understanding of cricket.

This paper describes the waRne package, the first to provide free and easy access to data for cricket for fans. Web scraping tools are avaiable for fans to easily scrape the play by play commentary data on espncricinfo. For the first time fans can evaluate their favourite teams and players and do so in a reproducible and accessible manner. We hope that this package can be used for fans to better understand the game, for teachers to use for interesting examples in class and for analysts in clubland who might not have access to ball by ball data.

## Why cricket needs reproducibility

Data accessibility enables fans, analyts and researchers to better understand the game. Through being able to reproduce common popular metrics, visualisations and article findings.

Through being able to reproduce, fans are able to make accessible findings for others and importantly they are able to extend and grow concepts. Unfortuntely what we see through leading cricket analytics providers is a track record of confusing output for fans. This can lead to lower engagement and dismissial of cricket analytics.

For example in this [series of tweets](https://twitter.com/benjonescricket/status/1219898578719723520) we see the narrative being pushed that Steve Smith is a good player vs pace bowling unfortunetly just a few months prior the same company and journalist published an article which had Steve Smith doing much worse against pace (balls above 140km/h). Unlike a similar sport baseball, fans have no easily accisible way of seeing if Steve Smith vs pace is a strength as alluded to in the original tweet, or a weakness like the same persons published online article. In comparsion, fans are able to get a breakdown of Mike Trout vs fastballs from using [baseballr](https://billpetti.github.io/2018-02-19-build-statcast-database-rstats/) which provides access through statcast data from [baseballsavant](https://baseballsavant.mlb.com/).

Unfortunetly this is just one of many examples whereby a relatively simple statistic is provided by media and fans have no mechanism to fact check. Fact checking is an important avenue for fans to not only engage and understand statistics, but having this mechanism also stops analytics people/companies from putting out misleading conclusions and findings.

However though using waRne we are able to not only compare Steve Smiths performance vs bowling type but we are able to easily compare Steve Smiths performance vs bowling types with other players.

### Steve Smith vs Bowling Type Chart

Fans of cricket and Australian cricket especially might be interested to see if this is a statistical qwerk of small samples or if Steve Smith generally does perform as an elite cricketer vs pace bowling. Teachers of statistics classes might use this as an interesting example to introduce [tidyverse](https://www.tidyverse.org/learn/) principals [@wickham2019welcome].


```
## `summarise()` regrouping output by 'bowling_style' (override with `.groups` argument)
```

<img src="paper_rob_working-_files/figure-html/unnamed-chunk-1-1.png" width="75%" style="display: block; margin: auto;" />



## Using waRne to teach undergraduate statistics

We can also use waRne and other R packages as an easy way to engage students in learning statistical concepts.

For example using the above we can not only look at the answers graphically, but we can run a statistical test. A common classroom example for learning the binomial distribution is to ask if a coin is based given a proportion of heads and tails given a sample size [@mcelreath2020statistical]. Instead of asking about coins, fans of sport and cricket might be interested to ask the question; after winning the coin toss, should a team decide to bat first or bat second.

While a coin toss is a seemingly trivial example, like in most professional sports the winner of the coin toss gets to decide what to do. In cricket, the winner of the coin toss gets to decide if they want to bat first and thus set the total that the opposition needs to pass by a single run to win, or if they want to bowl first and thus chase down the total set. Deciding what to do after winning a coin toss has proved to be a popular media piece in recent years \footnote{https://www.espncricinfo.com/video/clip/\_/id/23230682}

(<https://www.espncricinfo.com/video/clip/_/id/23230682%5D>)} \^[[https://www.espncricinfo.com/story/\_/id/21489056/stuart-wark-cricket-move-away-coin-toss?]](https://www.espncricinfo.com/story/_/id/21489056/stuart-wark-cricket-move-away-coin-toss?%5D) \^[[https://www.espncricinfo.com/story/\_/id/20429499/lehmann-backs-scrapping-toss]](https://www.espncricinfo.com/story/_/id/20429499/lehmann-backs-scrapping-toss%5D) \^[[https://www.espncricinfo.com/story/\_/id/28770755/how-much-does-losing-tosses-impact-visiting-teams]](https://www.espncricinfo.com/story/_/id/28770755/how-much-does-losing-tosses-impact-visiting-teams%5D) \^[<https://www.forbes.com/sites/tristanlavalette/2018/08/20/are-cricket-matches-being-decided-by-the-luck-of-a-coin-toss/#735456837eff>]

\^[<https://www.statsinsider.com.au/bbl/how-important-is-winning-the-toss-in-the-big-bash-league>]

\^[<https://www.espncricinfo.com/story/_/id/18568387/tim-wigmore-how-batting-second-become-more-fruitful-more-popular>]

Fans and analysts of the game might want to make this decision based on a simple statistic, for example they might make it based on answer these questions instead.

-   Do teams that bat second have a higher winning percentage than those who bat first?

-   Is this consistent across leagues and levels of cricket?



Instead of just looking at this graphically, fans of crickets and teachers of undergraduate statistics might use the dataset as a ''biased coin'' example. So instead of asking, given a proportion of heads over \$n\$ tosses instructers could ask is a team better off batting first or second.



## Easier engagement of fans

To the surprise of many, its hard to engage cricket fans into the analytics behind the game. Reasons for this are generally centred around reproducibility and explainability to fans.

Without an easy accessible medium how can crickets version of an analytics community grow.

something something look towards how EPA has changed the way fans are engaged in NFL analytics - so maybe the change in run rate can be like EPA?

<https://twitter.com/cricvizanalyst/status/1311260989267087361>

## Cool cricket examples

-   Dhoni spin/pace/yorkr

    -   shows off joins to player information and extracting information from play by play

-   espncricinfo articles but instead of ipl do same tables for big bash

-   recreate stats from espncricinfo like this [page](https://www.espncricinfo.com/series/19297/statistics/1187665/new-zealand-vs-england-1st-t20i-england-in-new-zealand-2019-20)

## Dhoni Dilemma

With this new dataset, we allow fans to put on their analyst thats and to be able to ask themselves, what exactly makes a good closer and what exactly is the Dhoni Dilemma.

To understand the Dhoni Deilemma, we need to see some interesting things to do with the change in RRR.

Cricket is an obvious sort of sport, to win in the end you have to get one run more than your opposition. A commonly used statistic during the broadcast is the required run rate, for example if you need 120 to win off 20 overs your RRR is \$6\$ runs an over

What follows is then obvious is the chasing team wins if they are able to lower the RRR that is score quicker.

Espncricinfo did article on teams that win games get a higher proportion of their runs in the powerplay and first 10 overs. But proportion of team runs in T20 games doesn't really make sense as a batting metric as we are not just concerned with how many runs they get, but how quickly they get them. So we introduce a new way to measure batsman change in RRR.



This graph makes sense as we can see the best openers in the world lower the required run rate so whats the Dhoni Dilemma?



Dhoni is renowed for being one of the worlds best closers but we can see it seems as though in the middle he is letting the game get out of hand. How can one of the worlds best closers consistently put his teams in worse positions?

## notes for James

So I don't think its like a complex paper, because of the way cricket is at the moment I think the biggest influence this stuff can make is

-   being able to fact check

-   having batsman metrics that are beyond average, total runs and SR

-   What we can maybe mention? is that change in RRR while simple seems to be quite effective, a better measurement would be the DLS which we know can be derived from the ball by ball data or alternatively if someone has a resource table they can instead of change in RRR use change in resources or whatever

For the examples, I think its pretty easy to get the graphs to look nice, its more about whats the dataset look like? i.e. do we need the first few lines for reproducibility to be joins or is it sort of like done already
