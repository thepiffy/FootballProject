

``` r
library(worldfootballR)
library(tidyverse)
library(ggsoccer)
```

In the first section, I'll perform EDA on the scraped data from the worldfootballR_data repo.
I'll start by loading match results for the top 5 European leagues for the past decade.  
Then, I'll briefly view the data and check if there's any gaps in important variables.


``` r
topleagues_data <- load_match_results(country = c("ENG", "ESP", "GER", "ITA", "FRA"), gender = c("M"), season_end_year = c(2015:2025), tier = "1st")
```

```
## → Data last updated 2025-02-04 17:32:08.29795408248901 UTC
```

``` r
nrow(topleagues_data)
```

```
## [1] 19969
```

``` r
glimpse(topleagues_data)
```

```
## Rows: 19,969
## Columns: 20
## $ Competition_Name [3m[38;5;246m<chr>[39m[23m "Premier League"[38;5;246m, [39m"Premier League"[38;5;246m, [39m"Premier League"[38;5;246m, [39m"Premier League"[38;5;246m, [39m"Premier League"[38;5;246m, [39m"Premier L…
## $ Gender           [3m[38;5;246m<chr>[39m[23m "M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m…
## $ Country          [3m[38;5;246m<chr>[39m[23m "ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"ENG"[38;5;246m, [39m"E…
## $ Season_End_Year  [3m[38;5;246m<int>[39m[23m 2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015[38;5;246m, [39m2015…
## $ Round            [3m[38;5;246m<chr>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Wk               [3m[38;5;246m<chr>[39m[23m "1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m…
## $ Day              [3m[38;5;246m<chr>[39m[23m "Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sun"[38;5;246m, [39m"Sun"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"S…
## $ Date             [3m[38;5;246m<date>[39m[23m 2014-08-16[38;5;246m, [39m2014-08-16[38;5;246m, [39m2014-08-16[38;5;246m, [39m2014-08-16[38;5;246m, [39m2014-08-16[38;5;246m, [39m2014-08-16[38;5;246m, [39m2014-08-16[38;5;246m, [39m2014-08-17[38;5;246m, [39m201…
## $ Time             [3m[38;5;246m<chr>[39m[23m "12:45"[38;5;246m, [39m"15:00"[38;5;246m, [39m"15:00"[38;5;246m, [39m"15:00"[38;5;246m, [39m"15:00"[38;5;246m, [39m"15:00"[38;5;246m, [39m"17:30"[38;5;246m, [39m"13:30"[38;5;246m, [39m"16:00"[38;5;246m, [39m"20:00"[38;5;246m, [39m"12:45"[38;5;246m, [39m"…
## $ Home             [3m[38;5;246m<chr>[39m[23m "Manchester Utd"[38;5;246m, [39m"Stoke City"[38;5;246m, [39m"Leicester City"[38;5;246m, [39m"QPR"[38;5;246m, [39m"West Ham"[38;5;246m, [39m"West Brom"[38;5;246m, [39m"Arsenal"[38;5;246m, [39m"Liverp…
## $ HomeGoals        [3m[38;5;246m<dbl>[39m[23m 1[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m2[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m4[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m3[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m3[38;5;246m, [39m3[38;5;246m, [39m3[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m4…
## $ Home_xG          [3m[38;5;246m<dbl>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Away             [3m[38;5;246m<chr>[39m[23m "Swansea City"[38;5;246m, [39m"Aston Villa"[38;5;246m, [39m"Everton"[38;5;246m, [39m"Hull City"[38;5;246m, [39m"Tottenham"[38;5;246m, [39m"Sunderland"[38;5;246m, [39m"Crystal Palace"[38;5;246m, [39m…
## $ AwayGoals        [3m[38;5;246m<dbl>[39m[23m 2[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m3[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m3[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m3[38;5;246m, [39m0[38;5;246m, [39m3[38;5;246m, [39m6[38;5;246m, [39m3[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m0…
## $ Away_xG          [3m[38;5;246m<dbl>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Attendance       [3m[38;5;246m<dbl>[39m[23m 75339[38;5;246m, [39m27478[38;5;246m, [39m31603[38;5;246m, [39m17603[38;5;246m, [39m34977[38;5;246m, [39m25468[38;5;246m, [39m59962[38;5;246m, [39m44736[38;5;246m, [39m50816[38;5;246m, [39m20699[38;5;246m, [39m30267[38;5;246m, [39m20565[38;5;246m, [39m24242[38;5;246m, [39m41604[38;5;246m, [39m27…
## $ Venue            [3m[38;5;246m<chr>[39m[23m "Old Trafford"[38;5;246m, [39m"Bet365 Stadium"[38;5;246m, [39m"King Power Stadium"[38;5;246m, [39m"Loftus Road Stadium"[38;5;246m, [39m"Boleyn Ground"[38;5;246m, [39m"The…
## $ Referee          [3m[38;5;246m<chr>[39m[23m "Mike Dean"[38;5;246m, [39m"Anthony Taylor"[38;5;246m, [39m"Mike Dean"[38;5;246m, [39m"Craig Pawson"[38;5;246m, [39m"Chris Foy"[38;5;246m, [39m"Niel Swarbrick"[38;5;246m, [39m"Jonathan…
## $ Notes            [3m[38;5;246m<chr>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ MatchURL         [3m[38;5;246m<chr>[39m[23m "https://fbref.com/en/matches/d192bd78/Manchester-United-Swansea-City-August-16-2014-Premier-League"…
```

``` r
summary(topleagues_data[, c("Home_xG", "Away_xG", "HomeGoals", "AwayGoals")])
```

```
##     Home_xG         Away_xG        HomeGoals        AwayGoals    
##  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.900   1st Qu.:0.700   1st Qu.: 1.000   1st Qu.:0.000  
##  Median :1.400   Median :1.100   Median : 1.000   Median :1.000  
##  Mean   :1.497   Mean   :1.215   Mean   : 1.542   Mean   :1.228  
##  3rd Qu.:2.000   3rd Qu.:1.600   3rd Qu.: 2.000   3rd Qu.:2.000  
##  Max.   :7.000   Max.   :5.600   Max.   :10.000   Max.   :9.000  
##  NA's   :6316    NA's   :6316    NA's   :806      NA's   :806
```

There's a lot of NA values, especially for xG. I'll use the naniar package to visually inspect where these values are.


``` r
library(naniar)
gg_miss_var(topleagues_data)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

``` r
vis_miss(topleagues_data)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-2.png)

A lot of NA values for Round is plausible (these are usually reserved for cup competitions).  
It's important to note the missing Attendance values, however.  
It appears the xG values that are missing originate at the beginning of the data for each league.
There are five blocks of missing xG data which corresponds to the five leagues I've imported.  
These blocks appear to be at the start of the data, which could mean xG data is only availabe past a certain year.  
I'll import a new dataset starting from a later year and run the same test to double-check.


``` r
recent_topleagues_data <- load_match_results(country = c("ENG", "ESP", "GER", "ITA", "FRA"), gender = c("M"), season_end_year = c(2018:2025), tier = "1st")
```

```
## → Data last updated 2025-02-04 17:32:08.29795408248901 UTC
```

``` r
vis_miss(recent_topleagues_data)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

It's clear that the missing data values for xG corresponded to my timeframe of data, as changing my start date from 2015 to 2018 reduced the missing values from 32% to 6%.  
However, there are still some values missing, and I think this is because the data was last updated in February 2025 while the current 2025 season ends in May.  
The fixtures up until May 2025 are confirmed, that's why they are in the dataset. The only things missing in theory should be the xG.  
This is because xG is recorded as the match is played, while all of the other data is determined and fixed ahead of the 24/25 season.
To confirm this hypothesis, I'll extract all data before this date and confirm no data is missing.
In order to do this, the values in the Date column must be of data type Date, which I'll confirm now.


``` r
topleagues_data <- topleagues_data %>%
  mutate(Date = as.Date(Date))
recent_topleagues_before_February <- recent_topleagues_data %>%
  filter(Date < as.Date("2025-02-04"))
vis_miss(recent_topleagues_before_February)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

Only 1% of the xG data remains missing. I'll make a new dataframe with just those rows and inspect it to see where the missing data is.


``` r
missing_xg_rows <- recent_topleagues_before_February %>%
  filter(is.na(Home_xG) | is.na(Away_xG))
missing_xg_rows %>%
  select(Date, Competition_Name, Home, Away, Home_xG, Away_xG)
```

```
##           Date   Competition_Name          Home          Away Home_xG Away_xG
## 1   2018-05-17 Fußball-Bundesliga     Wolfsburg Holstein Kiel      NA      NA
## 2   2018-05-21 Fußball-Bundesliga Holstein Kiel     Wolfsburg      NA      NA
## 3   2019-05-23 Fußball-Bundesliga     Stuttgart  Union Berlin      NA      NA
## 4   2019-05-27 Fußball-Bundesliga  Union Berlin     Stuttgart      NA      NA
## 5   2020-07-02 Fußball-Bundesliga Werder Bremen    Heidenheim      NA      NA
## 6   2020-07-06 Fußball-Bundesliga    Heidenheim Werder Bremen      NA      NA
## 7   2021-05-26 Fußball-Bundesliga          Köln Holstein Kiel      NA      NA
## 8   2021-05-29 Fußball-Bundesliga Holstein Kiel          Köln      NA      NA
## 9   2022-03-18 Fußball-Bundesliga        Bochum    M'Gladbach      NA      NA
## 10  2022-05-19 Fußball-Bundesliga    Hertha BSC  Hamburger SV      NA      NA
## 11  2022-05-23 Fußball-Bundesliga  Hamburger SV    Hertha BSC      NA      NA
## 12  2023-06-01 Fußball-Bundesliga     Stuttgart  Hamburger SV      NA      NA
## 13  2023-06-05 Fußball-Bundesliga  Hamburger SV     Stuttgart      NA      NA
## 14  2024-05-23 Fußball-Bundesliga        Bochum    Düsseldorf      NA      NA
## 15  2024-05-27 Fußball-Bundesliga    Düsseldorf        Bochum      NA      NA
## 16  2020-09-19            Serie A Hellas Verona          Roma      NA      NA
## 17  2018-05-23            Ligue 1       Ajaccio      Toulouse      NA      NA
## 18  2018-05-27            Ligue 1      Toulouse       Ajaccio      NA      NA
## 19  2019-05-30            Ligue 1          Lens         Dijon      NA      NA
## 20  2019-06-02            Ligue 1         Dijon          Lens      NA      NA
## 21  2020-03-13            Ligue 1          Lyon         Reims      NA      NA
## 22  2020-03-14            Ligue 1   Montpellier     Marseille      NA      NA
## 23  2020-03-14            Ligue 1        Nantes         Nîmes      NA      NA
## 24  2020-03-14            Ligue 1    Strasbourg         Dijon      NA      NA
## 25  2020-03-14            Ligue 1      Toulouse          Metz      NA      NA
## 26  2020-03-14            Ligue 1         Brest         Lille      NA      NA
## 27  2020-03-14            Ligue 1        Amiens        Angers      NA      NA
## 28  2020-03-15            Ligue 1      Bordeaux        Rennes      NA      NA
## 29  2020-03-15            Ligue 1        Monaco Saint-Étienne      NA      NA
## 30  2020-03-15            Ligue 1     Paris S-G          Nice      NA      NA
## 31  2020-03-18            Ligue 1    Strasbourg     Paris S-G      NA      NA
## 32  2020-03-20            Ligue 1         Lille        Monaco      NA      NA
## 33  2020-03-21            Ligue 1        Rennes          Lyon      NA      NA
## 34  2020-03-21            Ligue 1         Dijon        Amiens      NA      NA
## 35  2020-03-21            Ligue 1          Metz         Brest      NA      NA
## 36  2020-03-21            Ligue 1         Nîmes      Bordeaux      NA      NA
## 37  2020-03-21            Ligue 1        Angers      Toulouse      NA      NA
## 38  2020-03-21            Ligue 1          Nice   Montpellier      NA      NA
## 39  2020-03-22            Ligue 1 Saint-Étienne    Strasbourg      NA      NA
## 40  2020-03-22            Ligue 1         Reims        Nantes      NA      NA
## 41  2020-03-22            Ligue 1     Marseille     Paris S-G      NA      NA
## 42  2020-04-04            Ligue 1   Montpellier         Lille      NA      NA
## 43  2020-04-04            Ligue 1      Toulouse Saint-Étienne      NA      NA
## 44  2020-04-05            Ligue 1          Lyon         Nîmes      NA      NA
## 45  2020-04-05            Ligue 1     Paris S-G          Metz      NA      NA
## 46  2020-04-05            Ligue 1        Monaco        Nantes      NA      NA
## 47  2020-04-05            Ligue 1    Strasbourg        Angers      NA      NA
## 48  2020-04-05            Ligue 1         Dijon          Nice      NA      NA
## 49  2020-04-05            Ligue 1      Bordeaux         Reims      NA      NA
## 50  2020-04-05            Ligue 1        Amiens        Rennes      NA      NA
## 51  2020-04-05            Ligue 1         Brest     Marseille      NA      NA
## 52  2020-04-11            Ligue 1         Nîmes   Montpellier      NA      NA
## 53  2020-04-11            Ligue 1         Reims      Toulouse      NA      NA
## 54  2020-04-11            Ligue 1         Brest        Monaco      NA      NA
## 55  2020-04-11            Ligue 1        Angers     Paris S-G      NA      NA
## 56  2020-04-11            Ligue 1        Nantes          Lyon      NA      NA
## 57  2020-04-11            Ligue 1      Bordeaux        Amiens      NA      NA
## 58  2020-04-11            Ligue 1     Marseille         Dijon      NA      NA
## 59  2020-04-11            Ligue 1 Saint-Étienne        Rennes      NA      NA
## 60  2020-04-11            Ligue 1          Metz         Lille      NA      NA
## 61  2020-04-11            Ligue 1          Nice    Strasbourg      NA      NA
## 62  2020-04-18            Ligue 1        Monaco      Bordeaux      NA      NA
## 63  2020-04-18            Ligue 1          Lyon     Marseille      NA      NA
## 64  2020-04-18            Ligue 1     Paris S-G Saint-Étienne      NA      NA
## 65  2020-04-18            Ligue 1      Toulouse        Nantes      NA      NA
## 66  2020-04-18            Ligue 1         Dijon        Angers      NA      NA
## 67  2020-04-18            Ligue 1    Strasbourg         Brest      NA      NA
## 68  2020-04-18            Ligue 1   Montpellier         Reims      NA      NA
## 69  2020-04-18            Ligue 1         Lille          Nice      NA      NA
## 70  2020-04-18            Ligue 1        Amiens         Nîmes      NA      NA
## 71  2020-04-18            Ligue 1        Rennes          Metz      NA      NA
## 72  2020-04-26            Ligue 1        Rennes    Strasbourg      NA      NA
## 73  2020-04-26            Ligue 1         Nîmes         Lille      NA      NA
## 74  2020-04-26            Ligue 1        Nantes        Amiens      NA      NA
## 75  2020-04-26            Ligue 1 Saint-Étienne        Angers      NA      NA
## 76  2020-04-26            Ligue 1      Bordeaux      Toulouse      NA      NA
## 77  2020-04-26            Ligue 1         Brest   Montpellier      NA      NA
## 78  2020-04-26            Ligue 1          Metz         Dijon      NA      NA
## 79  2020-04-26            Ligue 1         Reims     Paris S-G      NA      NA
## 80  2020-04-26            Ligue 1          Lyon        Monaco      NA      NA
## 81  2020-04-26            Ligue 1     Marseille          Nice      NA      NA
## 82  2020-05-02            Ligue 1        Angers        Rennes      NA      NA
## 83  2020-05-02            Ligue 1         Dijon          Lyon      NA      NA
## 84  2020-05-02            Ligue 1     Paris S-G         Brest      NA      NA
## 85  2020-05-02            Ligue 1        Monaco          Metz      NA      NA
## 86  2020-05-02            Ligue 1         Lille         Reims      NA      NA
## 87  2020-05-02            Ligue 1      Toulouse         Nîmes      NA      NA
## 88  2020-05-02            Ligue 1          Nice        Nantes      NA      NA
## 89  2020-05-02            Ligue 1        Amiens Saint-Étienne      NA      NA
## 90  2020-05-02            Ligue 1    Strasbourg     Marseille      NA      NA
## 91  2020-05-02            Ligue 1   Montpellier      Bordeaux      NA      NA
## 92  2020-05-09            Ligue 1 Saint-Étienne         Lille      NA      NA
## 93  2020-05-09            Ligue 1      Toulouse     Paris S-G      NA      NA
## 94  2020-05-09            Ligue 1        Rennes         Dijon      NA      NA
## 95  2020-05-09            Ligue 1         Nîmes    Strasbourg      NA      NA
## 96  2020-05-09            Ligue 1     Marseille        Monaco      NA      NA
## 97  2020-05-09            Ligue 1          Lyon   Montpellier      NA      NA
## 98  2020-05-09            Ligue 1        Nantes         Brest      NA      NA
## 99  2020-05-09            Ligue 1      Bordeaux        Angers      NA      NA
## 100 2020-05-09            Ligue 1          Metz          Nice      NA      NA
## 101 2020-05-09            Ligue 1         Reims        Amiens      NA      NA
## 102 2020-05-16            Ligue 1          Nice Saint-Étienne      NA      NA
## 103 2020-05-16            Ligue 1         Lille        Amiens      NA      NA
## 104 2020-05-16            Ligue 1    Strasbourg      Bordeaux      NA      NA
## 105 2020-05-16            Ligue 1   Montpellier        Nantes      NA      NA
## 106 2020-05-16            Ligue 1        Monaco      Toulouse      NA      NA
## 107 2020-05-16            Ligue 1         Dijon         Reims      NA      NA
## 108 2020-05-16            Ligue 1     Paris S-G        Rennes      NA      NA
## 109 2020-05-16            Ligue 1     Marseille          Metz      NA      NA
## 110 2020-05-16            Ligue 1        Angers          Lyon      NA      NA
## 111 2020-05-16            Ligue 1         Brest         Nîmes      NA      NA
## 112 2020-05-23            Ligue 1          Metz        Angers      NA      NA
## 113 2020-05-23            Ligue 1         Reims     Marseille      NA      NA
## 114 2020-05-23            Ligue 1        Nantes    Strasbourg      NA      NA
## 115 2020-05-23            Ligue 1        Rennes        Monaco      NA      NA
## 116 2020-05-23            Ligue 1      Toulouse   Montpellier      NA      NA
## 117 2020-05-23            Ligue 1        Amiens          Nice      NA      NA
## 118 2020-05-23            Ligue 1          Lyon         Brest      NA      NA
## 119 2020-05-23            Ligue 1         Nîmes     Paris S-G      NA      NA
## 120 2020-05-23            Ligue 1      Bordeaux         Lille      NA      NA
## 121 2020-05-23            Ligue 1 Saint-Étienne         Dijon      NA      NA
## 122 2021-05-27            Ligue 1      Toulouse        Nantes      NA      NA
## 123 2021-05-30            Ligue 1        Nantes      Toulouse      NA      NA
## 124 2022-05-26            Ligue 1       Auxerre Saint-Étienne      NA      NA
## 125 2022-05-29            Ligue 1 Saint-Étienne       Auxerre      NA      NA
```

``` r
glimpse(missing_xg_rows)
```

```
## Rows: 125
## Columns: 20
## $ Competition_Name [3m[38;5;246m<chr>[39m[23m "Fußball-Bundesliga"[38;5;246m, [39m"Fußball-Bundesliga"[38;5;246m, [39m"Fußball-Bundesliga"[38;5;246m, [39m"Fußball-Bundesliga"[38;5;246m, [39m"Fußball-Bun…
## $ Gender           [3m[38;5;246m<chr>[39m[23m "M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m…
## $ Country          [3m[38;5;246m<chr>[39m[23m "GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"GER"[38;5;246m, [39m"G…
## $ Season_End_Year  [3m[38;5;246m<int>[39m[23m 2018[38;5;246m, [39m2018[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2020[38;5;246m, [39m2020[38;5;246m, [39m2021[38;5;246m, [39m2021[38;5;246m, [39m2022[38;5;246m, [39m2022[38;5;246m, [39m2022[38;5;246m, [39m2023[38;5;246m, [39m2023[38;5;246m, [39m2024[38;5;246m, [39m2024[38;5;246m, [39m2021[38;5;246m, [39m2018…
## $ Round            [3m[38;5;246m<chr>[39m[23m "German 1/2 Relegation/Promotion Playoffs"[38;5;246m, [39m"German 1/2 Relegation/Promotion Playoffs"[38;5;246m, [39m"German 1/2 …
## $ Wk               [3m[38;5;246m<chr>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m"27"[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m"1"[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m"29"[38;5;246m, [39m"29"[38;5;246m, [39m"29"[38;5;246m,[39m…
## $ Day              [3m[38;5;246m<chr>[39m[23m "Thu"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Thu"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Thu"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Wed"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Fri"[38;5;246m, [39m"Thu"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Thu"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Thu"[38;5;246m, [39m"M…
## $ Date             [3m[38;5;246m<date>[39m[23m 2018-05-17[38;5;246m, [39m2018-05-21[38;5;246m, [39m2019-05-23[38;5;246m, [39m2019-05-27[38;5;246m, [39m2020-07-02[38;5;246m, [39m2020-07-06[38;5;246m, [39m2021-05-26[38;5;246m, [39m2021-05-29[38;5;246m, [39m202…
## $ Time             [3m[38;5;246m<chr>[39m[23m "20:30"[38;5;246m, [39m"20:30"[38;5;246m, [39m"20:30"[38;5;246m, [39m"20:30"[38;5;246m, [39m"20:30"[38;5;246m, [39m"20:30"[38;5;246m, [39m"18:30"[38;5;246m, [39m"18:00"[38;5;246m, [39m"20:30"[38;5;246m, [39m"20:30"[38;5;246m, [39m"20:30"[38;5;246m, [39m"…
## $ Home             [3m[38;5;246m<chr>[39m[23m "Wolfsburg"[38;5;246m, [39m"Holstein Kiel"[38;5;246m, [39m"Stuttgart"[38;5;246m, [39m"Union Berlin"[38;5;246m, [39m"Werder Bremen"[38;5;246m, [39m"Heidenheim"[38;5;246m, [39m"Köln"[38;5;246m, [39m"H…
## $ HomeGoals        [3m[38;5;246m<dbl>[39m[23m 3[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m3[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m3[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m3[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Home_xG          [3m[38;5;246m<dbl>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Away             [3m[38;5;246m<chr>[39m[23m "Holstein Kiel"[38;5;246m, [39m"Wolfsburg"[38;5;246m, [39m"Union Berlin"[38;5;246m, [39m"Stuttgart"[38;5;246m, [39m"Heidenheim"[38;5;246m, [39m"Werder Bremen"[38;5;246m, [39m"Holstein …
## $ AwayGoals        [3m[38;5;246m<dbl>[39m[23m 1[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m5[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m3[38;5;246m, [39m3[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m3[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Away_xG          [3m[38;5;246m<dbl>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Attendance       [3m[38;5;246m<dbl>[39m[23m 28800[38;5;246m, [39m12000[38;5;246m, [39m58619[38;5;246m, [39m22012[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m75500[38;5;246m, [39m57000[38;5;246m, [39m48500[38;5;246m, [39m55500[38;5;246m, [39m26000[38;5;246m, [39m51500[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m25…
## $ Venue            [3m[38;5;246m<chr>[39m[23m "Volkswagen Arena"[38;5;246m, [39m"Holstein-Stadion"[38;5;246m, [39m"Mercedes-Benz Arena"[38;5;246m, [39m"Stadion An der Alten Försterei"[38;5;246m, [39m"We…
## $ Referee          [3m[38;5;246m<chr>[39m[23m ""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m"Benjamin Cortus"[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m"Daniele Chiffi"[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m,[39m…
## $ Notes            [3m[38;5;246m<chr>[39m[23m "Leg 1 of 2"[38;5;246m, [39m"Leg 2 of 2; Wolfsburg won"[38;5;246m, [39m"Leg 1 of 2"[38;5;246m, [39m"Leg 2 of 2; Union Berlin won"[38;5;246m, [39m"Leg 1 of 2…
## $ MatchURL         [3m[38;5;246m<chr>[39m[23m "https://fbref.com/en/matches/5dc40876/Wolfsburg-Holstein-Kiel-May-17-2018-German-12-RelegationPromo…
```

There's only 125 rows, which means the select() function showed the whole dataframe.  
The Ligue 1 (French league) 2019/20 season was suspended on March 13 2020 due to the coronavirus and cancelled on April 20, therefore those matches were never actually played.
This explains why there is no xG data for Ligue 1 during this timeframe.  
The remaining missing Ligue 1 and Bundesliga data (except for one game) is because those games were the relegation play-offs of those leagues.
These two leagues are the only top 5 leagues that adopt a system of relegation play-offs.
This is a system whereby 16th place in the league (third from bottom) plays 3rd place from the second tier in two games: one home and one away.
The winner, decided by aggregate score, either remains or is promoted to the first divison for next season. The loser remains/is relegated to the second division.
It's arguable as to whether these games count towards the league. Since they don't comprise of the 38 (or 34) games each team plays in a standard league season, I won't count them.
It feels unfair to take stats for some teams playing 34 games and others playing 36.  
There's one Bundesliga game that wasn't a playoff, however, which was the Bochum vs M'Gladbach match on 2022-03-18.
It's clear this isn't a playoff game because there was no return game between Gladbach and Bochum (all of the playoffs are in pairs).
This game also took place mid-season while the other games took place between May and July (att he end of the season).
In this game, Gladbach led 2-0 at around the 68-70th minute when assistant referee Christian Gittelmann was struck in the head by a beer cup thrown from the crowd at Bochum's stadium.
This resulted in a serious injury and referee Benjamin Cortus abandoned the match shortly after for security reasons.
The German Football Association awarded the game as a Gladbach 2-0 win as opposed to a replay. Bochum's appeal for a replay was later withdrew.  
The final outlier is the September 2020 game between Hellas Verona and Roma in Italy's Serie A.
The game ended 0-0 in regulation time, however Verona was awarded a 3-0 forfeit win by the Italian Football Federation afterwards.
This is because Roma fielded an ineligible player (Amadou Diawara) who had just turned 23 but was registered in the under-22 registration list.
It was a small mistake, but it gave Verona the win. This is probably the reason why the xG and goal count wasn't collected in the dataset.  
In conclusion, there are two outliers to look out for when analysing Bundesliga and Serie A data, and Ligue 1 data analysis for the 2019/20 season will be skewed because of its cancellation.  
Now, I'll do the same tests on data from the most prominent football cup competitions worldwide (domestic and international). I'll also take a quick look at them and check for missing data.  


``` r
cups <- c("Africa Cup of Nations", "CONMEBOL Copa América", "Copa del Rey", "Coppa Italia", "Coupe de France", " DFB-Pokal", "EFL Cup", "FA Cup", "FIFA World Cup", "UEFA Champions League", "UEFA Conference League", "UEFA Europa League", "UEFA European Football Championship")
cups_data <- load_match_comp_results(comp_name = cups)
```

```
## → Data last updated 2024-02-15 17:17:59.4536941051483 UTC
```

``` r
glimpse(cups_data)
```

```
## Rows: 20,003
## Columns: 20
## $ Competition_Name [3m[38;5;246m<chr>[39m[23m "Africa Cup of Nations"[38;5;246m, [39m"Africa Cup of Nations"[38;5;246m, [39m"Africa Cup of Nations"[38;5;246m, [39m"Africa Cup of Nations"[38;5;246m, [39m…
## $ Gender           [3m[38;5;246m<chr>[39m[23m "M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m…
## $ Country          [3m[38;5;246m<chr>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Season_End_Year  [3m[38;5;246m<int>[39m[23m 2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010[38;5;246m, [39m2010…
## $ Round            [3m[38;5;246m<chr>[39m[23m "Group stage"[38;5;246m, [39m"Group stage"[38;5;246m, [39m"Group stage"[38;5;246m, [39m"Group stage"[38;5;246m, [39m"Group stage"[38;5;246m, [39m"Group stage"[38;5;246m, [39m"Group sta…
## $ Wk               [3m[38;5;246m<chr>[39m[23m "1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"3"[38;5;246m, [39m"3"[38;5;246m, [39m"3"[38;5;246m, [39m"3"[38;5;246m, [39m…
## $ Day              [3m[38;5;246m<chr>[39m[23m "Sun"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Tue"[38;5;246m, [39m"Tue"[38;5;246m, [39m"Wed"[38;5;246m, [39m"Wed"[38;5;246m, [39m"Thu"[38;5;246m, [39m"Thu"[38;5;246m, [39m"Fri"[38;5;246m, [39m"Fri"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"S…
## $ Date             [3m[38;5;246m<date>[39m[23m 2010-01-10[38;5;246m, [39m2010-01-11[38;5;246m, [39m2010-01-11[38;5;246m, [39m2010-01-11[38;5;246m, [39m2010-01-12[38;5;246m, [39m2010-01-12[38;5;246m, [39m2010-01-13[38;5;246m, [39m2010-01-13[38;5;246m, [39m201…
## $ Time             [3m[38;5;246m<chr>[39m[23m "20:00"[38;5;246m, [39m"14:45"[38;5;246m, [39m"17:00"[38;5;246m, [39m"19:30"[38;5;246m, [39m"17:00"[38;5;246m, [39m"19:30"[38;5;246m, [39m"17:00"[38;5;246m, [39m"19:30"[38;5;246m, [39m"17:00"[38;5;246m, [39m"19:30"[38;5;246m, [39m"17:00"[38;5;246m, [39m"…
## $ Home             [3m[38;5;246m<chr>[39m[23m "Angola ao"[38;5;246m, [39m"Malawi mw"[38;5;246m, [39m"Côte d'Ivoire ci"[38;5;246m, [39m"Ghana gh"[38;5;246m, [39m"Egypt eg"[38;5;246m, [39m"Mozambique mz"[38;5;246m, [39m"Cameroon cm"…
## $ HomeGoals        [3m[38;5;246m<dbl>[39m[23m 4[38;5;246m, [39m3[38;5;246m, [39m0[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m3[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m3[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m3[38;5;246m, [39m0[38;5;246m, [39m3[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m0[38;5;246m, [39m3[38;5;246m, [39m2[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m3[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m1…
## $ Away             [3m[38;5;246m<chr>[39m[23m "ml Mali"[38;5;246m, [39m"dz Algeria"[38;5;246m, [39m"bf Burkina Faso"[38;5;246m, [39m"tg Togo"[38;5;246m, [39m"ng Nigeria"[38;5;246m, [39m"bj Benin"[38;5;246m, [39m"ga Gabon"[38;5;246m, [39m"tn Tun…
## $ AwayGoals        [3m[38;5;246m<dbl>[39m[23m 4[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m3[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m4[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m0…
## $ Attendance       [3m[38;5;246m<dbl>[39m[23m 48000[38;5;246m, [39m1000[38;5;246m, [39m5000[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m18000[38;5;246m, [39m15000[38;5;246m, [39m15000[38;5;246m, [39m17000[38;5;246m, [39m4000[38;5;246m, [39m48500[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m23000[38;5;246m, [39m8000[38;5;246m, [39m16000[38;5;246m, [39m16000[38;5;246m, [39m15000…
## $ Venue            [3m[38;5;246m<chr>[39m[23m "Estádio 11 de Novembro"[38;5;246m, [39m"Estádio 11 de Novembro"[38;5;246m, [39m"Estádio Nacional do Chiazi"[38;5;246m, [39m"Estádio Nacional …
## $ Referee          [3m[38;5;246m<chr>[39m[23m "Essam Abd El Fatah"[38;5;246m, [39m"Badara Diatta"[38;5;246m, [39m"Kacem Bennaceur"[38;5;246m, [39m""[38;5;246m, [39m"Rajindraparsad Seechurn"[38;5;246m, [39m"Khalid Abd…
## $ Notes            [3m[38;5;246m<chr>[39m[23m ""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m"Match Cancelled"[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m"Match Cancelled"[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""…
## $ MatchURL         [3m[38;5;246m<chr>[39m[23m "https://fbref.com/en/matches/4cca8840/Angola-Mali-January-10-2010-Africa-Cup-of-Nations"[38;5;246m, [39m"https://…
## $ Home_xG          [3m[38;5;246m<dbl>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Away_xG          [3m[38;5;246m<dbl>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
```

There's a lot of data here, though the most important is xG. I'll perform the same visual inspection of missing values.


``` r
gg_miss_var(cups_data)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

``` r
vis_miss(cups_data)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-2.png)

There's quite a lot of missing xG data. I'll first filter out all data before the 2017/18  season, as this is when xG collection began. I'll do the same step of converting the Date column values to have the Date data type.  
The other missing valuesw like Week and Attendance don't matter as much (Week can be pulled from the Date) and attendance isn't necessary for player/team analysis.


``` r
cups_data <- cups_data %>%
  mutate(Date = as.Date(Date))
filtered_cups_data <- filter(cups_data, Date > as.Date("2017-06-06"))
vis_miss(filtered_cups_data)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

It's better, going from 84% missing to 64% missing, though there's still a lot of missing data. I'll filter out the rows with missing xG and look at them now.  


``` r
missing_cup_xg_rows <- filtered_cups_data %>%
  filter(is.na(Home_xG) | is.na(Away_xG))
missing_cup_xg_rows %>%
  select(Date, Competition_Name, Home, Away, Home_xG, Away_xG)
```

```
##           Date      Competition_Name             Home             Away Home_xG Away_xG
## 1   2019-06-21 Africa Cup of Nations         Egypt eg      zw Zimbabwe      NA      NA
## 2   2019-06-22 Africa Cup of Nations      Congo DR cd        ug Uganda      NA      NA
## 3   2019-06-22 Africa Cup of Nations       Nigeria ng       bi Burundi      NA      NA
## 4   2019-06-22 Africa Cup of Nations        Guinea gn    mg Madagascar      NA      NA
## 5   2019-06-23 Africa Cup of Nations       Morocco ma       na Namibia      NA      NA
## 6   2019-06-23 Africa Cup of Nations       Senegal sn      tz Tanzania      NA      NA
## 7   2019-06-23 Africa Cup of Nations       Algeria dz         ke Kenya      NA      NA
## 8   2019-06-24 Africa Cup of Nations Côte d'Ivoire ci  za South Africa      NA      NA
## 9   2019-06-24 Africa Cup of Nations       Tunisia tn        ao Angola      NA      NA
## 10  2019-06-24 Africa Cup of Nations          Mali ml    mr Mauritania      NA      NA
## 11  2019-06-25 Africa Cup of Nations      Cameroon cm gw Guinea-Bissau      NA      NA
## 12  2019-06-25 Africa Cup of Nations         Ghana gh         bj Benin      NA      NA
## 13  2019-06-26 Africa Cup of Nations       Nigeria ng        gn Guinea      NA      NA
## 14  2019-06-26 Africa Cup of Nations        Uganda ug      zw Zimbabwe      NA      NA
## 15  2019-06-26 Africa Cup of Nations         Egypt eg      cd Congo DR      NA      NA
## 16  2019-06-27 Africa Cup of Nations    Madagascar mg       bi Burundi      NA      NA
## 17  2019-06-27 Africa Cup of Nations       Senegal sn       dz Algeria      NA      NA
## 18  2019-06-27 Africa Cup of Nations         Kenya ke      tz Tanzania      NA      NA
## 19  2019-06-28 Africa Cup of Nations       Tunisia tn          ml Mali      NA      NA
## 20  2019-06-28 Africa Cup of Nations       Morocco ma ci Côte d'Ivoire      NA      NA
## 21  2019-06-28 Africa Cup of Nations  South Africa za       na Namibia      NA      NA
## 22  2019-06-29 Africa Cup of Nations    Mauritania mr        ao Angola      NA      NA
## 23  2019-06-29 Africa Cup of Nations      Cameroon cm         gh Ghana      NA      NA
## 24  2019-06-29 Africa Cup of Nations         Benin bj gw Guinea-Bissau      NA      NA
## 25  2019-06-30 Africa Cup of Nations    Madagascar mg       ng Nigeria      NA      NA
## 26  2019-06-30 Africa Cup of Nations       Burundi bi        gn Guinea      NA      NA
## 27  2019-06-30 Africa Cup of Nations        Uganda ug         eg Egypt      NA      NA
## 28  2019-06-30 Africa Cup of Nations      Zimbabwe zw      cd Congo DR      NA      NA
## 29  2019-07-01 Africa Cup of Nations  South Africa za       ma Morocco      NA      NA
## 30  2019-07-01 Africa Cup of Nations       Namibia na ci Côte d'Ivoire      NA      NA
## 31  2019-07-01 Africa Cup of Nations         Kenya ke       sn Senegal      NA      NA
## 32  2019-07-01 Africa Cup of Nations      Tanzania tz       dz Algeria      NA      NA
## 33  2019-07-02 Africa Cup of Nations Guinea-Bissau gw         gh Ghana      NA      NA
## 34  2019-07-02 Africa Cup of Nations         Benin bj      cm Cameroon      NA      NA
## 35  2019-07-02 Africa Cup of Nations    Mauritania mr       tn Tunisia      NA      NA
## 36  2019-07-02 Africa Cup of Nations        Angola ao          ml Mali      NA      NA
## 37  2019-07-05 Africa Cup of Nations       Morocco ma         bj Benin      NA      NA
## 38  2019-07-05 Africa Cup of Nations        Uganda ug       sn Senegal      NA      NA
## 39  2019-07-06 Africa Cup of Nations       Nigeria ng      cm Cameroon      NA      NA
## 40  2019-07-06 Africa Cup of Nations         Egypt eg  za South Africa      NA      NA
## 41  2019-07-07 Africa Cup of Nations    Madagascar mg      cd Congo DR      NA      NA
## 42  2019-07-07 Africa Cup of Nations       Algeria dz        gn Guinea      NA      NA
## 43  2019-07-08 Africa Cup of Nations          Mali ml ci Côte d'Ivoire      NA      NA
## 44  2019-07-08 Africa Cup of Nations         Ghana gh       tn Tunisia      NA      NA
## 45  2019-07-10 Africa Cup of Nations       Senegal sn         bj Benin      NA      NA
## 46  2019-07-10 Africa Cup of Nations       Nigeria ng  za South Africa      NA      NA
## 47  2019-07-11 Africa Cup of Nations Côte d'Ivoire ci       dz Algeria      NA      NA
## 48  2019-07-11 Africa Cup of Nations    Madagascar mg       tn Tunisia      NA      NA
## 49  2019-07-14 Africa Cup of Nations       Senegal sn       tn Tunisia      NA      NA
## 50  2019-07-14 Africa Cup of Nations       Algeria dz       ng Nigeria      NA      NA
## 51  2019-07-17 Africa Cup of Nations       Tunisia tn       ng Nigeria      NA      NA
## 52  2019-07-19 Africa Cup of Nations       Senegal sn       dz Algeria      NA      NA
## 53  2022-01-09 Africa Cup of Nations      Cameroon cm  bf Burkina Faso      NA      NA
## 54  2022-01-09 Africa Cup of Nations      Ethiopia et    cv Cape Verde      NA      NA
## 55  2022-01-10 Africa Cup of Nations       Senegal sn      zw Zimbabwe      NA      NA
## 56  2022-01-10 Africa Cup of Nations       Morocco ma         gh Ghana      NA      NA
## 57  2022-01-10 Africa Cup of Nations        Guinea gn        mw Malawi      NA      NA
## 58  2022-01-10 Africa Cup of Nations       Comoros km         ga Gabon      NA      NA
## 59  2022-01-11 Africa Cup of Nations       Algeria dz  sl Sierra Leone      NA      NA
## 60  2022-01-11 Africa Cup of Nations       Nigeria ng         eg Egypt      NA      NA
## 61  2022-01-11 Africa Cup of Nations         Sudan sd gw Guinea-Bissau      NA      NA
## 62  2022-01-12 Africa Cup of Nations       Tunisia tn          ml Mali      NA      NA
## 63  2022-01-12 Africa Cup of Nations    Mauritania mr        gm Gambia      NA      NA
## 64  2022-01-12 Africa Cup of Nations   Equ. Guinea gq ci Côte d'Ivoire      NA      NA
## 65  2022-01-13 Africa Cup of Nations      Cameroon cm      et Ethiopia      NA      NA
## 66  2022-01-13 Africa Cup of Nations    Cape Verde cv  bf Burkina Faso      NA      NA
## 67  2022-01-14 Africa Cup of Nations       Senegal sn        gn Guinea      NA      NA
## 68  2022-01-14 Africa Cup of Nations       Morocco ma       km Comoros      NA      NA
## 69  2022-01-14 Africa Cup of Nations        Malawi mw      zw Zimbabwe      NA      NA
## 70  2022-01-14 Africa Cup of Nations         Gabon ga         gh Ghana      NA      NA
## 71  2022-01-15 Africa Cup of Nations       Nigeria ng         sd Sudan      NA      NA
## 72  2022-01-15 Africa Cup of Nations Guinea-Bissau gw         eg Egypt      NA      NA
## 73  2022-01-16 Africa Cup of Nations        Gambia gm          ml Mali      NA      NA
## 74  2022-01-16 Africa Cup of Nations       Tunisia tn    mr Mauritania      NA      NA
## 75  2022-01-16 Africa Cup of Nations Côte d'Ivoire ci  sl Sierra Leone      NA      NA
## 76  2022-01-16 Africa Cup of Nations       Algeria dz   gq Equ. Guinea      NA      NA
## 77  2022-01-17 Africa Cup of Nations  Burkina Faso bf      et Ethiopia      NA      NA
## 78  2022-01-17 Africa Cup of Nations    Cape Verde cv      cm Cameroon      NA      NA
## 79  2022-01-18 Africa Cup of Nations        Malawi mw       sn Senegal      NA      NA
## 80  2022-01-18 Africa Cup of Nations      Zimbabwe zw        gn Guinea      NA      NA
## 81  2022-01-18 Africa Cup of Nations         Ghana gh       km Comoros      NA      NA
## 82  2022-01-18 Africa Cup of Nations         Gabon ga       ma Morocco      NA      NA
## 83  2022-01-19 Africa Cup of Nations Guinea-Bissau gw       ng Nigeria      NA      NA
## 84  2022-01-19 Africa Cup of Nations         Egypt eg         sd Sudan      NA      NA
## 85  2022-01-20 Africa Cup of Nations  Sierra Leone sl   gq Equ. Guinea      NA      NA
## 86  2022-01-20 Africa Cup of Nations Côte d'Ivoire ci       dz Algeria      NA      NA
## 87  2022-01-20 Africa Cup of Nations          Mali ml    mr Mauritania      NA      NA
## 88  2022-01-20 Africa Cup of Nations        Gambia gm       tn Tunisia      NA      NA
## 89  2022-01-23 Africa Cup of Nations  Burkina Faso bf         ga Gabon      NA      NA
## 90  2022-01-23 Africa Cup of Nations       Nigeria ng       tn Tunisia      NA      NA
## 91  2022-01-24 Africa Cup of Nations        Guinea gn        gm Gambia      NA      NA
## 92  2022-01-24 Africa Cup of Nations      Cameroon cm       km Comoros      NA      NA
## 93  2022-01-25 Africa Cup of Nations       Senegal sn    cv Cape Verde      NA      NA
## 94  2022-01-25 Africa Cup of Nations       Morocco ma        mw Malawi      NA      NA
## 95  2022-01-26 Africa Cup of Nations Côte d'Ivoire ci         eg Egypt      NA      NA
## 96  2022-01-26 Africa Cup of Nations          Mali ml   gq Equ. Guinea      NA      NA
## 97  2022-01-29 Africa Cup of Nations        Gambia gm      cm Cameroon      NA      NA
## 98  2022-01-29 Africa Cup of Nations  Burkina Faso bf       tn Tunisia      NA      NA
## 99  2022-01-30 Africa Cup of Nations         Egypt eg       ma Morocco      NA      NA
## 100 2022-01-30 Africa Cup of Nations       Senegal sn   gq Equ. Guinea      NA      NA
## 101 2022-02-02 Africa Cup of Nations  Burkina Faso bf       sn Senegal      NA      NA
## 102 2022-02-03 Africa Cup of Nations      Cameroon cm         eg Egypt      NA      NA
## 103 2022-02-05 Africa Cup of Nations  Burkina Faso bf      cm Cameroon      NA      NA
## 104 2022-02-06 Africa Cup of Nations       Senegal sn         eg Egypt      NA      NA
## 105 2024-01-13 Africa Cup of Nations Côte d'Ivoire ci gw Guinea-Bissau      NA      NA
## 106 2024-01-14 Africa Cup of Nations       Nigeria ng   gq Equ. Guinea      NA      NA
## 107 2024-01-14 Africa Cup of Nations         Egypt eg    mz Mozambique      NA      NA
## 108 2024-01-14 Africa Cup of Nations         Ghana gh    cv Cape Verde      NA      NA
## 109 2024-01-15 Africa Cup of Nations       Senegal sn        gm Gambia      NA      NA
## 110 2024-01-15 Africa Cup of Nations      Cameroon cm        gn Guinea      NA      NA
## 111 2024-01-15 Africa Cup of Nations       Algeria dz        ao Angola      NA      NA
## 112 2024-01-16 Africa Cup of Nations  Burkina Faso bf    mr Mauritania      NA      NA
## 113 2024-01-16 Africa Cup of Nations       Tunisia tn       na Namibia      NA      NA
## 114 2024-01-16 Africa Cup of Nations          Mali ml  za South Africa      NA      NA
## 115 2024-01-17 Africa Cup of Nations       Morocco ma      tz Tanzania      NA      NA
## 116 2024-01-17 Africa Cup of Nations      Congo DR cd        zm Zambia      NA      NA
## 117 2024-01-18 Africa Cup of Nations   Equ. Guinea gq gw Guinea-Bissau      NA      NA
## 118 2024-01-18 Africa Cup of Nations Côte d'Ivoire ci       ng Nigeria      NA      NA
## 119 2024-01-18 Africa Cup of Nations         Egypt eg         gh Ghana      NA      NA
## 120 2024-01-19 Africa Cup of Nations    Cape Verde cv    mz Mozambique      NA      NA
## 121 2024-01-19 Africa Cup of Nations       Senegal sn      cm Cameroon      NA      NA
## 122 2024-01-19 Africa Cup of Nations        Guinea gn        gm Gambia      NA      NA
## 123 2024-01-20 Africa Cup of Nations       Algeria dz  bf Burkina Faso      NA      NA
## 124 2024-01-20 Africa Cup of Nations    Mauritania mr        ao Angola      NA      NA
## 125 2024-01-20 Africa Cup of Nations       Tunisia tn          ml Mali      NA      NA
## 126 2024-01-21 Africa Cup of Nations       Morocco ma      cd Congo DR      NA      NA
## 127 2024-01-21 Africa Cup of Nations        Zambia zm      tz Tanzania      NA      NA
## 128 2024-01-21 Africa Cup of Nations  South Africa za       na Namibia      NA      NA
## 129 2024-01-22 Africa Cup of Nations Guinea-Bissau gw       ng Nigeria      NA      NA
## 130 2024-01-22 Africa Cup of Nations   Equ. Guinea gq ci Côte d'Ivoire      NA      NA
## 131 2024-01-22 Africa Cup of Nations    Mozambique mz         gh Ghana      NA      NA
## 132 2024-01-22 Africa Cup of Nations    Cape Verde cv         eg Egypt      NA      NA
## 133 2024-01-23 Africa Cup of Nations        Gambia gm      cm Cameroon      NA      NA
## 134 2024-01-23 Africa Cup of Nations        Guinea gn       sn Senegal      NA      NA
## 135 2024-01-23 Africa Cup of Nations    Mauritania mr       dz Algeria      NA      NA
## 136 2024-01-23 Africa Cup of Nations        Angola ao  bf Burkina Faso      NA      NA
## 137 2024-01-24 Africa Cup of Nations       Namibia na          ml Mali      NA      NA
## 138 2024-01-24 Africa Cup of Nations  South Africa za       tn Tunisia      NA      NA
## 139 2024-01-24 Africa Cup of Nations      Tanzania tz      cd Congo DR      NA      NA
## 140 2024-01-24 Africa Cup of Nations        Zambia zm       ma Morocco      NA      NA
## 141 2024-01-27 Africa Cup of Nations        Angola ao       na Namibia      NA      NA
## 142 2024-01-27 Africa Cup of Nations       Nigeria ng      cm Cameroon      NA      NA
## 143 2024-01-28 Africa Cup of Nations   Equ. Guinea gq        gn Guinea      NA      NA
## 144 2024-01-28 Africa Cup of Nations         Egypt eg      cd Congo DR      NA      NA
## 145 2024-01-29 Africa Cup of Nations    Cape Verde cv    mr Mauritania      NA      NA
## 146 2024-01-29 Africa Cup of Nations       Senegal sn ci Côte d'Ivoire      NA      NA
## 147 2024-01-30 Africa Cup of Nations          Mali ml  bf Burkina Faso      NA      NA
## 148 2024-01-30 Africa Cup of Nations       Morocco ma  za South Africa      NA      NA
## 149 2024-02-02 Africa Cup of Nations       Nigeria ng        ao Angola      NA      NA
## 150 2024-02-02 Africa Cup of Nations      Congo DR cd        gn Guinea      NA      NA
## 151 2024-02-03 Africa Cup of Nations          Mali ml ci Côte d'Ivoire      NA      NA
## 152 2024-02-03 Africa Cup of Nations    Cape Verde cv  za South Africa      NA      NA
## 153 2024-02-07 Africa Cup of Nations       Nigeria ng  za South Africa      NA      NA
## 154 2024-02-07 Africa Cup of Nations Côte d'Ivoire ci      cd Congo DR      NA      NA
## 155 2024-02-10 Africa Cup of Nations  South Africa za      cd Congo DR      NA      NA
## 156 2024-02-11 Africa Cup of Nations       Nigeria ng ci Côte d'Ivoire      NA      NA
## 157 2017-08-30          Copa del Rey        Calahorra       Real Unión      NA      NA
## 158 2017-08-30          Copa del Rey       Peña Sport      CD Mirandés      NA      NA
## 159 2017-08-30          Copa del Rey         Badalona            Elche      NA      NA
## 160 2017-08-30          Copa del Rey         SD Leioa      Racing Sant      NA      NA
## 161 2017-08-30          Copa del Rey   Gim. Segoviana       Pontevedra      NA      NA
## 162 2017-08-30          Copa del Rey       Gimnastica          Durango      NA      NA
## 163 2017-08-30          Copa del Rey     San Fernando         Marbella      NA      NA
## 164 2017-08-30          Copa del Rey           Lleida          Melilla      NA      NA
## 165 2017-08-30          Copa del Rey   Olímpic Xàtiva             Olot      NA      NA
## 166 2017-08-30          Copa del Rey      Real Murcia         Cacereño      NA      NA
##  [ reached 'max' / getOption("max.print") -- omitted 5422 rows ]
```

``` r
glimpse(missing_cup_xg_rows)
```

```
## Rows: 5,588
## Columns: 20
## $ Competition_Name [3m[38;5;246m<chr>[39m[23m "Africa Cup of Nations"[38;5;246m, [39m"Africa Cup of Nations"[38;5;246m, [39m"Africa Cup of Nations"[38;5;246m, [39m"Africa Cup of Nations"[38;5;246m, [39m…
## $ Gender           [3m[38;5;246m<chr>[39m[23m "M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m"M"[38;5;246m, [39m…
## $ Country          [3m[38;5;246m<chr>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Season_End_Year  [3m[38;5;246m<int>[39m[23m 2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019[38;5;246m, [39m2019…
## $ Round            [3m[38;5;246m<chr>[39m[23m "Group stage"[38;5;246m, [39m"Group stage"[38;5;246m, [39m"Group stage"[38;5;246m, [39m"Group stage"[38;5;246m, [39m"Group stage"[38;5;246m, [39m"Group stage"[38;5;246m, [39m"Group sta…
## $ Wk               [3m[38;5;246m<chr>[39m[23m "1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"1"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m"2"[38;5;246m, [39m…
## $ Day              [3m[38;5;246m<chr>[39m[23m "Fri"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sat"[38;5;246m, [39m"Sun"[38;5;246m, [39m"Sun"[38;5;246m, [39m"Sun"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Mon"[38;5;246m, [39m"Tue"[38;5;246m, [39m"Tue"[38;5;246m, [39m"Wed"[38;5;246m, [39m"Wed"[38;5;246m, [39m"W…
## $ Date             [3m[38;5;246m<date>[39m[23m 2019-06-21[38;5;246m, [39m2019-06-22[38;5;246m, [39m2019-06-22[38;5;246m, [39m2019-06-22[38;5;246m, [39m2019-06-23[38;5;246m, [39m2019-06-23[38;5;246m, [39m2019-06-23[38;5;246m, [39m2019-06-24[38;5;246m, [39m201…
## $ Time             [3m[38;5;246m<chr>[39m[23m "22:00"[38;5;246m, [39m"16:30"[38;5;246m, [39m"19:00"[38;5;246m, [39m"22:00"[38;5;246m, [39m"16:30"[38;5;246m, [39m"19:00"[38;5;246m, [39m"22:00"[38;5;246m, [39m"16:30"[38;5;246m, [39m"19:00"[38;5;246m, [39m"22:00"[38;5;246m, [39m"19:00"[38;5;246m, [39m"…
## $ Home             [3m[38;5;246m<chr>[39m[23m "Egypt eg"[38;5;246m, [39m"Congo DR cd"[38;5;246m, [39m"Nigeria ng"[38;5;246m, [39m"Guinea gn"[38;5;246m, [39m"Morocco ma"[38;5;246m, [39m"Senegal sn"[38;5;246m, [39m"Algeria dz"[38;5;246m, [39m"Côt…
## $ HomeGoals        [3m[38;5;246m<dbl>[39m[23m 1[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m4[38;5;246m, [39m2[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m3[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0…
## $ Away             [3m[38;5;246m<chr>[39m[23m "zw Zimbabwe"[38;5;246m, [39m"ug Uganda"[38;5;246m, [39m"bi Burundi"[38;5;246m, [39m"mg Madagascar"[38;5;246m, [39m"na Namibia"[38;5;246m, [39m"tz Tanzania"[38;5;246m, [39m"ke Kenya"[38;5;246m, [39m…
## $ AwayGoals        [3m[38;5;246m<dbl>[39m[23m 0[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m1[38;5;246m, [39m2[38;5;246m, [39m1[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m0[38;5;246m, [39m2[38;5;246m, [39m2[38;5;246m, [39m4[38;5;246m, [39m1[38;5;246m, [39m4[38;5;246m, [39m3[38;5;246m, [39m3[38;5;246m, [39m2[38;5;246m, [39m0…
## $ Attendance       [3m[38;5;246m<dbl>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Venue            [3m[38;5;246m<chr>[39m[23m "Cairo International Stadium (Neutral Site)"[38;5;246m, [39m"Cairo International Stadium (Neutral Site)"[38;5;246m, [39m"Alexand…
## $ Referee          [3m[38;5;246m<chr>[39m[23m "Alioum Alioum"[38;5;246m, [39m"Rédouane Jiyed"[38;5;246m, [39m"Bernard Camille"[38;5;246m, [39m"Amin Omar"[38;5;246m, [39m"Louis Hakizimana"[38;5;246m, [39m"Sadok Selmi"…
## $ Notes            [3m[38;5;246m<chr>[39m[23m ""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m""[38;5;246m, [39m…
## $ MatchURL         [3m[38;5;246m<chr>[39m[23m "https://fbref.com/en/matches/7c214290/Egypt-Zimbabwe-June-21-2019-Africa-Cup-of-Nations"[38;5;246m, [39m"https://…
## $ Home_xG          [3m[38;5;246m<dbl>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
## $ Away_xG          [3m[38;5;246m<dbl>[39m[23m [31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m[31mNA[39m[38;5;246m, [39m…
```

``` r
View(missing_cup_xg_rows)
```

The dataset seems to be lacking quite a lot of data for the domestic cup competitions. Not only was it last updated in February 2024 (no data for the 24/25 season), very little xG data from 2018-2024 for the domestic cup competitions exists.
Simply because there is so much missing data I don't think this data is usable for my analysis, I'll most likely be using the UnderStat data (which I'll analyse in a later section) as there's simply too much missing here.  
The main problem is there's data missing which simply shouldn't be. For example, there should be xG data for the 2020 FA Cup tie between Tottenham and Norwhich (I can access it easily online) but it's just not there.  
There's many other databases I can pull data from with this package, so I'll use cup data from one of them.  
