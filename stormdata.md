# Reproducible Research: Project 2
patrick charles  

# Storm Data Analysis
- Course: Reproducible Research (repdata-013)
- Project: Course Project 2
- Author: Patrick Charles

# Synopsis

In this report, we'll explore the NOAA Storm Database and answer some
questions about severe weather events. Cumulative health and economic impacts
will be evaluated against the types of events recorded in the dataset.

The data set analyzed contains all storm events in the United States as
entered by NOAA's National Weather Service (NWS) from 1950 through 2011.

Severe weather causes billions of dollars in damage and kills or injures
thousands of people. In this analysis we'll quantitatively compare the
impact of the most damaging types of events.


# Data Processing

## Prerequisite libraries
ggplot2 used for all plots, and tidyr/dplyr pipeline is used for processing.

```r
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(scales)
    if(!require(lubridate)) install.packages("lubridate", dep=T)
    if(!require(gridExtra)) install.packages("gridExtra", dep=T)
```

Historical storm data is obtained from [NOAA/StormData.csv.bz2](https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2)

The data is uncompressed, read into a dataframe, damage values scaled and
the results stored as a table data frame for further processing and analysis.


```r
  # data can be fetched from the source via:
  #   wget https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2
  stormfile <- bzfile("StormData.csv.bz2","r")
  df.sd <- read.csv(stormfile) # read the data
  tdf.sd <- tbl_df(df.sd) # convert to a table dataframe for dplyr use
```


```r
  # conversion function to scale values based on separate magnitude 
  scaleValue <- function(val, exp) {
    switch(exp, "K" = val * 10^3, "M" = val * 10^6, "G" = val * 10^9, val)
  }
  scaleVector = Vectorize(scaleValue) # vectorized version of the scale function

  # scale damage values by the multiplier stored in the separate 'exp' variable
  tdf.sd$PROPDMG <- scaleVector(tdf.sd$PROPDMG, as.character(tdf.sd$PROPDMGEXP))
  tdf.sd$CROPDMG <- scaleVector(tdf.sd$CROPDMG, as.character(tdf.sd$CROPDMGEXP))

  # convert strings to dates
  tdf.sd$BGN_DATE <- as.Date(tdf.sd$BGN_DATE, "%m/%d/%Y") # convert event start dates
  tdf.sd$END_DATE <- as.Date(tdf.sd$END_DATE, "%m/%d/%Y") # convert event end dates

  first.date <- min(tdf.sd$BGN_DATE) # find the date of the earliest event
  last.date <- max(tdf.sd$BGN_DATE) # find the date of the latest event
  dim(tdf.sd) # query the total number of observations
```

```
## [1] 902297     37
```
The storm data set contains 902297 event observations and
measurements of 37 different variables.

The storm data set covers events occurring during the period from
1950-01-03 to 2011-11-30.


# Analysis

## Health Impact

To determine which events are most harmful with respect to health,
we'll be looking at the 'FATALITIES' and 'INJURIES' variables


```r
  tdf.health <- tdf.sd %>% group_by(EVTYPE) %>% # group by event
      summarize(Killed=sum(FATALITIES), Injured=sum(INJURIES), 
                total=sum(INJURIES)+sum(FATALITIES))

  # reformat the data and filter events which impacted > threshold people
  health.threshold = 1000
  tdf.health.effects <- gather(tdf.health, impact, value, -EVTYPE, -total)
  tdf.health.worst <- tdf.health.effects %>%
    filter(total > health.threshold) %>% arrange(-total)

  head(tdf.health.worst)
```

```
## Source: local data frame [6 x 4]
## 
##           EVTYPE total  impact value
## 1        TORNADO 96979  Killed  5633
## 2        TORNADO 96979 Injured 91346
## 3 EXCESSIVE HEAT  8428  Killed  1903
## 4 EXCESSIVE HEAT  8428 Injured  6525
## 5      TSTM WIND  7461  Killed   504
## 6      TSTM WIND  7461 Injured  6957
```

The events most harmful to health can be visualized using a stacked
bar chart. Event types are shown which had a cumulative effect
on > 1000 individuals.

```r
  # order the event type factor by magnitude of total effect
  evfactors <- reorder(tdf.health.worst$EVTYPE, tdf.health.worst$total)

  ggplot(tdf.health.worst, aes(x=evfactors, y=value, fill=impact)) +
    geom_bar(stat="identity") + coord_flip() +
    ggtitle("Most Harmful Event Types to Population Health") +
    ylab("Total Number Impacted (from 1950 through 2011)") + xlab("Event Type")
```

![](stormdata_files/figure-html/health_visual-1.png) 

## Economic Impact

To determine which events are most harmful with respect to economic cost,
we'll be looking at the crop and property damage variables.


```r
  tdf.econ <- tdf.sd %>% group_by(EVTYPE) %>% # group by event
      summarize(Crop=sum(CROPDMG), Property=sum(PROPDMG), 
                total=sum(CROPDMG)+sum(PROPDMG))

  # reformat data and filter events with total economic impact > econ.threshold
  econ.threshold = 3000000000
  tdf.econ.effects <- gather(tdf.econ, impact, value, -EVTYPE, -total)
  tdf.econ.worst <- tdf.econ.effects %>%
    filter(total > econ.threshold) %>% arrange(-total)

  head(tdf.econ.worst)
```

```
## Source: local data frame [6 x 4]
## 
##    EVTYPE       total   impact       value
## 1 TORNADO 52040614066     Crop   414953270
## 2 TORNADO 52040614066 Property 51625660796
## 3   FLOOD 27819678380     Crop  5661968450
## 4   FLOOD 27819678380 Property 22157709930
## 5    HAIL 16952904944     Crop  3025537890
## 6    HAIL 16952904944 Property 13927367054
```

The most economically harmful events can be visualized using a stacked
bar chart. Event types are shown which had a cumulative economic
impact > $3\times 10^{9}.

```r
  # order the event type factor by magnitude of total effect
  evfactors <- reorder(tdf.econ.worst$EVTYPE, tdf.econ.worst$total)

  ggplot(tdf.econ.worst, aes(x=evfactors, y=value, fill=impact)) +
    geom_bar(stat="identity") + coord_flip() + 
    ggtitle("Most Economically Harmful Event Types") +
    ylab("Total $ Impact (from 1950 through 2011)") + xlab("Event Type")
```

![](stormdata_files/figure-html/economic_visual-1.png) 


# Results

- Across the United States, the following types of events are most
harmful to population health: Tornado, Excessive Heat, Wind, Flood and
Lightning

- Across the United States, the following types of events have
the greatest economic consequences: Tornado, Flood, Hail, Flash Flood, and
Drought

- These results assume that the EVTYPE variable contains distinct and unique event descriptors. In the provided data, this is not the case for all events. Events sometimes use inconsistent labels (e.g. "COLD", "COLD WAVE", "RECORD COLD") to describe the same or similar events. Also, many of the observations confound multiple event types (e.g. "SNOW AND COLD"", "FOG AND COLD"). A more precise analysis of the events would systematically address these inconsistencies.


# Appendix

It is also potentially interesting to look at this data over time
to see how the total economic and health impacts have varied over
the years of observation. 


```r
  tdf.econ.yearly <- tdf.sd %>%
    mutate(year=year(BGN_DATE)) %>%
      group_by(year) %>%
        summarize(crop=sum(CROPDMG), property=sum(PROPDMG),
                  total=sum(CROPDMG)+sum(PROPDMG))

  tdf.econ.yearly.gather <- gather(tdf.econ.yearly, impact, value, -year, -total)
```


```r
  tdf.health.yearly <- tdf.sd %>%
    mutate(year=year(BGN_DATE)) %>%
      group_by(year) %>%
        summarize(hurt=sum(INJURIES), killed=sum(FATALITIES),
                  total=sum(INJURIES)+sum(FATALITIES))

  tdf.health.yearly.gather <- gather(tdf.health.yearly, impact, value, -year, -total)
```

## Figure 3

```r
  econ.plot <-
    ggplot(tdf.econ.yearly.gather, aes(x=year, y=total, fill=impact)) +
    geom_bar(stat="identity") +
    stat_smooth() + 
    ggtitle("NOAA Storm Events - Economic Damage by Year") +
    ylab("Total Yearly Economic Damage ($)") + xlab("Year")
  health.plot <-
    ggplot(tdf.health.yearly.gather, aes(x=year, y=total, fill=impact)) +
    geom_bar(stat="identity") +
    stat_smooth() + 
    ggtitle("NOAA Storm Events - Injuries and Deaths by Year") +
    ylab("Total Yearly Number Persons Impacted)") + xlab("Year")

  grid.arrange(econ.plot, health.plot, ncol=1)
```

![](stormdata_files/figure-html/time_plots-1.png) 

Note that the plots are not inflation or US population adjusted.


## Building

To build the analysis from scratch:
```
make render
```

To view the published report on rpubs:
* http://rpubs.com/pchuck/stormdata

