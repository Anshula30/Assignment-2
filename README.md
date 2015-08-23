# Storm Data Analysis

## Synopsis

Bad weather events causes health problems in people of all generations. This analysis is about the weather conditions. The data used for this data analysis is  NOAA storm database from 1950 to November 2011.

This analysis two question.

1.Across the United States, which types of events are most harmful with respect to population health?

2.Across the United States, which types of events have the greatest economic consequences?

## Data Processing

The data used foe the following data analysis was found at the following URL.

"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

For processing the data some liberaries were loaded

Code Chunk-1
```{r prep-envir}
library(data.table)
library(stringdist)
library(plyr)
library(ggplot2)
library(ggthemes)
library(reshape2)
graphics.off()  
setInternet2(use = TRUE) 
link_addr <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" 
```
Code Chunk-2
```{r load-the-file,cache=TRUE}
download.file(link_addr, destfile="repdata-data-StormData.csv.bz2", method="auto")
rawload <- read.csv("repdata-data-StormData.csv.bz2",
   header = TRUE, sep = ",",
   quote = "\"",stringsAsFactors = FALSE)

num_EVtypes <- length(unique(rawload$EVTYPE))
```
