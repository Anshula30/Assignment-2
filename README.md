# Storm Data Analysis

## Synopsis

The purpose for this analysis is to answer the following questions:

1.Across the United States, which types of events are most harmful with respect to population health?

2.Across the United States, which types of events have the greatest economic consequences?

The data used for this data analysis is NOAA storm database from 1950 to November 2011.

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

## Conclusion

To do the conclusion we have the following plots

Code Chunk-3
``` {r plot1, fig.height=15, fig.width=10,fig.align='right'}
par(mfrow = c(2, 1), mar = c(5,12,4,12), oma = c(0, 0, 2, 0))
bp<-barplot(rank_fatal_top$Freq, names.arg = rank_fatal_top$EVTYPE_NEW,
       col='white', space=1, las=2,
       main = "Number of Fatal People by Event from 1950-2011",
        xlab="Fatal",
        xlim=c(0,6000), horiz=TRUE, beside=TRUE,
        cex.axis=.75, cex.lab=.75,cex.main=1, cex.names=.75)
text(0,bp, labels=NULL, cex=.65, offset=25)
## Barplot 1b
bp<-barplot(rank_inj_top$Freq, names.arg = rank_inj_top$EVTYPE_NEW,
            col='yellow', space=1, las=2,
            main = "Total Injuries by Event Type (1950-2011)",
            xlab="Injured",
            xlim=c(0,90000), horiz=TRUE, beside=TRUE,
            cex.axis=.75, cex.lab=.75,cex.main=1, cex.names=.75)
text(0,bp, labels=NULL, cex=.65, offset=25)
box("outer")
```
