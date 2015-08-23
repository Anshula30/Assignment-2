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

##

## Conclusion

To do the conclusion we have the following plots

Code Chunk-3, It shows the rate of people died and got injured during the event.
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
According to the above plot we found that Tornadoes were the leading reason for population health. Than we had some other events like heat, thunderstorm etc.

Code Chunk-4, It shows the harm by the Event on the property and crop.
``` {r plot2,fig.height=15, fig.width=10,fig.align='right'}
par(mfrow = c(2, 1), mar = c(5,12,4,12), oma = c(0, 0, 2, 0))
bp<-barplot(rank_pdmg_top$SUM_PDMG, names.arg = rank_pdmg_top$EVTYPE_NEW,
            col='red', space=1, las=2,
            main = "Property Damage by Event",
            xlab="Property Damage ($US billions)",
            xlim=c(0,160), horiz=TRUE, beside=TRUE,
            cex.axis=.75, cex.lab=.75,cex.main=1, cex.names=.75)
text(0,bp, labels=NULL, cex=.65, offset=25)
bp<-barplot(rank_cdmg_top$SUM_CDMG, names.arg = rank_cdmg_top$EVTYPE_NEW,
            col='blue', space=1, las=2,
            main = "Crop Damage by Event",
            xlab="Crop Damage ($US billions)",
            xlim=c(0,15), horiz=TRUE, beside=TRUE,
            cex.axis=.75, cex.lab=.75,cex.main=1, cex.names=.75)
text(0,bp, labels=NULL, cex=.65, offset=25)
box("outer")
```

According to the above plot we firgured that Property damage is very much graeter than crop damage.Also flood and drought were the main reason for crop damage.
