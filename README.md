# Storm Data Analysis

## Synopsis

The purpose for this analysis is to answer the following questions:

1.Across the United States, which types of events are most harmful with respect to population health?

2.Across the United States, which types of events have the greatest economic consequences?

The data used for this data analysis is NOAA storm database from 1950 to November 2011.


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

## Processing Of Data

Code Chunk-3, It shows the whole processing of the data.

```{r prep-envir}
library(data.table)
library(stringdist)
library(plyr)
library(ggplot2)
library(ggthemes)
library(reshape2)
graphics.off()  
+setInternet2(use = TRUE)
link_addr <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" # Valid 8.12.2015
```


```{r load-the-file,cache=TRUE}
download.file(link_addr, destfile="repdata-data-StormData.csv.bz2", method="auto")
rawload <- read.csv("repdata-data-StormData.csv.bz2",
                    header = TRUE, sep = ",",
                    quote = "\"",stringsAsFactors = FALSE)

num_EVtypes <- length(unique(rawload$EVTYPE))
```

```{r fix-evtype}

valid_EV <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Frost/Freeze","Funnel Cloud","Freezing Fog","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane","Ice Storm","Lake-Effect Snow","Lakeshore Flood","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Sleet","Storm Surge/Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")

uniqs_EV_raw <- unique(rawload$EVTYPE)  

uniqs_EV_proc <- gsub("TSTM","THUNDERSTORM",uniqs_EV_raw)  # updates abbreviation for thunderstorms
uniqs_EV_proc <- gsub("FLD","FLOOD",uniqs_EV_proc)  # updates abbreviation for flood
uniqs_EV_proc <- gsub("FLOODS","FLOOD",uniqs_EV_proc)  # updates abbreviation for floods
uniqs_EV_proc <- gsub("CURRENTS","CURRENT",uniqs_EV_proc)  # updates abbreviation for flood
uniqs_EV_proc <- gsub("SNOW","WINTER STORM",uniqs_EV_proc)  # updates snow to winter storm
uniqs_EV_proc <- gsub("RECORD","EXTREME",uniqs_EV_proc)  # updates phrase "record" to "extreme"
uniqs_EV_proc <- gsub("FUNNEL","FUNNEL CLOUD",uniqs_EV_proc, fixed=TRUE)  
uniqs_EV_proc <- gsub("WIND","HIGH WIND",uniqs_EV_proc, fixed=TRUE)  

uniqs_EV_proc <- gsub("COLD","COLD/WIND CHILL",uniqs_EV_proc)  
uniqs_EV_proc <- gsub("FROST","FROST/FREEZE",uniqs_EV_proc)  
uniqs_EV_proc <- gsub("FREEZE","FROST/FREEZE",uniqs_EV_proc)  
uniqs_EV_proc <- gsub("TIDE","SURGE/TIDE",uniqs_EV_proc)  
uniqs_EV_proc <- gsub("SURGE","SURGE/TIDE",uniqs_EV_proc)   

for (i in seq_along(uniqs_EV_proc)){
      if (grepl("HURRICANE",toupper(uniqs_EV_proc[i]))){
            uniqs_EV_proc[i] <- "HURRICANE"
      }
}


EV_best<-as.data.frame(stringdistmatrix(toupper(uniqs_EV_proc),toupper(valid_EV),useNames="strings"))

raw_lbl <-uniqs_EV_raw
proc_lbl <-list()
new_lbl <-list()

for (i in seq_along(EV_best[,1])){
      proc_lbl[i]<-row.names(EV_best[i,])  
      if (EV_best[i,which.min(EV_best[i,])]<100 ){new_lbl[i]<-names(which.min(EV_best[i,]))}
      if (row.names(EV_best[i,]) == "SEICHE"){new_lbl[i]<-"SEICHE"}  
}


df_EV_tx <-as.data.frame(cbind(raw_lbl, proc_lbl, new_lbl),stringsAsFactors=FALSE) 
names(df_EV_tx)[1]<-"EVTYPE"
names(df_EV_tx)[3]<-"EVTYPE_NEW"


short_raw<-rawload[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]


simple_df<-merge(short_raw,df_EV_tx,by="EVTYPE", all.x=TRUE, all.y=FALSE, sort=FALSE)


remove(short_raw, df_EV_tx)  

```

## Conclusion

To do the conclusion we have the following plots

Code Chunk-4, It shows the rate of people died and got injured during the event.

```{r plot1, fig.height=15, fig.width=10,fig.align='right'}
par(mfrow = c(2, 1), mar = c(5,12,4,12), oma = c(0, 0, 2, 0))
bp<-barplot(rank_fatal_top$Freq, names.arg = rank_fatal_top$EVTYPE_NEW,
       col='white', space=1, las=2,
       main = "Number of Fatal People by Event from 1950-2011",
        xlab="Fatal",
        xlim=c(0,6000), horiz=TRUE, beside=TRUE,
        cex.axis=.75, cex.lab=.75,cex.main=1, cex.names=.75)
text(0,bp, labels=NULL, cex=.65, offset=25)
bp<-barplot(rank_inj_top$Freq, names.arg = rank_inj_top$EVTYPE_NEW,
            col='yellow', space=1, las=2,
            main = "Total Injuries by Event Type (1950-2011)",
            xlab="Injured",
            xlim=c(0,90000), horiz=TRUE, beside=TRUE,
            cex.axis=.75, cex.lab=.75,cex.main=1, cex.names=.75)
text(0,bp, labels=NULL, cex=.65, offset=25)
box("outer")
```
## According to the above plot we found that Tornadoes were the leading reason for population health. Than we had some other events like heat, thunderstorm etc. (Answer-1)

Code Chunk-5, It shows the harm by the Event on the property and crop.

```{r plot2,fig.height=15, fig.width=10,fig.align='right'}
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

## According to the above plot we firgured that Property damage is very much graeter than crop damage.Also flood and drought were the main reason for crop damage.(Answer-2)

## Thank You.
