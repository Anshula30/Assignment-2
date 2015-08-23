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

## Load the package "stringdist" for use later.
library(stringdist)
## Loading required package: parallel
require(stringdist)
## Download the file from the link given by the Reproducible Research Coursera course.
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destinationFile <- "/Users/Mario/StormData.csv.bz2"
download.file(fileURL, destinationFile, method="curl")

## Read the .csv file into a data frame.
data <- read.csv(destinationFile)
As mentioned, there are 37 columns and here are their column names:

## Returns the names of the columns in the data frame.
names(data)
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
In order to keep this analysis focused, only this subset of columns will be kept in the data frame:

“STATE__”
“BGN_DATE”
“BGN_TIME”
“STATE”
“EVTYPE”
“F”
“MAG”
“FATALITIES”
“INJURIES”
“PROPDMG”
“PROPDMGEXP”
“CROPDMG”
“CROPDMGEXP”
Please see the Appendix at the end of this document for more information about the meaning of each of these.

So we will next go ahead and subset the data frame to keep just these columns.

## Subset the data frame to include only the columns of interest.  The new data frame will be called "data_c" to represent the fact that it has been subsetted by columns.
data_c <- data[c("STATE__","BGN_DATE","BGN_TIME","STATE","EVTYPE","F","MAG","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
The next thing to do is create the “dictionary” of the 48 official event types. The 900+ event types in the original data set will be mapped to one of these 48 official event types using the amatch() function in the “stringdist” pachage, using the Levenshtein distance to compare the similarities and differences between strings.

## Create the dictionary
## These 48 event types can be found on pages 2, 3, and 4 at this url: http://www.nws.noaa.gov/directives/sym/pd01016005curr.pdf
## To view the original set of event types, use: levels(factor(data$EVTYPE)) .

dictionary <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")
First make the dictionary and the EVTYPE column both lowercase, and then add the replacement column to the data frame.

## Make both the dictionary and the EVTYPE column lowercase, to make the mapping more accurate.
dictionary <- tolower(dictionary)
data_c$EVTYPE <- tolower(data_c$EVTYPE)
data_c$EVTYPE_48 <- dictionary[amatch(data_c$EVTYPE,dictionary,method="lv", maxDist=20)]
I would however like to take a moment to talk about the validity of using this method to standardize the event types found in the column EVTYPE. There are numerous non-standard entries such as “?”, “dust devil waterspout”, “freezing drizzle”, “gustnado and”, “lack of snow”, “no severe weather”, “none”, “summary of june 6”, and even “excessive”. Under the assumption that these represent a minority of cases, and that events like “no severe weather” and “none” did not lead to casualties or property damage, I decided that it was okay to let the function amatch() decide what to assign to each of these values. I will also not be looking at averages, so this is another reason that this method is sufficient. This would not be a good method to use if, for example, one wanted to know how much damage tornados caused on average, per occurence.

Next, since the full range of 48 event types were not included into the data base until 1996, we'll subset the data frame to include only years greater than or equal to 1996. In reality this step could have been done before running the amatch() function. It would have saved a little time, but the amatch() function excuted rather quickly even on the full data set.

## Creates a column of dates for ease of use.
data_c$BGN_DATE_Stan <- as.Date(data_c$BGN_DATE,format="%m/%d/%Y")

## Creates a new data frame.  The "d" added to the name signifies that it has been subsetted according to date. (Starting January 1st, 1996 an onwards, inclusive)
data_cd <- data_c[data_c$BGN_DATE_Stan >= "1996-01-01",]
Further, the data will be subsetted to include only events that had either casualties or property damage. This is due to the fact that we will only be looking at totals rather than averages.

## Removes all rows that have a 0 in each of the 4 columns FATALITIES, INJURIES, PROPDMG, CROPDMG.  
## In other words, if a weather event led to no casualites or economic damage, we will ignore it from this point on.
data_cd0 <- data_cd[data_cd$FATALITIES != 0 | data_cd$INJURIES != 0 | data_cd$PROPDMG != 0 | data_cd$CROPDMG != 0,]
Before we begin to look at results, let's do a little sanity check and compare the 8 most common event types in the column EVTYPE to the 8 most common event types in EVTYPE_48. Remember that EVTYPE has many nonstandard and misspelled entries, while the latter only contains the standard 48 event types.

## These return the top 8 most common event types in the columns EVTYPE and EVTYPE_48.
head(sort(table(data_cd0$EVTYPE),decreasing=TRUE),n=8)
## 
##         tstm wind thunderstorm wind              hail       flash flood 
##             61776             43097             22679             19011 
##           tornado         lightning             flood         high wind 
##             12366             11152              9513              5402
head(sort(table(data_cd0$EVTYPE_48),decreasing=TRUE),n=8)
## 
##         high wind thunderstorm wind              hail       flash flood 
##             67244             43100             22683             19100 
##           tornado         lightning             flood       strong wind 
##             12366             11294              9744              3414
Pleasently, we see no immediately cause for concern with the way the function amatch() has cleaned up our EVTYPE column. “tstm wind” means “thunderstorm wind”, and it is a little unfortunate that “tstm wind” was apparently mapped to “high wind”, but the meanings are similar enough that we can accept this. The important thing is, we know that in the original data wind, hail, floods, and tornados were the most common event types, and they are as well in our new column of standard event types.

Another step of processing that will be done is to combine the data on fatalities and injuries into a total number of casualities. Instead of simply adding the two figures however, a weight of 10 will be given to each fatality. This is an arbitrary weight, though more reasonable than considering a fatality and an injury to be of equal importance. This will be added to a column called CASUALTIES.

## Adds a column called CASUALITIES to the data frame, which is a weighted combination of fatalties and injuries.
data_cd0$CASUALTIES <- 10*data_cd0$FATALITIES + data_cd0$INJURIES
The last step of processing the data is to find out what the total dollar amounts of property damage and crop damage are, as well as an overall total. This will require a bit of care because the amount of damage is found in one column as the base value, and in the adjacent column as an exponent. Luckily, at this point, the only values left in the exponents column are “K”, “M”, and “B”, whereas before we had done some subsetting, there were many nonstandard values.

## To have a look at what type of exponents we are dealing with, look at: table(data_cd0$PROPDMGEXP) and table(data_cd0$CROPDMGEXP).
## It was seen at the time of this analysis that the only values were blanks, "K"'s, "M"'s, and "B"'s.
## The following are two for loops to find the total amount of property damage per event.
## ******* These for loops take about 3 minutes each, they should be optimized somehow ****************
for (i in 1:length(data_cd0$PROPDMG)) {
  if (data_cd0$PROPDMGEXP[i] == "K") {
    data_cd0$PROPDMG_TOTAL[i] <- data_cd0$PROPDMG[i]*10^3
  }
  else if (data_cd0$PROPDMGEXP[i] == "M") {
    data_cd0$PROPDMG_TOTAL[i] <- data_cd0$PROPDMG[i]*10^6
  }
  else if (data_cd0$PROPDMGEXP[i] == "B") {
    data_cd0$PROPDMG_TOTAL[i] <- data_cd0$PROPDMG[i]*10^9
  }
  else data_cd0$PROPDMG_TOTAL[i] <- 0
}

## For loop to find the total amount of crop damage per event.
for (i in 1:length(data_cd0$CROPDMG)) {
  if (data_cd0$CROPDMGEXP[i] == "K") {
    data_cd0$CROPDMG_TOTAL[i] <- data_cd0$CROPDMG[i]*10^3
  }
  else if (data_cd0$CROPDMGEXP[i] == "M") {
    data_cd0$CROPDMG_TOTAL[i] <- data_cd0$CROPDMG[i]*10^6
  }
  else if (data_cd0$CROPDMGEXP[i] == "B") {
    data_cd0$CROPDMG_TOTAL[i] <- data_cd0$CROPDMG[i]*10^9
  }
  else data_cd0$CROPDMG_TOTAL[i] <- 0
}

## Now lets find the total monetary damage, giving equal weight to property and crop damage.
data_cd0$TOTALDMG <- data_cd0$CROPDMG_TOTAL + data_cd0$PROPDMG_TOTAL
## One last step, let's create a data table from our data frame.
library(data.table)
datatable <- data.table(data_cd0)
