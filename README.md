# State_STORMS
library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)
####1
getwd()
data <- read.csv("input.csv")
Storm_data <- read.csv("StormEvent_1997.csv")
###2
myvars <- c("BEGIN_YEARMONTH", "BEGIN_DAY", "BEGIN_TIME", "END_YEARMONTH", "END_DAY", "END_TIME", "EVENT_ID", "STATE", "STATE_FIPS", "CZ_TYPE", "CZ_FIPS", "CZ_NAME", "EVENT_TYPE", "SOURCE", "BEGIN_LAT", "BEGIN_LON", "END_LAT", "END_LON", "BEGIN_DATE_TIME", "END_DATE_TIME", "YEAR", "TOR_WIDTH", "TOR_LENGTH")
newdata <- Storm_data[myvars]
head(newdata)
###3
mutate(newdata, BEGIN_DATE_TIME = dmy_hms(BEGIN_DATE_TIME), END_DATE_TIME = dmy_hms(END_DATE_TIME))

####4
myvars2 <- c("STATE", "CZ_NAME")
newdata2 <- Storm_data[myvars2]
str_to_title(newdata2)
###5
newdata<-filter(newdata, newdata$CZ_TYPE == "C")

newdata = subset(newdata, select = -c(CZ_TYPE) )



###6
str_pad(newdata$STATE_FIPS, width = 3, side = "left" , pad = "0" )
str_pad(newdata$CZ_FIPS, width = 3, side = "left" , pad = "0")
unite(newdata, "ST_CZ", c("STATE_FIPS","CZ_FIPS"))
###7
rename_all(newdata, tolower)
###8
data("state")
us_state_info<-data.frame(state=state.name, region=state.region, area=state.area)
head(us_state_info)
####9
df <- data_frame("state")
table(newdata$STATE)
newset<- data.frame(table(newdata$STATE))
head(newset)
merged <- merge(x=newset,y=us_state_info,by.x="state", by.y="state")
newset1<-rename(newset, c("state"="Var1"))
merged <- merge(x=newset1,y=us_state_info,by.x="state", by.y="state")
head(merged)
head(newset1)
head(us_state_info)
head(merged)
head(newset)
table(us_state_info$state)
newdatasetname<-(mutate_all(us_state_info, toupper) )
        
head((newdatasetname))
merged <- merge(x=newset1,y=newdatasetname,by.x="state", by.y="state")
head(merged)

#10
library(ggplot2)
state_storms<-data_frame(merged)
table(merged$Freq)
head(state_storms)

storm_plot <- ggplot(state_storms, aes(x = area, y = Freq)) + geom_point(aes(color= region)) + labs(x = "Land area (square miles", y = "# of storm events in 1997") 
head(storm_plot)   
view(storm_plot)
ggplot(state_storms, aes(area, Freq, colour = region)) + geom_point()
