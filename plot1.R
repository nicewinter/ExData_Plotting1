plot1 <- function() {

 ##load packages for efficient date/time and data extration manipulation
library(lubridate)
library(dplyr)

 ## read raw data into a data frame
df<-read.csv("./power.txt", sep=";", colClass="character")
object.size(df)

 ## convert Date variable to POXISct object
df<-mutate(df, Date=dmy(Date))
d1<-ymd("2007-02-01")
d2<-ymd("2007-02-02")

## extract only those records for the two days specified
df2<-filter(df, Date == d1 | Date==d2)

## draw the histgram in the png graphics device as requested
png(filename="./plot1.png", width=480, height=480)
with(df2, hist(as.numeric(Global_active_power), main="Global Active Power", xlab="Global Active Power (kilowatts)", col="red"))
dev.off()

}