plot4 <- function() {

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

## We have already got df2 which contains only those two days' records from plot1.R in working space
## convert Time variable to POSIXct object (so in next step we can get absolute time interval)
df3<-mutate(df2, Time=as.POSIXct(strptime(paste(Date,Time),"%Y-%m-%d %H:%M:%S")), weekday=wday(Date))
## convert POSIXct object time to absolute time interval, for the sake of drawing plot (power dynamics vs. time elapse)
df3<-mutate(df3, Time=unclass(Time))

## get 3 sub data sets for drawing 
sub1<-data.frame(Time=df3$Time, submetering1=as.numeric(df3$Sub_metering_1))
sub2<-data.frame(Time=df3$Time, submetering2=as.numeric(df3$Sub_metering_2))
sub3<-data.frame(Time=df3$Time, submetering3=as.numeric(df3$Sub_metering_3))

## draw the plot in the png graphics device as requested
png(filename="./plot4.png", width=480, height=480)

## put 2 x 2 figs in the plot
par(mfrow=c(2,2))

## plot fig 1 at row1, col 1
## disable x axis and set empty xlabel
with(df3, plot(Time, Global_active_power, type="l", ylab="Global Active Power", xaxt="n", xlab=""))
## get the number of time intervals in those two days
num<-length(df3$Time)
## draw x labels of weekdays: "Thu", "Fri" and "Sat" at the corresponding timer interval positions
axis(1, at=c(df3$Time[1],df3$Time[num/2],df3$Time[num]), labels=c("Thu","Fri","Sat"))


## plot fig 2 at row1, col 2
## disable x axis and set empty xlabel
with(df3, plot(Time, Voltage, type="l", ylab="Voltage", xaxt="n", xlab="datetime"))
## get the number of time intervals in those two days
num<-length(df3$Time)
## draw x labels of weekdays: "Thu", "Fri" and "Sat" at the corresponding timer interval positions
axis(1, at=c(df3$Time[1],df3$Time[num/2],df3$Time[num]), labels=c("Thu","Fri","Sat"))


## plot fig 3 at row2, col 1
## disable x axis and set empty xlabel
plot(sub1, type="l", ylab="Energy sub metering", xaxt="n", xlab="")
## draw multiple y variables against the same x variable (Time) in the single plot
lines(sub2, col="red")
lines(sub3, col="blue")
## add legend
legend("topright", lty="solid", col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
## get the number of time intervals in those two days
num<-length(df3$Time)
## draw x labels of weekdays: "Thu", "Fri" and "Sat" at the corresponding timer interval positions
axis(1, at=c(df3$Time[1],df3$Time[num/2],df3$Time[num]), labels=c("Thu","Fri","Sat"))


## plot fig 4 at row2, col 2
## disable x axis and set empty xlabel
with(df3, plot(Time, Global_reactive_power, type="l", ylab="Global_reactive_power", xaxt="n", xlab="datetime"))
## get the number of time intervals in those two days
num<-length(df3$Time)
## draw x labels of weekdays: "Thu", "Fri" and "Sat" at the corresponding timer interval positions
axis(1, at=c(df3$Time[1],df3$Time[num/2],df3$Time[num]), labels=c("Thu","Fri","Sat"))


## turn off the graphics device after use!
dev.off()

}