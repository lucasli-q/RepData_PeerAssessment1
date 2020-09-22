###loading data
getwd() #checking wd
unzip("activity.zip",list=TRUE) #checking documents in zip file
unzip("activity.zip") #unzipping csv file inside zip file
activity <- read.csv(file="activity.csv")

###What is mean total number of steps taken per day?

##checking if there are NA values in the columns
sum(is.na(activity$steps)) #2304 NA values
sum(is.na(activity$date)) #0 NA values
sum(is.na(activity$interval)) #0 NA values

##excluding observations with NA values
activ_non_na <- na.omit(activity)

##arranging data by dates
library(plyr)
library(dplyr)
activ_arranged <- group_by(activ_non_na,date)

##calculating total number of steps by day
totaldailysteps <- aggregate(steps~date, data=activ_arranged, FUN=sum)

##plotting histogram of total daily steps
hist(totaldailysteps$steps, breaks=8, col=c("steelblue2"),main="Total daily steps", xlab="Number of steps",ylab="")

#creating png file
dev.copy(png, file="./plot1.png", width=480, height=480)
dev.off()

##summarizing data to specifically obtain only mean and median
dailysteps_summary <- summarize(totaldailysteps,meansteps=mean(steps),mediansteps=median(steps))
print(dailysteps_summary)

### What is the average daily activity pattern?

##arranging data by 5-minute interval
library(plyr)
library(dplyr)
activ_arranged2 <- arrange(activ_non_na,interval)
activ_arranged2 <- group_by(activ_non_na,interval)

##calculating means by interval
meandailysteps <- aggregate(steps~interval, data=activ_arranged2, FUN=mean)
head(meandailysteps)

##plotting steps mean by 5-minute interval
with(meandailysteps, plot(interval, steps,type="l", main="Mean number of steps by 5-minute interval", xlab="Interval", ylab="Steps Mean", col="steelblue2",pch=))

#creating png file
dev.copy(png, file="./plot2.png", width=480, height=480)
dev.off()


##obtaining the intervals that present the maximum value of mean steps
subset(meandailysteps, steps==max(meandailysteps$steps)) #interval 835

###Imputing missing values

##checking the number of NA values in the steps column of original data
sum(is.na(activity$steps)) #2304 NA values

##maintaining only rows that have NA values in them
activ_na <- filter(activity, is.na(steps))

##joining df containing rows with NA values and df containing means by interval
activ_na <- join(activ_na,meandailysteps,by=c("interval"))

##excluding former steps column and renaming stepsmean column for steps
activ_na <- activ_na[,2:4]
colnames(activ_na)<-c("date","interval","steps")

##merging the non NA values data with the former NA data now imputted with the respective 5-min interval mean
activity_na_imput <- rbind(activ_na,activ_non_na)
head(totaldailysteps2)

##arranging data by dates
activ_arranged3 <- group_by(activity_na_imput,date)

##calculating total number of steps by day
totaldailysteps2 <- aggregate(steps~date, data=activ_arranged3, FUN=sum)

##plotting histogram of total daily steps in the corrected NA values data
hist(totaldailysteps2$steps, breaks=8, col=c("steelblue2"),main="Total daily steps", xlab="Number of steps",ylab="")

#creating png file
dev.copy(png, file="./plot3.png", width=480, height=480)
dev.off()


##summarizing data to specifically obtain only mean and median
dailysteps_summary2 <- summarize(totaldailysteps2,meansteps=mean(steps),mediansteps=median(steps))



###Are there differences in activity patterns between weekdays and weekends?

##turning date variable from a character object to a date object
activity_na_imput$date <- as.Date(activity_na_imput$date)

##inserting weekday variable into df
activity_na_imput <- mutate(activity_na_imput, weekday=weekdays(activity_na_imput$date))

##turning weekdays into type of day variable (weekend or weekday)
activity_na_imput$weekday <- gsub("sábado","Weekend",activity_na_imput$weekday)
activity_na_imput$weekday <- gsub("domingo","Weekend",activity_na_imput$weekday)
activity_na_imput$weekday <- gsub("segunda-feira","Weekday",activity_na_imput$weekday)
activity_na_imput$weekday <- gsub("terça-feira","Weekday",activity_na_imput$weekday)
activity_na_imput$weekday <- gsub("quarta-feira","Weekday",activity_na_imput$weekday)
activity_na_imput$weekday <- gsub("quinta-feira","Weekday",activity_na_imput$weekday)
activity_na_imput$weekday <- gsub("sexta-feira","Weekday",activity_na_imput$weekday)
activity_na_imput$weekday <- as.factor(activity_na_imput$weekday)

##arranging data by date
activ_arranged4 <- group_by(activity_na_imput,date)

##calculating mean number of steps by date and aggregating
meandailysteps2 <- aggregate(steps~interval+weekday, data=activ_arranged4, FUN=mean)

##plotting graphics in lattice system comparing weekend days with weekday days
library(lattice)
meandailysteps2 <- transform(meandailysteps2, weekday=factor(weekday))
xyplot(steps~interval|weekday, data=meandailysteps2, layout=c(1,2), type="l")

#creating png file
dev.copy(png, file="./plot4.png", width=480, height=480)
dev.off()



##it is possible to plot in in the base system as well
#par(mfrow=c(2,1),mar=c(4,4,2,2))
#with(subset(meandailysteps2,weekday=="Weekend"), plot(interval, steps, type="l",col="steelblue2"))
#with(subset(meandailysteps2,weekday=="Weekday"), plot(interval, steps,type="l",col="steelblue2"))
