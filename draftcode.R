#urL="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#download.file(urL,dest="activity.zip")
#unzip("activity.zip")
activity<-read.csv("activity.csv")

#Creating a subset with no NA values in the 'step' variable of the data set
#activityNoNA<-subset(activity,!is.na(activity$steps))

#Calculating the total number of steps per day
TotalStepsPerDate<-tapply(activity$steps,activity$date,sum,na.rm=TRUE)
#plot
hist(TotalStepsPerDate,xlab="Total number of steps per day", main="Histogram")
#report of the mean and median values of the total number of steps per day
meanD=mean(TotalStepsPerDate)
medianD=median(TotalStepsPerDate)

#calculating the total number of columns with 'NA' values.



#Average daily activity pattern
#mean number of steps per interval average over across all days
avePerInterval<-tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
plot(1:length(unique(activity$interval)),avePerInterval,type="l",xlab="5 min intervals (per day)",ylab="Mean number of steps", main="Time series (Steps/Interval)")

indexM<-which.max(as.vector(avePerInterval))
ss<-as.vector(unique(activity$interval))
minuteMax<-ss[indexM]


avePerIntervalM<-as.matrix(avePerInterval)
#for (i in 1:length(activity$steps)){
#        if(is.na(activity$steps[i])==TRUE){
#                activity$interval[i]<-tapply(activity$steps[i],activity$interval[i],mean,na.rm=TRUE)
#        }
#}
activity2<-activity
tmp<-unique(activity$interval)
for (i in 1:length(activity$steps)){
        if(is.na(activity$steps[i])==TRUE){
                PP<-activity$interval[i]
                indexP<-match(PP,tmp)
                activity2$steps[i]<-avePerIntervalM[indexP]
        }
}
print(sum(is.na(activity$steps)))

par(mfrow=c(1,2))
hist(activity$steps,main="Original",xlab="steps")
hist(activity2$steps,main="Modified",xlab="steps")

#create a vector with the conversion of days to weekdays
wd=list()
for (i in 1:length(activity2$date)){
        wd<-c(wd,weekdays(as.Date(as.character(activity2$date[i]))))        
}
wd<-as.matrix(wd)
activity3<-cbind(activity2,wd)

#create a variable that is tru for weekend days
weekend<-(activity3$wd=="Saturday" | activity3$wd=="Sunday")
#add to activity3
activity3<-cbind(activity3,weekend)

tab1<-subset(activity3,activity3$weekend==TRUE)
tab2<-subset(activity3,activity3$weekend==FALSE)




