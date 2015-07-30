#OrigTable = read.csv( file = "D:/UCLA courses/S15/b230/Project/2008.csv", header=TRUE )
#dim(OrigTable)
#OrigTable[1,]

#modified_table = OrigTable[OrigTable[,2]<2,]
#dim(modified_table)
#colnames(modified_table)

#write.csv(modified_table, file = "D:/UCLA courses/S15/b230/Project/2008_modified2.csv",row.names=F)

---------------------------------------------------------------------------------
#Actual Project
Table = read.csv( file = "D:/UCLA courses/S15/b230/Project/2008_modified.csv", header=TRUE )
#[1] 605765     29      
mainData=Table[,3:29]
#605765     27

#Logical array to find conplete cases
completeData=complete.cases(mainData[,1:19])

#gets all delayed and on-time flights; removes diverted flights
data.complete.raw=mainData[completeData,]
#removed cancelled data information
data.complete=subset(data.complete.raw,select=-c(Diverted,Cancelled,CancellationCode))

dim(data.complete)
##[1] 587130     24

#Get all airports
origin=unique(data.complete[,15])
dest=unique(data.complete[,16])
loc=union(origin,dest)

#Ordering the data by day of month January
day_order=data.complete[order(data.complete$DayofMonth),]
colnames(day_order)

---------------------------------------------------------------------------------------------------------------------
#~Analysis Part 1
#construct only delayed-flight data
notOnTime=complete.cases(day_order[,20:24])

delayed.flights=day_order[notOnTime,]
head(delayed.flights)

#Plotting delayed data
#number of flights delayed by day
week_delay=c()
for (i in 1:7)
{
  week_delay=c(week_delay,length(delayed.flights[delayed.flights$DayOfWeek==i,11]))  
}
#Total number of flights by day
week_flights=c()
for (i in 1:7)
{
  week_flights=c(week_flights,length(day_order[day_order$DayOfWeek==i,11]))  
}
#Finding probability od delay
days=c("Mon","Tue","Wed","Thurs","Fri","Sat","Sun")
delay.proportion=week_delay/week_flights
dotchart(delay.proportion,labels=days, color='red',lcolor="blue",xlab="Percentage Of Flights Delayed",ylab="Days",main="Plot Of Percentage Of Flight Delay On A Day")
total=sum(delay.proportion)
dotchart(delay.proportion/total,labels=days, color='red',lcolor="blue",xlab="Probability Of Delay Across All Seven Days",ylab="Days",main="Plot To Show Probability Delay In A Week")
##If you take a flight on Thursday, your flight will be delayed 0.168 percent irrespective of the carrier


--------------------------------------------------------------------------------------------------------------------------
#~Analysis Part 2
#Find out max delay by a flight

head(day_order)

number.carrier=levels(delayed.flights$UniqueCarrier)
number.carrier[1]
delay.by.carrier=c()
for(i in 1:length(number.carrier))
{
  delay.by.carrier=c(delay.by.carrier,length(delayed.flights[delayed.flights$UniqueCarrier==number.carrier[i],7]))
}
a=c(1:20)

plot(a,delay.by.carrier, type="n")
text(a,delay.by.carrier,number.carrier)
dotchart(delay.by.carrier,labels=number.carrier,ylab="Airline Carriers",xlab="Total Flights Delayed", main="Number of Flights Delayed of Carriers During One Month Of Analysis", color='red',lcolor="blue")
delay.by.carrier
##Answer from the plot southwest has max delay of flights

#Get percent delay of flights

total.flights.by.carrier=c()
for(i in 1:length(number.carrier))
{
  total.flights.by.carrier=c(total.flights.by.carrier,nrow(day_order[day_order$UniqueCarrier==number.carrier[i],]))
}

percent.delay.carrier = delay.by.carrier/total.flights.by.carrier

plot(a,percent.delay.carrier,type="n")
text(a,percent.delay.carrier,number.carrier)
dotchart(percent.delay.carrier,labels=number.carrier,ylab="Airline Carriers",xlab="Total Flights Delayed", main="Percent of Total Flights Delayed of Carriers During One Month Of Analysis", color='red',lcolor="blue")
##UA is worst percent delay
--------------------------------------------------------------------------------------------------------------------------
#~Analysis Part 3 and Part 4
  
hub=c()
arrive.delayvector=c()
depart.delayvector=c()
number.depart.carrier=c()
number.arrive.carrier=c()

for(i in 1:length(loc))
{
  print(i)
  for(j in 1:length(number.carrier))
  {    
    hub=c(hub,nrow(day_order[((day_order$UniqueCarrier==number.carrier[j]) & (day_order$Origin==loc[i] | day_order$Dest==loc[i])),])) 
    dforigin=day_order[(day_order$UniqueCarrier==number.carrier[j] & day_order$Origin==loc[i]),14]
    dfdest=day_order[((day_order$UniqueCarrier==number.carrier[j]) & (day_order$Dest==loc[i])),13]
    
    depart.delayvector=c(depart.delayvector,sum(dforigin))
    number.depart.carrier=c(number.depart.carrier,length(dforigin))
    arrive.delayvector=c(arrive.delayvector,sum(dfdest))
    number.arrive.carrier=c(number.arrive.carrier,length(dfdest))    
  }
}
hub_matrix=matrix(hub,nrow=286,ncol=20,byrow=T)
depart.delaymatrix=matrix(depart.delayvector,nrow=286,ncol=20,byrow=T)
arrive.delaymatrix=matrix(arrive.delayvector,nrow=286,ncol=20,byrow=T)
number.depart.carrier=matrix(number.depart.carrier,nrow=286,ncol=20,byrow=T)
number.arrive.carrier=matrix(number.arrive.carrier,nrow=286,ncol=20,byrow=T)

depart.delay.per.flight=depart.delaymatrix/number.depart.carrier
arrive.delay.per.flight=arrive.delaymatrix/number.arrive.carrier

##Hub answers
hub.indices=c()

for (i in 1:20)
{
  index=which.max(hub_matrix[,i])
  hub.indices=c(hub.indices,loc[index])
}

print("carrier  Hub")
##part 3 output below
for (i in 1:20)
{
  cat(sprintf("%s   %s\n",number.carrier[i],hub.indices[i]))
}

extract.max.depart=depart.delay.per.flight
extract.max.arrive=arrive.delay.per.flight
max.depart=matrix(nrow=3,ncol=20,byrow=F)
max.arrive=matrix(nrow=3,ncol=20,byrow=F)
for (i in 1:20)
{ 
  for (j in 1:3)
  {
    
    max_index=which.max(na.pass(extract.max.depart[,i]))
    max.depart[j,i]=loc[max_index]
    extract.max.depart[max_index,i]=NA
    
    max_index=which.max(na.pass(extract.max.arrive[,i]))
    max.arrive[j,i]=loc[max_index]
    extract.max.arrive[max_index,i]=NA
    
  }
}
hub.traffic.depart=c()
hub.traffic.arrive=c()
for (i in 1:20)
{
  if (any(is.element(max.depart[,i],hub.indices[i])))
    hub.traffic.depart=c(hub.traffic.depart,TRUE)  
  if (any(is.element(max.arrive[,i],hub.indices[i])))
    hub.traffic.arrive=c(hub.traffic.arrive,TRUE)  
}
hub.traffic.depart
hub.traffic.arrive

##No delay because of hub airport for that airline

--------------------------------------------------------------------------------------------------------------------------  
#~Analysis Part 5
  
#Find number of flights departing and ariving into a city
airport.depart.count=rowSums(number.depart.carrier)
airport.arrive.count=rowSums(number.arrive.carrier)

#Calculating departure delay wrt airports

qplot(airport.depart.count,rowSums(depart.delaymatrix)/airport.depart.count,ylab="Total Departure Delay By Airports(In Minutes)",xlab="Total Number Of Departures From An Airport",main="Plot Showing Total Departure Delay against Number of Total Departures From All Airports")

qplot(airport.arrive.count,rowSums(arrive.delaymatrix)/airport.arrive.count,ylab="Total Arrival Delay By Airports(In Minutes)",xlab="Total Number Of Arrivals At An Airport",main="Plot Showing Total Arrival Delay against Number of Total Arrivals From All Airports")
library(ggplot2)

##Plotting log answers

qplot(log(airport.depart.count),rowSums(depart.delaymatrix)/airport.depart.count,,ylab="Total Departure Delay By Airports(In Minutes)",xlab="Log(Total Number Of Departures From An Airport)",main="Plot Showing Total Departure Delay against Log(Number of Total Departures) From All Airports")

qplot(log(airport.arrive.count),rowSums(arrive.delaymatrix)/airport.arrive.count,ylab="Total Arrival Delay By Airports(In Minutes)",xlab="Log(Total Number Of Arrivals At An Airport)",main="Plot Showing Total Arrival Delay against Log(Number of Total Arrivals) From All Airports")


##No coorelation from whereever you book.
---------------------------------------------------------------------------------------------------------------------------
#~Analysis Part 6

##############################Prdeiction thinking:
lmrecords=c()
for(i in 1:length(number.carrier))
{
  data_subset=day_order[day_order$UniqueCarrier==number.carrier[i],1:17]
  newrecord=lm(ArrDelay ~ (Distance), data=data_subset)
  title=sprintf("Plot Of Arrival Delay Against Distance Of A Carrier %s",number.carrier[i])
  plot((data_subset$Distance),data_subset$ArrDelay,main=title,xlab=("Distance Of Flights Carrier(in miles)"),ylab=("Arrival Delay Of Flights For Carrier(in minutes)"))
  abline(newrecord, col = "brown")
  
  lmrecords=c(lmrecords,newrecord)
}

---------------------------------------------------------------------------------------------------------------------------
#~Analysis Part 7
#How much weather plays a part in delay
  
head(delayed.flights)
total.delayed=nrow(delayed.flights)
weather.delay.carrier=rep(0,20)
for (i in 1:total.delayed)
{
  currDelay=delayed.flights[i,21]
  if(currDelay>0)
  {
    carrier=delayed.flights[i,7]
    index=which(number.carrier == carrier)
    weather.delay.carrier[index] = weather.delay.carrier[index] + currDelay
  }
}

total.delay.by.carrier=colSums(arrive.delaymatrix)
percent.weather.responsible=weather.delay.carrier/total.delay.by.carrier
plot(percent.weather.responsible,total.delay.by.carrier)

number.carrier[14]

which(number.carrier=="UA")
splom
##############################################################
#Study delay on two scales: delay and cacelled

corr=subset(day_order,select=c(ArrDelay,Distance))
biplot(princomp(corr,cor=T),cex=c(.6,.6),
       pc.biplot=T)
day_order$m=c(day_order$Origin)
