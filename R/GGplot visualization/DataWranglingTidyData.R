# Unit Data Wrangling

install.packages("tidyr")
install.packages("data.table")

DF = read.csv("2017-Q1-Trips-History-Data.csv")

### Tidy data 
#################################################

library(tidyr)

# check structure
str(DF)

# there are different formats for data
DF$End.date

# unify date format
DF$End.date=as.character(DF$End.date)
DF$End.date=gsub("\\.","/", DF$End.date)
DF$End.date

# date and time reflect two different variables
DF=separate(DF,End.date, c("End.date", "End.time"), sep=" ")
str(DF)

# create date variable 
DF$End.date=as.Date(DF$End.date, "%m/%d/%Y")
?strptime

# unite two values into new variable
DF=unite(DF, connection, Start.station.number, Start.station, sep="-")

# registered and casual seem to be values of the same variable
dummies=DF[,c("id", "Casual", "Registered")]
head(dummies, 20)
dummies=gather(dummies, Member.Type, value, -id)
head(dummies, 20)
str(dummies)
dummies=dummies[dummies$value==1,]
str(dummies)


### Data Manipulation with data.table
#################################################

library(data.table)

#performance benchmark
start=Sys.time()
DF = read.csv("2017-Q1-Trips-History-Data.csv")
Sys.time()-start

start=Sys.time()
DT = fread("2017-Q1-Trips-History-Data.csv")
Sys.time()-start

#transforming data.frames into data.tables
DT=setDT(DF)
str(DT)
DT

### Filtering with i 

# subsetting data.tables (one criterion)
DT[End.station=="Potomac & Pennsylvania Ave SE"]

# subsetting data.tables (two criteria)
DT[ End.date >= as.Date("03-30-2017" , "%m-%d-%Y" ) & End.station=="Potomac & Pennsylvania Ave SE"]
DT[ End.date >= as.Date("03-30-2017" , "%m-%d-%Y" ) & Duration < 200000]


### Transforming with j

DT[ End.date >= as.Date("03-30-2017" , "%m-%d-%Y" ) & Duration < 200000,
    .(Duration=Duration/(1000*60),
      Log.Duration=log(Duration),
      Max.Duration=max(Duration),
      End.date=End.date)]

### Aggregate with by

# daily aggregate 
DT[ End.date >= as.Date("02-01-2017" , "%m-%d-%Y" ) & Duration < 200000,
    .(Duration=mean(Duration/(1000*60), na.rm=TRUE),
      Log.Duration=sum(log(Duration), na.rm=TRUE)),
    by=.(End.date=End.date)]

# weekly aggregate 
DT[ End.date >= as.Date("02-01-2017" , "%m-%d-%Y" ) & Duration < 200000,
    .(Duration=mean(Duration/(1000*60), na.rm=TRUE),
      Log.Duration=sum(log(Duration), na.rm=TRUE)),
    by=.(End.date=week(End.date))]

# monthly aggregate 
DT[ End.date >= as.Date("02-01-2017" , "%m-%d-%Y" ) & Duration < 200000,
    .(Duration=mean(Duration/(1000*60), na.rm=TRUE),
      Log.Duration=sum(log(Duration), na.rm=TRUE)),
    by=.(End.date=month(End.date))]

# other aggregate options
?IDateTime


# aggregate by multiple criteria
DT[ End.date >= as.Date("02-01-2017" , "%m-%d-%Y" ) & Duration < 200000,
    .(Duration=mean(Duration/(1000*60), na.rm=TRUE),
      Log.Duration=sum(log(Duration), na.rm=TRUE)),
    by=.(End.date=month(End.date),
         End.station=End.station)]


### sorting 

# descending order
DT[order(-Duration)]

# ascending order
DT[order(Duration)]

# order by multiple criteria
DT[order(End.date, -Duration)]

# chaining 

DT[ End.date >= as.Date("02-01-2017" , "%m-%d-%Y" ) & Duration < 200000,
    .(Duration=mean(Duration/(1000*60), na.rm=TRUE),
      Log.Duration=sum(log(Duration), na.rm=TRUE)),
    by=.(End.date=month(End.date),
         End.station=End.station)][order(End.date, -Duration)]


### Merging data sets
#################################################

locations=read.csv("Capital_Bike_Share_Locations.csv")
str(locations)
locations=locations[,4:6]
locations=setDT(locations)

## inner join: keep matching instances only 
DTM=merge(DT,locations,by.x="End.station.number", by.y="TERMINAL_NUMBER")

## left join: keep all instances 1st data set (x;DT) 
DTM=merge(DT,locations,by.x="End.station.number", by.y="TERMINAL_NUMBER", all.x=TRUE)

## right join: keep all instances 2nd data set (y;locations) 
DTM=merge(DT,locations,by.x="End.station.number", by.y="TERMINAL_NUMBER", all.y=TRUE)

## outer join: keep all instances of both data sets (DT;locations) 
DTM=merge(DT,locations,by.x="End.station.number", by.y="TERMINAL_NUMBER", all=TRUE)

## merge back cleaned dummies variable
DT=merge(DT, dummies, by="id")
