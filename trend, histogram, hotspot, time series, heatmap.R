chchdata<- read.csv("christchurch-data.csv")

head(chchdata)

library(data.table)
# change the header
setnames(chchdata,"Month.Year", "Month_Year",skip_absent=TRUE) 
setnames(chchdata,"ANZSOC.Division","ANZSOC_Division",skip_absent=TRUE)
setnames(chchdata,"Area.Unit","Area_Unit",skip_absent=TRUE)
setnames(chchdata,"Occurrence.Day.Of.Week","Occurrence_Day_Of_Week",skip_absent=TRUE)
setnames(chchdata,"Occurrence.Hour.Of.Day","Occurrence_Hour_Of_Day",skip_absent=TRUE)
# change the value names
chchdata[chchdata == "Theft and Related Offences"] <- "Theft & RO"
chchdata[chchdata == "Abduction, Harassment and Other Related Offences Against a Person"] <- "Abduction/Harassment & RO"
chchdata[chchdata == "Robbery, Extortion and Related Offences"] <- "Robbery/Extortion & RO"
chchdata[chchdata == "Sexual Assault and Related Offences"] <- "Sexual Assault & RO" 
chchdata[chchdata == "Unlawful Entry With Intent/Burglary, Break and Enter"] <- "Unlawful Entry" 
chchdata[chchdata == "Monday"] <- "MON"
chchdata[chchdata == "Tuesday"] <- "TUE"
chchdata[chchdata == "Wednesday"] <- "WED"
chchdata[chchdata == "Thursday"] <- "THU"
chchdata[chchdata == "Friday"] <- "FRI"
chchdata[chchdata == "Saturday"] <- "SAT"
chchdata[chchdata == "Sunday"] <- "SUN"
# set the Month_Year column to be in date format
chchdata$Month_Year <- as.Date(chchdata$Month_Year)
# ensure the Area_Unit is character in order to proceed to the next step which is to remove the "." at the end.
is.character(chchdata$Area_Unit)
# to remove the "." after the area unit for further processing
chchdata$Area_Unit <- substr(chchdata$Area_Unit,
                        start= 1,
                        stop= nchar(chchdata$Area_Unit)-1 ) 

#######################################################################################################################
library(tidyverse)
library(dplyr)
library(ggplot2)

freq1 <- chchdata %>% group_by(Area_Unit) %>% summarise(Freq=n()) 
#count the frequency of crime in different suburb of Christchurch

freq1rank <- rank(-freq1$Freq)
freq1rank1 <- freq1[order(freq1rank,decreasing = F),]
#now we can rank the suburb with most crime in Christchurch

c1<-freq1rank1[1]
c1
b1<-head(c1,128)
W1<-top_n(freq1rank1,128)
#overview christchurch crime
Q2<-chchdata[,c("Area_Unit","ANZSOC_Division")]
summary(Q2)
head(Q2)
Q2$freq<-1
Q3<-aggregate(freq~Area_Unit ,data=Q2,FUN=sum)

#chart 0 , overview Christchurch crime suburb
Q3$Area_Unit<-factor(Q3$Area_Unit, levels = Q3$Area_Unit[order(Q3$freq)])
chart0<- ggplot(Q3,aes(Area_Unit,freq)) +
  geom_bar(stat = "identity")+theme(text = element_text(size=8),axis.text.x = element_text(angle=55, hjust=1)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), title = element_text(size = 12))+
  xlab("Area Unit") +
  ylab("Frequency") +
  ggtitle("Crime Frequency in Christchurch by Area Unit, July 2016 to June 2021")
chart0

#chart 1 top 10 geom_bar 
c2<-head(c1,10)
c2#now we can get the top 10 suburb with most crime in Christchurch
W3<- subset(chchdata, subset = Area_Unit  %in% c("Cathedral Square","Sydenham","Riccarton","Linwood East",
                                                 "Shirley East","Hagley Park","Northcote","Linwood","Hornby North","Avon Loop"))
#subset the dataset to meet the Area.unit column equal to the top 10 areas
W3$Area_Unit
#create barchart for top 10 hotspot in CHCH
chart1<- ggplot(W3,aes(W3$Area_Unit,fill=W3$Area_Unit))+
  geom_bar()+xlab("Area Unit")+ylab("Frequency")+ 
  theme(text = element_text(size=10),axis.text.x = element_text(angle=55, hjust=1), legend.position = "none") +
  ggtitle("Top 10 Hotspot Barchart")
chart1

#chart 2, Crime frequency in top 10 suburb
f1<-c("Cathedral Square","Sydenham","Riccarton","Linwood East","Shirley East","Hagley Park","Northcote",
      "Linwood","Hornby North","Avon Loop")
f2<-c(7222,3210,3006,2496,2477,2372,2338,2310,2166,2152)
colors = c("black", "grey13", "grey22", "grey35","grey45", "grey55", "grey65", "grey80","grey91","white") 
chart2<-pie(f2, labels = f1, main="Crime Frequency in Top 10 Suburb of Christchurch",col=colors)


#Chart 3, bar chart count the frequency of each crime in top 10 suburb
chart3<-ggplot(data=W3, aes(x=W3$ANZSOC_Division,fill=ANZSOC_Division )) +
  geom_bar()+ xlab("ANZSOC Division") + ylab("Frequency")+
  ggtitle("Crime Frequency in Christchurch by Area Unit by Crime Type, July 2016 to June 2021") +
  facet_grid(.~Area_Unit,scale="free") + 
  theme(text = element_text(size=12),axis.text.x = element_blank())
chart3

#######################################################################################################################

#create a new dataframe that only contains the required columns
daycrime <- chchdata[, c("ANZSOC_Division","Occurrence_Day_Of_Week")]

#rearrange the order of the day of the week
daycrime$Occurrence_Day_Of_Week <- factor(daycrime$Occurrence_Day_Of_Week, 
                                     levels = c("MON","TUE","WED","THU","FRI","SAT","SUN","UNKNOWN"))
# create a barchart to show the crime occurrence count each day of the week
ggplot(data = daycrime) +
  geom_bar(aes(x = Occurrence_Day_Of_Week, fill = ANZSOC_Division)) + 
  xlab("Occurrence Day of Week") +
  ylab("Frequency") +
  ggtitle("Crime Frequency in Christchurch by Crime Type by Occurrence Day, July 2016 to June 2021") +
  theme(text = element_text(size=12))

#######################################################################################################################

# create a heatmap to visualise the crime time distribution
# create a dataframe:totalfreq to record the total frequency of all crime 
totalfreq <- chchdata[, c("Occurrence_Day_Of_Week","Occurrence_Hour_Of_Day")]
# calculate the accumulated count for crimes in each time slot
totalfreq$Frequency <- 1 
totalfreq <- aggregate(Frequency~Occurrence_Hour_Of_Day+Occurrence_Day_Of_Week,data=totalfreq,FUN=sum)
# filter out crime records with unknown time (day and hour) 
totalfreq<-subset(totalfreq, Occurrence_Day_Of_Week!="UNKNOWN" & Occurrence_Hour_Of_Day!="99")
head(totalfreq)
# calculate the percentage of the crime occurrence for each time slot instead of total counts for better visualisation 
totalfreq$Percentage <- totalfreq$Frequency/53982
# reorder the sequence of the days and hours
totalfreq$Occurrence_Day_Of_Week <- factor(totalfreq$Occurrence_Day_Of_Week, 
                                            levels = c("MON","TUE","WED","THU","FRI","SAT","SUN"))
totalfreq$Occurrence_Hour_Of_Day <- factor(totalfreq$Occurrence_Hour_Of_Day, 
                                            levels = c("23","22","21","20","19","18","17","16","15","14","13","12","11","10","9","8","7","6","5","4","3","2","1","0"))
# create a "percent" function so that the numbers can be displaced in the format of decimal representation
percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}
# create the heatmap to display the crime occurrence time distribution
ggplot(totalfreq, aes(x = Occurrence_Day_Of_Week, y = Occurrence_Hour_Of_Day, fill = Percentage)) +
  geom_tile() +
  geom_text(aes(label = percent(Percentage)))+
  scale_fill_gradient(low = "white", high = "red")+
  xlab("Occurrence Day of Week")+
  ylab("Occurrence Hour of Day") +
  theme(legend.position = "none") +
  ggtitle("Crime Occurrence Time Distribution")

#######################################################################################################################

# making a heatmap more in detail showing for each different type of crime
# create dataframe: crimefreq to have the count frequency of types crimes
crimefreq <- chchdata[, c("ANZSOC_Division","Occurrence_Day_Of_Week","Occurrence_Hour_Of_Day","Area_Unit")]
crimefreq$Frequency <- 1
crimefreq <- aggregate(Frequency~Occurrence_Hour_Of_Day+Occurrence_Day_Of_Week+ANZSOC_Division,data=crimefreq,FUN=sum)
# filter out crime records with unknown day and hour info
crimefreq<-subset(crimefreq, Occurrence_Day_Of_Week!="UNKNOWN" & Occurrence_Hour_Of_Day!="99")
# reorder the sequence of the days and hours
crimefreq$Occurrence_Day_Of_Week <- factor(crimefreq$Occurrence_Day_Of_Week, 
                                          levels = c("MON","TUE","WED","THU","FRI","SAT","SUN"))
crimefreq$Occurrence_Hour_Of_Day <- factor(crimefreq$Occurrence_Hour_Of_Day, 
                                          levels = c("23","22","21","20","19","18","17","16","15","14",
                                                     "13","12","11","10","9","8","7","6","5","4","3","2","1","0"))
# create the heatmap to display the crime occurrence time distribution for different crime types
ggplot(crimefreq, aes(x = Occurrence_Day_Of_Week, y = Occurrence_Hour_Of_Day, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = round(Frequency, 1)))+
  scale_fill_gradient(low = "white", high = "red")+
  xlab("Occurrence Day of Week")+
  ylab("Occurrence Hour of Day") +
  facet_grid(.~ANZSOC_Division) +
  ggtitle("Crime Occurrence Time Distribution by Crime Type")

#######################################################################################################################

# making a heatmap more in detail showing the data for top 10 crime areas in Christchurch
# create dataframe: areafreq to have the count frequency of crime at top 10 area unit in Christchurch
areafreq <- chchdata[, c("ANZSOC_Division","Occurrence_Day_Of_Week","Occurrence_Hour_Of_Day","Area_Unit")]
areafreq$Frequency <- 1
areafreq <- aggregate(Frequency~Occurrence_Hour_Of_Day+Occurrence_Day_Of_Week+Area_Unit,data=areafreq,FUN=sum)
# filter out crime records with unknown day and hour info
areafreq<-subset(areafreq, Occurrence_Day_Of_Week!="UNKNOWN" & Occurrence_Hour_Of_Day!="99")
# gathering the top 10 area units with crimes
areafreq <- areafreq%>% 
  filter(areafreq$"Area_Unit" == "Cathedral Square" | areafreq$"Area_Unit" == "Sydenham" | areafreq$"Area_Unit" == "Riccarton" |
           areafreq$"Area_Unit" == "Linwood East" | areafreq$"Area_Unit" == "Shirley East" | areafreq$"Area_Unit" == "Hagley Park" |
           areafreq$"Area_Unit" == "Northcote" |areafreq$"Area_Unit" == "Linwood"|areafreq$"Area_Unit" == "Hornby North" |
           areafreq$"Area_Unit" == "Avon Loop")
# reorder the sequence of the days and hours
areafreq$Occurrence_Day_Of_Week <- factor(areafreq$Occurrence_Day_Of_Week, 
                                           levels = c("MON","TUE","WED","THU","FRI","SAT","SUN"))
areafreq$Occurrence_Hour_Of_Day <- factor(areafreq$Occurrence_Hour_Of_Day, 
                                           levels = c("23","22","21","20","19","18","17","16","15","14","13",
                                                      "12","11","10","9","8","7","6","5","4","3","2","1","0"))
# reorder the area units by frequency in descending order
areafreq$Area_Unit <- factor(areafreq$Area_Unit,
                              levels = c("Cathedral Square", "Sydenham", "Riccarton", "Linwood East", "Shirley East", "Hagley Park", 
                                         "Northcote", "Linwood", "Hornby North","Avon Loop"))
# create the heatmap for crime time for top 10 areas in Christchurch with ggplot
ggplot(areafreq, aes(x = Occurrence_Day_Of_Week, y = Occurrence_Hour_Of_Day, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = round(Frequency, 1)))+
  scale_fill_gradient(low = "white", high = "red")+
  xlab("Occurrence Day of Week")+
  ylab("Occurrence Hour of Day") +
  facet_grid(.~Area_Unit) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Crime Occurrence Time Distribution by Top 10 Crime Area Units")
                     
#######################################################################################################################

# map for crimes in Christchurch
# create a new dataframe for crime map
crimemap <- data.frame(chchdata$Area_Unit)
colnames(crimemap)[1] <- "Area_Unit"
# calculate the frequency of the total crime in each area unit in Christchurch
crimemap$Frequency <- 1
crimemap <- aggregate(Frequency~Area_Unit,data=crimemap,FUN=sum)
head(crimemap)
is.data.frame(crimemap)
# filter out unknown crime location
crimemap<-subset(crimemap, Area_Unit!= "99999") 

library(rgdal)
library(sp)
# The following lines are to display the crime counts on the map with geospacial data.
# Shapefile is a type of geospatial vector data format which is commonly used in GIS, and the shapefile used in this report is downloaded from the Stats NZ website.
# The area unit classification in this shapefile is matching with the ones in the dataset we are using from the NZ Police.
# Therefore, it is possible to visualise the total count of crimes of each different area unit in Christchurch on the map.

# The shapefile used from Stats NZ includes a dataframe which contains information about each area unit, and polygon data about the digital geographic boundaries for each area unit.
# For this purpose, the rgdal package is used to read the shapefile to import the geographical location information for New Zealand.
# "https://datafinder.stats.govt.nz/layer/25743-area-unit-2013/"
map = readOGR(dsn = ".", layer = "area-unit-2013")

# To count the numbers, "merge.data.frame" function was initially used with the matching area unit , but it is unable to align to the correct polygon.
# Therefore, I choose to manually add the frequency count to the map dataframe for the 128 area units in Christchurch.
# Code like "which(grepl("Lyttelton", map@data$AU2013_V_1))" is used to locate the row number of the area unit.
map@data$Frequency <- 0
map@data["478", "Frequency"] <- 1570
map@data["1691","Frequency"] <- 282
map@data["1783","Frequency"] <- 134
map@data["583","Frequency"] <- 91
map@data["1737","Frequency"] <- 818
map@data["1749","Frequency"] <- 1016
map@data["1780","Frequency"] <- 23
map@data["1736","Frequency"] <- 2152
map@data["1747","Frequency"] <- 488
map@data["1712","Frequency"] <- 368
map@data["1699","Frequency"] <- 368
map@data["1752","Frequency"] <- 597
map@data["585","Frequency"] <- 34
map@data["1766","Frequency"] <- 1345
map@data["1767","Frequency"] <- 453
map@data["1772","Frequency"] <- 312
map@data["466","Frequency"] <- 1475
map@data["1703","Frequency"] <- 354
map@data["1757","Frequency"] <- 569
map@data["1696","Frequency"] <- 592
map@data["1705","Frequency"] <- 774
map@data["1758","Frequency"] <- 1326
map@data["1685","Frequency"] <- 390
map@data["1711","Frequency"] <- 411
map@data["1708","Frequency"] <- 317
map@data["1745","Frequency"] <- 197
map@data["463","Frequency"] <- 645
map@data["1732","Frequency"] <- 434
map@data["1730","Frequency"] <- 835
map@data["474","Frequency"] <- 7222
map@data["1746","Frequency"] <-  319
map@data["1714","Frequency"] <- 330
map@data["408","Frequency"] <- 129
map@data["920","Frequency"] <- 930
map@data["1761","Frequency"] <- 1352
map@data["464","Frequency"] <- 922
map@data["1760","Frequency"] <- 1102
map@data["450","Frequency"] <- 116
map@data["1735","Frequency"] <- 2372
map@data["1692","Frequency"] <- 173
map@data["1681","Frequency"] <- 325
map@data["1688","Frequency"] <- 249
map@data["1706","Frequency"] <- 509
map@data["1715","Frequency"] <- 229
map@data["1734","Frequency"] <- 347
map@data["1693","Frequency"] <- 159
map@data["1724","Frequency"] <- 193
map@data["1771","Frequency"] <- 620
map@data["1713","Frequency"] <- 287
map@data["1769","Frequency"] <- 422
map@data["1770","Frequency"] <- 201
map@data["1472","Frequency"] <- 2166
map@data["1670","Frequency"] <- 901
map@data["1717","Frequency"] <- 632
map@data["543","Frequency"] <- 9
map@data["544","Frequency"] <- 1
map@data["461","Frequency"] <- 796
map@data["1710","Frequency"] <- 606
map@data["1700","Frequency"] <- 80
map@data["1753","Frequency"] <- 2310
map@data["1756","Frequency"] <- 2496
map@data["1755","Frequency"] <- 920
map@data["607","Frequency"] <-87
map@data["407","Frequency"] <- 709
map@data["912","Frequency"] <- 485
map@data["1722","Frequency"] <- 315
map@data["1697","Frequency"] <- 119
map@data["1739","Frequency"] <- 1247
map@data["465","Frequency"] <- 285
map@data["1719","Frequency"] <- 1740
map@data["1727","Frequency"] <- 1079
map@data["1782","Frequency"] <- 356
map@data["1777","Frequency"] <- 372
map@data["1775","Frequency"] <- 858
map@data["405","Frequency"] <- 938
map@data["1720","Frequency"] <- 2338
map@data["1682","Frequency"] <- 487
map@data["460","Frequency"] <- 164
map@data["1762","Frequency"] <- 1157
map@data["1731","Frequency"] <- 1177
map@data["1686","Frequency"] <- 828
map@data["1726","Frequency"] <- 643
map@data["1754","Frequency"] <- 1820
map@data["1779","Frequency"] <- 15
map@data["1725","Frequency"] <- 591
map@data["1733","Frequency"] <- 315
map@data["1774","Frequency"] <- 1567
map@data["1698","Frequency"] <- 648
map@data["1702","Frequency"] <- 571
map@data["470","Frequency"] <- 770 
map@data["1728","Frequency"] <- 3006
map@data["1729","Frequency"] <- 734
map@data["1750","Frequency"] <- 763
map@data["1751","Frequency"] <- 1043
map@data["1707","Frequency"] <- 668
map@data["1740","Frequency"] <- 752
map@data["1704","Frequency"] <- 771
map@data["1744","Frequency"] <- 2477
map@data["1743","Frequency"] <- 1089
map@data["1505","Frequency"] <- 1766
map@data["479","Frequency"] <- 452
map@data["1776","Frequency"] <- 481
map@data["1768","Frequency"] <- 1266
map@data["1742","Frequency"] <- 1144
map@data["1741","Frequency"] <- 548
map@data["1763","Frequency"] <- 840
map@data["1738","Frequency"] <- 657
map@data["469","Frequency"] <- 490
map@data["1689","Frequency"] <- 631
map@data["406","Frequency"] <- 547
map@data["1764","Frequency"] <- 3210
map@data["1687","Frequency"] <- 156
map@data["475","Frequency"] <- 390
map@data["1721","Frequency"] <- 651
map@data["1718","Frequency"] <- 1160
map@data["468","Frequency"] <- 559
map@data["1748","Frequency"] <- 1241
map@data["1709","Frequency"] <- 435
map@data["477","Frequency"] <- 767
map@data["1716","Frequency"] <- 419
map@data["1723","Frequency"] <- 221
map@data["1690","Frequency"] <- 91
map@data["466","Frequency"] <- 611
map@data["1683","Frequency"] <- 1294
map@data["476","Frequency"] <- 757
map@data["1759","Frequency"] <- 1171
map@data["1684","Frequency"] <- 1087

# The sp package is used to create the map with spplot.
library(RColorBrewer)
spplot(map, "Frequency", xlim = c(1540000, 1590000), ylim= c(5163000, 5198000), 
       main=list(label="Total Crime Distribution in Christchurch, July 2016 to June 2021"),
       cuts = 8, col.regions = brewer.pal(9, "Blues"))

#######################################################################################################################

# make trend charts
# create dataframe for total crime and different crime types over the 5-year-period.
crimetrend <- chchdata[,c("Month_Year","ANZSOC_Division")]
crimetrend$Frequency <- 1
crimetrendtotal<- aggregate(Frequency~Month_Year,data=crimetrend,FUN=sum)
crimetrendc <- aggregate(Frequency~Month_Year+ANZSOC_Division, data = crimetrend, FUN = sum)

# trend chart for all crimes
library(ggplot2)
ggplot(data = crimetrendtotal, aes(x = Month_Year, y = Frequency, colour = "black")) + 
  geom_line(size = 1.3) +
  theme(legend.position = "none") +
  ggtitle("Total Crime Number Trend in Christchurch, July 2016 to June 2021") +
  xlab("Year") +
  ylab("Total Crime Count") +
  geom_smooth(method=lm)

# trend chart for different crime types
ggplot(data = crimetrendc, aes(x = Month_Year, y = Frequency, group = ANZSOC_Division, colour = ANZSOC_Division)) + 
  geom_line(size=1.3) +
  ggtitle("Crime Number Trend by Crime Type in Christchurch, July 2016 to June 2021") +
  xlab("Year") +
  ylab("Crime Count") +
  geom_smooth(method=lm)

###########################################################################################################################

# Time Series

# create the count number for total crime over the time period
chch <- chchdata[, c("Month_Year","ANZSOC_Division")]
chch$chchfreq <- 1
chch <- aggregate(chchfreq~Month_Year,data=chch,FUN=sum)

# transform the dataframe into a time series for analysis
chch_ts = ts(chch[,2], start = c(2016,7), frequency = 12)
head(chch_ts)
plot(chch_ts,
     col = "red",
     xlab = "Year", 
     ylab = "Crime Frequency", 
     main = "Time Series plot for total crime in Christchurch, July 2016 to Jun 2021")

# acf and pacf for original time series
acf(chch_ts, lag.max = 60, main = "Crime Time Series ACF", xlab = "Lag (year)")
pacf(chch_ts, lag.max = 60, main = "Crime Time Series PACF", xlab = "Lag (year)")

# carry out ADF test and KPSS test to test the stationarity of the time series
library(tseries)
adf.test(chch_ts)
kpss.test(chch_ts)

# first differencing to remove trend components in the series
chch_ts1 <- diff(chch_ts, differences = 1)

# ADF and KPSS test on the detrended time series
adf.test(chch_ts1)
kpss.test(chch_ts1)

# plot the detrended time series and autocorrelation diagram 
plot(chch_ts1, main = "Crime Time Series after First???differencing", xlab = "Year")
acf(chch_ts1, lag.max = 60, main = "ACF Crime Time Series after First???differencing", xlab = "Lag (year)")

# Ljung Box test for all lags
Box.test(chch_ts1, lag = 1, type = "Ljung-Box")
Box.test(chch_ts1, lag = 1, type = "Ljung-Box")
Box.test(chch_ts1, lag = 2, type = "Ljung-Box")
Box.test(chch_ts1, lag = 3, type = "Ljung-Box")
Box.test(chch_ts1, lag = 4, type = "Ljung-Box")
Box.test(chch_ts1, lag = 5, type = "Ljung-Box")
Box.test(chch_ts1, lag = 6, type = "Ljung-Box")
Box.test(chch_ts1, lag = 7, type = "Ljung-Box")
Box.test(chch_ts1, lag = 8, type = "Ljung-Box")
Box.test(chch_ts1, lag = 9, type = "Ljung-Box")
Box.test(chch_ts1, lag = 10, type = "Ljung-Box")
Box.test(chch_ts1, lag = 11, type = "Ljung-Box")
Box.test(chch_ts1, lag = 12, type = "Ljung-Box")
Box.test(chch_ts1, lag = 13, type = "Ljung-Box")
Box.test(chch_ts1, lag = 14, type = "Ljung-Box")
Box.test(chch_ts1, lag = 15, type = "Ljung-Box")
Box.test(chch_ts1, lag = 16, type = "Ljung-Box")
Box.test(chch_ts1, lag = 17, type = "Ljung-Box")
Box.test(chch_ts1, lag = 18, type = "Ljung-Box")
Box.test(chch_ts1, lag = 19, type = "Ljung-Box")
Box.test(chch_ts1, lag = 20, type = "Ljung-Box")
Box.test(chch_ts1, lag = 21, type = "Ljung-Box")
Box.test(chch_ts1, lag = 22, type = "Ljung-Box")
Box.test(chch_ts1, lag = 23, type = "Ljung-Box")
Box.test(chch_ts1, lag = 24, type = "Ljung-Box")
Box.test(chch_ts1, lag = 25, type = "Ljung-Box")
Box.test(chch_ts1, lag = 26, type = "Ljung-Box")
Box.test(chch_ts1, lag = 27, type = "Ljung-Box")
Box.test(chch_ts1, lag = 28, type = "Ljung-Box")
Box.test(chch_ts1, lag = 29, type = "Ljung-Box")
Box.test(chch_ts1, lag = 30, type = "Ljung-Box")
Box.test(chch_ts1, lag = 31, type = "Ljung-Box")
Box.test(chch_ts1, lag = 32, type = "Ljung-Box")
Box.test(chch_ts1, lag = 33, type = "Ljung-Box")
Box.test(chch_ts1, lag = 34, type = "Ljung-Box")
Box.test(chch_ts1, lag = 35, type = "Ljung-Box")
Box.test(chch_ts1, lag = 36, type = "Ljung-Box")
Box.test(chch_ts1, lag = 37, type = "Ljung-Box")
Box.test(chch_ts1, lag = 38, type = "Ljung-Box")
Box.test(chch_ts1, lag = 39, type = "Ljung-Box")
Box.test(chch_ts1, lag = 40, type = "Ljung-Box")
Box.test(chch_ts1, lag = 41, type = "Ljung-Box")
Box.test(chch_ts1, lag = 42, type = "Ljung-Box")
Box.test(chch_ts1, lag = 43, type = "Ljung-Box")
Box.test(chch_ts1, lag = 44, type = "Ljung-Box")
Box.test(chch_ts1, lag = 45, type = "Ljung-Box")
Box.test(chch_ts1, lag = 46, type = "Ljung-Box")
Box.test(chch_ts1, lag = 47, type = "Ljung-Box")
Box.test(chch_ts1, lag = 48, type = "Ljung-Box")
Box.test(chch_ts1, lag = 49, type = "Ljung-Box")
Box.test(chch_ts1, lag = 50, type = "Ljung-Box")
Box.test(chch_ts1, lag = 51, type = "Ljung-Box")
Box.test(chch_ts1, lag = 52, type = "Ljung-Box")
Box.test(chch_ts1, lag = 53, type = "Ljung-Box")
Box.test(chch_ts1, lag = 54, type = "Ljung-Box")
Box.test(chch_ts1, lag = 55, type = "Ljung-Box")
Box.test(chch_ts1, lag = 56, type = "Ljung-Box")
Box.test(chch_ts1, lag = 57, type = "Ljung-Box")
Box.test(chch_ts1, lag = 58, type = "Ljung-Box")

################################################################################################################################

