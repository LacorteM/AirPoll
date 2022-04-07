################################################################################
###############                  Initialisation                  ###############
################################################################################
#### Add packages and functionFile ####
library(tidyverse)
library(chron)
library(dplyr)
library(tidyr)
library(imputeTS)

source("FunctionFile_v1.R")
source("WindRose.R")

#### Import the data base on R ####
getwd()
sionCSV <- file.path("data","SIO_2021.csv")
file.exists(sionCSV)
sionTXT <- file.path("data","SIO_Wind.txt")
file.exists(sionTXT)
magadinoCSV <- file.path("data", "MAG_2021.csv")
file.exists(magadinoCSV)
magadinoTXT <- file.path("data","MAG_Wind.txt")
file.exists(magadinoTXT)
####   Read the data and create the dataframe in combining with the wind measurement   ####
sionWind <- read.table(sionTXT, quote="\"", comment.char="",
            col.names=c("year", "month", "day", "hour", "minute", "windDirection", "windSpeed"))
magadinoWind <- read.table(magadinoTXT, quote="\"", comment.char="",
            col.names=c("year", "month", "day", "hour", "minute", "windDirection", "windSpeed"))
combinedWind <- full_join(cbind(site="SIO",MeanWind(sionWind)), cbind(site="MAG", MeanWind(magadinoWind)))
head(combinedWind)
####   Read the data and create the dataframe in combining with the pollution measurement   ####
sionData <- read.table(sionCSV,sep=";",skip=6)
sionData <- cbind(sionData, MeanWind(sionWind))
sionOriginal <- sionData
colnames(sionData)<-c("datetime","O3","NO2","PM10","PM2.5","NOX","TEMP","PREC","RAD", "W_DIR", "W_SPD")
magadinoData <- read.table(magadinoCSV,sep=";",skip=6)
magadinoData <- cbind(magadinoData, MeanWind(magadinoWind))
magadinoOriginal <- magadinoData
colnames(magadinoData)<-c("datetime","O3","NO2","SO2","PM10","PM2.5","EC","NOX","TEMP","PREC","RAD", "W_DIR", "W_SPD")
head(sionData)
#ControlMinute(sionWind[,"minute"])
#ControlMinute(magadinoWind[,"minute"])

####   Convert one variable datatime in month and date for the plot in Magadino   ####
magadinoData[,"datetime"] <- as.chron(magadinoData[,"datetime"], "%d.%m.%Y %H:%M") - 1/24
magadinoData[,"date"] <- dates(magadinoData[,"datetime"])
magadinoData[,"year"] <- years(magadinoData[,"datetime"])
magadinoData[,"month"] <- months(magadinoData[,"datetime"])
magadinoData[,"day"] <- days(magadinoData[,"datetime"])
magadinoData[,"hour"] <- hours(magadinoData[,"datetime"])
magadinoData[,"dayofwk"] <- weekdays(magadinoData[,"datetime"])
magadinoData[,"daytype"] <- ifelse(magadinoData[,"dayofwk"] %in% c("Sat","Sun"), "Weekend", "Weekday")
magadinoData[,"season"] <- Month2Season(unclass(magadinoData[,"month"]))
head(magadinoData)
tail(magadinoData)
####   Convert one variable datatime in month and date for the plot in Sion   ####
sionData[,"datetime"] <- as.chron(sionData[,"datetime"], "%d.%m.%Y %H:%M") - 1/24
sionData[,"date"] <- dates(sionData[,"datetime"])
sionData[,"year"] <- years(sionData[,"datetime"])
sionData[,"month"] <- months(sionData[,"datetime"])
sionData[,"day"] <- days(sionData[,"datetime"])
sionData[,"hour"] <- hours(sionData[,"datetime"])
sionData[,"dayofwk"] <- weekdays(sionData[,"datetime"])
sionData[,"daytype"] <- ifelse(sionData[,"dayofwk"] %in% c("Sat","Sun"), "Weekend", "Weekday")
sionData[,"season"] <- Month2Season(unclass(sionData[,"month"]))
head(sionData)
####   create the dataframe ####
combinedData <- full_join(cbind(site="SIO", sionData), cbind(site="MAG", magadinoData))
head(combinedData)
# Save the data frame
saveRDS(combinedData, "data/SIO-MAG_2021.rds")
# Modify the combined data frame for the plots
lf <- combinedData %>%
  gather(variable, value, -c(site, date, datetime, season, year, month, day, hour, dayofwk, daytype, season))
################################################################################
###############          Exemples with data of Magadino          ###############
################################################################################
# ####   Montly mean values of PM10 ####
# unique.months <- levels(magadinoData[,"month"])
# 
# PM10.monthly_mag <- NULL
# for(.month in unique.months) {
#   table <- filter(magadinoData, month == .month)
#   tmp <- data.frame(month=.month, PM10=mean(table[,"PM10"], na.rm=TRUE))
#   PM10.monthly_mag <- rbind(PM10.monthly_mag, tmp)
# }
# print(PM10.monthly_mag)
# 
# ####   Plot all points with lines of all values ####
# lf <- gather(magadinoData, variable, value, -c(datetime, month, date, year, day, dayofwk, daytype))
# ggplot(lf) +
#   facet_grid(variable~., scale="free_y") +
#   geom_line(aes(datetime, value))+
#   scale_x_chron()
# 
# ####   Plot all monthly histogram ####
# lf <- gather(magadinoData, variable, value, -c(datetime, month, date, year, day, dayofwk, daytype))
# result <- lf %>%
#   group_by(date, variable) %>%
#   summarize(value = mean(value, na.rm=TRUE))
# 
# spread(result, variable, value)
# ggplot(lf) +
#   facet_grid(variable~., scale="free_y") +
#   geom_bar(aes(month, value), stat="summary", fun="mean") +
#   scale_y_continuous(expand=expansion(mult=c(0, 0.1)))
# 
# ####   Plot all montly histogram with quantile   ####
# lf <- gather(sionData, variable, value, -c(datetime, month, date))
# ggplot(lf, aes(month, value)) +
#   facet_grid(variable~., scale="free_y") +
#   geom_bar(stat="summary", fun="mean") +
#   geom_errorbar(stat="summary",
#                 fun.min=min, #function(x) quantile(x, .25)
#                 fun.max=max, #function(x) quantile(x, .75)
#                 width=0.1)
# 
# ####   Finding missing values   ####
# x<-vector()
# for(i in 1:length(magadinoData[,"PM10"])){
#   if(is.na(magadinoData[i,"PM10"])){
#     x <- append(x,i)
#   }
# }
# print(paste("begin NAN at", x[1]))
# for(i in 2:(length(x)-1)){
#   if((x[i]-1) != x[i-1]){
#     print(paste("begin NAN at", x[i]));
#   }
#   if((x[i]+1) != x[i+1]){
#     print(paste("end NAN at", x[i]));
#   }
# }
# print(paste("end NAN at", x[length(x)]))
################################################################################
###############            Exemples with data combined           ###############
################################################################################
####   View variability in pollutant concentrations   ####
ggplot(lf)+                                          # `lf` is the data frame
  facet_grid(variable~site, scale="free_y")+         # panels created out of these variables
  geom_line(aes(datetime, value, color=site))+       # plot `value` vs. `time` as lines
  scale_x_chron()+                                   # format x-axis labels (time units)
  theme(axis.text.x=element_text(angle=30, hjust=1)) # rotate x-axis labels
####   Monthly variations with histogram   ####
ggplot(lf) +
  facet_grid(variable ~ site, scale = "free_y") +
  geom_boxplot(aes(month, value), outlier.size = 0.5, outlier.shape = 3)
####   Histogram by day type and season without NA in SION  ####
lf %>%
  filter(site=="SIO" & !is.na(value)) %>%
  ggplot +
  facet_grid(variable ~ season, scale = "free_y") +
  geom_boxplot(aes(daytype, value), outlier.size = 0.5, outlier.shape = 3)
####   Histogram by day type and season and precipitation without NA   ####
lf %>%
  filter(site=="SIO" & !is.na(value) & variable=="PREC") %>%
  ggplot +
  facet_grid(variable ~ season, scale = "free_y") +
  geom_bar(aes(daytype, value), stat="summary", fun="mean", show.legend = FALSE) +
  scale_y_continuous("Daily mean precipitation (mm)", expand=expansion(mult=c(0, 0.1)))
####   Correlation between O3 and temperature   ####
## calculate the daily maximum values for each variable
variables <- c("O3", "NO2", "PM10", "PM2.5", "NOX", "TEMP", "PREC", "RAD", "SO2", "EC")
lf <- gather(combinedData, variable, value, all_of(variables))
daily.max <- lf %>%
  group_by(site, year, month, day, season, variable) %>%
  summarize(value=max(value, na.rm=TRUE)) %>%
  spread(variable, value)
ggplot(daily.max)+
  facet_grid(site~season)+
  geom_point(aes(TEMP, O3))
## correlation between O3 and Temperature
(cor.values <- daily.max %>% group_by(site, season) %>%
    summarize(correlation=cor(TEMP, O3, use="pairwise.complete.obs")))
## histogram des correlation
ggplot(cor.values)+
  geom_col(aes(season, correlation))+
  scale_y_continuous(limits=c(0,1))+
  facet_grid(.~site)

####   Lag   ####
lagged <- combinedData %>%
  group_by(site, season) %>%
  do(rbind(Lag(.[,c("RAD","O3")], 1),
           Lag(.[,c("RAD","O3")], 2),
           Lag(.[,c("RAD","O3")], 3),
           Lag(.[,c("RAD","O3")], 4),
           Lag(.[,c("RAD","O3")], 5),
           Lag(.[,c("RAD","O3")], 6)))
ggplot(lagged) +
  geom_point(aes(RAD, O3, group=site, color=site), shape=4)+
  facet_grid(lag~season)

lagged.values <- combinedData %>% group_by(site, season) %>%
  do(LaggedCorrelation(.[,c("RAD","O3")], lag.max=6))
ggplot(lagged.values)+
  geom_segment(aes(x=lag,xend=lag,y=0,yend=value))+
  facet_grid(site~season)+
  xlab("lag (hours)")+
  ylab("Cross correlation coefficient")

################################################################################
#############          Data analysis assignment SIO-MAG!           #############
################################################################################
##### Problem 2 #####
# Modify the combined data frame for the plots
dataP2 <- combinedData %>%
  gather(variable, value, -c(site, date, datetime, season, year, month, day, hour, dayofwk, daytype, season, W_DIR, W_SPD))
####   View variability in pollutant concentrations
plotP2 <- ggplot(dataP2)+                                          # `lf` is the data frame
  facet_grid(variable~site, scale="free_y")+         # panels created out of these variables
  geom_line(aes(datetime, value, color=site))+       # plot `value` vs. `time` as lines
  scale_x_chron()+                                   # format x-axis labels (time units)
  theme(axis.text.x=element_text(angle=30, hjust=1)) # rotate x-axis labels
plotP2
png(plotP2, "plotP2.png")
#### Promblem2a ####
## Max hourly values for O3 and PM10
sionMaxHourO3 <- max(sionData$O3, na.rm = TRUE)
sionMaxHourPM10 <- max(sionData$PM10, na.rm = TRUE)
magadinoMaxHourO3 <- max(magadinoData$O3, na.rm = TRUE)
magadinoMaxHourPM10 <- max(magadinoData$PM10, na.rm = TRUE)

## Max daily values for NO2 and So2
sionMeanDayNO2 <- sionData %>% 
  group_by(date) %>%
  summarise(value = mean(NO2, na.rm = TRUE))
sionMaxDayNO2 <- max(sionMeanDayNO2$value)

magadinoMeanDayNO2 <- magadinoData %>% 
  group_by(date) %>%
  summarise(value = mean(NO2, na.rm = TRUE))
magadinoMaxDayNO2 <- max(magadinoMeanDayNO2$value)

magadinoMeanDaySO2 <- magadinoData %>% 
  group_by(date) %>%
  summarise(value = mean(SO2, na.rm = TRUE))
magadinoMaxDaySO2 <- max(magadinoMeanDaySO2$value)

## Yearly values for NO2, PM10, PM2.5 & SO2
sionMeanYearNO2 <- sionData %>% 
  group_by(year) %>%
  summarise(value = mean(NO2, na.rm = TRUE))
sionMaxYearNO2 <- max(sionMeanYearNO2$value)

sionMeanYearPM10 <- sionData %>% 
  group_by(year) %>%
  summarise(value = mean(PM10, na.rm = TRUE))
sionMaxYearPM10 <- max(sionMeanYearPM10$value)

sionMeanYearPM2.5 <- sionData %>% 
  group_by(year) %>%
  summarise(value = mean(PM2.5, na.rm = TRUE))
sionMaxYearPM2.5 <- max(sionMeanYearPM2.5$value)

magadinoMeanYearNO2 <- magadinoData %>% 
  group_by(year) %>%
  summarise(value = mean(NO2, na.rm = TRUE))
magadinoMaxYearNO2 <- max(magadinoMeanYearNO2$value)

magadinoMeanYearPM10 <- magadinoData %>% 
  group_by(year) %>%
  summarise(value = mean(PM10, na.rm = TRUE))
magadinoMaxYearPM10 <- max(magadinoMeanYearPM10$value)

magadinoMeanYearPM2.5 <- magadinoData %>% 
  group_by(year) %>%
  summarise(value = mean(PM2.5, na.rm = TRUE))
magadinoMaxYearPM2.5 <- max(magadinoMeanYearPM2.5$value)

magadinoMeanYearSO2 <- magadinoData %>% 
  group_by(year) %>%
  summarise(value = mean(SO2, na.rm = TRUE))
magadinoMaxYearSO2 <- max(magadinoMeanYearSO2$value)

#### Problem 2b ####
length(which(sionData$PM10 > 50))
length(which(sionData$O3 > 120))
length(which(magadinoData$PM10 > 50))
length(which(magadinoData$O3 > 120))
##### Problem 3 #####
## Seasonal variations of pollutant per site with histogram
# Creating data frame for P3
dataP3 <- combinedData %>%
  gather(variable, value, -c(site, date, datetime, season, year, month, day, hour, dayofwk, daytype, season))
# Creating the plot for P3
plotP3 <- dataP3 %>%
  filter(!is.na(value)) %>%
  ggplot +
  facet_grid(variable ~ season, scale = "free_y") +
  geom_boxplot(aes(site, value), outlier.size = 0.5, outlier.shape = 3)
plotP3
png(plotP3, "plotP3.png")


##### Problem 4 #####
setdiff(magadinoData$PM10, magadinoData$PM10bis)
any(is.na(magadinoData$PM10bis))

length(setdiff(as.vector(magadinoData$PM10), as.vector(magadinoData$PM10bis)))


##### Problem 5 #####


##### Problem 6 #####


##### Problem 7 #####


##### Problem 8 #####

