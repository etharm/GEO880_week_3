library(readr)
library(dplyr)        
library(ggplot2)      
library(sf)           
library(terra)        
library(lubridate)    
library(tidyr)
#-------------------------------------------------------------------------------------------------------------------------

#import data
caro60 <- read_delim("caro60.csv",",")

# modify time format of Column DateTimeUTC
caro60$DateTimeUTC <- format(as.POSIXct(strptime(caro60$DatetimeUTC, "%d.%m.%Y %H:%M", tz="UTC")), format = "%Y-%m-%d %H:%M")
caro60$DateTimeUTC <- ymd_hm(caro60$DateTimeUTC)

#-------------------------------------------------------------------------------------------------------------------------

#TASK 1: SEGMENTATION

#Example: sampling interval 15min and temporal window 60min => 4 fixes
#Now: sampling interval 1min and temporal window  6min => 6 fixes

caro60$PosM3 <- sqrt((lag(caro60$E,3)-caro60$E)^2+(lag(caro60$N,3)-caro60$N)^2) # dist Minus 3
caro60$PosM2 <- sqrt((lag(caro60$E,2)-caro60$E)^2+(lag(caro60$N,2)-caro60$N)^2) # dist Minus 2
caro60$PosM1 <- sqrt((lag(caro60$E,1)-caro60$E)^2+(lag(caro60$N,1)-caro60$N)^2) # dist Minus 1
caro60$PosP1 <- sqrt((caro60$E-lead(caro60$E,1))^2+(caro60$N-lead(caro60$N,1))^2) # dist Plus 1
caro60$PosP2 <- sqrt((caro60$E-lead(caro60$E,2))^2+(caro60$N-lead(caro60$N,2))^2) # dist Plus 2
caro60$PosP3 <- sqrt((caro60$E-lead(caro60$E,3))^2+(caro60$N-lead(caro60$N,3))^2) # dist Plus 3

#-------------------------------------------------------------------------------------------------------------------------

#TASK 2: SPECIFY AND APPLY THRESHOLD d

#Adding new column with mean values
caro60 <- caro60 %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(PosM3, PosM2, PosM1, PosP1, PosP2, PosP3), na.rm=TRUE) #na.rm to get mean values for first and last 3 rows
  ) %>%
  ungroup() 

#Histogram

ggplot(caro60) + geom_histogram(mapping = aes(stepMean))

#Threshold (below mean of stepMean => static=True)

caro60$static <- caro60$stepMean < mean(caro60$stepMean)

#-------------------------------------------------------------------------------------------------------------------------

