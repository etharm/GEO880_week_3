library(readr)
library(dplyr)        
library(ggplot2)      
library(sf)           
library(terra)        
library(lubridate)    
library(tidyr)
library(SimilarityMeasures)
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
    stepMean = mean(c(PosM3, PosM2, PosM1, PosP1, PosP2, PosP3))
  ) %>%
  ungroup() 

#Histogram

ggplot(caro60) + geom_histogram(mapping = aes(stepMean))

#Threshold (above mean of stepMean => static=True)

caro60$static <- caro60$stepMean < mean(caro60$stepMean, na.rm=TRUE)

#-------------------------------------------------------------------------------------------------------------------------

#TASK 3: VISUALIZE SEGMENTED TRAJECTORIES

caro60%>%
  ggplot(aes(E, N))  +
  geom_point(aes(colour = static)) +
  geom_path() +
  theme(legend.position = "right") +
  coord_equal()

#-------------------------------------------------------------------------------------------------------------------------

#TASK 4: SEGMENT-BASED ANALYSIS

# it assigns unique IDs based on the column static

rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

# all following TRUE same ID until first FALSE. New ID for following FALSE

caro60 <- caro60%>%
  mutate(segment_ID = rle_id(static))

# Plot with just moving segments
  
caro60%>%
  filter(static == FALSE) %>% #Filter out non-moving segments
  ggplot(aes(E, N))  +
  geom_point(aes(colour = segment_ID)) +
  geom_path(aes(colour = segment_ID)) +
  theme(legend.position = "none") +
  ggtitle("All moving segments")
  coord_equal()

# Plot with just long moving segments(>=5)

caro60$lengths <- sequence(rle(as.character(caro60$static))$lengths) # 1,2,3.. for each column as long as static FALSE or TRUE

Stats <- caro60 %>% group_by(segment_ID) %>% #calculate sum of lengths for each segment_ID
  summarize(longSeg = sum(lengths))

caro60 <- merge(caro60, Stats)


caro60%>%
  filter(static == FALSE & longSeg > 10) %>% #Filter out non-moving segments and short moving segments 
  ggplot(aes(E, N))  +
  geom_point(aes(colour = segment_ID)) +
  geom_path(aes(colour = segment_ID)) +
  theme(legend.position = "none") +
  ggtitle("Long segments (>=5)") +
  coord_equal()

#IF longSeg > 10 not enough segments are removed.The task was to remove segments <5. Therefore segments, which are only represented with 4 or less
# rows in caro60 will be removed. Segments with 4 or less rows have a sum of 10 or less. (1+2+3+4)

#-------------------------------------------------------------------------------------------------------------------------

#TASK 5: SIMILARITY MEASUREMENTS

pedestrian <- read_delim("pedestrian.csv",",")


pedestrian$TrajID<-as.factor(pedestrian$TrajID)
pedestrian$TrajID2<-pedestrian$TrajID


ggplot(data=pedestrian, aes(E, N)) +
geom_point(data=pedestrian[,2:5], color="grey")+
geom_point(aes(alpha=TrajID))+
geom_point(aes(colour=as.factor(TrajID))) +
facet_wrap(~TrajID, labeller = labeller(TrajID = c("1" = "TrajID: 1", "2" = "TrajID: 2", "3" = "TrajID: 3", "4" = "TrajID: 4", "5" = "TrajID: 5", "6" = "TrajID: 6"))) +
geom_path(aes(color=TrajID))+                 
theme(legend.position = "none") +
labs(title = "Visual comparison of the 6 trajectories", subtitle = "Each subplot highlights a trajectory") +
coord_equal()
 
#-------------------------------------------------------------------------------------------------------------------------

#TASK 6: CALCULATE SIMILARITY

# Numeric matrix with 3 colums (TrajID,E,N)

ped1 <- data.matrix(filter(pedestrian[,1:3], TrajID==1))
ped2 <- data.matrix(filter(pedestrian[,1:3], TrajID==2))
ped3 <- data.matrix(filter(pedestrian[,1:3], TrajID==3))
ped4 <- data.matrix(filter(pedestrian[,1:3], TrajID==4))
ped5 <- data.matrix(filter(pedestrian[,1:3], TrajID==5))
ped6 <- data.matrix(filter(pedestrian[,1:3], TrajID==6))

#DTW

DTW1 <- DTW(ped1,ped1)
DTW2 <- DTW(ped1,ped2)
DTW3 <- DTW(ped1,ped3)
DTW4 <- DTW(ped1,ped4)
DTW5 <- DTW(ped1,ped5)
DTW6 <- DTW(ped1,ped6)

#EditDist

editDist1 <- EditDist(ped1,ped1)
editDist2 <- EditDist(ped1,ped2)
editDist3 <- EditDist(ped1,ped3)
editDist4 <- EditDist(ped1,ped4)
editDist5 <- EditDist(ped1,ped5)
editDist6 <- EditDist(ped1,ped6)

#Frechet

frechet1 <- Frechet(ped1,ped1)
frechet2 <- Frechet(ped1,ped2)
frechet3 <- Frechet(ped1,ped3) #Error: The Frechet distance was unable to be found
frechet4 <- Frechet(ped1,ped4)
frechet5 <- Frechet(ped1,ped5)
frechet6 <- Frechet(ped1,ped6)

#LCSS

LCSS1 <- LCSS(ped1,ped1, 2,2,0.5)
LCSS2 <- LCSS(ped1,ped2, 2,2,0.5)
LCSS3 <- LCSS(ped1,ped3, 2,2,0.5)
LCSS4 <- LCSS(ped1,ped4, 2,2,0.5)
LCSS5 <- LCSS(ped1,ped5, 2,2,0.5)
LCSS6 <- LCSS(ped1,ped6, 2,2,0.5)


#Combinte to dataframe

dtw <- c(DTW1,DTW2,DTW3,DTW4,DTW5,DTW6)
editDist <- c(editDist1,editDist2,editDist3,editDist4,editDist5,editDist6)
frechet <- c(frechet1,frechet2,frechet3,frechet4,frechet5,frechet6)
lcss <- c(LCSS1,LCSS2,LCSS3,LCSS4,LCSS5,LCSS6)

similarity.df <- data.frame(dtw,editDist,frechet,lcss)
similarity.df$tj <- c(1,2,3,4,5,6)

#Plot

require(gridExtra)

pl1<- ggplot(data=similarity.df, aes(tj, dtw)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  ggtitle("DTW")

pl2 <- ggplot(data=similarity.df, aes(tj, editDist)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  ggtitle("editDist")

pl3 <- ggplot(data=similarity.df, aes(tj, frechet)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  ggtitle("Frechet")

pl4 <- ggplot(data=similarity.df, aes(tj, lcss)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  ggtitle("LCSS")

grid.arrange(pl1,pl2,pl3,pl4, ncol=2, nrow=2)

# DTW: DTW is 0 if both axis represent same trajectory. Therefore I guess trajectory 3 is most different from trajectory 1.
# editDist: 0 edits needed if both trajectories are equal. Trajectory 3 needs most edit to look like trajectory 1.
# Frechet: #Error: The Frechet distance was unable to be found. Hard to interpret.
# LCSS: It makes sense that LCSS value is high, when plotting two identical trajectories (Two people doing gymnastics, where
# one copies every movement exacty in time and space. LCSS values will increase for other trajectories, if larger buffers for deviations 
#in space and time are chosen)
