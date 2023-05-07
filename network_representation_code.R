library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(igraph)
library(chron)
library(reshape2)
library(plot.matrix)
library(hrbrthemes)
library(stringr)
library(chron)
library(ggmap)
library(frechet)
library(RColorBrewer)
library(ggpubr)

Sys.setlocale("LC_TIME","English")

setwd('.../final_project/data')
case_df <- read.csv('NYC_cases_by_day.csv',header=T)
case_df$date <- as.Date(case_df$date_of_interest,format = "%m/%d/%Y")

area_name <- read.csv("taxi_zone.csv")

sel_area <- area_name[area_name$Borough=='Manhattan',]
locationID <- sel_area$LocationID 
m <- length(locationID)
names(locationID) <- c(1:m)

taxi <- list()

for(i in 4:8){
  taxi[[i]] <- read.csv(paste0(".../final_project/data/yellowTripdata2020//yellow_tripdata_2020", '-', paste0('0', i), '.csv'))
}

startDate <- '2020-04-01'
endDate <- '2020-09-01'

for(i in 4:8){
  taxi[[i]] <- taxi[[i]] %>% 
    drop_na(c(tpep_pickup_datetime, PULocationID, DOLocationID, passenger_count)) %>% # remove rows with missing values
    filter((PULocationID %in% locationID) & (DOLocationID %in% locationID)) %>% 
    mutate(date = substr(tpep_pickup_datetime, start = 1, stop = 10)) %>%
    filter((date > startDate) & (date < endDate)) %>%
    select(date, PULocationID, DOLocationID, weight = passenger_count)
}

taxi <- bind_rows(taxi[[4]], taxi[[5]], taxi[[6]], taxi[[7]], taxi[[8]])
taxi$PULocationID <- apply(sapply(locationID, function(locationIDi) is.element(taxi$PULocationID, locationIDi)), 1, which)
taxi$DOLocationID <- apply(sapply(locationID, function(locationIDi) is.element(taxi$DOLocationID, locationIDi)), 1, which)
taxi <- taxi %>% filter(date >= startDate & date < endDate)
taxi$date <- as.Date(taxi$date)
taxi$day_type <- as.factor(is.weekend(taxi$date))

taxi1 <- taxi[taxi$day_type=='TRUE',]
taxi2 <- taxi[taxi$day_type=='FALSE',]

taxigl1 <- taxi1 %>% dlply(.(date), function(taxi1) {
  g <- graph_from_data_frame(d = taxi1 %>% dplyr::select(PULocationID, DOLocationID, weight),
                             vertices = 1:m, directed = FALSE)
  g <- simplify(g)# returns a simple graph (remove self-loops and combine multi-edges)
  gl <- graph.laplacian(g, sparse = FALSE)
  dimnames(gl) <- list(names(locationID), names(locationID))
  gl
})

taxigl2 <- taxi2 %>% dlply(.(date), function(taxi2) {
  g <- graph_from_data_frame(d = taxi2 %>% dplyr::select(PULocationID, DOLocationID, weight),
                             vertices = 1:m, directed = FALSE)
  g <- simplify(g)# returns a simple graph (remove self-loops and combine multi-edges)
  gl <- graph.laplacian(g, sparse = FALSE)
  dimnames(gl) <- list(names(locationID), names(locationID))
  gl
})

Fmean1 <- CovFMean(M=taxigl1,optns=list(metric="frobenius"))
Fmean2 <- CovFMean(M=taxigl2,optns=list(metric="frobenius"))

var1<-NA
for (i in 1:length(taxigl1)){
  mat1 <- matrix(taxigl1[[i]],nrow=m,ncol=m)
  mat2 <- matrix(Fmean1$Mout[[1]],nrow=m,ncol=m)
  mat3 <- mat1-mat2
  var1[i] <- norm(mat3,'F')
}

var_df1 <- data.frame(date=as.Date(names(taxigl1)),value=var1,day_type='Weekend')

var2<-NA
for (i in 1:length(taxigl2)){
  mat1 <- matrix(taxigl2[[i]],nrow=m,ncol=m)
  mat2 <- matrix(Fmean2$Mout[[1]],nrow=m,ncol=m)
  mat3 <- mat1-mat2
  var2[i] <- norm(mat3,'F')
}
var_df2 <- data.frame(date=as.Date(names(taxigl2)),value=var2,day_type='Weekday')
var_df <- rbind(var_df1,var_df2)
scale <- max(var_df$value)/max(var_df$MN_CASE_COUNT_7DAY_AVG)

p1 <- ggplot(var_df, aes(x=date, y=value,color = day_type)) + geom_point() +
  geom_smooth(method = "gam", formula = y ~ poly(x, 5))+
  ggtitle("Manhattan")
p1

case_df$date_of_interest <- as.Date(case_df$date_of_interest, format = "%m/%d/%Y")
case_df1 <- case_df[case_df$date_of_interest >= startDate & case_df$date_of_interest < endDate, c('date_of_interest', 'CASE_COUNT','MN_CASE_COUNT_7DAY_AVG')]
case_df1$date <- case_df1$date_of_interest

var_df <- merge(var_df,case_df1,by='date')

p2 <- ggplot(var_df, aes(x=value, y=MN_CASE_COUNT_7DAY_AVG,color = day_type)) + geom_point() +
  ggtitle("Staten Island")+
  stat_cor(method = "pearson",)
p2



