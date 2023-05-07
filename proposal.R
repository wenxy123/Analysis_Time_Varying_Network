library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(chron)
library(ggmap)

setwd('D:/桌面的数据/RUC_class/2_sem1/机器学习/final_project/data')
case_df <- read.csv('NYC_cases_by_day.csv',header=T)
traffic4 <- read.csv("yellowTripdata2020/yellow_tripdata_2020-04.csv",header=T)

str(case_df,list.len=10)

case_df$date <- as.Date(case_df$date_of_interest,format = "%m/%d/%Y")

Sys.setlocale("LC_TIME","English")

p1 <- ggplot(case_df, aes(x=date, y=CASE_COUNT)) +
  geom_line( color="steelblue") + 
  geom_point() +
  xlab("") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2020-02-29"),as.Date("2021-01-01"))) +
  ylim(0,max(case_df$CASE_COUNT))

p1

p2 <- ggplot(case_df, aes(x=date, y=DEATH_COUNT)) +
  geom_line( color="steelblue") + 
  geom_point() +
  xlab("") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2020-02-29"),as.Date("2021-01-01"))) +
  ylim(0,max(case_df$DEATH_COUNT))

p2

traffic4[c('date', 'time')] <- str_split_fixed(traffic4$tpep_pickup_datetime, ' ', 2)

count_traffic <- traffic4 %>% group_by(date) %>% 
  summarise(total_count=n(),
            .groups = 'drop')
count_traffic

# Convert tibble to df
count_traffic2 <- count_traffic %>% as.data.frame()
count_traffic2 <- count_traffic2[12:41,]
count_traffic2$date <- as.Date(count_traffic2$date)
count_traffic2$day_type <- as.factor(is.weekend(count_traffic2$date))

p3 <- ggplot(data=count_traffic2, aes(x=date, y=total_count,fill = day_type)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2020-04-01"),as.Date("2020-04-30")))+
  theme_classic()+
  scale_fill_discrete(labels=c('workday', 'weekend'))
p3

sel_case <- case_df[33:62, ]

new_df <- merge(sel_case,count_traffic2,by='date')       

p4 <- ggplot(new_df, aes(x=total_count, y=CASE_COUNT, color=day_type)) + 
  geom_point(size=3) +
  theme_classic()+
  scale_color_discrete(labels=c('workday', 'weekend'))+ 
  theme(legend.position = "top")

p5 <- ggplot(new_df, aes(x=total_count, y=DEATH_COUNT, color=day_type)) + 
  geom_point(size=3) +
  theme_classic()+
  scale_color_discrete(labels=c('workday', 'weekend'))+ 
  theme(legend.position="top")

g1 <- ggpubr::ggarrange(p4, p5, nrow = 1, ncol = 2)
g1

#match area name
traffic4_01 <- traffic4[traffic4$date=='2020-04-12',]
area_name <- read.csv("D:/桌面的数据/RUC_class/2_sem2/advanced_applied_statistics/taxi_zone.csv")
df1 <- merge(traffic4_01, area_name, by.x = c("PULocationID"), by.y = c("LocationID"), 
      all.x = TRUE, all.y = TRUE)
df1$PUZone <- df1$Zone
df2 <- merge(df1, area_name, by.x = c("DOLocationID"), by.y = c("LocationID"), 
             all.x = TRUE, all.y = TRUE)
df2$DOZone <- df2$Zone.y
write.csv(df2,'D:/my_data/traffic_0412.csv')

neighborID <- c(4, 12, 13, 24, 41, 42, 43, 45, 48, 50, 
                68, 74, 75, 79, 87, 88, 90, 100, 103, 104, 
                105, 107, 113, 114, 116, 120, 125, 127, 128, 137, 
                140, 141, 142, 143, 144, 148, 151, 152, 153, 158, 
                161, 162, 163, 164, 166, 170, 186, 194, 202, 209, 
                211, 224, 229, 230, 231, 232, 233, 234, 236, 237, 
                238, 239, 243, 244, 246, 249, 261, 262, 263)
neighborName <- c("Alphabet City", "Battery Park", "Battery Park City", 
                  "Bloomingdale", "Central Harlem", "Central Harlem North", 
                  "Central Park", "Chinatown", "Clinton East", 
                  "Clinton West", "East Chelsea", "East Harlem North", 
                  "East Harlem South", "East Village", "Financial District North", 
                  "Financial District South", "Flatiron", "Garment District", 
                  "Liberty Island", "Ellis Island", "Governor's Island", 
                  "Gramercy", "Greenwich Village North", "Greenwich Village South", 
                  "Hamilton Heights", "Highbridge Park", "Hudson Sq", 
                  "Inwood", "Inwood Hill Park", "Kips Bay", 
                  "Lenox Hill East", "Lenox Hill West", "Lincoln Square East", 
                  "Lincoln Square West", "Little Italy/NoLiTa", "Lower East Side", 
                  "Manhattan Valley", "Manhattanville", "Marble Hill", 
                  "Meatpacking/West Village West", "Midtown Center", "Midtown East", 
                  "Midtown North", "Midtown South", "Morningside Heights", 
                  "Murray Hill", "Penn Station/Madison Sq West", "Randalls Island", 
                  "Roosevelt Island", "Seaport", "SoHo", 
                  "Stuy Town/Peter Cooper Village", "Sutton Place/Turtle Bay North", "Times Sq/Theatre District", 
                  "TriBeCa/Civic Center", "Two Bridges/Seward Park", "UN/Turtle Bay South", 
                  "Union Sq", "Upper East Side North", "Upper East Side South", 
                  "Upper West Side North", "Upper West Side South", "Washington Heights North", 
                  "Washington Heights South", "West Chelsea/Hudson Yards", "West Village", 
                  "World Trade Center", "Yorkville East", "Yorkville West")
names(neighborID) <- neighborName
neighborID <- neighborID[!is.element(neighborID, 103:105)]# exclude the islands

zoneID <- list(c(12, 13, 87, 88, 209, 231, 261), 
               c(113, 114, 125, 144, 158, 211, 249), 
               c(4, 45, 79, 148, 232), 
               c(48, 50, 68, 90, 246), 
               c(100, 161, 163, 164, 186, 230, 234),
               c(107, 137, 162, 170, 224, 229, 233), 
               c(24, 142, 143, 151, 238, 239), 
               c(140, 141, 202, 236, 237, 262, 263), 
               c(116, 152, 166), 
               c(41, 42), 
               c(74, 75, 194), 
               c(120, 127, 128, 153, 243, 244), 
               c(43))# based on community districts
m <- length(zoneID)
names(zoneID) <- c(101:112, 164)

traffic4 <- traffic4 %>% 
  filter(!is.na(passenger_count))%>%
  filter((PULocationID %in% neighborID) & (DOLocationID %in% neighborID)) %>% 
  mutate(date = substr(tpep_pickup_datetime, start = 1, stop = 10)) %>%
  select(date, PULocationID, DOLocationID, weight = passenger_count)

traffic4$PUAreaID <- apply(sapply(zoneID, function(zoneIDi) is.element(traffic4$PULocationID, zoneIDi)), 1, which)
traffic4$DOAreaID <- apply(sapply(zoneID, function(zoneIDi) is.element(traffic4$DOLocationID, zoneIDi)), 1, which)

traffic4_01 <- traffic4[traffic4$date=='2020-04-01',]
write.csv(traffic4_01,'D:/my_data/traffic_0401.csv')

library(igraph)
library(networkD3)

# create a dataset:
net_df <- tibble::data_frame(
  from=traffic4_01$PUAreaID,
  to=traffic4_01$DOAreaID
)

# Plot
p <- simpleNetwork(net_df, height="100px", width="100px",        
                   Source = 1,                 # column number of source
                   Target = 2,                 # column number of target
                   linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                   charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 14,               # size of the node names
                   fontFamily = "serif",       # font og node names
                   linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                   nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   zoom = T                    # Can you zoom on the figure?
)

p

network <- graph_from_data_frame(d=net_df, directed=F) 

# Count the number of degree for each node:
deg <- degree(network, mode="all")

# Plot
plot(network, vertex.size=deg*0.005, vertex.color=rgb(0.1,0.7,0.8,0.5) )

