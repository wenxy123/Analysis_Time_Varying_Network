# Analysis_Time_Varying_Network

## Background
The development of social science, biology and economics poses challenges to the research of network data. Identifying the correlation between two sets of graphs or random graphs and their attributes and testing for independence is crucial in many random graph analyses. Calculating the correlation coefficient between two random graphs can provide more information and help us make statistical inferences.

## Data Source
The yellow and green taxi trip records on pick-up and drop-off dates/times, pick-up and drop-off locations, trip distances, itemized fares, rate types, payment types and driver-reported passenger counts, collected by New York City Taxi and Limousine Commission(NYC TLC), are publicly available at https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page. Additionally, NYC Coronavirus Disease 2019 (COVID-19) data are available at https://github.com/nychealth/coronavirus-data, where one can find citywide and borough-specific daily counts of probable and confirmed COVID-19 cases in New York City since February 29, 2020. This is the date at which according to the Health Department the COVID-19 outbreak in NYC began.

## Aim of Our Work
In this project, we want to explore how the COVID-19 cases will effect the New York yellow taxi traffic flow. Specifically, we mainly focus on the following three goals.

-Establish a traffic flow network of yellow taxis in New York and analyze the structure of the network, including node and path analysis.
-Reduce the dimensionality of the transportation network based on the spectral decomposition of the adjacency matrix, and obtain the low-dimensional vector representation of each node.
-Construct an effective model to analyze the time series network and the correlation of node attributes and determine the traffic nodes most affected by the COVID-19.
