#load the data
library(plotly)


Data<-read.csv("country_and_rollup_with_forecast.csv",header = T)

colnames(Data)[2]<-"COUNTRY"
colnames(Data)[3]<-"YEAR"
colnames(Data)[6]<-"CATEGORY_ROLLUP"

