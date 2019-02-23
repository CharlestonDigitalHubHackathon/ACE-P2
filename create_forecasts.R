##based off of 

library('ggplot2')
library('forecast')
library('tseries')
library('car')

### Inputs:
##### roll_up_data.csv
##### country_up_data.csv
### Outputs:
#### rollup_with_forecast (dataFrame: Country, Category_Rollup, Year, KWH, CO2)
#### country_with_forecast (dataFrame: Country, Year, KWH, CO2)

#filepath


### Build a Country, Category Rollup, Year, KWH, CO2 dataset (wiht 10y forecast)
to_category_rollup = read.csv("~/Desktop/hackathon/cleaned_rollup_data.csv", stringsAsFactors=FALSE)
to_category_rollup <- to_category_rollup[order(to_category_rollup$Country,to_category_rollup$Category_Rollup,to_category_rollup$Year),]

# Deterimne Values to List
list_of_countires = unique(to_category_rollup$Country)
list_of_category_rollup = unique(to_category_rollup$Category_Rollup)


rollup_with_forecasts = to_category_rollup
### run ARIMA model
for(country in list_of_countires){
  for(rollup in list_of_category_rollup){
    kwh = to_category_rollup$KWH[which(to_category_rollup$Country==country & to_category_rollup$Category_Rollup==rollup)]
    co2 = to_category_rollup$CO2[which(to_category_rollup$Country==country & to_category_rollup$Category_Rollup==rollup)]
    if(length(kwh)> 5) {
      kwh_arima_fit = arima(kwh, order=c(1,1,1),method="ML")
      kwh_fcast <- forecast(kwh_arima_fit, h=10)
      co2_arima_fit = arima(co2, order=c(1,1,1), method="ML")
      co2_fcast <- forecast(co2_arima_fit, h=10)
      Temp.Data<-data.frame(matrix(nrow = 10,ncol = 5))
      colnames(Temp.Data)<-colnames(to_category_rollup)
      Temp.Data$Country<-country
      Temp.Data$Category_Rollup<-rollup
      Temp.Data$KWH<-as.numeric(kwh_fcast$mean)
      Temp.Data$CO2<-as.numeric(co2_fcast$mean)
      Temp.Data$Year<-seq(2015,2024)
      rollup_with_forecasts = rbind(rollup_with_forecasts, Temp.Data)      
    }
  }
}
rollup_with_forecasts <- rollup_with_forecasts[order(rollup_with_forecasts$Country,rollup_with_forecasts$Category_Rollup,rollup_with_forecasts$Year),]

### Build a Country, Year, KWH, CO2 dataset (wiht 10y forecast)
### Build a Country, Category Rollup, Year, KWH, CO2 dataset (wiht 10y forecast)
to_country = read.csv("~/Desktop/hackathon/cleaned_totals_data.csv", stringsAsFactors=FALSE)
to_country <- to_country[order(to_country$Country,to_country$Year),]

# Deterimne Values to List
list_of_countires = unique(to_country$Country)


country_with_forecasts = to_country
### run ARIMA model
for(country in list_of_countires){
    kwh = to_country$KWH[which(to_country$Country==country )]
    co2 = to_country$CO2[which(to_country$Country==country )]
    if(length(kwh)> 5) {
      kwh_arima_fit = arima(kwh, order=c(1,1,1), method="ML")
      kwh_fcast <- forecast(kwh_arima_fit, h=10)
      co2_arima_fit = arima(co2, order=c(1,1,1), method="ML")
      co2_fcast <- forecast(co2_arima_fit, h=10)
      Temp.Data<-data.frame(matrix(nrow = 10,ncol = 4))
      colnames(Temp.Data)<-colnames(to_country)
      Temp.Data$Country<-country
      Temp.Data$KWH<-as.numeric(kwh_fcast$mean)
      Temp.Data$CO2<-as.numeric(co2_fcast$mean)
      Temp.Data$Year<-seq(2015,2024)
      country_with_forecasts = rbind(country_with_forecasts, Temp.Data)      
  }
}

country_with_forecasts <- country_with_forecasts[order(country_with_forecasts$Country,country_with_forecasts$Year),]

country_with_forecasts$Category_Rollup = "Total"

country_and_rollup_with_forecast= rbind(country_with_forecasts,rollup_with_forecasts)
write.csv(country_and_rollup_with_forecast, "~/Desktop/hackathon/country_and_rollup_with_forecast.csv")
