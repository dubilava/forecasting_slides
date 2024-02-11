library(data.table)
library(ggplot2)
library(readrba)


check_dt <- as.data.table(read_rba(table_no = "g1"))

check_dt[,.N,by=.(series)]


unique(as.data.table(browse_rba_tables())$title)


actual_dt <- check_dt[series=="Year-ended inflation"]

actual_dt[,date:=as.Date(paste0(substr(date,1,7),"-01"))]

ggplot(actual_dt[date>="1990-01-01"],aes(x=date,y=value)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Year-ended inflation")



checkfor_dt <- as.data.table(rba_forecasts())
checkfor_dt[,.N,by=.(series)]

forecast_dt <- checkfor_dt[series=="cpi_annual_inflation"] # underlying_annual_inflation


forecast_dt[,forecast_horizon:=as.numeric(date-forecast_date)]

ggplot(forecast_dt[date>="2019-01-01" & forecast_horizon>0]) +
  geom_line(data=actual_dt[date>="2019-01-01"],aes(x=date,y=value),color="dimgray",linewidth=1)+
  geom_line(aes(x=date,y=value,group=forecast_date,color=forecast_horizon),linewidth=.8) +
  scale_color_viridis(option="B",begin=.2,end=.8)+
  theme_minimal() +
  labs(title = "Annual inflation: actual and forecasts")
