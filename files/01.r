library(data.table)
library(ggplot2)
library(readrba)
library(viridis)

checkact_dt <- as.data.table(read_rba(table_no = "g1"))

actual_dt <- checkact_dt[series=="Year-ended inflation"]

actual_dt[,date:=as.Date(paste0(substr(date,1,7),"-01"))]

checkfor_dt <- as.data.table(rba_forecasts())
checkfor_dt[,.N,by=.(series)]

forecast_dt <- checkfor_dt[series=="cpi_annual_inflation"] # underlying_annual_inflation

forecast_dt[,forecast_horizon:=as.numeric(date-forecast_date)]

gg <- ggplot(forecast_dt[date>="2019-01-01" & forecast_horizon>0]) +
  geom_line(data=actual_dt[date>="2019-01-01"],aes(x=date,y=value),color="black",linewidth=.8)+
  geom_line(aes(x=date,y=value,group=forecast_date,color=forecast_horizon),linewidth=.6) +
  scale_color_viridis(option="B",begin=.2,end=.8)+
  theme_minimal() +
  labs(subtitle="Annual inflation (%): actual and forecasts",y="",x="Year")+
  theme(legend.title=element_text(size=12,vjust=0),legend.text=element_text(size=10,hjust=0.5,vjust=0),legend.position="bottom",legend.key.height=unit(.1,"in"),legend.key.width=unit(.5,"in"),panel.background=element_rect(fill="white",color=NA),plot.background=element_rect(fill="white",color=NA),plot.title.position="plot",axis.title=element_text(size=12),plot.subtitle=element_text(size=12),axis.text=element_text(size=10))+
  guides(color=guide_colorbar(title="Forecast horizon (days)",title.position="left",label.position="bottom",nrow=1))

ggsave("figures/lecture1/inflation.png",gg,width=6.5,height=6.5*9/16,dpi="retina")


