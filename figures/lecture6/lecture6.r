# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
# library(camcorder)
library(extrafont)
# font_import()
loadfonts(device="win",quiet=T)
library(cowplot)
library(crypto2)
library(fredr)
fredr_set_key("7a1db535f59c2ac4382b9c22a15b5f06")
library(stringr)
library(readabs)
Sys.setenv(R_READABS_PATH = "figures/lecture5")
library(lmtest)
library(sandwich)

library(fastDummies)

all_wpi <- read_abs("6345.0")

# # camcorder stuff
# camcorder::gg_record(
#   dir='figures/lecture4',
#   width=6.5,
#   height=6.5*9/16,
#   dpi=300,
#   bg="white"
# )

# plot aesthetics
theme_eg <- function(base_size=12,base_family="Segoe Print",border=F){
  theme(
    panel.background=element_rect(fill="white",color=NA),
    panel.grid=element_line(colour=NULL,linetype=3,linewidth=.3),
    panel.grid.major=element_line(colour="dimgray"),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill="white",color=NA),
    plot.title=element_text(family=base_family,size=rel(1.2),colour="dimgray"),
    plot.caption=element_text(family=base_family,colour="darkgray"),
    plot.margin=margin(.25,.25,.25,.25,"lines"),
    axis.title=element_text(family=base_family,face="bold",size=rel(1.3),colour="dimgray"),
    axis.text=element_text(family=base_family,size=rel(1.1),colour="dimgray",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    legend.background=element_rect(fill="transparent",color=NA),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(family=base_family,size=rel(1.1),colour="dimgray"),
    legend.key.size=unit(.75,'lines'),
    strip.background=element_blank(),
    strip.text=element_text(family=base_family,size=rel(0.9),colour="dimgray",face="bold",margin=margin(.5,0,.5,0,"lines"))
  )
}




unrate_dt <- data.table(fredr(series_id="UNRATE",observation_start=as.Date("1980-01-01"),observation_end=as.Date("2019-12-31"),frequency="m",units="lin"))

unratensa_dt <- data.table(fredr(series_id="UNRATENSA",observation_start=as.Date("1980-01-01"),observation_end=as.Date("2019-12-31"),frequency="m",units="lin"))

ur_dt <- rbind(unrate_dt,unratensa_dt)
ur_dt$series_id <- factor(ur_dt$series_id,levels=c("UNRATENSA","UNRATE"),labels=c("not seasonally adjusted","seasonally adjusted"))

gg_ur <- ggplot(ur_dt,aes(x=date,y=value,color=series_id,linetype=series_id))+
  geom_line(linewidth=.5,na.rm=T)+
  scale_color_manual(values=c("dimgray","coral"))+
  scale_linetype_manual(values=c(1,5))+
  scale_y_continuous(breaks=c(2,4,6,8,10,12))+
  labs(x="Year",y="Unemployment rate (%)")+
  coord_cartesian(ylim=c(2,12),xlim=c(as.Date("1980-01-01"),as.Date("2019-12-31")))+
  theme_eg()+
  theme(legend.position="top",legend.key=element_rect(fill="transparent"),legend.key.width=unit(.4,"in"))

ggsave("figures/lecture6/unemployment_rates.png",gg_ur,width=6.5,height=6.5*9/16,dpi="retina",device="png")



natgas_dt <- data.table(fredr(series_id="NATURALGAS",observation_start=as.Date("2000-01-01"),observation_end=as.Date("2010-12-31"),frequency="m",units="lin"))

gg_ng <- ggplot(natgas_dt,aes(x=date,y=value))+
  geom_line(linewidth=.5,na.rm=T,color="dimgray")+
  labs(x="Year",y="Billion Cubic Feet")+
  coord_cartesian(ylim=c(1000,3000),xlim=c(as.Date("2000-01-01"),as.Date("2010-12-31")))+
  theme_eg()

ggsave("figures/lecture6/natgas.png",gg_ng,width=6.5,height=6.5*9/16,dpi="retina",device="png")



natgas_dt$month <- month(natgas_dt$date)
natgas_dum <- dummy_cols(natgas_dt$month,remove_selected_columns=T)
colnames(natgas_dum) <- paste0("d",c(1:12))

natgas_dt <- cbind(natgas_dt,natgas_dum)

est <- lm(value~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12-1,data=natgas_dt)

natgas_dt$value_fit <- fitted(est)

natgas_lg <- melt(natgas_dt[,.(date,value,value_fit)],id.vars="date")
natgas_lg$variable <- factor(natgas_lg$variable,levels=c("value","value_fit"),labels=c("observed data","fitted data"))

gg_ngfit <- ggplot(natgas_lg,aes(x=date,y=value,color=variable,linetype=variable))+
  geom_line(linewidth=.5,na.rm=T)+
  scale_color_manual(values=c("dimgray","coral"))+
  scale_linetype_manual(values=c(1,5))+
  labs(x="Year",y="Billion Cubic Feet")+
  coord_cartesian(ylim=c(1000,3000),xlim=c(as.Date("2000-01-01"),as.Date("2010-12-31")))+
  theme_eg()

ggsave("figures/lecture6/natgasfit.png",gg_ngfit,width=6.5,height=6.5*9/16,dpi="retina",device="png")



est <- lm(value~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11,data=natgas_dt[date<=as.Date("2006-12-31")])
natgas_dt[,`:=`(value_f=value,value_s=est$coefficients[1]+as.matrix(natgas_dt[,.(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11)])%*%as.matrix(est$coefficients[-1]))]

natgas_dt[date<=as.Date("2006-12-31")]$value_f <- NA
natgas_dt[date<=as.Date("2006-12-31")]$value_s <- NA

natgas_dt[,`:=`(value_lo=value_s-1.96*summary(est)$sigma,value_hi=value_s+1.96*summary(est)$sigma)]

gg_ngfor <- ggplot(natgas_dt,aes(x=date))+
  geom_ribbon(aes(ymin=value_lo,ymax=value_hi),fill="coral",alpha=.2)+
  geom_line(aes(y=value),linewidth=.6,color="dimgray",na.rm=T)+
  geom_line(aes(y=value_f),linewidth=.6,color="gray",na.rm=T)+
  geom_line(aes(y=value_lo),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=value_hi),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=value_s),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  coord_cartesian(ylim=c(1000,3000),xlim=c(as.Date("2000-01-01"),as.Date("2010-12-31")))+
  theme_eg()

ggsave("figures/lecture6/natgasfor.png",gg_ngfor,width=6.5,height=6.5*9/16,dpi="retina",device="png")
