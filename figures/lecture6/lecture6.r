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

# all_wpi <- read_abs("6345.0")

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


natgas_dt[,trend:=1:nrow(natgas_dt)]
natgas_dt[,`:=`(s1=sin(2*pi*1*trend/12),c1=cos(2*pi*1*trend/12),s2=sin(2*pi*2*trend/12),c2=cos(2*pi*2*trend/12),s3=sin(2*pi*3*trend/12),c3=cos(2*pi*3*trend/12),s4=sin(2*pi*4*trend/12),c4=cos(2*pi*4*trend/12),s5=sin(2*pi*5*trend/12),c5=cos(2*pi*5*trend/12),s6=sin(2*pi*6*trend/12),c6=cos(2*pi*6*trend/12))]

ggplot(natgas_dt[trend%in%1:12],aes(x=date))+
  geom_line(aes(y=s1))+
  geom_line(aes(y=c1))+
  geom_line(aes(y=s2))+
  geom_line(aes(y=c2))


est1 <- lm(value~s1+c1,data=natgas_dt[date<=as.Date("2006-12-31")])
est2 <- lm(value~s1+c1+s2+c2,data=natgas_dt[date<=as.Date("2006-12-31")])
est3 <- lm(value~s1+c1+s2+c2+s3+c3,data=natgas_dt[date<=as.Date("2006-12-31")])
est4 <- lm(value~s1+c1+s2+c2+s3+c3+s4+c4,data=natgas_dt[date<=as.Date("2006-12-31")])
est5 <- lm(value~s1+c1+s2+c2+s3+c3+s4+c4+s5+c5,data=natgas_dt[date<=as.Date("2006-12-31")])
est6 <- lm(value~s1+c1+s2+c2+s3+c3+s4+c4+s5+c5+s6+c6,data=natgas_dt[date<=as.Date("2006-12-31")])

icf <- function(m,ic){
  aic=log(crossprod(m$residuals))+2*length(m$coefficients)/length(m$residuals)
  sic=log(crossprod(m$residuals))+log(length(m$residuals))*length(m$coefficients)/length(m$residuals)
  if(ic=="a"){return(aic)}else{return(sic)}
}

dt <- data.table(aic=round(sapply(list(est1,est2,est3,est4,est5,est6),icf,ic="a"),3),sic=round(sapply(list(est1,est2,est3,est4,est5,est6),icf,ic="s"),3))




natgas_dt[,`:=`(value_h=est3$coefficients[1]+as.matrix(natgas_dt[,.(s1,c1,s2,c2,s3,c3)])%*%as.matrix(est3$coefficients[-1]))]

natgas_dt[date<=as.Date("2006-12-31")]$value_h <- NA

natgas_dt[,`:=`(value_hlo=value_h-1.96*summary(est3)$sigma,value_hhi=value_h+1.96*summary(est3)$sigma)]

gg_nghar <- ggplot(natgas_dt,aes(x=date))+
  geom_ribbon(aes(ymin=value_hlo,ymax=value_hhi),fill="coral",alpha=.2)+
  geom_line(aes(y=value),linewidth=.6,color="dimgray",na.rm=T)+
  geom_line(aes(y=value_f),linewidth=.6,color="gray",na.rm=T)+
  geom_line(aes(y=value_hlo),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=value_hhi),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=value_h),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  coord_cartesian(ylim=c(1000,3000),xlim=c(as.Date("2000-01-01"),as.Date("2010-12-31")))+
  theme_eg()

ggsave("figures/lecture6/natgashar.png",gg_nghar,width=6.5,height=6.5*9/16,dpi="retina",device="png")






tide_dt <- fread("figures/lecture6/cairns_2020.csv")

tide_dt[,`:=`(Date=as.Date(Date),Time=paste0(Time,":00"))]

format <- "%Y-%m-%d %H:%M:%S"

tide_dt[,DateTime:=as.POSIXct(paste(Date,Time),format=format)]


tseq_dt <- data.table(DateTime=seq(from=tide_dt$DateTime[1],length.out=366*24*6,by=600))

tide_dt <- merge(tseq_dt,tide_dt,by="DateTime",all.x=T)

tide_dt[,Date:=as.IDate(DateTime)]
tide_dt[,Time:=as.ITime(DateTime)]
tide_dt[,Reading:=na.approx(Reading)]

ggplot(tide_dt[Date>="2020-10-01" & Date<="2020-10-31"],aes(x=DateTime,y=Reading))+
  geom_line()

tide_dt[,trend:=1:nrow(tide_dt)]

freq <- 24*6+5

tide_dt[,`:=`(s1=sin(2*pi*1*trend/freq),c1=cos(2*pi*1*trend/freq),s2=sin(2*pi*2*trend/freq),c2=cos(2*pi*2*trend/freq),s3=sin(2*pi*3*trend/freq),c3=cos(2*pi*3*trend/freq),s4=sin(2*pi*4*trend/freq),c4=cos(2*pi*4*trend/freq),s5=sin(2*pi*5*trend/freq),c5=cos(2*pi*5*trend/freq),s6=sin(2*pi*6*trend/freq),c6=cos(2*pi*6*trend/freq))]



est1 <- lm(Reading~s1+c1,data=tide_dt)
est2 <- lm(Reading~s1+c1+s2+c2,data=tide_dt)
est3 <- lm(Reading~s1+c1+s2+c2+s3+c3,data=tide_dt)
est4 <- lm(Reading~s1+c1+s2+c2+s3+c3+s4+c4,data=tide_dt)
est5 <- lm(Reading~s1+c1+s2+c2+s3+c3+s4+c4+s5+c5,data=tide_dt)
est6 <- lm(Reading~s1+c1+s2+c2+s3+c3+s4+c4+s5+c5+s6+c6,data=tide_dt)


icf <- function(m,ic){
  aic=log(crossprod(m$residuals))+2*length(m$coefficients)/length(m$residuals)
  sic=log(crossprod(m$residuals))+log(length(m$residuals))*length(m$coefficients)/length(m$residuals)
  if(ic=="a"){return(aic)}else{return(sic)}
}

dt <- data.table(aic=round(sapply(list(est1,est2,est3,est4,est5,est6),icf,ic="a"),5),sic=round(sapply(list(est1,est2,est3,est4,est5,est6),icf,ic="s"),5))



tide_dt[,`:=`(Reading_h2=est2$coefficients[1]+as.matrix(tide_dt[,.(s1,c1,s2,c2)])%*%as.matrix(est2$coefficients[-1]),Reading_h4=est4$coefficients[1]+as.matrix(tide_dt[,.(s1,c1,s2,c2,s3,c3,s4,c4)])%*%as.matrix(est4$coefficients[-1]))]

ggplot(tide_dt[Date>="2020-10-01" & Date<="2020-10-31"],aes(x=DateTime))+
  geom_line(aes(y=Reading),linewidth=.6,color="dimgray",na.rm=T)+
  geom_line(aes(y=Reading_h4),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  theme_eg()






natgas_dt <- data.table(fredr(series_id="NATURALGAS",observation_start=as.Date("2000-01-01"),observation_end=as.Date("2022-12-31"),frequency="m",units="lin"))

gg_ng <- ggplot(natgas_dt,aes(x=date,y=value))+
  geom_line(linewidth=.5,na.rm=T,color="dimgray")+
  labs(x="Year",y="Billion Cubic Feet")+
  coord_cartesian(ylim=c(1000,4000),xlim=c(as.Date("2000-01-01"),as.Date("2022-12-31")))+
  theme_eg()

ggsave("figures/lecture6/natgaslong.png",gg_ng,width=6.5,height=6.5*9/16,dpi="retina",device="png")



natgas_dt$month <- month(natgas_dt$date)
natgas_dum <- dummy_cols(natgas_dt$month,remove_selected_columns=T)
colnames(natgas_dum) <- paste0("d",c(1:12))

natgas_dt <- cbind(natgas_dt,natgas_dum)
natgas_dt[,trend:=1:nrow(natgas_dt)]

est <- lm(value~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+trend-1,data=natgas_dt)

natgas_dt$value_fit <- fitted(est)

natgas_lg <- melt(natgas_dt[,.(date,value,value_fit)],id.vars="date")
natgas_lg$variable <- factor(natgas_lg$variable,levels=c("value","value_fit"),labels=c("observed data","fitted data"))

gg_ngfit <- ggplot(natgas_lg,aes(x=date,y=value,color=variable,linetype=variable))+
  geom_line(linewidth=.5,na.rm=T)+
  scale_color_manual(values=c("dimgray","coral"))+
  scale_linetype_manual(values=c(1,5))+
  labs(x="Year",y="Billion Cubic Feet")+
  coord_cartesian(ylim=c(1000,4000),xlim=c(as.Date("2000-01-01"),as.Date("2022-12-31")))+
  theme_eg()

ggsave("figures/lecture6/natgasfitlong.png",gg_ngfit,width=6.5,height=6.5*9/16,dpi="retina",device="png")





h1 <- as.Date("2010-12-01")

h2 <- max(natgas_dt$date)

oos <- natgas_dt[date>h1]$date
ins <- natgas_dt[date>h1]$date

natgas_dt[,`:=`(f_s=as.numeric(NA),f_sl=as.numeric(NA),f_su=as.numeric(NA))]

for(i in oos){
  
  # starting point
  s <- natgas_dt[trend==natgas_dt[date==i]$trend-132]$date
  
  # linear trend
  est <- lm(value~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+trend-1,data=natgas_dt[date>= s & date<i])
  natgas_dt[date==i,f_s:=est$coefficients%*%t(as.matrix(natgas_dt[date==i,.(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,trend)]))]
  
  natgas_dt[date==i,`:=`(f_sl=f_s-1.96*summary(est)$sigma,f_su=f_s+1.96*summary(est)$sigma)]
  
}


gg_ng1 <- ggplot(natgas_dt,aes(x=date,y=value))+
  geom_ribbon(aes(ymin=f_sl,ymax=f_su),fill="coral",alpha=.2)+
  geom_line(color="dimgray",linewidth=.6)+
  geom_line(data=natgas_dt[date>h1],color="gray",linewidth=.6)+
  geom_line(aes(y=f_s),color="coral",linewidth=.6,linetype=5,na.rm=T)+
  geom_line(aes(y=f_sl),color="coral",linewidth=.4,linetype=2,na.rm=T)+
  geom_line(aes(y=f_su),color="coral",linewidth=.4,linetype=2,na.rm=T)+
  labs(x="Year",y="Billion Cubic Feet")+
  coord_cartesian(ylim=c(1000,4000),xlim=c(as.Date("2000-01-01"),as.Date("2022-12-31")))+
  theme_eg()

ggsave("figures/lecture6/natgas1_forecast.png",gg_ng1,width=6.5,height=6.5*9/16,dpi="retina",device="png")

natgas_dt[,`:=`(e_t=value-f_s)]

gg_ng1e <- ggplot(natgas_dt[date>h1],aes(x=date,y=e_t))+
  geom_line(color="dimgray",linewidth=.6)+
  geom_point(shape=21,size=1.5,stroke=.6,color="black",fill="gray")+
  labs(x="Year",y="Billion Cubic Feet")+
  coord_cartesian(ylim=c(-500,500),xlim=c(as.Date("2011-01-01"),as.Date("2022-12-31")))+
  theme_eg()

ggsave("figures/lecture6/natgas1_error.png",gg_ng1e,width=6.5,height=6.5*9/16,dpi="retina",device="png")

mz_reg <- lm(e_t~f_s,data=natgas_dt)
coeftest(mz_reg,vcov.=vcovHAC(mz_reg))

# obtain autocorrelations
maxlag <- 36
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(natgas_dt[date%in%oos]$e_t,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_acf <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(natgas_dt[date%in%oos])),1.96/sqrt(nrow(natgas_dt[date%in%oos]))),linewidth=.8,linetype=5,col="dimgray")+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="gray")+
  geom_point(shape=21,size=2.5,stroke=.8,color="black",fill="gray")+
  scale_x_continuous(breaks=seq(5,maxlag,5),labels=seq(5,maxlag,5))+
  scale_y_continuous(breaks=seq(-.4,1,.2),labels=sprintf("%.1f",round(seq(-.4,1,.2),1)))+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.4,1),xlim=c(1.5,maxlag-0.5))+
  theme_eg()

gg_acf

ggsave("figures/lecture6/ac_natgas.png",gg_acf,width=6.5,height=6.5*9/16,dpi="retina",device="png")



est_w <- lm(w~t,data=interest_rates[date<=h1])
interest_rates[,`:=`(f_w=est_w$coefficients[1]+est_w$coefficients[2]*t)]

interest_rates[date<=h1]$f_w <- NA

interest_rates[,`:=`(l_w=f_w-1.96*summary(est_w)$sigma,u_w=f_w+1.96*summary(est_w)$sigma)]

ggplot(interest_rates,aes(x=date,y=y))+
  geom_ribbon(aes(ymin=l,ymax=u),fill="coral",alpha=.2)+
  geom_line(color="black",linewidth=.6)+
  geom_line(data=interest_rates[date>h1],color="gray",linewidth=.6)+
  geom_line(aes(y=exp(f_w)),color="coral",linewidth=.6,linetype=5,na.rm=T)+
  geom_line(aes(y=exp(l_w)),color="coral",linewidth=.4,linetype=2,na.rm=T)+
  geom_line(aes(y=exp(u_w)),color="coral",linewidth=.4,linetype=2,na.rm=T)+
  labs(x="Year",y="Interest Rate (%)")+
  theme_eg()+
  theme(axis.title = element_text(size=22),axis.text = element_text(size=18))











est <- lm(value~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11,data=natgas_dt[date<=as.Date("2010-12-31")])
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


