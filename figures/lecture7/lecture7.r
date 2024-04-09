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
library(forecast)

library(fastDummies)

rm(list=ls())
gc()

# all_wpi <- read_abs("6345.0")

# plot aesthetics
theme_eg <- function(base_size=12,base_family="Segoe Print",border=F){
  theme(
    panel.background=element_rect(fill="white",color=NA),
    panel.grid=element_line(colour=NULL,linetype=3),
    panel.grid.major=element_line(colour="dimgray"),
    panel.grid.major.x=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill="white",color=NA),
    plot.title=element_text(family=base_family,size=rel(1.3),colour="dimgray"),
    plot.subtitle=element_text(family=base_family,size=rel(1.2),colour="dimgray",hjust=0),
    plot.caption=element_text(colour="darkgray",size=rel(0.8),hjust=0),
    plot.margin=margin(.25,.25,.25,.25,"lines"),
    plot.title.position="plot",
    plot.caption.position="plot",
    axis.title=element_text(family=base_family,size=rel(1.2),colour="dimgray"),
    axis.title.x=element_text(hjust=1),
    axis.text=element_text(family=base_family,size=rel(1.1),colour="dimgray"),
    axis.line=element_line(colour="dimgray"),
    axis.line.y=element_blank(),
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

# unemployment series ----
unrate_dt <- data.table(fredr(series_id="UNRATE",observation_start=as.Date("1960-01-01"),observation_end=as.Date("2019-12-31"),frequency="q",units="lin"))

unratensa_dt <- data.table(fredr(series_id="UNRATENSA",observation_start=as.Date("1960-01-01"),observation_end=as.Date("2019-12-31"),frequency="q",units="lin"))

ur_dt <- rbind(unrate_dt,unratensa_dt)
ur_dt$series_id <- factor(ur_dt$series_id,levels=c("UNRATENSA","UNRATE"),labels=c("not seasonally adjusted","seasonally adjusted"))

gg_ur <- ggplot(unrate_dt,aes(x=date,y=value))+
  geom_line(linewidth=.6,color="dimgray",na.rm=T)+
  scale_y_continuous(breaks=seq(0,12,2))+
  labs(y="",x="Year",subtitle="Unemployment (%)")+
  coord_cartesian(ylim=c(0,12),xlim=c(as.Date("1960-01-01"),as.Date("2019-12-31")))+
  theme_eg()

gg_ur

ggsave("figures/lecture7/unrate.png",gg_ur,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# autocorrelograms ----
unrate_dt <- unrate_dt[,.(date,y=value)]

unrate_dt[,`:=`(y1=shift(y,1),y2=shift(y,2),y3=shift(y,3),y4=shift(y,4),y5=shift(y,5),y6=shift(y,6),y7=shift(y,7),y8=shift(y,8))]

# obtain autocorrelations
maxlag <- 80
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(unrate_dt$y,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_acf <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(unrate_dt)),1.96/sqrt(nrow(unrate_dt))),linewidth=.6,linetype=5,col="dimgray")+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="gray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="gray")+
  scale_x_continuous(breaks=seq(5,maxlag,5),labels=seq(5,maxlag,5))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(y="",x="k",subtitle=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1.5,maxlag-0.5))+
  theme_eg()

gg_acf

ggsave("figures/lecture7/unrate_ac.png",gg_acf,width=6.5,height=6.5*9/16,dpi="retina",device="png")


pacf_dt <- data.table(k=c(1:maxlag),rho=c(pacf(unrate_dt$y,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_pacf <- ggplot(pacf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(unrate_dt)),1.96/sqrt(nrow(unrate_dt))),linewidth=.6,linetype=5,col="dimgray")+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="gray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="gray")+
  scale_x_continuous(breaks=seq(5,maxlag,5),labels=seq(5,maxlag,5))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(y="",x="k",subtitle=expression(hat(pi)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1.5,maxlag-0.5))+
  theme_eg()

gg_pacf

ggsave("figures/lecture7/unrate_pac.png",gg_pacf,width=6.5,height=6.5*9/16,dpi="retina",device="png")



# Lag selection ----

# unrate_dt[,`:=`(dy=y-y1,dy1=y1-y2,dy2=y2-y3,dy3=y3-y4)]

ur_dt <- unrate_dt[complete.cases(unrate_dt)]


est1 <- lm(y~y1,data=ur_dt)
est2 <- lm(y~y1+y2,data=ur_dt)
est3 <- lm(y~y1+y2+y3,data=ur_dt)
est4 <- lm(y~y1+y2+y3+y4,data=ur_dt)

icf <- function(m,ic){
  aic=log(crossprod(m$residuals))+2*length(m$coefficients)/length(m$residuals)
  sic=log(crossprod(m$residuals))+log(length(m$residuals))*length(m$coefficients)/length(m$residuals)
  if(ic=="a"){return(aic)}else{return(sic)}
}

dt <- data.table(aic=round(sapply(list(est1,est2,est3,est4),icf,ic="a"),3),sic=round(sapply(list(est1,est2,est3,est4),icf,ic="s"),3))


# adf1 <- lm(dy~y1+dy1,data=unrate_dt)
# 
# summary(adf1)


# one-step-ahead ----

h1 <- as.Date("2010-01-01")
h2 <- max(unrate_dt$date)


est <- lm(y~y1+y2,data=unrate_dt[date<h1])
unrate_dt[,`:=`(y_f=y)]
unrate_dt[date<h1]$y_f <- NA

oos <- unrate_dt[date>h1]$date

unrate_dt[,`:=`(f_i=as.numeric(NA),f_il=as.numeric(NA),f_iu=as.numeric(NA))]

unrate_dt[,trend:=1:nrow(unrate_dt)]

for(i in oos){
  
  # starting point
  s <- unrate_dt[trend==unrate_dt[date==i]$trend-unrate_dt[date==h1]$trend+1]$date
  
  # linear trend
  est <- lm(y~y1+y2,data=unrate_dt[date>= s & date<i])
  unrate_dt[date==i,f_i:=est$coefficients%*%as.matrix(c(1,as.matrix(unrate_dt[date==i,.(y1,y2)])))]
  
  unrate_dt[date==i,`:=`(f_il=f_i-1.96*summary(est)$sigma,f_iu=f_i+1.96*summary(est)$sigma)]
  
}


gg_osa <- ggplot(unrate_dt,aes(x=date))+
  geom_ribbon(aes(ymin=f_il,ymax=f_iu),fill="coral",alpha=.2)+
  geom_line(aes(y=y),linewidth=.8,color="dimgray",na.rm=T)+
  geom_line(aes(y=y_f),linewidth=.8,color="gray",na.rm=T)+
  geom_line(aes(y=f_il),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=f_iu),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=f_i),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  scale_y_continuous(breaks=seq(2,12,2))+
  labs(y="",x="Year",subtitle="Unemployment (%)")+
  coord_cartesian(ylim=c(0,12),xlim=c(as.Date("1960-01-01"),as.Date("2019-12-31")))+
  theme_eg()

gg_osa

ggsave("figures/lecture7/unrate_osa.png",gg_osa,width=6.5,height=6.5*9/16,dpi="retina",device="png")




# one-step-ahead: iterated ----

unrate_dt[,`:=`(f_i=as.numeric(NA),f_il=as.numeric(NA),f_iu=as.numeric(NA))]

unrate_dt[,trend:=1:nrow(unrate_dt)]

s <- unrate_dt[trend==unrate_dt[date==i]$trend-unrate_dt[date==h1]$trend+1]$date

# linear trend
est <- lm(y~y1+y2,data=unrate_dt[date<h1])

y <- unrate_dt[date<h1]$y
ar2 <- ar(y,order.max=2,demean=F,intercept=T,method="ols")
ar2f <- forecast(ar2,h=length(unrate_dt[date>=h1]$y),level=95)

unrate_dt$f <- c(rep(NA,length(unrate_dt[date<h1]$y)),as.matrix(ar2f$mean))
unrate_dt$l <- c(rep(NA,length(unrate_dt[date<h1]$y)),as.matrix(ar2f$lower))
unrate_dt$u <- c(rep(NA,length(unrate_dt[date<h1]$y)),as.matrix(ar2f$upper))


gg_ite <- ggplot(unrate_dt,aes(x=date))+
  geom_ribbon(aes(ymin=l,ymax=u),fill="coral",alpha=.2)+
  geom_line(aes(y=y),linewidth=.8,color="dimgray",na.rm=T)+
  geom_line(aes(y=y_f),linewidth=.8,color="gray",na.rm=T)+
  geom_line(aes(y=l),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=u),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=f),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  scale_y_continuous(breaks=seq(0,12,2))+
  labs(y="",x="Year",subtitle="Unemployment (%)")+
  coord_cartesian(ylim=c(0,12),xlim=c(as.Date("1960-01-01"),as.Date("2019-12-31")))+
  theme_eg()

gg_ite

ggsave("figures/lecture7/unrate_ite.png",gg_ite,width=6.5,height=6.5*9/16,dpi="retina",device="png")





# one-step-ahead: direct ----

unrate_dt[,paste0('y',1:(length(oos)+2)) := shift(y,1:(length(oos)+2))]


unrate_dt[,`:=`(f_d=as.numeric(NA),f_dl=as.numeric(NA),f_du=as.numeric(NA))]

unrate_dt[,trend:=1:nrow(unrate_dt)]

for(i in oos){
  
  p <- unrate_dt[trend==unrate_dt[date==i]$trend-unrate_dt[date==h1]$trend+1]$trend
  
  # formula
  fmla <- as.formula(paste0("y~y",p,"+y",p+1))
  
  lagvec <- c(paste0("y",p),paste0("y",p+1))
  
  # linear trend
  est <- lm(fmla,data=unrate_dt[date<i])
  unrate_dt[date==i,f_d:=est$coefficients%*%as.matrix(c(1,as.matrix(unrate_dt[date==i,.SD,.SDcols=lagvec])))]
  
  unrate_dt[date==i,`:=`(f_dl=f_d-1.96*summary(est)$sigma,f_du=f_d+1.96*summary(est)$sigma)]
  
}



gg_dir <- ggplot(unrate_dt,aes(x=date))+
  geom_ribbon(aes(ymin=f_dl,ymax=f_du),fill="coral",alpha=.2)+
  geom_line(aes(y=y),linewidth=.8,color="dimgray",na.rm=T)+
  geom_line(aes(y=y_f),linewidth=.8,color="gray",na.rm=T)+
  geom_line(aes(y=f_dl),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=f_du),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=f_d),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  scale_y_continuous(breaks=seq(0,12,2))+
  labs(y="",x="Year",subtitle="Unemployment (%)")+
  coord_cartesian(ylim=c(0,12),xlim=c(as.Date("1960-01-01"),as.Date("2019-12-31")))+
  theme_eg()

gg_dir


ggsave("figures/lecture7/unrate_dir.png",gg_dir,width=6.5,height=6.5*9/16,dpi="retina",device="png")





