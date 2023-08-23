library(zoo)
# library(reshape2)
library(ggplot2)
library(data.table)
library(stringr)
# library(timelineS)
library(lmtest)
library(sandwich)


# ##---
# 
# df <- data.frame(event=c("First","Second","Third"),date=as.Date(c("2017-03-11","2017-03-15","2017-03-19")))
# 
# png("timeline.png",width=2400,height=1100,res=300)
# par(mar=c(5,2,1,2))
# timelineS(df,scale="day",buffer.days=4,scale.format="%d",labels=c("CPC-IRI Update / Official ENSO \nForecast (Second Thursday)","IRI Seasonal \nClimate Forecast","IRI Technical ENSO Update / IRI-CPC \nModel-Based ENSO Forecast / \n ENSO Predictions Plume"),label.cex=1,label.direction="up",label.length=c(0.1,1.0,0.3),label.position=c(3,3,3),label.color=c("white","red","blue"),point.color=c("white","red","blue"),xlab="Day of the Month")
# dev.off()
# 
# ##---

# ## this is to plot a histogram of the announcement times during 2013/08-2019/03
# times <- read.csv("times.csv",header=T)
# colnames(times) <- c("date","time","period")
# times$date <- as.Date(times$date,format="%d/%m/%Y")
# times$time <- as.POSIXct(times$time,format="%H:%M:%S")
# 
# times$time <- as.numeric(times$time - trunc(times$time,"days"))
# 
# hmin <- as.numeric(0)
# hmax <- as.numeric(24)
# fmin <- as.numeric(9.5)
# fmax <- as.numeric(14.33)
# 
# ggtime <- ggplot(data=times,aes(x=time)) +
#   geom_histogram(binwidth=1,color="white",fill="steelblue",alpha=.8) +
#   geom_segment(aes(x = fmin, y = -.2, xend = fmax, yend = -.2),color="indianred",size=2) +
#   coord_cartesian(xlim=c(2,21)) +
#   scale_x_continuous(labels=function(x)paste0(x,":00"),name="Time of the day",breaks = c(0,6,12,18,24)) +
#   scale_y_continuous(name="Frequency")+
#   theme_classic()+
#   theme(legend.position = "top", legend.title = element_blank(), legend.text = element_text(size=12), axis.text = element_text(size=12),axis.title = element_text(size=14),legend.spacing.x = unit(0.5, 'cm'))
# 
# ggsave("../Tex/Figures/daytime.png",ggtime,width=6.5,height=3.75)

# gg0 <- ggplot(data=times,aes(x=time)) +
#   geom_histogram(binwidth=1,color="white",fill="#00BFC4") +
#   geom_segment(aes(x = fmin, y = -.2, xend = fmax, yend = -.2),color="#F8766D",size=2) +
#   coord_cartesian(xlim=c(2,21)) +
#   scale_x_continuous(labels=function(x)paste0(x,":00"),name="Time of the day",breaks = c(0,6,12,18,24)) +
#   scale_y_continuous(name="Frequency")+
#   theme_classic() +
#   theme(legend.title=element_blank(),legend.text=element_text(size=10),legend.text.align=0,axis.text=element_text(size=16),axis.title=element_text(size=16),legend.background=element_rect(fill="transparent"),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
# 
# png("daytime.png",width=2400,height=1400,res=300)
# ggdraw() +
#   draw_plot(gg0,x=0,y=0,width=1,height=1)
# dev.off()

##---
# 
# df <- data.frame(event=c("First","Third"),date=as.Date(c("2017-03-11","2017-03-18")))
# 
# # png("timeline.png",width=2400,height=1100,res=300)
# par(mar=c(5,2,1,2))
# timelineS(df,scale="day",buffer.days=4,scale.format="%d",labels=c("CPC Update / Official ENSO \nForecast (Second Thursday)","IRI Technical Update / \nModel-Based ENSO Forecast / \n ENSO Predictions Plume"),label.cex=1,label.direction="up",label.length=c(0.1),label.position=c(3),label.color=c("white"),point.color=c("white"),xlab="Day of the Month")
# # dev.off()

##---


##-- dates to work with

y.beg <- 2002
m.beg <- 01
y.end <- 2019 
m.end <- 03

d.beg <- as.Date(paste0(y.beg,"-",m.beg,"-","01"))
d.end <- as.Date(paste0(y.end,"-",m.end,"-","31"))

##--

# get the SST anomalies from the web (internet connection needed)
sst.raw <- as.data.table(read.table("http://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices",header=T))
sst.sub <- sst.raw[,.(year=YR,mo=MON,ssta=ANOM.3)]
sst.sub[,date := as.Date(paste0(year,"-",str_pad(mo,2,pad="0"),"-01"))]
sst.sub <- sst.sub[date>=d.beg & date <= d.end]

l=2
sst.sub[,paste0("ssta.l",1:l) := data.table::shift(ssta,1:l)]

sst.sub$e <- c(rep(NA,l),lm(ssta~ssta.l1+ssta.l2,data=sst.sub)$residuals)
sst.sub[,s := abs(e)]


dt <- fread("enso_fcst_iri_corrected.csv")

dt <- dt[,.(date=as.Date(V12,format="%d/%m/%Y"),month_observed=V16,sst_observed=V17,season_observed=V14,oni_observed=V15,season_forecast=V13,oni_f1=as.numeric(V3)/100,oni_f2=as.numeric(V4)/100,oni_f3=as.numeric(V5)/100,oni_f4=as.numeric(V6)/100,oni_f5=as.numeric(V7)/100,oni_f6=as.numeric(V8)/100,oni_f7=as.numeric(V9)/100,oni_f8=as.numeric(V10)/100,oni_f9=as.numeric(V11)/100,model=V1)]

iri_dt <- dt[,lapply(.SD,function(x)replace(x,which(x==-9.99),NA))]


##---

sub_dt <- iri_dt[,.(date,ssn=season_observed,ssn_f=season_forecast,oni=oni_observed,oni_f=oni_f1,model)]

sub_dt <- sub_dt[complete.cases(sub_dt)]

models <- names(which(table(sub_dt$model)==max(table(sub_dt$model))))

sub_dt <- sub_dt[model%in%models]

sub_dt <- sub_dt[order(model,date)]
sub_dt[,`:=`(ssn_fl=shift(ssn_f,3),oni_fl=shift(oni_f,3)),by=.(model)]

dt <- sub_dt[,.(date=as.Date(paste0(substr(date-45,1,7),"-01")),ssn,oni_y=oni,oni_f=oni_fl,model)]
dt <- dt[complete.cases(dt)]

dt[,`:=`(oni_e=oni_y-oni_f)]
dt[,`:=`(oni_a=abs(oni_e),oni_s=(oni_e^2))]
dt[,`:=`(mafe=mean(oni_a),rmsfe=sqrt(mean(oni_s))),by=.(model)]

dt <- dt[date>="2003-01-01" & date<="2018-12-31"]

ggplot()+
  geom_boxplot(data=dt,aes(x=date,y=oni_f,group=date),color="coral",fill="cornsilk",outlier.size=1)+
  geom_line(data=dt[model==unique(model)[1]],aes(x=date,y=oni_y),size=1,color="dimgray")+
  labs(x="Year",y="ONI")+
  theme_minimal()


accuracy_dt <- unique(dt[,.(model,mafe,rmsfe)])
accuracy_dt <- accuracy_dt[order(rmsfe)]

dm1 <- merge(dt[model=="ECMWF",.(date,e1=oni_s)],dt[model=="JMA",.(date,e2=oni_s)],by="date")
dm1[,`:=`(d=e1-e2)]

reg1 <- lm(d~1,data=dm1)
coeftest(reg1,vcov.=vcovHAC(reg1))

dt_wide <- dcast(dt,date~model,value.var="oni_e")
dt_wide[,`:=`(Combined=rowMeans(.SD)),by=date]

dm2 <- merge(dt[model=="ECMWF",.(date,e1=oni_s)],dt_wide[,.(date,e2=Combined)],by="date")
dm2[,`:=`(d=e1-e2)]

reg2 <- lm(d~1,data=dm2)
coeftest(reg2,vcov.=vcovHAC(reg2))

##---

save(iri_dt,file="iri_rep.RData")

dat.mod <- dat[,.(model=V1,date)]

tab.mod <- table(dat.mod)

long.mod <- melt(tab.mod)

long.mod$value <- as.factor(long.mod$value)

long.mod$date <- as.Date(long.mod$date,format="%Y-%m-%d")

long.mod$model <- factor(long.mod$model,levels=levels(dat.mod$model)[order(table(dat.mod$model))])


gg_models <- ggplot(long.mod,aes(x=date,y=model,fill=value)) + 
  geom_tile(colour="gray90",size=.01) +
  scale_color_manual(values=c("gray90","steelblue")) + 
  scale_fill_manual(values=c("gray90","steelblue")) + 
  scale_x_date(date_labels="%Y",breaks = as.Date(c("2002-01-01","2006-01-01","2010-01-01","2014-01-01","2018-01-01"))) +
  labs(x="Year",y="Climate Models")+
  theme_bw() + 
  theme(legend.position="none",panel.grid.major=element_blank(),panel.border=element_blank(),axis.text.y=element_text(hjust=0))

ggsave("../Tex/Figures/modeltile.png",gg_models,width=6.5,height=4.5)


retain <- c("AUS/POAMA","CPC CA","CPC MRKOV","CSU CLIPR","ECMWF","JMA","KMA SNU","LDEO","NASA GMAO","UBC NNET")
# dat <- dat[dat$V1 %in% retain]

dat[,f.1 := as.numeric(dat$V3)/100]
dat$f.1[which(dat$f.1==-9.99)] <- NA
dat[,f.2 := as.numeric(dat$V4)/100]
dat$f.2[which(dat$f.2==-9.99)] <- NA
dat[,f.3 := as.numeric(dat$V5)/100]
dat$f.3[which(dat$f.3==-9.99)] <- NA
dat[,f.4 := as.numeric(dat$V6)/100]
dat$f.4[which(dat$f.4==-9.99)] <- NA
dat[,f.5 := as.numeric(dat$V7)/100]
dat$f.5[which(dat$f.5==-9.99)] <- NA
dat[,f.6 := as.numeric(dat$V8)/100]
dat$f.6[which(dat$f.6==-9.99)] <- NA
dat[,f.7 := as.numeric(dat$V9)/100]
dat$f.7[which(dat$f.7==-9.99)] <- NA
dat[,f.8 := as.numeric(dat$V10)/100]
dat$f.8[which(dat$f.8==-9.99)] <- NA
dat[,f.9 := as.numeric(dat$V11)/100]
dat$f.9[which(dat$f.9==-9.99)] <- NA

pct.trs <- function(x,trs,greater=T){
  x <- x[!is.na(x)]
  if(greater==T){
    length(x[which(x>=trs)])/length(x)
  }else{
    length(x[which(x<trs)])/length(x)
  }
}

pct.within <- function(x,trs){
  x <- x[!is.na(x)]
  length(x[which(abs(x)<trs)])/length(x)
}

dat.m <- dat[,.(ssn.f=V13,ssn.o=V14,oni.o=V15,mon.o=V16,sst.o=V17,f1.pt=mean(f.1,na.rm=T),f2.pt=mean(f.2,na.rm=T),f3.pt=mean(f.3,na.rm=T),f4.pt=mean(f.4,na.rm=T),f5.pt=mean(f.5,na.rm=T),f6.pt=mean(f.6,na.rm=T),f7.pt=mean(f.7,na.rm=T),f8.pt=mean(f.8,na.rm=T),f9.pt=mean(f.9,na.rm=T),f1.pe=pct.trs(f.1,0),f2.pe=pct.trs(f.2,0),f3.pe=pct.trs(f.3,0),f4.pe=pct.trs(f.4,0),f5.pe=pct.trs(f.5,0),f6.pe=pct.trs(f.6,0),f7.pe=pct.trs(f.7,0),f8.pe=pct.trs(f.8,0),f9.pe=pct.trs(f.9,0),f1.pl=pct.trs(f.1,0,greater=F),f2.pl=pct.trs(f.2,0,greater=F),f3.pl=pct.trs(f.3,0,greater=F),f4.pl=pct.trs(f.4,0,greater=F),f5.pl=pct.trs(f.5,0,greater=F),f6.pl=pct.trs(f.6,0,greater=F),f7.pl=pct.trs(f.7,0,greater=F),f8.pl=pct.trs(f.8,0,greater=F),f9.pl=pct.trs(f.9,0,greater=F),f1.pw=pct.within(f.1,.5),f2.pw=pct.within(f.2,.5),f3.pw=pct.within(f.3,.5),f4.pw=pct.within(f.4,.5),f5.pw=pct.within(f.5,.5),f6.pw=pct.within(f.6,.5),f7.pw=pct.within(f.7,.5),f8.pw=pct.within(f.8,.5),f9.pw=pct.within(f.9,.5)),by=.(date)]
dat.m <- unique(dat.m)

# i=3
# 
# mo <- as.numeric(substr(dat.m$date,6,7))[i]
# ld <- 7-mo
# if(ld)
# gsf <- dat.m[i,.SD,.SDcols=mo+5]



dat.m$d1.pt <- NA
dat.m$d2.pt <- NA
dat.m$d3.pt <- NA
dat.m$d4.pt <- NA
dat.m$d5.pt <- NA
dat.m$d6.pt <- NA
dat.m$d7.pt <- NA
dat.m$d8.pt <- NA
for(i in 2:nrow(dat.m)){
  dat.m$d1.pt[i] <- dat.m$f1.pt[i]-dat.m$f2.pt[i-1]
  dat.m$d2.pt[i] <- dat.m$f2.pt[i]-dat.m$f3.pt[i-1]
  dat.m$d3.pt[i] <- dat.m$f3.pt[i]-dat.m$f4.pt[i-1]
  dat.m$d4.pt[i] <- dat.m$f4.pt[i]-dat.m$f5.pt[i-1]
  dat.m$d5.pt[i] <- dat.m$f5.pt[i]-dat.m$f6.pt[i-1]
  dat.m$d6.pt[i] <- dat.m$f6.pt[i]-dat.m$f7.pt[i-1]
  dat.m$d7.pt[i] <- dat.m$f7.pt[i]-dat.m$f8.pt[i-1]
  dat.m$d8.pt[i] <- dat.m$f8.pt[i]-dat.m$f9.pt[i-1]
}

dat.m$d1.pe <- NA
dat.m$d2.pe <- NA
dat.m$d3.pe <- NA
dat.m$d4.pe <- NA
dat.m$d5.pe <- NA
dat.m$d6.pe <- NA
dat.m$d7.pe <- NA
dat.m$d8.pe <- NA
for(i in 2:nrow(dat.m)){
  dat.m$d1.pe[i] <- dat.m$f1.pe[i]-dat.m$f2.pe[i-1]
  dat.m$d2.pe[i] <- dat.m$f2.pe[i]-dat.m$f3.pe[i-1]
  dat.m$d3.pe[i] <- dat.m$f3.pe[i]-dat.m$f4.pe[i-1]
  dat.m$d4.pe[i] <- dat.m$f4.pe[i]-dat.m$f5.pe[i-1]
  dat.m$d5.pe[i] <- dat.m$f5.pe[i]-dat.m$f6.pe[i-1]
  dat.m$d6.pe[i] <- dat.m$f6.pe[i]-dat.m$f7.pe[i-1]
  dat.m$d7.pe[i] <- dat.m$f7.pe[i]-dat.m$f8.pe[i-1]
  dat.m$d8.pe[i] <- dat.m$f8.pe[i]-dat.m$f9.pe[i-1]
}

dat.m$d1.pl <- NA
dat.m$d2.pl <- NA
dat.m$d3.pl <- NA
dat.m$d4.pl <- NA
dat.m$d5.pl <- NA
dat.m$d6.pl <- NA
dat.m$d7.pl <- NA
dat.m$d8.pl <- NA
for(i in 2:nrow(dat.m)){
  dat.m$d1.pl[i] <- dat.m$f1.pl[i]-dat.m$f2.pl[i-1]
  dat.m$d2.pl[i] <- dat.m$f2.pl[i]-dat.m$f3.pl[i-1]
  dat.m$d3.pl[i] <- dat.m$f3.pl[i]-dat.m$f4.pl[i-1]
  dat.m$d4.pl[i] <- dat.m$f4.pl[i]-dat.m$f5.pl[i-1]
  dat.m$d5.pl[i] <- dat.m$f5.pl[i]-dat.m$f6.pl[i-1]
  dat.m$d6.pl[i] <- dat.m$f6.pl[i]-dat.m$f7.pl[i-1]
  dat.m$d7.pl[i] <- dat.m$f7.pl[i]-dat.m$f8.pl[i-1]
  dat.m$d8.pl[i] <- dat.m$f8.pl[i]-dat.m$f9.pl[i-1]
}


dat.m$yrmo <- substr(dat.m$date,start=1,stop=7)

dat.enso <- dat.m#merge(dat.m,ensoshocks,by="yrmo")

dat.enso <- dat.enso[complete.cases(dat.enso),]

iri_dt <- dat.enso

# dat.shocks <- data.frame(date=dat.enso$date,yrmo=dat.enso$yrmo,resid=dat.enso$resid,f.e=dat.enso$f.e,f.u=dat.enso$f.u,e.e=dat.enso$e.e,e.u=dat.enso$e.u)
# 
# dat.shocks$resid <- dat.shocks$resid/sd(dat.shocks$resid)
# dat.shocks$f.e <- dat.shocks$f.e/sd(dat.shocks$f.e)
# dat.shocks$f.u <- dat.shocks$f.u/sd(dat.shocks$f.u)
# dat.shocks$e.e <- dat.shocks$e.e/sd(dat.shocks$e.e)
# dat.shocks$e.u <- dat.shocks$e.u/sd(dat.shocks$e.u)

rm(list=setdiff(ls(),c("iri_dt")))

save.image("../R/Main/Data/iri_forecasts.RData")



