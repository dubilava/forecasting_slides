# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(extrafont)
loadfonts(device="win",quiet=T)
# font_import()
# loadfonts(device="win")
library(cowplot)
library(readrba)
library(viridis)
library(car)
library(stringr)
library(sandwich)
library(lmtest)


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
    plot.caption=element_text(colour="darkgray",size=rel(0.8)),
    plot.margin=margin(.25,.25,.25,.25,"lines"),
    plot.title.position="plot",
    plot.caption.position="plot",
    axis.title=element_text(family=base_family,size=rel(1.2),colour="dimgray"),
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



load("figures/lecture3/iri_rep.RData")

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
dt[,`:=`(mafe=round(mean(oni_a),3),rmsfe=round(sqrt(mean(oni_s)),3)),by=.(model)]

dt <- dt[date>="2003-01-01" & date<="2018-12-31"]

gg1 <- ggplot()+
  geom_boxplot(data=dt,aes(x=date,y=oni_f,group=date),color="dimgray",fill="lightgray",linewidth=.3,outlier.size=.1,shape=1)+
  geom_line(data=dt[model==unique(model)[1]],aes(x=date,y=oni_y),linewidth=.6,color="black")+
  labs(subtitle="Oceanic Nino Index (°C)",y="",x="Year",caption="Source: The International Research Institute for Climate and Society, Columbia University Climate School")+
  coord_cartesian(ylim=c(-2,3))+
  theme_eg()+
  theme(plot.title.position="plot",plot.caption.position="plot",plot.caption=element_text(hjust=0))

ggsave("figures/lecture3/historical-enso.png",gg1,width=6.5,height=6.5*9/16,dpi="retina")


accuracy_dt <- unique(dt[,.(model,mafe,rmsfe)])
accuracy_dt <- accuracy_dt[order(rmsfe)]

accuracy_dt[,`:=`(num=rmsfe^(-2))]
accuracy_dt[,`:=`(denom=sum(num))]
accuracy_dt[,`:=`(w=num/denom)]

dm1 <- merge(dt[model=="ECMWF",.(date,e1=oni_s)],dt[model=="JMA",.(date,e2=oni_s)],by="date")
dm1[,`:=`(d=e1-e2)]

reg1 <- lm(d~1,data=dm1)
coeftest(reg1,vcov.=vcovHAC(reg1))

dt$model <- factor(dt$model,levels=unique(accuracy_dt$model))

dt_wide <- dcast(dt,date~model,value.var="oni_e")
dt_wide[,`:=`(Combined=rowMeans(.SD)),by=date]
dt_wide$Combined_w <- as.matrix(dt_wide[,2:(length(models)+1)])%*%accuracy_dt$w

dm2 <- merge(dt[model=="ECMWF",.(date,e1=oni_s)],dt_wide[,.(date,e2=Combined)],by="date")
dm2[,`:=`(d=e1-e2)]

reg2 <- lm(d~1,data=dm2)
coeftest(reg2,vcov.=vcovHAC(reg2))


dm3 <- merge(dt[model=="ECMWF",.(date,e1=oni_s)],dt_wide[,.(date,e2=Combined_w)],by="date")
dm3[,`:=`(d=e1-e2)]

reg3 <- lm(d~1,data=dm3)
coeftest(reg3,vcov.=vcovHAC(reg3))

##---



# dat.mod <- dat[,.(model=V1,date)]
# 
# tab.mod <- table(dat.mod)
# 
# long.mod <- melt(tab.mod)
# 
# long.mod$value <- as.factor(long.mod$value)
# 
# long.mod$date <- as.Date(long.mod$date,format="%Y-%m-%d")
# 
# long.mod$model <- factor(long.mod$model,levels=levels(dat.mod$model)[order(table(dat.mod$model))])
# 
# 
# gg_models <- ggplot(long.mod,aes(x=date,y=model,fill=value)) + 
#   geom_tile(colour="gray90",size=.01) +
#   scale_color_manual(values=c("gray90","steelblue")) + 
#   scale_fill_manual(values=c("gray90","steelblue")) + 
#   scale_x_date(date_labels="%Y",breaks = as.Date(c("2002-01-01","2006-01-01","2010-01-01","2014-01-01","2018-01-01"))) +
#   labs(x="Year",y="Climate Models")+
#   theme_bw() + 
#   theme(legend.position="none",panel.grid.major=element_blank(),panel.border=element_blank(),axis.text.y=element_text(hjust=0))
# 
# ggsave("../Tex/Figures/modeltile.png",gg_models,width=6.5,height=4.5)
# 
# 
# retain <- c("AUS/POAMA","CPC CA","CPC MRKOV","CSU CLIPR","ECMWF","JMA","KMA SNU","LDEO","NASA GMAO","UBC NNET")
# # dat <- dat[dat$V1 %in% retain]
# 
# dat[,f.1 := as.numeric(dat$V3)/100]
# dat$f.1[which(dat$f.1==-9.99)] <- NA
# dat[,f.2 := as.numeric(dat$V4)/100]
# dat$f.2[which(dat$f.2==-9.99)] <- NA
# dat[,f.3 := as.numeric(dat$V5)/100]
# dat$f.3[which(dat$f.3==-9.99)] <- NA
# dat[,f.4 := as.numeric(dat$V6)/100]
# dat$f.4[which(dat$f.4==-9.99)] <- NA
# dat[,f.5 := as.numeric(dat$V7)/100]
# dat$f.5[which(dat$f.5==-9.99)] <- NA
# dat[,f.6 := as.numeric(dat$V8)/100]
# dat$f.6[which(dat$f.6==-9.99)] <- NA
# dat[,f.7 := as.numeric(dat$V9)/100]
# dat$f.7[which(dat$f.7==-9.99)] <- NA
# dat[,f.8 := as.numeric(dat$V10)/100]
# dat$f.8[which(dat$f.8==-9.99)] <- NA
# dat[,f.9 := as.numeric(dat$V11)/100]
# dat$f.9[which(dat$f.9==-9.99)] <- NA
# 
# pct.trs <- function(x,trs,greater=T){
#   x <- x[!is.na(x)]
#   if(greater==T){
#     length(x[which(x>=trs)])/length(x)
#   }else{
#     length(x[which(x<trs)])/length(x)
#   }
# }
# 
# pct.within <- function(x,trs){
#   x <- x[!is.na(x)]
#   length(x[which(abs(x)<trs)])/length(x)
# }
# 
# dat.m <- dat[,.(ssn.f=V13,ssn.o=V14,oni.o=V15,mon.o=V16,sst.o=V17,f1.pt=mean(f.1,na.rm=T),f2.pt=mean(f.2,na.rm=T),f3.pt=mean(f.3,na.rm=T),f4.pt=mean(f.4,na.rm=T),f5.pt=mean(f.5,na.rm=T),f6.pt=mean(f.6,na.rm=T),f7.pt=mean(f.7,na.rm=T),f8.pt=mean(f.8,na.rm=T),f9.pt=mean(f.9,na.rm=T),f1.pe=pct.trs(f.1,0),f2.pe=pct.trs(f.2,0),f3.pe=pct.trs(f.3,0),f4.pe=pct.trs(f.4,0),f5.pe=pct.trs(f.5,0),f6.pe=pct.trs(f.6,0),f7.pe=pct.trs(f.7,0),f8.pe=pct.trs(f.8,0),f9.pe=pct.trs(f.9,0),f1.pl=pct.trs(f.1,0,greater=F),f2.pl=pct.trs(f.2,0,greater=F),f3.pl=pct.trs(f.3,0,greater=F),f4.pl=pct.trs(f.4,0,greater=F),f5.pl=pct.trs(f.5,0,greater=F),f6.pl=pct.trs(f.6,0,greater=F),f7.pl=pct.trs(f.7,0,greater=F),f8.pl=pct.trs(f.8,0,greater=F),f9.pl=pct.trs(f.9,0,greater=F),f1.pw=pct.within(f.1,.5),f2.pw=pct.within(f.2,.5),f3.pw=pct.within(f.3,.5),f4.pw=pct.within(f.4,.5),f5.pw=pct.within(f.5,.5),f6.pw=pct.within(f.6,.5),f7.pw=pct.within(f.7,.5),f8.pw=pct.within(f.8,.5),f9.pw=pct.within(f.9,.5)),by=.(date)]
# dat.m <- unique(dat.m)
# 
# # i=3
# # 
# # mo <- as.numeric(substr(dat.m$date,6,7))[i]
# # ld <- 7-mo
# # if(ld)
# # gsf <- dat.m[i,.SD,.SDcols=mo+5]
# 
# 
# 
# dat.m$d1.pt <- NA
# dat.m$d2.pt <- NA
# dat.m$d3.pt <- NA
# dat.m$d4.pt <- NA
# dat.m$d5.pt <- NA
# dat.m$d6.pt <- NA
# dat.m$d7.pt <- NA
# dat.m$d8.pt <- NA
# for(i in 2:nrow(dat.m)){
#   dat.m$d1.pt[i] <- dat.m$f1.pt[i]-dat.m$f2.pt[i-1]
#   dat.m$d2.pt[i] <- dat.m$f2.pt[i]-dat.m$f3.pt[i-1]
#   dat.m$d3.pt[i] <- dat.m$f3.pt[i]-dat.m$f4.pt[i-1]
#   dat.m$d4.pt[i] <- dat.m$f4.pt[i]-dat.m$f5.pt[i-1]
#   dat.m$d5.pt[i] <- dat.m$f5.pt[i]-dat.m$f6.pt[i-1]
#   dat.m$d6.pt[i] <- dat.m$f6.pt[i]-dat.m$f7.pt[i-1]
#   dat.m$d7.pt[i] <- dat.m$f7.pt[i]-dat.m$f8.pt[i-1]
#   dat.m$d8.pt[i] <- dat.m$f8.pt[i]-dat.m$f9.pt[i-1]
# }
# 
# dat.m$d1.pe <- NA
# dat.m$d2.pe <- NA
# dat.m$d3.pe <- NA
# dat.m$d4.pe <- NA
# dat.m$d5.pe <- NA
# dat.m$d6.pe <- NA
# dat.m$d7.pe <- NA
# dat.m$d8.pe <- NA
# for(i in 2:nrow(dat.m)){
#   dat.m$d1.pe[i] <- dat.m$f1.pe[i]-dat.m$f2.pe[i-1]
#   dat.m$d2.pe[i] <- dat.m$f2.pe[i]-dat.m$f3.pe[i-1]
#   dat.m$d3.pe[i] <- dat.m$f3.pe[i]-dat.m$f4.pe[i-1]
#   dat.m$d4.pe[i] <- dat.m$f4.pe[i]-dat.m$f5.pe[i-1]
#   dat.m$d5.pe[i] <- dat.m$f5.pe[i]-dat.m$f6.pe[i-1]
#   dat.m$d6.pe[i] <- dat.m$f6.pe[i]-dat.m$f7.pe[i-1]
#   dat.m$d7.pe[i] <- dat.m$f7.pe[i]-dat.m$f8.pe[i-1]
#   dat.m$d8.pe[i] <- dat.m$f8.pe[i]-dat.m$f9.pe[i-1]
# }
# 
# dat.m$d1.pl <- NA
# dat.m$d2.pl <- NA
# dat.m$d3.pl <- NA
# dat.m$d4.pl <- NA
# dat.m$d5.pl <- NA
# dat.m$d6.pl <- NA
# dat.m$d7.pl <- NA
# dat.m$d8.pl <- NA
# for(i in 2:nrow(dat.m)){
#   dat.m$d1.pl[i] <- dat.m$f1.pl[i]-dat.m$f2.pl[i-1]
#   dat.m$d2.pl[i] <- dat.m$f2.pl[i]-dat.m$f3.pl[i-1]
#   dat.m$d3.pl[i] <- dat.m$f3.pl[i]-dat.m$f4.pl[i-1]
#   dat.m$d4.pl[i] <- dat.m$f4.pl[i]-dat.m$f5.pl[i-1]
#   dat.m$d5.pl[i] <- dat.m$f5.pl[i]-dat.m$f6.pl[i-1]
#   dat.m$d6.pl[i] <- dat.m$f6.pl[i]-dat.m$f7.pl[i-1]
#   dat.m$d7.pl[i] <- dat.m$f7.pl[i]-dat.m$f8.pl[i-1]
#   dat.m$d8.pl[i] <- dat.m$f8.pl[i]-dat.m$f9.pl[i-1]
# }
# 
# 
# dat.m$yrmo <- substr(dat.m$date,start=1,stop=7)
# 
# dat.enso <- dat.m#merge(dat.m,ensoshocks,by="yrmo")
# 
# dat.enso <- dat.enso[complete.cases(dat.enso),]
# 
# iri_dt <- dat.enso
# 
# # dat.shocks <- data.frame(date=dat.enso$date,yrmo=dat.enso$yrmo,resid=dat.enso$resid,f.e=dat.enso$f.e,f.u=dat.enso$f.u,e.e=dat.enso$e.e,e.u=dat.enso$e.u)
# # 
# # dat.shocks$resid <- dat.shocks$resid/sd(dat.shocks$resid)
# # dat.shocks$f.e <- dat.shocks$f.e/sd(dat.shocks$f.e)
# # dat.shocks$f.u <- dat.shocks$f.u/sd(dat.shocks$f.u)
# # dat.shocks$e.e <- dat.shocks$e.e/sd(dat.shocks$e.e)
# # dat.shocks$e.u <- dat.shocks$e.u/sd(dat.shocks$e.u)
# 
# rm(list=setdiff(ls(),c("iri_dt")))
# 
# save.image("../R/Main/Data/iri_forecasts.RData")



