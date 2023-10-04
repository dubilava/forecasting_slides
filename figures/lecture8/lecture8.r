# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(extrafont)
loadfonts(device="win",quiet=T)
library(cowplot)
library(crypto2)
library(fredr)
fredr_set_key("7a1db535f59c2ac4382b9c22a15b5f06")
library(stringr)
# library(readabs)
# Sys.setenv(R_READABS_PATH="figures/lecture8")
library(lmtest)
library(sandwich)
library(forecast)

library(fastDummies)

rm(list=ls())
gc()

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


n=180
mu=5
set.seed(1)
e <- rnorm(n,0,.5)

theta=.5
y <- e
for(i in 2:n){
  y[i] <- mu+e[i]+theta*e[i-1]
}
y[1] <- mu+e[1]
dt <- data.table(t=1:n,y)

maxlag <- 12
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(y,plot=F)[1:maxlag]$acf),pi=c(pacf(y,plot=F)[1:maxlag]$acf))

gg1 <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))

gg2 <- ggplot(acf_dt,aes(x=k,y=pi))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(pi)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_ts1 <- ggplot(dt,aes(x=t,y=y))+
  geom_line(linewidth=.6,color="coral")+
  labs(x="t",y=expression(y[t])) +
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_all1 <- ggdraw() +
  draw_plot(gg1,    x = 0, y =.5, width =.4, height =.5) +
  draw_plot(gg2,    x = 0, y = 0, width =.4, height =.5) +
  draw_plot(gg_ts1, x =.4, y = 0, width =.6, height = 1)

ggsave("figures/lecture8/ma1a.png",gg_all1,width=6.5,height=6.5*9/16,dpi="retina",device="png")



theta=-1

y <- e
for(i in 2:n){
  y[i] <- mu+e[i]+theta*e[i-1]
}
y[1] <- mu+e[1]
dt <- data.table(t=1:n,y)

maxlag <- 12
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(y,plot=F)[1:maxlag]$acf),pi=c(pacf(y,plot=F)[1:maxlag]$acf))

gg1 <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))

gg2 <- ggplot(acf_dt,aes(x=k,y=pi))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(pi)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_ts1 <- ggplot(dt,aes(x=t,y=y))+
  geom_line(linewidth=.6,color="coral")+
  labs(x="t",y=expression(y[t])) +
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_all2 <- ggdraw() +
  draw_plot(gg1,    x = 0, y =.5, width =.4, height =.5) +
  draw_plot(gg2,    x = 0, y = 0, width =.4, height =.5) +
  draw_plot(gg_ts1, x =.4, y = 0, width =.6, height = 1)

ggsave("figures/lecture8/ma1b.png",gg_all2,width=6.5,height=6.5*9/16,dpi="retina",device="png")






theta=2

y <- e
for(i in 2:n){
  y[i] <- mu+e[i]+theta*e[i-1]
}
y[1] <- mu+e[1]
dt <- data.table(t=1:n,y)

maxlag <- 12
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(y,plot=F)[1:maxlag]$acf),pi=c(pacf(y,plot=F)[1:maxlag]$acf))

gg1 <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))

gg2 <- ggplot(acf_dt,aes(x=k,y=pi))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(pi)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_ts1 <- ggplot(dt,aes(x=t,y=y))+
  geom_line(linewidth=.6,color="coral")+
  labs(x="t",y=expression(y[t])) +
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_all3 <- ggdraw() +
  draw_plot(gg1,    x = 0, y =.5, width =.4, height =.5) +
  draw_plot(gg2,    x = 0, y = 0, width =.4, height =.5) +
  draw_plot(gg_ts1, x =.4, y = 0, width =.6, height = 1)

ggsave("figures/lecture8/ma1c.png",gg_all3,width=6.5,height=6.5*9/16,dpi="retina",device="png")





theta1=0.5
theta2=0.5
y <- e
for(i in 3:n){
  y[i] <- mu+e[i]+theta1*e[i-1]+theta2*e[i-2]
}
y[1] <- mu+e[1]
y[2] <- mu+e[2]
dt <- data.table(t=1:n,y)

maxlag <- 12
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(y,plot=F)[1:maxlag]$acf),pi=c(pacf(y,plot=F)[1:maxlag]$acf))

gg1 <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))

gg2 <- ggplot(acf_dt,aes(x=k,y=pi))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(pi)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_ts1 <- ggplot(dt,aes(x=t,y=y))+
  geom_line(linewidth=.6,color="coral")+
  labs(x="t",y=expression(y[t])) +
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_all3 <- ggdraw() +
  draw_plot(gg1,    x = 0, y =.5, width =.4, height =.5) +
  draw_plot(gg2,    x = 0, y = 0, width =.4, height =.5) +
  draw_plot(gg_ts1, x =.4, y = 0, width =.6, height = 1)

ggsave("figures/lecture8/ma2a.png",gg_all3,width=6.5,height=6.5*9/16,dpi="retina",device="png")



theta1=-1
theta2=0.5
y <- e
for(i in 3:n){
  y[i] <- mu+e[i]+theta1*e[i-1]+theta2*e[i-2]
}
y[1] <- mu+e[1]
y[2] <- mu+e[2]
dt <- data.table(t=1:n,y)

maxlag <- 12
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(y,plot=F)[1:maxlag]$acf),pi=c(pacf(y,plot=F)[1:maxlag]$acf))

gg1 <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))

gg2 <- ggplot(acf_dt,aes(x=k,y=pi))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(pi)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_ts1 <- ggplot(dt,aes(x=t,y=y))+
  geom_line(linewidth=.6,color="coral")+
  labs(x="t",y=expression(y[t])) +
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_all3 <- ggdraw() +
  draw_plot(gg1,    x = 0, y =.5, width =.4, height =.5) +
  draw_plot(gg2,    x = 0, y = 0, width =.4, height =.5) +
  draw_plot(gg_ts1, x =.4, y = 0, width =.6, height = 1)

ggsave("figures/lecture8/ma2b.png",gg_all3,width=6.5,height=6.5*9/16,dpi="retina",device="png")


theta1=1
theta2=-0.5
y <- e
for(i in 3:n){
  y[i] <- mu+e[i]+theta1*e[i-1]+theta2*e[i-2]
}
y[1] <- mu+e[1]
y[2] <- mu+e[2]
dt <- data.table(t=1:n,y)

maxlag <- 12
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(y,plot=F)[1:maxlag]$acf),pi=c(pacf(y,plot=F)[1:maxlag]$acf))

gg1 <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))

gg2 <- ggplot(acf_dt,aes(x=k,y=pi))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(pi)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_ts1 <- ggplot(dt,aes(x=t,y=y))+
  geom_line(linewidth=.6,color="coral")+
  labs(x="t",y=expression(y[t])) +
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_all3 <- ggdraw() +
  draw_plot(gg1,    x = 0, y =.5, width =.4, height =.5) +
  draw_plot(gg2,    x = 0, y = 0, width =.4, height =.5) +
  draw_plot(gg_ts1, x =.4, y = 0, width =.6, height = 1)

ggsave("figures/lecture8/ma2c.png",gg_all3,width=6.5,height=6.5*9/16,dpi="retina",device="png")



load("figures/lecture8/btc.RData")

# keep only date and closing price (expressed in thousand dollars)
btc_dt <- btc_dt[,.(date=as.Date(substr(timestamp,1,10)),BTC=close/1000)]

btc_dt[,`:=`(lnBTC=log(BTC))]
btc_dt[,`:=`(BTC1=shift(BTC,7),lnBTC1=shift(lnBTC,7))]
btc_dt[,`:=`(dBTC=BTC-BTC1,rBTC=(lnBTC-lnBTC1)*100)]
btc_dt <- btc_dt[complete.cases(btc_dt)]

y <- btc_dt$rBTC

maxlag <- 12
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(y,plot=F)[1:maxlag]$acf),pi=c(pacf(y,plot=F)[1:maxlag]$acf))

gg1 <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))

gg2 <- ggplot(acf_dt,aes(x=k,y=pi))+
  geom_hline(yintercept=c(-1.96/sqrt(n),1.96/sqrt(n)),linewidth=.6,linetype=5,col="gray")+
  geom_segment(aes(xend=k,yend=0),linewidth=.8,col="dimgray")+
  geom_point(shape=21,size=1,stroke=.5,color="white",fill="dimgray")+
  scale_x_continuous(breaks=seq(2,maxlag,2),labels=seq(2,maxlag,2))+
  scale_y_continuous(breaks=seq(-.6,1,.2),labels=sprintf("%.1f",round(seq(-.6,1,.2),1)))+
  labs(x="k",y=expression(hat(pi)[k]))+
  coord_cartesian(ylim=c(-.6,1),xlim=c(1,maxlag))+
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_ts1 <- ggplot(btc_dt,aes(x=date,y=rBTC))+
  geom_line(linewidth=.6,color="coral")+
  labs(x="t",y=expression(y[t])) +
  theme_eg()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8))


gg_btc <- ggdraw() +
  draw_plot(gg1,    x = 0, y =.5, width =.4, height =.5) +
  draw_plot(gg2,    x = 0, y = 0, width =.4, height =.5) +
  draw_plot(gg_ts1, x =.4, y = 0, width =.6, height = 1)

ggsave("figures/lecture8/btc.png",gg_btc,width=6.5,height=6.5*9/16,dpi="retina",device="png")





dt <- data.table(q=1:9,AIC=NA,SIC=NA)

ma1 <- arima(y,order=c(0,0,1))
ma2 <- arima(y,order=c(0,0,2))
ma3 <- arima(y,order=c(0,0,3))
ma4 <- arima(y,order=c(0,0,4))
ma5 <- arima(y,order=c(0,0,5))
ma6 <- arima(y,order=c(0,0,6))
ma7 <- arima(y,order=c(0,0,7))
ma8 <- arima(y,order=c(0,0,8))
ma9 <- arima(y,order=c(0,0,9))

dt$AIC[1] <- log(sum(ma1$resid^2,na.rm=T))+2*length(ma1$coef)/length(ma1$resid)
dt$AIC[2] <- log(sum(ma2$resid^2,na.rm=T))+2*length(ma2$coef)/length(ma2$resid)
dt$AIC[3] <- log(sum(ma3$resid^2,na.rm=T))+2*length(ma3$coef)/length(ma3$resid)
dt$AIC[4] <- log(sum(ma4$resid^2,na.rm=T))+2*length(ma4$coef)/length(ma4$resid)
dt$AIC[5] <- log(sum(ma5$resid^2,na.rm=T))+2*length(ma5$coef)/length(ma5$resid)
dt$AIC[6] <- log(sum(ma6$resid^2,na.rm=T))+2*length(ma6$coef)/length(ma6$resid)
dt$AIC[7] <- log(sum(ma7$resid^2,na.rm=T))+2*length(ma7$coef)/length(ma7$resid)
dt$AIC[8] <- log(sum(ma8$resid^2,na.rm=T))+2*length(ma8$coef)/length(ma8$resid)
dt$AIC[9] <- log(sum(ma9$resid^2,na.rm=T))+2*length(ma9$coef)/length(ma9$resid)

dt$SIC[1] <- log(sum(ma1$resid^2,na.rm=T))+log(length(ma1$resid))*length(ma1$coef)/length(ma1$resid)
dt$SIC[2] <- log(sum(ma2$resid^2,na.rm=T))+log(length(ma2$resid))*length(ma2$coef)/length(ma2$resid)
dt$SIC[3] <- log(sum(ma3$resid^2,na.rm=T))+log(length(ma3$resid))*length(ma3$coef)/length(ma3$resid)
dt$SIC[4] <- log(sum(ma4$resid^2,na.rm=T))+log(length(ma4$resid))*length(ma4$coef)/length(ma4$resid)
dt$SIC[5] <- log(sum(ma5$resid^2,na.rm=T))+log(length(ma5$resid))*length(ma5$coef)/length(ma5$resid)
dt$SIC[6] <- log(sum(ma6$resid^2,na.rm=T))+log(length(ma6$resid))*length(ma6$coef)/length(ma6$resid)
dt$SIC[7] <- log(sum(ma7$resid^2,na.rm=T))+log(length(ma7$resid))*length(ma7$coef)/length(ma7$resid)
dt$SIC[8] <- log(sum(ma8$resid^2,na.rm=T))+log(length(ma8$resid))*length(ma8$coef)/length(ma8$resid)
dt$SIC[9] <- log(sum(ma9$resid^2,na.rm=T))+log(length(ma9$resid))*length(ma9$coef)/length(ma9$resid)



start_date <- as.Date("2021-05-20")
cutoff_date <- as.Date("2021-06-30")
end_date <- as.Date("2021-08-10")

r <- btc_dt[date<=cutoff_date]$rBTC
ma1 <- arima(r,order=c(0,0,7))

ma1f <- forecast(ma1,h=length(btc_dt[date>cutoff_date]$rBTC),level=95)

btc_dt$f <- c(rep(NA,length(btc_dt[date<=cutoff_date]$rBTC)),as.matrix(ma1f$mean))
btc_dt$l <- c(rep(NA,length(btc_dt[date<=cutoff_date]$rBTC)),as.matrix(ma1f$lower))
btc_dt$u <- c(rep(NA,length(btc_dt[date<=cutoff_date]$rBTC)),as.matrix(ma1f$upper))


gg_btcfor <- ggplot(btc_dt[date>=start_date & date<=end_date],aes(x=date))+
  geom_ribbon(aes(ymin=l,ymax=u),fill="coral",alpha=.2)+
  geom_line(aes(y=rBTC),linewidth=.6,color="dimgray",na.rm=T)+
  geom_line(data=btc_dt[date>cutoff_date & date<=end_date],aes(y=rBTC),color="gray",linewidth=.6)+
  geom_line(aes(y=l),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=u),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=f),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  scale_y_continuous(breaks=seq(-30,30,10))+
  labs(x="2021",y="Bitcoin Returns (%)")+
  coord_cartesian(ylim=c(-30,30),xlim=c(start_date,end_date))+
  theme_eg()


ggsave("figures/lecture8/btc_forecast.png",gg_btcfor,width=6.5,height=6.5*9/16,dpi="retina",device="png")

                                                                  
                                                                           

