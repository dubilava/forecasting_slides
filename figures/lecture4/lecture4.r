# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(camcorder)
library(extrafont)
# font_import()
loadfonts(device="win",quiet=T)
library(cowplot)
library(crypto2)
library(fredr)
fredr_set_key("7a1db535f59c2ac4382b9c22a15b5f06")

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
    # panel.grid.major.x=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill="white",color=NA),
    plot.title=element_text(family=base_family,size=rel(1.2),colour="dimgray"),
    plot.caption=element_text(family=base_family,colour="darkgray"),
    plot.margin=margin(.25,.25,.25,.25,"lines"),
    axis.title=element_text(family=base_family,face="bold",size=rel(1.3),colour="dimgray"),
    # axis.title.x=element_text(),
    axis.text=element_text(family=base_family,size=rel(1.1),colour="dimgray",margin=margin(t=1,r=1,b=1,l=1)),
    # axis.line=element_line(colour="dimgray"),
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

# 2.1 - time series ----

# generate a sample of time series
n <- 12

set.seed(17)
x <- rnorm(n,2,1.5)
for(i in 2:n) x[i] <- .3*x[i-1]+x[i]
dt <- data.table(x=x,t=(1-4):(n-4))

# graph the time series
gg_ts <- ggplot(dt,aes(x=t,y=x))+
  geom_line(data=dt[t>0 & t<=6],color="black",linetype=1,linewidth=.8)+
  geom_line(data=dt[t<=1],color="gray",linetype=5,linewidth=.8)+
  geom_line(data=dt[t>5],color="gray",linetype=5,linewidth=.8)+
  geom_point(data=dt[t>0 & t<=6],color="black",fill="gray",stroke = 1,shape=21,size=3)+
  geom_point(data=dt[t<=0 | t>6],color="gray",fill="gray",stroke = 1,shape=21,size=3)+
  ylim(0,7)+
  labs(x=expression(t),y=expression(y[t]))+
  theme_eg()

gg_ts

ggsave("figures/lecture4/tseries.png",gg_ts,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# 2.2 - stationarity ----

# generate a sample of time series
n <- 16

set.seed(133)
x <- rnorm(n,3,1.5)
for(i in 2:n) x[i] <- .2*x[i-1]+x[i]
dt <- data.table(x=round(x),t=1:n)

# graph the time series
gg_ts1 <- ggplot(dt[t<=5],aes(x=t,y=x))+
  geom_line(color="black",linetype=5,linewidth=.8)+
  geom_point(color="black",fill="gray",stroke = 1,shape=21,size=3)+
  ylim(0,8)+
  labs(x=expression(t),y=expression(y[t]))+
  scale_x_continuous(breaks=c(1:5),labels=c(1:5),limits=c(1,6))+
  theme_eg()+
  theme(panel.background=element_rect(fill="transparent",color=NA),plot.background=element_rect(fill="transparent",color=NA))#,plot.margin=margin(.5,0,.2,0,"cm"))

# graph the dot-density (stationary)
gg_hist <- ggplot(dt,aes(x=x))+
  geom_dotplot(binwidth=.3,color=NA,fill="gray",stroke=2)+
  xlim(0,8)+
  coord_flip()+
  theme_eg()+
  theme(panel.background=element_rect(fill="transparent",color=NA),plot.background=element_rect(fill="transparent",color=NA),axis.title=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.line=element_blank(),panel.grid=element_blank(),panel.grid.major=element_blank())#,plot.margin=margin(.5,0,.2,0,"cm"))

# align the graphs to illustrate stationary process
aligned_plots <- align_plots(gg_hist,gg_ts1,align="hv",axis="tblr")

gg_stationary <- ggdraw(aligned_plots[[1]]) + 
  draw_plot(aligned_plots[[1]],x=1/6+0.001,y=0,width=1,height=1) + 
  draw_plot(aligned_plots[[1]],x=2/6+0.0025,y=0,width=1,height=1) + 
  draw_plot(aligned_plots[[1]],x=3/6+0.004,y=0,width=1,height=1) + 
  draw_plot(aligned_plots[[1]],x=4/6+0.0055,y=0,width=1,height=1) + 
  draw_plot(aligned_plots[[2]],x=0.007,y=0,width=1,height=1)

ggsave("figures/lecture4/stationary.png",gg_stationary,width=6.5,height=6.5*9/16,dpi="retina",device="png")



# graph the time series
gg_ts2 <- ggplot(dt[t<=5],aes(x=t,y=x))+
  geom_line(color="black",linetype=5,linewidth=.8)+
  geom_point(color="black",fill="gray",stroke = 1,shape=21,size=3)+
  ylim(0,8)+
  labs(x=expression(t),y=expression(y[t]))+
  scale_x_continuous(breaks=c(1:5),labels=c(1:5),limits=c(1,6))+
  theme_eg()+
  theme(panel.background=element_rect(fill="transparent",color=NA),plot.background=element_rect(fill="transparent",color=NA))#,plot.margin=margin(.5,0,.2,0,"cm"))

# graph the dot-densities (nonstationary)
gg_h1 <- ggplot(dt,aes(x=x-2))+
  geom_dotplot(binwidth=.3,color=NA,fill="gray",stroke = 2)+
  xlim(0,8)+
  coord_flip()+
  theme_eg()+
  theme(panel.background=element_rect(fill="transparent",color=NA),plot.background=element_rect(fill="transparent",color=NA),axis.title=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.line=element_blank(),panel.grid=element_blank(),panel.grid.major=element_blank())#,plot.margin=margin(.5,0,.2,0,"cm"))

gg_h2 <- ggplot(dt,aes(x=x-1))+
  geom_dotplot(binwidth=.3,color=NA,fill="gray",stroke = 2)+
  xlim(0,8)+
  coord_flip()+
  theme_eg()+
  theme(panel.background=element_rect(fill="transparent",color=NA),plot.background=element_rect(fill="transparent",color=NA),axis.title=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.line=element_blank(),panel.grid=element_blank(),panel.grid.major=element_blank())#,plot.margin=margin(.5,0,.2,0,"cm"))

gg_h3 <- ggplot(dt,aes(x=x-0))+
  geom_dotplot(binwidth=.3,color=NA,fill="gray",stroke = 2)+
  xlim(0,8)+
  coord_flip()+
  theme_eg()+
  theme(panel.background=element_rect(fill="transparent",color=NA),plot.background=element_rect(fill="transparent",color=NA),axis.title=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.line=element_blank(),panel.grid=element_blank(),panel.grid.major=element_blank())#,plot.margin=margin(.5,0,.2,0,"cm"))

gg_h4 <- ggplot(dt,aes(x=x+1))+
  geom_dotplot(binwidth=.3,color=NA,fill="gray",stroke = 2)+
  xlim(0,8)+
  coord_flip()+
  theme_eg()+
  theme(panel.background=element_rect(fill="transparent",color=NA),plot.background=element_rect(fill="transparent",color=NA),axis.title=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.line=element_blank(),panel.grid=element_blank(),panel.grid.major=element_blank())#,plot.margin=margin(.5,0,.2,0,"cm"))

gg_h5 <- ggplot(dt,aes(x=x+2))+
  geom_dotplot(binwidth=.3,color=NA,fill="gray",stroke = 2)+
  xlim(0,8)+
  coord_flip()+
  theme_eg()+
  theme(panel.background=element_rect(fill="transparent",color=NA),plot.background=element_rect(fill="transparent",color=NA),axis.title=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.line=element_blank(),panel.grid=element_blank(),panel.grid.major=element_blank())#,plot.margin=margin(.5,0,.2,0,"cm"))

# align the graphs to illustrate the nonstationary process
aligned_plots <- align_plots(gg_h1,gg_h2,gg_h3,gg_h4,gg_h5,gg_ts2,align="hv",axis="tblr")

gg_nonstationary <- ggdraw(aligned_plots[[1]]) + 
  draw_plot(aligned_plots[[2]],x=1/6+0.001,y=0,width=1,height=1) + 
  draw_plot(aligned_plots[[3]],x=2/6+0.0025,y=0,width=1,height=1) + 
  draw_plot(aligned_plots[[4]],x=3/6+0.004,y=0,width=1,height=1) + 
  draw_plot(aligned_plots[[5]],x=4/6+0.0055,y=0,width=1,height=1) + 
  draw_plot(aligned_plots[[6]],x=0.007,y=0,width=1,height=1)


ggsave("figures/lecture4/nonstationary.png",gg_nonstationary,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# # combine the two graphs
# gg_combined <- plot_grid(gg_stationary,gg_nonstationary,align="hv",ncol=1,hjust=0,vjust=1)
# 
# ggsave("figures/c2/stochastic.png",gg_combined,width=6.5,height=6.5,dpi="retina",device="png")
# ggsave("figures/c2/stochastic.eps",gg_combined,width=6.5,height=6.5,dpi="retina",device=cairo_ps)


# 2.3 - white noise ----
n <- 120
set.seed(1)
x <- rnorm(n)
wn_dt <- data.table(t=1:n,x=x)

# plot the time series
gg_wn <- ggplot(wn_dt,aes(x=t,y=x))+
  geom_line(linewidth=.5,color="black")+
  ylim(-2.5,2.5)+
  xlim(0,125)+
  labs(x="t",y=expression(y[t]))+
  theme_eg()

# graph the dot-density of the series
gg_dots <- ggplot(wn_dt,aes(x=x))+
  geom_dotplot(binwidth=.14,color="black",fill="gray",stroke=1,method="histodot",stackratio=1.1)+
  xlim(-2.5,2.5)+
  coord_flip()+
  theme_eg()+
  theme(axis.title=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.line=element_blank(),panel.grid.major=element_blank())

# combine the two graphs
gg_comb <- plot_grid(gg_wn,gg_dots,align="hv",ncol=2,rel_widths = c(3,1))

gg_comb

ggsave("figures/lecture4/wn.png",gg_comb,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# 2.4 - time series (unrate) ----

# # u.s. unemployment rates
# unrate_dt <- as.data.table(fredr(
#   series_id = "UNRATENSA",
#   observation_start = as.Date("1980-01-01"),
#   observation_end = as.Date("2022-12-31"),
#   frequency = "m",
#   aggregation_method = "avg"
# ))
# 
# # store the dataset as the data.table object
# unrate_dt <- unrate_dt[,.(date,y=value)]
# save(unrate_dt,file="data/c2/unrate.RData")

load("data/c2/unrate.RData")


sub_dt <- unrate_dt[date<="2019-12-31"]

# plot the time series
gg_ur <- ggplot(sub_dt,aes(x=date,y=y))+
  geom_line(linewidth=0.6,color="black")+
  labs(x="Year",y="Unemployment rate (%)")+
  theme_eg()

ggsave("figures/c2/unrate.png",gg_ur,width=6.5,height=3.75,dpi="retina",device="png")
ggsave("figures/c2/unrate.eps",gg_ur,width=6.5,height=3.75,dpi="retina",device=cairo_ps)


# 2.5 - correlations (unrate) ----
sub_dt[,`:=`(y1=shift(y,1),y12=shift(y,12))]

# plot the time series
gg_ur1 <- ggplot(sub_dt,aes(x=y1,y=y))+
  geom_point(shape=21,size=1,color="black",fill="gray",na.rm=T)+
  labs(title="(a) lag 1 autocorrelation",x=expression(u[t-1]),y=expression(u[t]))+
  theme_eg()

gg_ur12 <- ggplot(sub_dt,aes(x=y12,y=y))+
  geom_point(shape=21,size=1,color="black",fill="gray",na.rm=T)+
  labs(title="(a) lag 12 autocorrelation",x=expression(u[t-12]),y=expression(u[t]))+
  theme_eg()

gg_comb <- plot_grid(gg_ur1,gg_ur12,align="hv",ncol=2)

ggsave("figures/c2/unratecor.png",gg_comb,width=6.5,height=3.75,dpi="retina",device="png")
ggsave("figures/c2/unratecor.eps",gg_comb,width=6.5,height=3.75,dpi="retina",device=cairo_ps)


# 2.6 - autocorrelation (wn) ----

# obtain autocorrelations
maxlag <- 30
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(wn_dt$x,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_acf <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(wn_dt)),1.96/sqrt(nrow(wn_dt))),linewidth=.8,linetype=5,col="dimgray")+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="gray")+
  geom_point(shape=21,size=2.5,stroke=.8,color="black",fill="gray")+
  scale_x_continuous(breaks=seq(5,maxlag,5),labels=seq(5,maxlag,5))+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.25,1),xlim=c(1.5,maxlag-0.5))+
  theme_eg()

ggsave("figures/c2/autocor_wn.png",gg_acf,width=6.5,height=3.75,dpi="retina",device="png")
ggsave("figures/c2/autocor_wn.eps",gg_acf,width=6.5,height=3.75,dpi="retina",device=cairo_ps)


# 2.7 - bitcoin series ----

# # load all active coins
# coins <- crypto_list()
# 
# # store as the data.table object
# coins_dt <- data.table(coins)
# 
# # select the coin of interest -- Bitcoin
# coins_sub_dt <- coins_dt[symbol=="BTC"]
# 
# # fetch the historical data beginning from 1 Jan 2020
# btc_tb <- crypto_history(coin_list = coins_sub_dt,start_date="20200101",end_date="20221231")
# 
# # reformat the dates
# btc_tb$timestamp <- as.POSIXct(btc_tb$timestamp,format="%Y-%m-%d")
# btc_tb$time_open <- as.POSIXct(btc_tb$time_open,format="%Y-%m-%d")
# btc_tb$time_close <- as.POSIXct(btc_tb$time_close,format="%Y-%m-%d")
# btc_tb$time_high <- as.POSIXct(btc_tb$time_high,format="%Y-%m-%d")
# btc_tb$time_low <- as.POSIXct(btc_tb$time_low,format="%Y-%m-%d")
# 
# # store the dataset as the data.table object
# btc_dt <- data.table(btc_tb)
# save(btc_dt,file="data/c2/btc.RData")

load("data/c2/btc.RData")

# keep only date and closing price (expressed in thousand dollars)
btc_dt <- btc_dt[,.(date=as.Date(substr(timestamp,1,10)),BTC=close/1000)]

# plot the time series
gg_btc <- ggplot(btc_dt,aes(x=date,y=BTC))+
  geom_line(linewidth=.6,color="black")+
  labs(x="Year",y="Bitcoin price ('000 USD)")+
  theme_eg()

ggsave("figures/c2/bitcoin.png",gg_btc,width=6.5,height=3.75,dpi="retina",device="png")
ggsave("figures/c2/bitcoin.eps",gg_btc,width=6.5,height=3.75,dpi="retina",device=cairo_ps)


# 2.8 - autocorrelation (btc) ----
# obtain autocorrelations
maxlag <- 30
btcacf_dt <- data.table(k=c(1:maxlag),rho=c(acf(btc_dt$BTC,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_btcacf <- ggplot(btcacf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(btc_dt)),1.96/sqrt(nrow(btc_dt))),linewidth=.8,linetype=5,col="dimgray")+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="gray")+
  geom_point(shape=21,size=2.5,stroke=.8,color="black",fill="gray")+
  scale_x_continuous(breaks=seq(5,maxlag,5),labels=seq(5,maxlag,5))+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.25,1),xlim=c(1.5,maxlag-0.5))+
  theme_eg()

ggsave("figures/c2/autocor_btc.png",gg_btcacf,width=6.5,height=3.75,dpi="retina",device="png")
ggsave("figures/c2/autocor_btc.eps",gg_btcacf,width=6.5,height=3.75,dpi="retina",device=cairo_ps)


# 2.9 - autocorrelation mediation----

# set up the coordinates for nodes and edges
nudge <- .05
dt <- data.table(x=c(0,1,2),y=c(0-nudge,0-nudge,0-nudge),xbeg=c(1-2*nudge,2-2*nudge,2-nudge),xend=c(0+2*nudge,1+2*nudge,0+nudge),ybeg=c(0-nudge,0-nudge,0-.5*nudge),yend=c(0-nudge,0-nudge,0-.5*nudge),labels=c("Y[t]","Y[t-1]","Y[t-2]"))

# plot the graph
gg_dag <- ggplot(dt)+
  geom_text(aes(x=x,y=y,label=labels),size=6,parse=T,color=c("black","black","black"),na.rm=T)+
  geom_segment(data=dt[1:2,],aes(x=xbeg,y=ybeg,xend=xend,yend=yend),lineend = c("round"),linejoin = c("round"),arrow=arrow(type="closed",angle=30,length=unit(0.1,"in")),linewidth=1,linetype=c(1,1),color=c("black","black"),na.rm=T)+
  geom_curve(data=dt[3,],aes(x=xbeg,y=ybeg,xend=xend,yend=yend),curvature=.4,arrow=arrow(type="closed",angle=30,length=unit(0.1,"in")),linewidth=1,linetype=5,color="black")+
  geom_segment(data=data.table(xbeg=0.1,ybeg=0-.25*nudge,xend=0+nudge,yend=0-.5*nudge),aes(x=xbeg,y=ybeg,xend=xend,yend=yend),arrow=arrow(type="closed",angle=30,length=unit(0.1,"in")),linewidth=1,linetype=1,color="black")+
  ylim(0-1.5*nudge,0+2.5*nudge)+
  coord_cartesian(clip="off")+
  theme_eg()+
  theme(axis.title=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.line=element_blank(),panel.grid.major = element_blank())


ggsave("figures/c2/dag_autocor.png",gg_dag,width=6.5,height=2.0,dpi="retina",device="png")
ggsave("figures/c2/dag_autocor.eps",gg_dag,width=6.5,height=2.0,dpi="retina",device=cairo_ps)


# 2.10 - partial autocorrelation (btc)----
btcpacf_dt <- data.table(k=c(1:maxlag),rho=c(pacf(btc_dt$BTC,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_btcpacf <- ggplot(btcpacf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(btc_dt)),1.96/sqrt(nrow(btc_dt))),linewidth=.8,linetype=5,col="dimgray")+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="gray")+
  geom_point(shape=21,size=2.5,stroke=.8,color="black",fill="gray")+
  scale_x_continuous(breaks=seq(5,maxlag,5),labels=seq(5,maxlag,5))+
  labs(x="k",y=expression(hat(pi)[k]))+
  coord_cartesian(ylim=c(-.25,1),xlim=c(1.5,maxlag-0.5))+
  theme_eg()

ggsave("figures/c2/pautocor_btc.png",gg_btcpacf,width=6.5,height=3.75,dpi="retina",device="png")
ggsave("figures/c2/pautocor_btc.eps",gg_btcpacf,width=6.5,height=3.75,dpi="retina",device=cairo_ps)


# 2.11 - first differenced ----

btc_dt[,`:=`(lnBTC=log(BTC))]
btc_dt[,`:=`(BTC1=shift(BTC),lnBTC1=shift(lnBTC))]
btc_dt[,`:=`(dBTC=BTC-BTC1,rBTC=(lnBTC-lnBTC1)*100)]

# plot the time series
gg_ts <- ggplot(btc_dt,aes(x=date,y=dBTC))+
  geom_line(linewidth=.6,color="black",na.rm=T)+
  labs(x="Year",y="Change in price ('000 USD)")+
  coord_cartesian(ylim=c(-8,8))+
  theme_eg()

ggsave("figures/c2/bitcoin_change.png",gg_ts,width=6.5,height=3.75,dpi="retina",device="png")
ggsave("figures/c2/bitcoin_change.eps",gg_ts,width=6.5,height=3.75,dpi="retina",device=cairo_ps)

# 2.12 - rate of change ----

# plot the time series
gg_ts <- ggplot(btc_dt,aes(x=date,y=rBTC))+
  geom_line(linewidth=.6,color="black",na.rm=T)+
  labs(x="Year",y="Return (%)")+
  coord_cartesian(ylim=c(-25,25))+
  theme_eg()

ggsave("figures/c2/bitcoin_return.png",gg_ts,width=6.5,height=3.75,dpi="retina",device="png")
ggsave("figures/c2/bitcoin_return.eps",gg_ts,width=6.5,height=3.75,dpi="retina",device=cairo_ps)




