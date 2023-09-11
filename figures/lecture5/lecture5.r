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


# 4.1 - trends ----

corn_dt <- fread("figures/lecture5/Corn_Yields_USA.csv")

gg_corn <- ggplot(corn_dt[Year>1960 & Year<=2020],aes(x=Year,y=Value))+
  geom_line(linewidth=.8,na.rm=T,color="dimgray")+
  labs(title="Maize (USA)",x="Year",y="Yield (bu/acre)")+
  coord_cartesian(ylim=c(50,200),xlim=c(1960,2023))+
  theme_eg()+
  theme(plot.title=element_text(size=rel(1.1),colour="dimgray"))


life_dt <- fread("figures/lecture5/life-expectancy-at-birth-including-the-un-projections.csv")

gg_life <- ggplot(life_dt[Country=="Japan" & Year>1950 & Year<=2020],aes(x=Year,y=Estimate))+
  geom_line(linewidth=.8,na.rm=T,color="dimgray")+
  labs(title="Life expectancy (Japan)",x="Year",y="Years")+
  coord_cartesian(ylim=c(60,85),xlim=c(1950,2023))+
  theme_eg()+
  theme(plot.title=element_text(size=rel(1.1),colour="dimgray"))


mortgate_dt <- data.table(fredr(series_id="MORTGAGE30US",observation_start=as.Date("1990-01-01"),observation_end=as.Date("2022-12-31"),frequency="m",units="lin"))

# interest_dt <- fread("figures/lecture5/real_interest_rates_10y.csv")

gg_mortgage <- ggplot(mortgate_dt,aes(x=date,y=value))+
  geom_line(linewidth=.8,na.rm=T,color="dimgray")+
  scale_y_continuous(breaks=c(2,4,6,8,10,12))+
  labs(title="Mortgage average (USA)",x="Year",y="Interest rate (%)")+
  coord_cartesian(ylim=c(2,12),xlim=c(as.Date("1990-01-01"),as.Date("2022-12-31")))+
  theme_eg()+
  theme(plot.title=element_text(size=rel(1.1),colour="dimgray"))


gg_combined <- plot_grid(gg_life,gg_mortgage,ncol=2,align="hv",hjust=0,vjust=1)

gg_combined


ggsave("figures/lecture5/trends_combined.png",gg_combined,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# 4.2 - trending process

# 4.3 - linear trend ----

sub_dt <- corn_dt[Year>1960 & Year<=2020]
sub_dt[,Trend:=1:nrow(sub_dt)]
reg <- lm(Value~Trend,data=sub_dt)
sub_dt[,Fitted:=(reg$coefficients["(Intercept)"]+reg$coefficients["Trend"]*Trend)]

sub_lg <- melt(sub_dt[,.(Year,Observed=Value,Fitted)],id.vars="Year",variable.name = "Series",value.name = "Value")

gg_corn <- ggplot(sub_lg,aes(x=Year,y=Value,color=Series,linetype=Series))+
  geom_line(linewidth=.8)+
  scale_color_manual(values=c("dimgray","black"))+
  scale_linetype_manual(values=c(1,5))+
  labs(title="",x="Year",y="Yield (bu/acre)")+
  coord_cartesian(ylim=c(50,200),xlim=c(1960,2023))+
  theme_eg()+
  theme(legend.position="none",legend.key=element_rect(colour="transparent",fill="white"),legend.key.width= unit(2,'lines'))

ggsave("figures/lecture5/corn_fitted.png",gg_corn,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# 4.4 - residuals ----
sub_dt[,Residuals:=Value-Fitted]

gg_resid <- ggplot(sub_dt,aes(x=Year,y=Residuals))+
  geom_line(linewidth=.8,color="dimgray")+
  labs(title="",x="Year",y="Yield residual (bu/acre)")+
  coord_cartesian(ylim=c(-35,35),xlim=c(1960,2023))+
  theme_eg()+
  theme(legend.position="top",legend.key=element_rect(colour="transparent",fill="white"),legend.key.width= unit(2,'lines'))

gg_dots <- ggplot(sub_dt,aes(x=Residuals))+
  geom_dotplot(binwidth=2,color="black",fill="gray",stroke=1,method="histodot",stackratio=1.1)+
  xlim(-35,35)+
  coord_flip()+
  theme_eg()+
  theme(axis.title=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.line=element_blank(),panel.grid.major=element_blank())

# combine the two graphs
gg_comb <- plot_grid(gg_resid,gg_dots,align="hv",ncol=2,rel_widths = c(3,1))

ggsave("figures/lecture5/residuals.png",gg_comb,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# 4.5 - autocorrelogram ----

# obtain autocorrelations
maxlag <- round(sqrt(nrow(sub_dt)))
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(sub_dt$Residuals,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_acf <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(sub_dt)),1.96/sqrt(nrow(sub_dt))),linewidth=.8,linetype=5,col="dimgray")+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="gray")+
  geom_point(shape=21,size=2.5,stroke=.8,color="black",fill="gray")+
  scale_x_continuous(breaks=1:8)+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.25,1))+
  theme_eg()

ggsave("figures/lecture5/autocorrelogram.png",gg_acf,width=6.5,height=6.5*9/16,dpi="retina",device="png")

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

load("figures/lecture5/btc.RData")

h1=as.Date("2022-01-01")

# keep only date and closing price (expressed in thousand dollars)
btc_dt <- btc_dt[,.(date=as.Date(substr(timestamp,1,10)),BTC=close/1000)]
btc_dt[,`:=`(btc_y=ifelse(date<h1,BTC,NA),btc_f=ifelse(date<h1,NA,BTC),btc_rw=ifelse(date<h1,NA,btc_dt[date==h1-1]$BTC))]

btc_dt[,`:=`(h=c(rep(NA,nrow(btc_dt[date<h1])),1:nrow(btc_dt[date>=h1])))]

btc_dt[,`:=`(btc_lo=btc_rw-1.96*sqrt(h)*ifelse(date<h1,NA,sd(diff(btc_dt[date<h1]$BTC))),btc_hi=btc_rw+1.96*sqrt(h)*ifelse(date<h1,NA,sd(diff(btc_dt[date<h1]$BTC))))]

# plot the time series
gg_btc <- ggplot(btc_dt,aes(x=date))+
  geom_ribbon(aes(ymin=btc_lo,ymax=btc_hi),fill="coral",alpha=.2)+
  geom_line(aes(y=btc_y),linewidth=.6,color="dimgray",na.rm=T)+
  geom_line(aes(y=btc_f),linewidth=.6,color="gray",na.rm=T)+
  geom_line(aes(y=btc_lo),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=btc_hi),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=btc_rw),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  labs(x="Year",y="Bitcoin price ('000 USD)")+
  coord_cartesian(ylim=c(0,100))+
  theme_eg()

ggsave("figures/lecture5/btc_forecast.png",gg_btc,width=6.5,height=6.5*9/16,dpi="retina",device="png")




load("figures/lecture5/interest_rates.RData")

interest_rates <- mortgate_dt[,.(date,y=value)]

interest_rates[,`:=`(w=log(y),t=1:nrow(interest_rates))]
interest_rates[,`:=`(haty=fitted(lm(y~t)))]
interest_rates[,`:=`(hatw=fitted(lm(w~t)))]
interest_rates[,`:=`(hatz=exp(hatw+summary(lm(w~t))$sigma^2/2))]

ggplot(interest_rates,aes(x=date,y=y))+
  geom_line(color="dimgray",linewidth=.8)+
  geom_line(aes(y=haty),color="black",linewidth=.8,linetype=5)+
  labs(x="Year",y="Interest Rate (%)")+
  theme_eg()+
  theme(axis.title = element_text(size=22),axis.text = element_text(size=18))

h1 <- as.Date("2010-12-31")

est <- lm(y~t,data=interest_rates[date<=h1])
interest_rates[,`:=`(f=est$coefficients[1]+est$coefficients[2]*t)]

interest_rates[date<=h1]$f <- NA

interest_rates[,`:=`(l=f-1.96*summary(est)$sigma,u=f+1.96*summary(est)$sigma)]

gg_mortgage <- ggplot(interest_rates,aes(x=date,y=y))+
  geom_ribbon(aes(ymin=l,ymax=u),fill="coral",alpha=.2)+
  geom_line(color="dimgray",linewidth=.6)+
  geom_line(data=interest_rates[date>h1],color="gray",linewidth=.6)+
  geom_line(aes(y=f),color="coral",linewidth=.6,linetype=5,na.rm=T)+
  geom_line(aes(y=l),color="coral",linewidth=.4,linetype=2,na.rm=T)+
  geom_line(aes(y=u),color="coral",linewidth=.4,linetype=2,na.rm=T)+
  scale_y_continuous(breaks=c(2,4,6,8,10,12))+
  labs(x="Year",y="Interest rate (%)")+
  coord_cartesian(ylim=c(1,13),xlim=c(as.Date("1990-01-01"),as.Date("2022-12-31")))+
  theme_eg()

ggsave("figures/lecture5/mortgage_forecast.png",gg_mortgage,width=6.5,height=6.5*9/16,dpi="retina",device="png")


h2 <- max(interest_rates$date)

oos <- interest_rates[date>h1]$date

interest_rates[,`:=`(f_t=as.numeric(NA),f_r=as.numeric(NA),f_tl=as.numeric(NA),f_tu=as.numeric(NA))]

for(i in oos){
  
  # linear trend
  est <- lm(y~t,data=interest_rates[date<i])
  interest_rates[date==i,f_t:=est$coefficients[1]+est$coefficients[2]*interest_rates[date==i]$t]
  
  interest_rates[date==i,f_r:=interest_rates[date==max(interest_rates[date<i]$date)]$y]
  
  interest_rates[date==i,`:=`(f_tl=f_t-1.96*summary(est)$sigma,f_tu=f_t+1.96*summary(est)$sigma)]
  
}


gg_mortgage1 <- ggplot(interest_rates,aes(x=date,y=y))+
  geom_ribbon(aes(ymin=f_tl,ymax=f_tu),fill="coral",alpha=.2)+
  geom_line(color="dimgray",linewidth=.6)+
  geom_line(data=interest_rates[date>h1],color="gray",linewidth=.6)+
  geom_line(aes(y=f_t),color="coral",linewidth=.6,linetype=5,na.rm=T)+
  geom_line(aes(y=f_tl),color="coral",linewidth=.4,linetype=2,na.rm=T)+
  geom_line(aes(y=f_tu),color="coral",linewidth=.4,linetype=2,na.rm=T)+
  scale_y_continuous(breaks=c(2,4,6,8,10,12))+
  labs(x="Year",y="Interest rate (%)")+
  coord_cartesian(ylim=c(1,13),xlim=c(as.Date("1990-01-01"),as.Date("2022-12-31")))+
  theme_eg()

ggsave("figures/lecture5/mortgage1_forecast.png",gg_mortgage1,width=6.5,height=6.5*9/16,dpi="retina",device="png")

interest_rates[,`:=`(e_t=y-f_t)]

mz_reg <- lm(e_t~f_t,data=interest_rates)
coeftest(mz_reg,vcov.=vcovHAC(mz_reg))

# obtain autocorrelations
maxlag <- 36
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(interest_rates[date%in%oos]$e_t,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_acf <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(interest_rates[date%in%oos])),1.96/sqrt(nrow(interest_rates[date%in%oos]))),linewidth=.8,linetype=5,col="dimgray")+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="gray")+
  geom_point(shape=21,size=2.5,stroke=.8,color="black",fill="gray")+
  scale_x_continuous(breaks=seq(5,maxlag,5),labels=seq(5,maxlag,5))+
  scale_y_continuous(breaks=seq(-.2,1,.2),labels=sprintf("%.1f",round(seq(-.2,1,.2),1)))+
  labs(x="k",y=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.2,1),xlim=c(1.5,maxlag-0.5))+
  theme_eg()

gg_acf

ggsave("figures/lecture5/ac_mortgage.png",gg_acf,width=6.5,height=6.5*9/16,dpi="retina",device="png")



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



r <- 1000
n <- 120
tr <- 1:n

set.seed(1)
y <- 0.3*tr+rnorm(n)
set.seed(n)
x <- 0.5*tr+rnorm(n)

dt <- data.table(tr=tr,x=x,y=y)
dl <- melt(dt,id.vars="tr")

gg_spurious_d <- ggplot(dl,aes(x=tr,y=value,color=variable,linetype=variable)) +
  geom_line(linewidth=.8) +
  scale_color_manual(values=c("dimgray","coral"))+
  scale_linetype_manual(values=c(1,5))+
  labs(x="t",y=expression(paste(y[t],", ",x[t],sep=""))) +
  theme_eg()

ggsave("figures/lecture5/spurious_d.png",gg_spurious_d,width=6.5,height=6.5*9/16,dpi="retina",device="png")


set.seed(1)
y <- cumsum(rnorm(n))
set.seed(n)
x <- cumsum(rnorm(n))

dt <- data.table(tr=tr,x=x,y=y)
dl <- melt(dt,id.vars="tr")

gg_spurious_s <- ggplot(dl,aes(x=tr,y=value,color=variable,linetype=variable)) +
  geom_line(linewidth=.8) +
  scale_color_manual(values=c("dimgray","coral"))+
  scale_linetype_manual(values=c(1,5))+
  labs(x="t",y=expression(paste(y[t],", ",x[t],sep=""))) +
  theme_eg()

ggsave("figures/lecture5/spurious_s.png",gg_spurious_s,width=6.5,height=6.5*9/16,dpi="retina",device="png")