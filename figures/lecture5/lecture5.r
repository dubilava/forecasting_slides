# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
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


# 5.1 - trends ----

corn_dt <- fread("figures/lecture5/Corn_Yields_USA.csv")

gg_corn <- ggplot(corn_dt[Year>1950 & Year<=2020],aes(x=Year,y=Value))+
  geom_line(linewidth=.8,na.rm=T,color="dimgray")+
  labs(y="",x="Year",subtitle="USA maize yield (bu/acre)")+
  coord_cartesian(ylim=c(20,200),xlim=c(1950,2022))+
  theme_eg()


life_dt <- fread("figures/lecture5/life-expectancy-at-birth-including-the-un-projections.csv")

gg_life <- ggplot(life_dt[Country=="Japan" & Year>1950 & Year<=2020],aes(x=Year,y=Estimate))+
  geom_line(linewidth=.8,na.rm=T,color="dimgray")+
  labs(y="",x="Year",subtitle="Life expectancy in Japan (years)")+
  coord_cartesian(ylim=c(54,90),xlim=c(1950,2022))+
  theme_eg()


gg_combined <- plot_grid(gg_life,gg_corn,ncol=2,align="hv",hjust=0,vjust=1)

gg_combined

ggsave("figures/lecture5/trends_combined.png",gg_combined,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# 5.2 - linear trend ----

sub_dt <- corn_dt[Year>1950 & Year<=2020]
sub_dt[,Trend:=1:nrow(sub_dt)]
reg <- lm(Value~Trend,data=sub_dt)
sub_dt[,Fitted:=(reg$coefficients["(Intercept)"]+reg$coefficients["Trend"]*Trend)]

sub_lg <- melt(sub_dt[,.(Year,Observed=Value,Fitted)],id.vars="Year",variable.name = "Series",value.name = "Value")

gg_corn <- ggplot(sub_lg,aes(x=Year,y=Value,color=Series,linetype=Series))+
  geom_line(linewidth=.8)+
  scale_color_manual(values=c("dimgray","black"))+
  scale_linetype_manual(values=c(1,5))+
  labs(y="",x="Year",subtitle="USA maize yield (bu/acre): realized and fitted")+
  coord_cartesian(ylim=c(30,200),xlim=c(1950,2022))+
  theme_eg()+
  theme(legend.position="none")

gg_corn

ggsave("figures/lecture5/corn_fitted.png",gg_corn,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# 5.3 - residuals ----
sub_dt[,Residuals:=Value-Fitted]

gg_resid <- ggplot(sub_dt,aes(x=Year,y=Residuals))+
  geom_line(linewidth=.8,color="dimgray")+
  labs(y="",x="Year",subtitle="USA maize yield residuals (bu/acre)")+
  coord_cartesian(ylim=c(-40,25),xlim=c(1950,2022))+
  theme_eg()

gg_dots <- ggplot(sub_dt,aes(x=Residuals))+
  geom_dotplot(binwidth=1.6,color="dimgray",fill="lightgray",stroke=1,method="histodot",stackratio=1.1)+
  xlim(-40,25)+
  coord_flip()+
  theme_eg()+
  theme(axis.title=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text=element_blank(),axis.line=element_blank(),panel.grid.major=element_blank())

# combine the two graphs
gg_comb <- plot_grid(gg_resid,gg_dots,align="hv",ncol=2,rel_widths = c(3,1))

gg_comb

ggsave("figures/lecture5/corn_residuals.png",gg_comb,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# 5.4 - autocorrelogram ----

# obtain autocorrelations
maxlag <- round(sqrt(nrow(sub_dt)))
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(sub_dt$Residuals,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_acf <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(sub_dt)),1.96/sqrt(nrow(sub_dt))),linewidth=.8,linetype=5,col="dimgray")+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="lightgray")+
  geom_point(shape=21,size=2.5,stroke=.8,color="dimgray",fill="lightgray")+
  scale_x_continuous(breaks=1:8)+
  scale_y_continuous(breaks=seq(-.2,1,.2),labels=sprintf("%.1f",round(seq(-.2,1,.2),1)))+
  labs(y="",x="k",subtitle=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.25,1))+
  theme_eg()

gg_acf

ggsave("figures/lecture5/corn_autocorrelogram.png",gg_acf,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# 5.5 - random walk ----

load("figures/lecture4/btc.RData")

h1 <- as.Date("2023-01-01")

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
  labs(y="",x="Year",subtitle="Bitcoin price ('000 USD)")+
  coord_cartesian(ylim=c(-30,70))+
  theme_eg()

gg_btc

ggsave("figures/lecture5/btc_forecast.png",gg_btc,width=6.5,height=6.5*9/16,dpi="retina",device="png")


# 5.6 - bad forecast ----

mortgate_dt <- data.table(fredr(series_id="MORTGAGE30US",observation_start=as.Date("1990-01-01"),observation_end=as.Date("2022-12-31"),frequency="m",units="lin"))

# interest_dt <- fread("figures/lecture5/real_interest_rates_10y.csv")

# gg_mortgage <- ggplot(mortgate_dt,aes(x=date,y=value))+
#   geom_line(linewidth=.8,na.rm=T,color="dimgray")+
#   scale_y_continuous(breaks=c(2,4,6,8,10,12))+
#   labs(y="",x="Year",subtitle="USA average mortgage rate (%)")+
#   coord_cartesian(ylim=c(2,12),xlim=c(as.Date("1990-01-01"),as.Date("2022-12-31")))+
#   theme_eg()

# load("figures/lecture5/interest_rates.RData")

interest_rates <- mortgate_dt[,.(date,y=value)]

interest_rates[,`:=`(w=log(y),t=1:nrow(interest_rates))]
interest_rates[,`:=`(haty=fitted(lm(y~t)))]
interest_rates[,`:=`(hatw=fitted(lm(w~t)))]
interest_rates[,`:=`(hatz=exp(hatw+summary(lm(w~t))$sigma^2/2))]

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
  scale_y_continuous(breaks=seq(0,10,2))+
  labs(y="",x="Year",subtitle="USA average mortgage rate (%)")+
  coord_cartesian(ylim=c(0,11),xlim=c(as.Date("1990-01-01"),as.Date("2022-12-31")))+
  theme_eg()

gg_mortgage

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
  scale_y_continuous(breaks=seq(0,10,2))+
  labs(y="",x="Year",subtitle="USA average mortgage rate (%)")+
  coord_cartesian(ylim=c(0,11),xlim=c(as.Date("1990-01-01"),as.Date("2022-12-31")))+
  theme_eg()

gg_mortgage1

ggsave("figures/lecture5/onestep_forecast.png",gg_mortgage1,width=6.5,height=6.5*9/16,dpi="retina",device="png")

interest_rates[,`:=`(e_t=y-f_t)]

mz_reg <- lm(e_t~f_t,data=interest_rates)
coeftest(mz_reg,vcov.=vcovHAC(mz_reg))

# obtain autocorrelations
maxlag <- 36
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(interest_rates[date%in%oos]$e_t,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_acf <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(interest_rates[date%in%oos])),1.96/sqrt(nrow(interest_rates[date%in%oos]))),linewidth=.8,linetype=5,col="dimgray")+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="lightgray")+
  geom_point(shape=21,size=2.5,stroke=.8,color="dimgray",fill="lightgray")+
  scale_x_continuous(breaks=seq(5,maxlag,5),labels=seq(5,maxlag,5))+
  scale_y_continuous(breaks=seq(-.2,1,.2),labels=sprintf("%.1f",round(seq(-.2,1,.2),1)))+
  labs(y="",x="k",subtitle=expression(hat(rho)[k]))+
  coord_cartesian(ylim=c(-.2,1),xlim=c(1.5,maxlag-0.5))+
  theme_eg()

gg_acf

ggsave("figures/lecture5/mortgage_autocorrelogram.png",gg_acf,width=6.5,height=6.5*9/16,dpi="retina",device="png")



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





# 5.7 - spurious trends ----

# 5.7.1 - deterministic ----


life_sub <- life_dt[Country=="Japan" & Year>1950 & Year<=2020]
corn_sub <- corn_dt[Year>1950 & Year<=2020]

trends_dt <- merge(life_sub,corn_sub,by="Year")

dt <- trends_dt[,.(Year,Life=Estimate,Corn=Value)]

reg <- lm(Life~Corn,data=dt)
summary(reg)

reg <- lm(Life~Corn+Year,data=dt)
summary(reg)



# 5.7.1 - stochastic ----

btc_sub <- btc_dt[,.(date,y=BTC)]

M <- 1000

dt <- data.table(iter=1:M,trat=as.numeric(NA))

for(i in 1:M){
  
  set.seed(i)
  x_i <- cumsum(rnorm(nrow(btc_sub)))
  
  btc_sub[,x:=x_i]
  
  reg <- lm(y~x,data=btc_sub)
  
  dt[i,]$trat <- coefficients(summary(reg))["x","t value"]
}

dt[,col:=ifelse(abs(trat)>1.96,"sig","non")]

gg_spurious_s <- ggplot(dt,aes(x=trat,color=col,fill=col))+
  geom_dotplot(binwidth=1.2,stroke=.5,method="histodot",stackratio=1.1,dotsize=.9)+
  scale_color_manual(values=c("dimgray","coral"))+
  scale_fill_manual(values=c("lightgray","coral"))+
  coord_cartesian(xlim=c(-60,60))+
  theme_eg()+
  theme(axis.title=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.line=element_blank(),panel.grid.major=element_blank())

gg_spurious_s

ggsave("figures/lecture5/spurious_s.png",gg_spurious_s,width=6.5,height=6.5*9/16,dpi="retina",device="png")


dt <- data.table(iter=1:M,trat=as.numeric(NA))

for(i in 1:M){
  
  set.seed(i)
  x_i <- cumsum(rnorm(nrow(btc_sub)))
  
  btc_sub[,x:=x_i]
  
  btc_sub[,`:=`(dy=c(0,diff(y)),dx=c(0,diff(x)))]
  
  reg <- lm(dy~dx,data=btc_sub)
  
  dt[i,]$trat <- coefficients(summary(reg))["dx","t value"]
}

dt[,col:=ifelse(abs(trat)>1.96,"sig","non")]

gg_fixed_s <- ggplot(dt,aes(x=trat,color=col,fill=col))+
  geom_dotplot(binwidth=0.1,stroke=.5,method="histodot",stackratio=1.1,dotsize=.9)+
  scale_color_manual(values=c("dimgray","coral"))+
  scale_fill_manual(values=c("lightgray","coral"))+
  coord_cartesian(xlim=c(-5,5))+
  theme_eg()+
  theme(axis.title=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.line=element_blank(),panel.grid.major=element_blank())

gg_fixed_s

ggsave("figures/lecture5/fixed_s.png",gg_fixed_s,width=6.5,height=6.5*9/16,dpi="retina",device="png")


