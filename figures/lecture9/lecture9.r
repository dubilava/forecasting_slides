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
# library(readabs)
# Sys.setenv(R_READABS_PATH = "figures/lecture5")
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

# inflation series ----
inflation_dt <- data.table(fredr(series_id="CPIAUCNS",observation_start=as.Date("1970-01-01"),observation_end=as.Date("2022-12-31"),frequency="m",units="pc1"))

save(inflation_dt,file="figures/lecture9/inflation.RData")

gg_inflation <- ggplot(inflation_dt,aes(x=date,y=value))+
  geom_line(linewidth=.5,color="dimgray",na.rm=T)+
  scale_y_continuous(breaks=seq(0,15,5))+
  labs(x="Year",y="Year-on-Year Inflation (%)")+
  coord_cartesian(ylim=c(-2,16),xlim=c(as.Date("1970-01-01"),as.Date("2022-12-31")))+
  theme_eg()

gg_inflation

ggsave("figures/lecture9/inflation.png",gg_inflation,width=6.5,height=6.5*9/16,dpi="retina",device="png")



inflation_dt[,y:=value]

inflation_dt[,`:=`(y1=shift(y,1),y2=shift(y,2),y3=shift(y,3),y4=shift(y,4))]

inflation_dt <- inflation_dt[complete.cases(inflation_dt)]

ar1 <- lm(y~y1,data=inflation_dt)
ar2 <- lm(y~y1+y2,data=inflation_dt)
ar3 <- lm(y~y1+y2+y3,data=inflation_dt)
ar4 <- lm(y~y1+y2+y3+y4,data=inflation_dt)

icf <- function(m,ic){
  aic=log(crossprod(m$residuals))+2*length(m$coefficients)/length(m$residuals)
  sic=log(crossprod(m$residuals))+log(length(m$residuals))*length(m$coefficients)/length(m$residuals)
  if(ic=="a"){return(aic)}else{return(sic)}
}

dt <- data.table(aic=round(sapply(list(ar1,ar2,ar3,ar4),icf,ic="a"),3),sic=round(sapply(list(ar1,ar2,ar3,ar4),icf,ic="s"),3))

  
quantiles <- round(quantile(inflation_dt$y,c(.15,.85)),1)

candidates <- seq(quantiles[1],quantiles[2],by=.1)

ssr_dt <- data.table(candidates,ssr=NA)
for(i in 1:length(candidates)){
  inflation_dt[,`:=`(ind=ifelse(y1>candidates[i],1,0))]
  setar3 <- lm(y~(y1+y2):I(1-ind)+(y1+y2):I(ind),data=inflation_dt)
  ssr_dt$ssr[i] <- sum(setar3$residuals^2)
}

trs <- ssr_dt[ssr==min(ssr)]$candidates
  
gg_ssr <- ggplot(ssr_dt,aes(x=candidates,y=ssr))+
  geom_line(linewidth=.5,color="coral",na.rm=T)+
  labs(x="Thresholds (Inflation, %)",y="SSR")+
  theme_eg()

gg_ssr

ggsave("figures/lecture9/ssr.png",gg_ssr,width=6.5*.85,height=6.5*9/16,dpi="retina",device="png")


inflation_dt[,`:=`(ind=ifelse(y1>trs,1,0))]
setar2 <- lm(y~(y1+y2):I(1-ind)+(y1+y2):I(ind),data=inflation_dt)
summary(setar2)

aic <- round(icf(setar2,ic="a"),3)
sic <- round(icf(setar2,ic="s"),3)

out <- t(summary(setar2)$coef[,c(1,2)])
rownames(out) <- c("estimate","s.e.")




est_end <- paste0(substr(inflation_dt$date[round(.75*length(inflation_dt$date))],1,4),"-12-01")
R <- which(inflation_dt$date==est_end)
P <- nrow(inflation_dt)-R

setar2 <- lm(y~(y1+y2):I(1-ind)+(y1+y2):I(ind),data=inflation_dt[1:R])

eps <- setar2$residuals

B <- 5000

boot_mat <- matrix(nrow=R+P,ncol=B)

for(b in 1:B){
  set.seed(b)
  inflation_dt[,`:=`(yb=y,eb=sample(eps,R+P,replace=T))]
  
  for(i in 1:P){
    
    inflation_dt$yb[R+i] <- setar2$coefficients["(Intercept)"]+(setar2$coefficients["y1:I(1 - ind)"]*inflation_dt$yb[R-1+i]+setar2$coefficients["y2:I(1 - ind)"]*inflation_dt$yb[R-2+i])*(1-ifelse(inflation_dt$yb[R-1+i]>trs,1,0))+(setar2$coefficients["y1:I(ind)"]*inflation_dt$yb[R-1+i]+setar2$coefficients["y2:I(ind)"]*inflation_dt$yb[R-2+i])*ifelse(inflation_dt$yb[R-1+i]>trs,1,0)+inflation_dt$eb[R+i]
    
  }
  
  boot_mat[,b] <- inflation_dt$yb
}

inflation_dt[,`:=`(y_f=y,f=apply(boot_mat,1,mean),l=apply(boot_mat,1,quantile,.05),u=apply(boot_mat,1,quantile,.95))]
inflation_dt$y_f[1:R] <- NA
inflation_dt$f[1:R] <- NA
inflation_dt$l[1:R] <- NA
inflation_dt$u[1:R] <- NA

ggplot(inflation_dt,aes(x=date,y=y))+
  geom_ribbon(aes(ymin=l,ymax=u),fill="coral",alpha=.2)+
  geom_line(color="powderblue",size=.8)+
  geom_line(data=inflation_dt[date>as.Date(est_end)],color="gray",size=.8)+
  geom_line(aes(y=f),color="coral",size=.8,linetype=5,na.rm=T)+
  labs(x="Year",y="Year-Over-Year Inflation",caption="retrieved from FRED, Federal Reserve Bank of St. Louis\nhttps://fred.stlouisfed.org/series/CPIAUCNS")+
  theme_classic()+
  theme(axis.title = element_text(size=22),axis.text = element_text(size=18))


gg_boot <- ggplot(inflation_dt,aes(x=date))+
  geom_ribbon(aes(ymin=l,ymax=u),fill="coral",alpha=.2)+
  geom_line(aes(y=y),linewidth=.6,color="dimgray",na.rm=T)+
  geom_line(aes(y=y_f),linewidth=.6,color="gray",na.rm=T)+
  geom_line(aes(y=l),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=u),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=f),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  scale_y_continuous(breaks=seq(0,15,5))+
  labs(x="Year",y="Year-on-Year Inflation (%)")+
  coord_cartesian(ylim=c(-2,16),xlim=c(as.Date("1970-01-01"),as.Date("2022-12-31")))+
  theme_eg()

gg_boot


ggsave("figures/lecture9/inflation_boot.png",gg_boot,width=6.5,height=6.5*9/16,dpi="retina",device="png")



dt <- data.table(x=boot_mat[R+P,])
gg_den <- ggplot(dt,aes(x=x))+
  geom_density(color="coral",size=1,fill="coral",alpha=.2)+
  labs(x="156-step-ahead forecast (Inflation, %)",y="Density")+
  theme_eg()


ggsave("figures/lecture9/inflation_den.png",gg_den,width=6.5,height=6.5*9/16,dpi="retina",device="png")

