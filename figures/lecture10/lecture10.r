# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(extrafont)
# font_import()
loadfonts(device="win",quiet=T)
library(cowplot)
library(fredr)
fredr_set_key("7a1db535f59c2ac4382b9c22a15b5f06")
library(stringr)
library(lmtest)
library(sandwich)
library(forecast)
library(vars)
library(fastDummies)

rm(list=ls())
gc()

# plot aesthetics
theme_eg <- function(base_size=12,base_family="Segoe Print",border=F){
  theme(
    panel.background=element_rect(fill="white",color=NA),
    panel.grid=element_line(colour=NULL,linetype=3,linewidth=.3),
    panel.grid.major=element_line(colour="dimgray"),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill="white",color=NA),
    plot.title=element_text(family=base_family,size=rel(1.2),colour="dimgray"),
    plot.subtitle=element_text(family=base_family,size=rel(1.2),colour="dimgray"),
    plot.caption=element_text(family="Segoe UI",colour="darkgray"),
    plot.margin=margin(.25,.25,.25,.25,"lines"),
    axis.title=element_text(family=base_family,face="bold",size=rel(1.2),colour="dimgray"),
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

# crude oil price series ----
crudeoil_dt <- data.table(fredr(series_id="DCOILWTICO",observation_start=as.Date("2004-01-01"),observation_end=as.Date("2022-12-31"),frequency="m",units="lin"))

save(crudeoil_dt,file="figures/lecture10/crudeoil.RData")

# 5-yr expected inflation ----
inflation_dt <- data.table(fredr(series_id="T5YIE",observation_start=as.Date("2004-01-01"),observation_end=as.Date("2022-12-31"),frequency="m",units="lin"))

save(inflation_dt,file="figures/lecture10/inflation.RData")

dt <- merge(inflation_dt[,.(date,infl=value)],crudeoil_dt[,.(date,coil=value)],by="date")

dl <- melt(dt,id.vars="date")
  
dl$variable <- factor(dl$variable,levels=c("coil","infl"),labels=c("Crude Oil Prices","5-Year Expected Inflation"))
  
gg1 <- ggplot(dt,aes(x=date,y=coil))+
  geom_line(color="dimgray",size=.6)+
  labs(y="",x="Year",title="Crude Oil Prices - WTI",subtitle="($/bbl)")+
  coord_cartesian(ylim=c(0,150),xlim=c(as.Date("2004-01-01"),as.Date("2022-12-31")))+
  theme_eg()

gg2 <- ggplot(dt,aes(x=date,y=infl))+
  geom_line(color="coral",size=.6)+
  labs(y="",x="Year",title="5-yr Expected Inflation",subtitle="(%)")+
  coord_cartesian(ylim=c(-2,4),xlim=c(as.Date("2004-01-01"),as.Date("2022-12-31")))+
  theme_eg()

gg <- plot_grid(gg1,gg2,align="hv",axis="tb",nrow=1)

ggsave("figures/lecture10/series.png",gg,width=6.5,height=6.5*8/16,dpi="retina",device="png")
  

dt[,`:=`(coil1=shift(coil,1),coil2=shift(coil,2),coil3=shift(coil,3),coil4=shift(coil,4),infl1=shift(infl,1),infl2=shift(infl,2),infl3=shift(infl,3),infl4=shift(infl,4))]
    
var11 <- lm(coil~coil1+infl1,data=dt)
var12 <- lm(infl~coil1+infl1,data=dt)

var21 <- lm(coil~coil1+coil2+infl1+infl2,data=dt)
var22 <- lm(infl~coil1+coil2+infl1+infl2,data=dt)

var31 <- lm(coil~coil1+coil2+coil3+infl1+infl2+infl3,data=dt)
var32 <- lm(infl~coil1+coil2+coil3+infl1+infl2+infl3,data=dt)

var41 <- lm(coil~coil1+coil2+coil3+coil4+infl1+infl2+infl3+infl4,data=dt)
var42 <- lm(infl~coil1+coil2+coil3+coil4+infl1+infl2+infl3+infl4,data=dt)
    
    
p <- 1
k <- 2
var1r <- cbind(var11$residuals,var12$residuals)
cov1r <- crossprod(var1r)/(nrow(dt)-(p*k^2+k))

AIC1 <- log(det(cov1r))+2*(p*k^2+k)/nrow(dt)


p <- 2
k <- 2
var2r <- cbind(var21$residuals,var22$residuals)
cov2r <- crossprod(var2r)/(nrow(dt)-(p*k^2+k))

AIC2 <- log(det(cov2r))+2*(p*k^2+k)/nrow(dt)


p <- 3
k <- 2
var3r <- cbind(var31$residuals,var32$residuals)
cov3r <- crossprod(var3r)/(nrow(dt)-(p*k^2+k))

AIC3 <- log(det(cov3r))+2*(p*k^2+k)/nrow(dt)


p <- 4
k <- 2
var4r <- cbind(var41$residuals,var42$residuals)
cov4r <- crossprod(var4r)/(nrow(dt)-(p*k^2+k))

AIC4 <- log(det(cov4r))+2*(p*k^2+k)/nrow(dt)

aic_dt <- data.table(p=1:4,AIC=NA)

aic_dt$AIC[1] <- AIC1
aic_dt$AIC[2] <- AIC2
aic_dt$AIC[3] <- AIC3
aic_dt$AIC[4] <- AIC4





# multistep: known x ----

h1 <- as.Date("2016-01-01")
h2 <- max(dt$date)

R <- which(dt$date=="2015-12-01")
P <- nrow(dt)-R

dt[,`:=`(f_i=as.numeric(NA),f_il=as.numeric(NA),f_iu=as.numeric(NA))]

# dt <- dt[-c(1:4)]

# linear trend
est <- lm(infl~infl1+infl2+coil+coil1+coil2,data=dt[date<h1])

eps <- est$residuals

B <- 5000

boot_mat <- matrix(nrow=R+P,ncol=B)

for(b in 1:B){
  set.seed(b)
  dt[,`:=`(yb=infl,eb=sample(eps,R+P,replace=T))]
  
  for(i in 1:P){
    
    dt$yb[R+i] <- est$coefficients["(Intercept)"]+est$coefficients["infl1"]*dt$yb[R+i-1]+est$coefficients["infl2"]*dt$yb[R+i-2]+est$coefficients["coil"]*dt$coil[R+i]+est$coefficients["coil1"]*dt$coil1[R+i]+est$coefficients["coil2"]*dt$coil2[R+i]+dt$eb[R+i]
    
  }
  
  boot_mat[,b] <- dt$yb
}

dt[,`:=`(y_f=infl,f=apply(boot_mat,1,mean),l=apply(boot_mat,1,quantile,.05),u=apply(boot_mat,1,quantile,.95))]
dt$y_f[1:R] <- NA
dt$f[1:R] <- NA
dt$l[1:R] <- NA
dt$u[1:R] <- NA



gg_boot <- ggplot(dt,aes(x=date))+
  geom_ribbon(aes(ymin=l,ymax=u),fill="coral",alpha=.2)+
  geom_line(aes(y=infl),linewidth=.6,color="dimgray",na.rm=T)+
  geom_line(aes(y=y_f),linewidth=.6,color="gray",na.rm=T)+
  geom_line(aes(y=l),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=u),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=f),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  labs(x="Year",y="5-yr Expected Inflation (%)")+
  coord_cartesian(ylim=c(-2,4),xlim=c(as.Date("2004-01-01"),as.Date("2022-12-31")))+
  theme_eg()

gg_boot

ggsave("figures/lecture10/inflation_known.png",gg_boot,width=6.5,height=6.5*8/16,dpi="retina",device="png")


boot_mat <- matrix(nrow=R+P,ncol=B)

for(b in 1:B){
  set.seed(b)
  dt[,`:=`(yb=infl,xb=dt[date==h1]$coil,eb=sample(eps,R+P,replace=T))]
  dt[date<h1,xb:=coil]
  
  for(i in 1:P){
    
    dt$yb[R+i] <- est$coefficients["(Intercept)"]+est$coefficients["infl1"]*dt$yb[R+i-1]+est$coefficients["infl2"]*dt$yb[R+i-2]+est$coefficients["coil"]*dt$xb[R+i]+est$coefficients["coil1"]*dt$xb[R+i-1]+est$coefficients["coil2"]*dt$xb[R+i]+dt$eb[R+i-2]
    
  }
  
  boot_mat[,b] <- dt$yb
}

dt[,`:=`(y_f=infl,f=apply(boot_mat,1,mean),l=apply(boot_mat,1,quantile,.05),u=apply(boot_mat,1,quantile,.95))]
dt$y_f[1:R] <- NA
dt$f[1:R] <- NA
dt$l[1:R] <- NA
dt$u[1:R] <- NA



gg_boot <- ggplot(dt,aes(x=date))+
  geom_ribbon(aes(ymin=l,ymax=u),fill="coral",alpha=.2)+
  geom_line(aes(y=infl),linewidth=.6,color="dimgray",na.rm=T)+
  geom_line(aes(y=y_f),linewidth=.6,color="gray",na.rm=T)+
  geom_line(aes(y=l),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=u),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=f),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  labs(x="Year",y="5-yr Expected Inflation (%)")+
  coord_cartesian(ylim=c(-2,4),xlim=c(as.Date("2004-01-01"),as.Date("2022-12-31")))+
  theme_eg()

gg_boot

ggsave("figures/lecture10/inflation_fixed.png",gg_boot,width=6.5,height=6.5*8/16,dpi="retina",device="png")




  

ar21 <- lm(infl~infl1+infl2,data=dt)
ar22 <- lm(coil~coil1+coil2,data=dt)

var21 <- lm(infl~infl1+infl2+coil1+coil2,data=dt)
var22 <- lm(coil~infl1+infl2+coil1+coil2,data=dt)

f21 <- anova(var21,ar21)
f22 <- anova(var22,ar22)

out <- rbind(f21[2,c(5:6)],f22[2,c(5:6)])



R <- which(dt$date=="2015-12-01")
P <- nrow(dt)-R

ts_dt <- ts(dt[1:R,.(coil,infl)],start=c(as.numeric(substr(dt$date[1],1,4)),as.numeric(substr(dt$date[1],6,7))),frequency = 12)

var3 <- VAR(ts_dt,p=3,type="const")

for3 <- forecast::forecast(var3,level=95,h=P)


dt$f.coil <- NA
dt$l.coil <- NA
dt$u.coil <- NA

dt$f.infl <- NA
dt$l.infl <- NA
dt$u.infl <- NA

dt$f.coil[(R+1):(R+P)] <- for3$forecast$coil$mean
dt$l.coil[(R+1):(R+P)] <- for3$forecast$coil$lower
dt$u.coil[(R+1):(R+P)] <- for3$forecast$coil$upper

dt$f.infl[(R+1):(R+P)] <- for3$forecast$infl$mean
dt$l.infl[(R+1):(R+P)] <- for3$forecast$infl$lower
dt$u.infl[(R+1):(R+P)] <- for3$forecast$infl$upper

gg1 <- ggplot(dt,aes(x=date))+
  geom_ribbon(aes(ymin=l.coil,ymax=u.coil),fill="coral",alpha=.2)+
  geom_line(aes(y=coil),linewidth=.6,color="dimgray",na.rm=T)+
  geom_line(data=dt[date>=h1],aes(y=coil),linewidth=.6,color="gray",na.rm=T)+
  geom_line(aes(y=l.coil),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=u.coil),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=f.coil),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  labs(y="",x="Year",title="Crude Oil Prices - WTI",subtitle="($/bbl)")+
  coord_cartesian(ylim=c(0,150),xlim=c(as.Date("2004-01-01"),as.Date("2022-12-31")))+
  theme_eg()

gg2 <- ggplot(dt,aes(x=date))+
  geom_ribbon(aes(ymin=l.infl,ymax=u.infl),fill="coral",alpha=.2)+
  geom_line(aes(y=infl),linewidth=.6,color="dimgray",na.rm=T)+
  geom_line(data=dt[date>=h1],aes(y=infl),linewidth=.6,color="gray",na.rm=T)+
  geom_line(aes(y=l.infl),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=u.infl),linetype=2,linewidth=.4,color="coral",na.rm=T)+
  geom_line(aes(y=f.infl),linetype=5,linewidth=.6,color="coral",na.rm=T)+
  labs(y="",x="Year",title="5-yr Expected Inflation",subtitle="(%)")+
  coord_cartesian(ylim=c(-2,4),xlim=c(as.Date("2004-01-01"),as.Date("2022-12-31")))+
  theme_eg()
    
gg <- plot_grid(gg1,gg2,align="hv",axis="tb",nrow=1)

ggsave("figures/lecture10/forecasts.png",gg,width=6.5,height=6.5*8/16,dpi="retina",device="png")
  
  
  # Forecasting infl rate
  
  .right-column[
    ```{r echo=FALSE, message=FALSE, cache=FALSE}
    
    ```
  ]

---
  
  
  # Out-of-Sample Granger Causality
  
  .right-column[
    ```{r echo=FALSE, message=FALSE, cache=FALSE}
    dt$funds.r <- NA
    dt$funds.u <- NA
    dt$inflation.r <- NA
    dt$inflation.u <- NA
    
    for(i in 1:P){
      
      ar31 <- lm(funds~funds1+funds2+funds3,data=dt[i:(R-1+i)])
      ar32 <- lm(inflation~inflation1+inflation2+inflation3,data=dt[i:(R-1+i)])
      
      var31 <- lm(funds~funds1+funds2+funds3+inflation1+inflation2+inflation3,data=dt[i:(R-1+i)])
      var32 <- lm(inflation~funds1+funds2+funds3+inflation1+inflation2+inflation3,data=dt[i:(R-1+i)])
      
      dt$funds.r[R+i] <- ar31$coefficients[1]+ar31$coefficients[2]*dt$funds[R-1+i]+ar31$coefficients[3]*dt$funds[R-2+i]+ar31$coefficients[4]*dt$funds[R-3+i]
      dt$inflation.r[R+i] <- ar32$coefficients[1]+ar32$coefficients[2]*dt$inflation[R-1+i]+ar32$coefficients[3]*dt$inflation[R-2+i]+ar32$coefficients[4]*dt$inflation[R-3+i]
      
      dt$funds.u[R+i] <- var31$coefficients[1]+var31$coefficients[2]*dt$funds[R-1+i]+var31$coefficients[3]*dt$funds[R-2+i]+var31$coefficients[4]*dt$funds[R-3+i]+var31$coefficients[5]*dt$inflation[R-1+i]+var31$coefficients[6]*dt$inflation[R-2+i]+var31$coefficients[7]*dt$inflation[R-3+i]
      dt$inflation.u[R+i] <- var32$coefficients[1]+var32$coefficients[2]*dt$funds[R-1+i]+var32$coefficients[3]*dt$funds[R-2+i]+var32$coefficients[4]*dt$funds[R-3+i]+var32$coefficients[5]*dt$inflation[R-1+i]+var32$coefficients[6]*dt$inflation[R-2+i]+var32$coefficients[7]*dt$inflation[R-3+i]
      
    }
    
    dt[,`:=`(e_funds.r=funds-funds.r,e_inflation.r=inflation-inflation.r,e_funds.u=funds-funds.u,e_inflation.u=inflation-inflation.u)]
    
    rmsfe_fr <- round(sqrt(mean(dt$e_funds.r^2,na.rm=T)),4)
    rmsfe_fu <- round(sqrt(mean(dt$e_funds.u^2,na.rm=T)),4)
    
    rmsfe_ir <- round(sqrt(mean(dt$e_inflation.r^2,na.rm=T)),4)
    rmsfe_iu <- round(sqrt(mean(dt$e_inflation.u^2,na.rm=T)),4)
    ```
    
    In the 'interest rate' equation, the RMSFE of the unrestricted model (`r rmsfe_fu`) is greater than the RMSFE of the restricted model (`r rmsfe_fr`), thus indicating that inflation rates do not out-of-sample Granger cause interest rates.
    
    In the 'inflation rate' equation, the RMSFE of the unrestricted model (`r rmsfe_iu`) is less than the RMSFE of the restricted model (`r rmsfe_ir`), thus providing evidence that interest rates out-of-sample Granger cause inflation rates.
    
  ]

---
  

























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

