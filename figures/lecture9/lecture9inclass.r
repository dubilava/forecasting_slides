# unit: forecasting for economics and business
# lecture: forecasting regime dependent series
# author: david ublava
# date: 2023-10-19
# script: simulated example

# load the needed packages
library(data.table)
library(ggplot2)

# generate a regime-dependent time series
n <- 200
set.seed(9)
e <- rnorm(n)
y <- e
for(i in 3:n){
  y[i] <- 0.8+(1.4*y[i-1]-1.0*y[i-2])*ifelse(y[i-1]<3,1,0)+(0.8*y[i-1])*ifelse(y[i-1]>=3,1,0)+e[i]
}

# store the series in the data table
dt <- data.table(t=1:n,y=y)

# illustrate the series 
gg_ts <- ggplot(dt,aes(x=t))+
  geom_line(aes(y=y),linewidth=1,color="dimgray",na.rm=T)+
  labs(x="t",y=expression(y[t]))+
  theme_minimal()

gg_ts


# obtain the lagged dependent variables
dt[,`:=`(y1=shift(y,1),y2=shift(y,2))]

# use grid-search to find the optimal threshold
bounds <- round(quantile(dt$y,c(.15,.85)),1)
thresholds <- seq(bounds[1],bounds[2],by=.1)

ssr_dt <- data.table(thresholds,ssr=NA)
for(i in 1:length(thresholds)){
  dt[,`:=`(D=ifelse(y1>=thresholds[i],1,0))]
  setar2 <- lm(y~(y1+y2):I(1-D)+(y1+y2):I(D),data=dt)
  ssr_dt$ssr[i] <- sum(setar2$residuals^2)
}

trs <- ssr_dt[ssr==min(ssr)]$thresholds
dt[,`:=`(D=ifelse(y1>=trs,1,0))]

# illustrate the optimal threshold selection
gg_ssr <- ggplot(ssr_dt,aes(x=thresholds,y=ssr))+
  geom_line(linewidth=1,color="coral",na.rm=T)+
  labs(x="Threshold",y="SSR")+
  theme_minimal()

gg_ssr


# generate multi-step ahead forecasts
R <- round(.75*n)
P <- n-R

setar2 <- lm(y~(y1+y2):I(1-D)+(y1+y2):I(D),data=dt[1:R])
summary(setar2)

# use bootstrap method (5000 iterations)
B <- 5000

eps <- setar2$residuals

boot_mat <- matrix(nrow=R+P,ncol=B)

gg_b <- ggplot(dt)+
  geom_line(aes(x=t,y=y),linewidth=1,color="gray",na.rm=T)+
  labs(x="t",y=expression(y[t]))+
  coord_cartesian(ylim=c(-6,8))+
  theme_minimal()

for(b in 1:B){
  set.seed(b)
  dt[,`:=`(yb=y,eb=sample(eps,R+P,replace=T))]
  for(i in 1:P){
    dt$yb[R+i] <- setar2$coefficients["(Intercept)"]+(setar2$coefficients["y1:I(1 - D)"]*dt$yb[R+i-1]+setar2$coefficients["y2:I(1 - D)"]*dt$yb[R+i-2])*(1-ifelse(dt$yb[R+i-1]>=trs,1,0))+(setar2$coefficients["y1:I(D)"]*dt$yb[R+i-1]+setar2$coefficients["y2:I(D)"]*dt$yb[R+i-2])*ifelse(dt$yb[R+i-1]>=trs,1,0)+dt$eb[R+i]
  }
  dt$yb[1:R] <- NA
  if(b<=10){
    gg_b <- gg_b+
      geom_line(data=dt[t>R],aes(x=t,y=yb),linetype=1,linewidth=.8,color="coral",alpha=.5,na.rm=T)
    print(gg_b)
    Sys.sleep(1)
  }
  boot_mat[,b] <- dt$yb
}

# store bootstrap point and interval forecasts
dt[,`:=`(y_f=y,f=apply(boot_mat,1,mean,na.rm=T),l=apply(boot_mat,1,quantile,.05,na.rm=T),u=apply(boot_mat,1,quantile,.95,na.rm=T))]

dt$y_f[1:R] <- NA


# illustrate bootstrap point and interval forecasts 
gg_boot <- ggplot(dt,aes(x=t))+
  geom_ribbon(aes(ymin=l,ymax=u),fill="coral",alpha=.2)+
  geom_line(aes(y=y),linewidth=1,color="dimgray",na.rm=T)+
  geom_line(aes(y=y_f),linewidth=1,color="gray",na.rm=T)+
  geom_line(aes(y=l),linetype=2,linewidth=.5,color="coral",na.rm=T)+
  geom_line(aes(y=u),linetype=2,linewidth=.5,color="coral",na.rm=T)+
  geom_line(aes(y=f),linetype=5,linewidth=1,color="coral",na.rm=T)+
  labs(x="t",y=expression(y[t]))+
  coord_cartesian(ylim=c(-6,8))+
  theme_minimal()

gg_boot


# illustrate the density of a multi-step ahead forecast
den_dt <- data.table(x=boot_mat[R+P,])
gg_den <- ggplot(den_dt,aes(x=x))+
  geom_density(color="coral",linewidth=1,fill="coral",alpha=.2)+
  labs(x="Forecast",y="Density")+
  theme_minimal()

gg_den


