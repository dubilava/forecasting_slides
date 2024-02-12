library(data.table)
library(ggplot2)
library(tseries)
library(lmtest)
library(sandwich)
library(readrba)

# download the unemployment rates from RBA
unemp_rate_dt <- data.table(read_rba(series_id = "GLFSURSA"))

unemp_rate_dt <- unemp_rate_dt[,.(date,unemployment=value)]

ggplot(unemp_rate_dt,aes(x=date,y=unemployment))+
  geom_line()

# perform ADF test
adf.test(unemp_rate_dt$unemployment)

# work with first-differnced series, ie, changes in unemployment rates
dt <- unemp_rate_dt[,.(date,t=1:nrow(unemp_rate_dt),y=c(0,diff(unemployment)))]

# obtain the lagged dependent variables
dt[,`:=`(y1=shift(y,1),y2=shift(y,2),y3=shift(y,3),y4=shift(y,4),y5=shift(y,5),y6=shift(y,6))]

ar1 <- lm(y~y1,data=dt)
ar2 <- lm(y~y1+y2,data=dt)
ar3 <- lm(y~y1+y2+y3,data=dt)
ar4 <- lm(y~y1+y2+y3+y4,data=dt)
ar5 <- lm(y~y1+y2+y3+y4+y5,data=dt)
ar6 <- lm(y~y1+y2+y3+y4+y5+y6,data=dt)

icf <- function(m,ic){
  aic=log(crossprod(m$residuals))+2*length(m$coefficients)/length(m$residuals)
  sic=log(crossprod(m$residuals))+log(length(m$residuals))*length(m$coefficients)/length(m$residuals)
  if(ic=="a"){return(aic)}else{return(sic)}
}

ic_dt <- data.table(aic=round(sapply(list(ar1,ar2,ar3,ar4,ar5,ar6),icf,ic="a"),3),sic=round(sapply(list(ar1,ar2,ar3,ar4,ar5,ar6),icf,ic="s"),3))



# use grid-search to find the optimal threshold
bounds <- round(quantile(dt$y,c(.15,.85)),1)
thresholds <- seq(bounds[1],bounds[2],by=.01)

ssr_dt <- data.table(thresholds,ssr=NA)
for(i in 1:length(thresholds)){
  dt[,`:=`(D=ifelse(y1>=thresholds[i],1,0))]
  setar4 <- lm(y~(y1+y2+y3+y4):I(1-D)+(y1+y2+y3+y4):I(D),data=dt)
  ssr_dt$ssr[i] <- sum(setar4$residuals^2)
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
n <- nrow(dt)
R <- round(.75*n)
P <- n-R

setar4 <- lm(y~(y1+y2+y3+y4):I(1-D)+(y1+y2+y3+y4):I(D),data=dt[1:R])
summary(setar4)


## one step ahead forecasts: compare ar4 with setar4

h1 <- R
h2 <- max(dt$t)

dt[,`:=`(y_f=y)]
dt[t<h1]$y_f <- NA

oos <- dt[t>h1]$t

dt[,`:=`(f_ar=as.numeric(NA),f_setar=as.numeric(NA))]

for(i in oos){
  
  # starting point
  s <- i-R
  
  # ar
  ar4 <- lm(y~y1+y2+y3+y4,data=dt[t>= s & t<i])
  xhat <- c(1,as.matrix(dt[t==i,.(y1,y2,y3,y4)]))
  dt[t==i,f_ar:=ar4$coefficients%*%as.matrix(xhat)]
  
  # setar
  setar4 <- lm(y~(y1+y2+y3+y4):I(1-D)+(y1+y2+y3+y4):I(D),data=dt[t>= s & t<i])
  xhat <- c(1,as.matrix(dt[t==i,.(y1,y2,y3,y4)]*(1-dt[t==i]$D)),as.matrix(dt[t==i,.(y1,y2,y3,y4)]*(dt[t==i]$D)))
  dt[t==i,f_setar:=setar4$coefficients%*%as.matrix(xhat)]
  
}


dt[,`:=`(e_ar=y-f_ar,e_setar=y-f_setar)]

dt[,`:=`(e2_ar=e_ar^2,e2_setar=e_setar^2)]


dt[,`:=`(d=e2_ar-e2_setar,da=e2_ar-e2_setar+(f_ar-f_setar)^2)]


dm <- lm(d~1,dt)
coeftest(dm,vcov.=vcovHAC(dm))

dma <- lm(da~1,dt)
coeftest(dma,vcov.=vcovHAC(dma))

