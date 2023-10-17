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




# U.S. Interest Rates and Inflation

.right-column[
  ```{r echo=FALSE, message=FALSE, cache=FALSE}
  load("../../Books/forecasting/data/funds.RData")
  load("../../Books/forecasting/data/inflation.RData")
  
  dt <- merge(funds,inflation,by="date")
  colnames(dt) <- c("date","funds","inflation")
  
  dl <- melt(dt,id.vars="date")
  
  dl$variable <- factor(dl$variable,levels=c("funds","inflation"),labels=c("Federal Funds Rate","Year-Over-Year Inflation"))
  
  ggplot(dl,aes(x=date,y=value,color=variable,linetype=variable))+
    geom_line(size=.8)+
    scale_color_manual(values=c("powderblue","coral"))+
    scale_linetype_manual(values=c(1,5))+
    labs(x="Year",y="%",caption="retrieved from FRED, Federal Reserve Bank of St. Louis\nhttps://fred.stlouisfed.org/series/CPIAUCNS")+
    theme_classic()+
    theme(axis.title = element_text(size=22),axis.text = element_text(size=18),legend.position="top",legend.title=element_blank(),legend.text=element_text(size=18))
  ```
]

---
  
  # U.S. Interest Rates and Inflation: VAR(3)
  
  .pull-left[
    ```{r echo=FALSE, message=FALSE, cache=FALSE}
    dt[,`:=`(funds1=shift(funds,1),funds2=shift(funds,2),funds3=shift(funds,3),funds4=shift(funds,4),inflation1=shift(inflation,1),inflation2=shift(inflation,2),inflation3=shift(inflation,3),inflation4=shift(inflation,4))]
    
    var11 <- lm(funds~funds1+inflation1,data=dt)
    var12 <- lm(inflation~funds1+inflation1,data=dt)
    
    var21 <- lm(funds~funds1+funds2+inflation1+inflation2,data=dt)
    var22 <- lm(inflation~funds1+funds2+inflation1+inflation2,data=dt)
    
    var31 <- lm(funds~funds1+funds2+funds3+inflation1+inflation2+inflation3,data=dt)
    var32 <- lm(inflation~funds1+funds2+funds3+inflation1+inflation2+inflation3,data=dt)
    
    var41 <- lm(funds~funds1+funds2+funds3+funds4+inflation1+inflation2+inflation3+inflation4,data=dt)
    var42 <- lm(inflation~funds1+funds2+funds3+funds4+inflation1+inflation2+inflation3+inflation4,data=dt)
    
    
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
    
    knitr::kable(aic_dt,format='html',digits=3,align="l", table.attr='class="myTable"')
    
    ```
  ]

.pull-right[
  ```{r echo=FALSE, message=FALSE, cache=FALSE}
  out1 <- t(summary(var31)$coef[,c(1,2)])
  rownames(out1) <- c("estimate","s.e.")
  
  out2 <- t(summary(var32)$coef[,c(1,2)])
  rownames(out2) <- c("estimate","s.e.")
  
  out <- rbind(out1,out2)
  kable(out, booktabs = TRUE,digits=3,align="r", table.attr='class="myTable"',row.names = T,col.names = c("$\\alpha_i$","$\\pi_{i1}^{(1)}$","$\\pi_{i1}^{(2)}$","$\\pi_{i1}^{(3)}$","$\\pi_{i2}^{(1)}$","$\\pi_{i2}^{(2)}$","$\\pi_{i2}^{(3)}$"),escape = FALSE) %>% pack_rows(
    index = c("Interest Rate (i=1)" = 2, "Inflation Rate (i=2)" = 2)
  ) %>%
    kable_styling(font_size = 22)
  ```
]

---
  
  
  
  # Testing in-sample Granger causality
  
  .right-column[
    ```{r echo=FALSE, message=FALSE, cache=FALSE}
    
    ar31 <- lm(funds~funds1+funds2+funds3,data=dt)
    ar32 <- lm(inflation~inflation1+inflation2+inflation3,data=dt)
    
    f31 <- anova(var31,ar31)
    f32 <- anova(var32,ar32)
    
    out <- rbind(f31[2,c(5:6)],f32[2,c(5:6)])
    rownames(out) <- c("Inflation Rate GC Interest Rate","Interest Rate GC Inflation Rate")
    
    kable(out, booktabs = TRUE,digits=3,align="l", table.attr='class="myTable"',row.names = T)
    
    ```
  ]

---
  
  
  
  
  # Forecasting interest rate
  
  .right-column[
    ```{r echo=FALSE, message=FALSE, cache=FALSE}
    est_end <- paste0(substr(dt$date[round(.75*length(dt$date))],1,4),"-12-01")
    R <- which(dt$date==est_end)
    P <- nrow(dt)-R
    
    ts_dt <- ts(dt[1:R,.(funds,inflation)],start=c(as.numeric(substr(dt$date[1],1,4)),as.numeric(substr(dt$date[1],6,7))),frequency = 12)
    
    var3 <- VAR(ts_dt,p=3,type="const")
    
    for3 <- forecast(var3,level=95,h=P)
    
    
    dt$f.funds <- NA
    dt$l.funds <- NA
    dt$u.funds <- NA
    
    dt$f.inflation <- NA
    dt$l.inflation <- NA
    dt$u.inflation <- NA
    
    dt$f.funds[(R+1):(R+P)] <- for3$forecast$funds$mean
    dt$l.funds[(R+1):(R+P)] <- for3$forecast$funds$lower
    dt$u.funds[(R+1):(R+P)] <- for3$forecast$funds$upper
    
    dt$f.inflation[(R+1):(R+P)] <- for3$forecast$inflation$mean
    dt$l.inflation[(R+1):(R+P)] <- for3$forecast$inflation$lower
    dt$u.inflation[(R+1):(R+P)] <- for3$forecast$inflation$upper
    
    ggplot(dt,aes(x=date,y=funds))+
      geom_ribbon(aes(ymin=l.funds,ymax=u.funds),fill="coral",alpha=.2)+
      geom_line(color="powderblue",size=.8)+
      geom_line(data=dt[date>as.Date(est_end)],color="gray",size=.8)+
      geom_line(aes(y=f.funds),color="coral",size=.8,linetype=5,na.rm=T)+
      labs(x="Year",y="Federal Funds Effective Rate",caption="retrieved from FRED, Federal Reserve Bank of St. Louis\nhttps://fred.stlouisfed.org/series/FEDFUNDS")+
      theme_classic()+
      theme(axis.title = element_text(size=22),axis.text = element_text(size=18))
    ```
  ]

---
  
  
  # Forecasting inflation rate
  
  .right-column[
    ```{r echo=FALSE, message=FALSE, cache=FALSE}
    ggplot(dt,aes(x=date,y=inflation))+
      geom_ribbon(aes(ymin=l.inflation,ymax=u.inflation),fill="coral",alpha=.2)+
      geom_line(color="powderblue",size=.8)+
      geom_line(data=dt[date>as.Date(est_end)],color="gray",size=.8)+
      geom_line(aes(y=f.inflation),color="coral",size=.8,linetype=5,na.rm=T)+
      labs(x="Year",y="Year-Over-Year Inflation",caption="retrieved from FRED, Federal Reserve Bank of St. Louis\nhttps://fred.stlouisfed.org/series/CPIAUCNS")+
      theme_classic()+
      theme(axis.title = element_text(size=22),axis.text = element_text(size=18))
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

