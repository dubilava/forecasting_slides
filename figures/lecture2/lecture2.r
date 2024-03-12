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


# 2.1 - bias (series) ----

# generate a sample of time series
n <- 40
set.seed(6)
y <- rnorm(n,0,.5)
for(i in 2:n){
  y[i] <- (2/n)*i+.1*y[i-1]+y[i]
}
y_dt <- data.table(x=1:n,y=y[1:n]+1)
R <- round(n/2)
P <- n-R

y_dt[,`:=`(recursive=as.numeric(NA),rolling=as.numeric(NA),fixed=as.numeric(NA))]
for(i in 1:P){
  y_dt[(R+i)]$recursive <- mean(y_dt[1:(R-1+i)]$y)
  y_dt[(R+i)]$rolling <- mean(y_dt[i:(R-1+i)]$y)
  y_dt[(R+i)]$fixed <- mean(y_dt[1:(R)]$y)
}

gg_trending <- ggplot(y_dt,aes(x=x,y=y))+
  geom_line(linewidth=.8,na.rm=T,color="dimgray")+
  geom_point(data=y_dt[x%in%c(R+1:n)],aes(x=x,y=rolling),size=2.5,stroke=.8,shape=21,color="dimgray",fill="lightgray")+
  labs(y="",x="t",subtitle=expression(y[t]))+
  coord_cartesian(x=c(1,n+1),y=c(0,4))+
  theme_eg()

ggsave("figures/lecture2/trending.png",gg_trending,width=6.5,height=6.5*9/16,dpi="retina")


# 2.2 - bias (errors) ----

y_dt[,`:=`(e_rec=y-recursive,e_rol=y-rolling)]

sqrt(mean(y_dt$e_rol^2,na.rm=T))
sqrt(mean((y_dt$e_rol-mean(y_dt$e_rol,na.rm=T))^2,na.rm=T))

gg_errors <- ggplot(y_dt,aes(x=x,y=e_rol))+
  geom_line(linewidth=.8,na.rm=T,color="dimgray",linetype=5)+
  geom_point(size=2.5,stroke=.8,shape=21,color="dimgray",fill="lightgray",na.rm=T)+
  scale_x_continuous(breaks=seq((R+5),n,5))+
  labs(y="",x="t",subtitle=expression(e[t]))+
  coord_cartesian(x=c(R+.5,n+.5),y=c(-2,2))+
  theme_eg()

# graph the dot-density of the series
gg_errordots <- ggplot(y_dt,aes(x=e_rol))+
  geom_dotplot(binwidth=.14,color="dimgray",fill="lightgray",stroke=1.5,stackratio=1.1,na.rm=T)+
  xlim(-2,2)+
  coord_flip()+
  theme_eg()+
  theme(axis.title=element_blank(),axis.title.x=element_blank(),axis.text=element_blank(),axis.line=element_blank())

# combine the two graphs
gg_comb <- plot_grid(gg_errors,gg_errordots,align="hv",ncol=2,rel_widths = c(3,1))

ggsave("figures/lecture2/trending_errors.png",gg_comb,width=6.5,height=6.5*9/16,dpi="retina")


# 2.3 - efficiency (series) ----

# generate a sample of a time series
n <- 40
# s=s+1
set.seed(18)
y <- rnorm(n,.2,.8)
trs <- 1
for(i in 2:n){
  # if(abs(y[i-1])>=trs){
  y[i] <- .5*y[i-1]+y[i]
  # }else{
  # y[i] <- y[i-1]+y[i]
  # }
}

y_dt <- data.table(x=1:n,y=y[1:n])
R <- round(n/2)
P <- n-R

y_dt[,`:=`(efficient=as.numeric(NA),inefficient=as.numeric(NA))]
for(i in 1:P){
  y_dt[(R+i)]$efficient <- .5*y_dt[R-1+i]$y
  y_dt[(R+i)]$inefficient <- y_dt[R-1+i]$y
}

gg_ts <- ggplot(y_dt,aes(x=x,y=y))+
  geom_line(linewidth=.8,na.rm=T,color="dimgray")+
  geom_point(data=y_dt[x%in%c(R+1:n)],aes(x=x,y=inefficient),size=2.5,stroke=.8,shape=21,color="dimgray",fill="lightgray")+
  labs(y="",x="t",subtitle=expression(y[t]))+
  coord_cartesian(x=c(1,n+1),y=c(-2,2),clip="off")+
  theme_eg()

ggsave("figures/lecture2/meanreverting.png",gg_ts,width=6.5,height=6.5*9/16,dpi="retina")


# 2.4 - efficiency (errors) ----

y_dt[,`:=`(e_efficient=y-efficient,e_inefficient=y-inefficient)]

round(sqrt(mean(y_dt$e_inefficient^2,na.rm=T)),2)
round(sqrt(mean(y_dt$e_efficient^2,na.rm=T)),2)

gg_errors <- ggplot(y_dt,aes(x=x,y=e_inefficient))+
  geom_line(linewidth=.8,na.rm=T,color="dimgray",linetype=5)+
  geom_point(size=2.5,stroke=.8,shape=21,color=ifelse(y_dt$inefficient>=0,"dimgray","dimgray"),fill=ifelse(y_dt$inefficient>=0,"dimgray","white"),na.rm=T)+
  scale_x_continuous(breaks=seq((R+5),n,5))+
  labs(y="",x="t",subtitle=expression(e[t]))+
  coord_cartesian(x=c(R+.5,n+.5),y=c(-2,2))+
  theme_eg()


y_dt[,`:=`(pos=factor(ifelse(inefficient>=0,1,0)))]

# graph the dot-density of the series
gg_errordots <- ggplot(y_dt,aes(x=e_inefficient,color=pos,fill=pos))+
  geom_dotplot(binwidth=.14,binpositions = "all",stackgroups=T,stroke=1.5,stackratio=1.1,na.rm=T)+
  scale_color_manual(values=c("dimgray","dimgray"))+
  scale_fill_manual(values=c("white","dimgray"))+
  xlim(-2,2)+
  coord_flip()+
  theme_eg()+
  theme(axis.title=element_blank(),axis.title.x=element_blank(),axis.text=element_blank(),axis.line=element_blank())

# combine the two graphs
gg_comb <- plot_grid(gg_errors,gg_errordots,align="hv",ncol=2,rel_widths = c(3,1))

ggsave("figures/lecture2/meanreverting_errors.png",gg_comb,width=6.5,height=6.5*9/16,dpi="retina")


# 3.7 - prices mincer-zarnowitz ----

# dt <- fread("data/imf_1990-2022.csv")
dt <- fread("data/wb_1960-2022.csv",skip=2)

headers <- names(fread("data/wb_1960-2022.csv",nrows=0))
colnames(dt) <- headers

# store Date is the Date format
dt[,`:=`(Date=as.Date(paste0(substr(Date,1,4),"-",str_pad(substring(Date,6),2,pad="0"),"-01")))]

i=5

dt <- dt[,c(1,..i)]
nm <- colnames(dt)[2]

colnames(dt) <- c("Date","y")

dt[,`:=`(f=shift(frollmean(y,12,algo="exact",align="right"),1))]
dt[,`:=`(f=shift(y,12))]
dt[,`:=`(e=y-f)]

dt <- dt[complete.cases(dt)]

sub_dt <- dt[Date>="2001-01-01"]


# plot time series
gg_ts <- ggplot(sub_dt,aes(x=Date,y=y))+
  geom_line(linewidth=.6,color="dimgray")+
  scale_x_continuous(breaks=c(as.Date("2000-01-01"),as.Date("2010-01-01"),as.Date("2020-01-01")),labels=c("2000","2010","2020"))+
  labs(y="",x="Year",subtitle="Observed price ($/bbl)")+
  coord_cartesian(xlim=c(as.Date("2001-01-01"),as.Date("2023-01-01")),ylim=c(0,140))+
  theme_eg()#+
# theme(plot.margin=margin(0.75,0,.15,0,"cm"))

# ggsave("figures/oil_ts.png",gg_ts,width=6.5,height=3.25,dpi="retina",device="png")
# ggsave("figures/oil_ts.eps",gg_ts,width=6.5,height=3.25,dpi="retina",device=cairo_ps)

# plot scatter 
gg_mz <- ggplot(sub_dt,aes(x=f,y=y))+
  geom_point(shape=21,size=1,color="dimgray",fill="lightgray",na.rm=T)+
  geom_vline(xintercept=mean(sub_dt$f,na.rm=T),color="black",linewidth=.4,linetype=2)+
  geom_hline(yintercept=mean(sub_dt$y,na.rm=T),color="black",linewidth=.4,linetype=2)+
  geom_abline(slope=1,intercept=0,color="black",linewidth=.4,linetype=2)+
  geom_smooth(method="lm",formula=y~x,se=F,color="dimgray",linewidth=.8,linetype=5)+
  labs(y="",x="Forecast price ($/bbl)",subtitle="Observed price ($/bbl)")+
  coord_cartesian(xlim=c(0,140),ylim=c(0,140))+
  theme_eg()+
  theme(axis.line=element_blank(),panel.grid.major.x=element_line(colour="dimgray"))

# ggsave("figures/oil_mz.png",gg_mz,width=5.25,height=5.25,dpi="retina",device="png")
# ggsave("figures/oil_mz.eps",gg_mz,width=5.25,height=5.25,dpi="retina",device=cairo_ps)

gg_combined <- plot_grid(gg_ts,gg_mz,ncol=2,align="hv",hjust=0,vjust=1)

ggsave("figures/lecture2/mincer-zarnowitz.png",gg_combined,width=6.5,height=6.5*9/16,dpi="retina")


# test statistics
adequate <- lm(e~f,data=sub_dt)
unbiased <- lm(e~1,data=sub_dt)

summary(unbiased)
coeftest(unbiased,vcov.=vcovHAC(unbiased))
qt(p=.025,df=unbiased$df.residual,lower.tail=F)

summary(adequate)
coeftest(adequate,vcov.=vcovHAC(adequate))
qt(p=.025,df=adequate$df.residual,lower.tail=F)

linearHypothesis(adequate,c("(Intercept)=0","f=0"),vcov.=vcovHAC(adequate))
qf(p=.05,df1=2,df2=adequate$df.residual,lower.tail=F)