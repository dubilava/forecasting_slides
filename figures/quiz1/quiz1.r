# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(extrafont)
loadfonts(device="win",quiet=T)
# font_import()
# loadfonts(device="win")
library(cowplot)

# plot aesthetics
theme_eg <- function(base_size=12,base_family="Segoe Print",border=F){
  theme(
    panel.background=element_rect(fill="white",color=NA),
    panel.grid=element_line(colour=NULL,linetype=3),
    panel.grid.major=element_line(colour="dimgray"),
    panel.grid.major.x=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill="white",color=NA),
    plot.title=element_text(family=base_family,size=rel(1.2),colour="dimgray"),
    plot.caption=element_text(family=base_family,colour="darkgray"),
    plot.margin=margin(.25,.25,.25,.25,"lines"),
    axis.title=element_text(family=base_family,face="bold",size=rel(1.3),colour="dimgray"),
    # axis.title.x=element_text(),
    axis.text=element_text(family=base_family,size=rel(1.1),colour="dimgray",margin=margin(t=1,r=1,b=1,l=1)),
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


# 1.2 - time series ----

# generate a sample of time series
n <- 8

set.seed(3)
x <- rnorm(n,2,1)
for(i in 2:n) x[i] <- .5*x[i-1]+x[i]
dt <- data.table(x=round(x),t=1:n)

# graph the time series
gg_ts <- ggplot(dt,aes(x=t,y=x))+
  geom_line(color="black",linetype=1,linewidth=.8)+
  geom_point(color="black",fill="lightgray",stroke=1,shape=21,size=3)+
  ylim(0,6)+
  labs(x=expression(t),y=expression(x[t]))+
  theme_eg()


ggsave("fig-1.png",gg_ts,width=1920*.5,height=1080*.5,units="px",dpi=150)
