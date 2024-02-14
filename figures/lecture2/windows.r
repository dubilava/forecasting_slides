library(data.table)
library(ggplot2)
library(ggbrace)
library(magick)
library(stringr)
library(fredr)
library(extrafont)
loadfonts(device="win",quiet=T)

theme_eg <- function(base_size=12,base_family="Segoe Print",border=F){
  theme(
    panel.background=element_rect(fill="white",color=NA),
    panel.grid=element_line(colour=NULL,linetype=3),
    panel.grid.major=element_line(colour="dimgray"),
    panel.grid.major.x=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill="white",color=NA),
    plot.title=element_text(family=base_family,size=rel(1.3),colour="dimgray"),
    plot.subtitle=element_text(family=base_family,size=rel(1.2),colour="dimgray"),
    plot.caption=element_text(colour="darkgray"),
    plot.margin=margin(.25,.25,.25,.25,"lines"),
    axis.title=element_text(family=base_family,size=rel(1.2),colour="dimgray"),
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

# generate a sample of time series
n <- 20
set.seed(1)
y <- rnorm(n,0,.5)
for(i in 2:n){
  y[i] <- .7*y[i-1]+y[i]
}
y_dt <- data.table(x=1:n,y=y[1:n]+1.1)
R <- 10
P <- n-R

y_dt[,`:=`(recursive=as.numeric(NA),rolling=as.numeric(NA),fixed=as.numeric(NA))]
for(i in 1:P){
  y_dt[(R+i)]$recursive <- mean(y_dt[1:(R-1+i)]$y)
  y_dt[(R+i)]$rolling <- mean(y_dt[i:(R-1+i)]$y)
  y_dt[(R+i)]$fixed <- mean(y_dt[1:(R)]$y)
}


for(i in 1:(n-R)){
  
  gg <- ggplot(y_dt,aes(x=x,y=y))+
    geom_line(linewidth=.8,na.rm=T,color="gray")+
    geom_line(data=y_dt[x%in%c(i:(R+i-1))],aes(x=x,y=y),linewidth=.8,na.rm=T,color="black")+
    geom_point(data=y_dt[x%in%c(R+i)],aes(x=x,y=rolling),size=3,stroke=1,shape=21,color="black",fill="gray")+
    geom_text(data=y_dt[x%in%c(R+i)],aes(x=x,y=rolling,label="forecast"),size=5,nudge_y = c(-.2),nudge_x = c(-.5),family="Segoe Print",colour="black")+
    labs(title="",x="t",y="",subtitle=expression(y[t]))+
    geom_brace(aes(x=c(i,R+i-1),y=c(2.2,2.3)),inherit.data=F,rotate=0,bending=.2)+
    geom_text(data=data.table(x=mean(c(i,R+i-1)),y=mean(c(2.4,2.5)),lab="estimation window"),aes(x=x,y=y,label=lab),size=5,family="Segoe Print")+
    coord_cartesian(x=c(1,n+1),y=c(0,2.3),clip="off")+
    theme_eg()+
    theme(axis.title.y=element_blank(),plot.title.position="plot")
  
  ggsave(paste0("figures/lecture2/rolling/w",str_pad(i,3,pad="0"),".png"),width=1920*.5,height=1080*.5,units="px",dpi=150)
  
}

# this next few lines create a gif from the pre-generated images

## create a list of the png file names in the temp folder
the_list <- paste0("figures/lecture2/rolling/",list.files("figures/lecture2/rolling/"))

## store the graphs as a list in frames
frames <- lapply(the_list,image_read)

## generate a gif
animation <- image_animate(image_join(frames),fps=2)

## save the gif
image_write(animation,"figures/lecture2/rolling.gif")




for(i in 1:(n-R)){
  
  gg <- ggplot(y_dt,aes(x=x,y=y))+
    geom_line(linewidth=.8,na.rm=T,color="gray")+
    geom_line(data=y_dt[x%in%c(1:(R+i-1))],aes(x=x,y=y),linewidth=.8,na.rm=T,color="black")+
    geom_point(data=y_dt[x%in%c(R+i)],aes(x=x,y=recursive),size=3,stroke=1,shape=21,color="black",fill="gray")+
    geom_text(data=y_dt[x%in%c(R+i)],aes(x=x,y=recursive,label="forecast"),size=5,nudge_y = c(-.2),nudge_x = c(-.5),family="Segoe Print",colour="black")+
    labs(title="",x="t",y="",subtitle=expression(y[t]))+
    geom_brace(aes(x=c(1,R+i-1),y=c(2.2,2.3)),inherit.data=F,rotate=0,bending=.2)+
    geom_text(data=data.table(x=mean(c(1,R+i-1)),y=mean(c(2.4,2.5)),lab="estimation window"),aes(x=x,y=y,label=lab),size=5,family="Segoe Print")+
    coord_cartesian(x=c(1,n+1),y=c(0,2.3),clip="off")+
    theme_eg()+
    theme(axis.title.y=element_blank(),plot.title.position="plot")
  
  ggsave(paste0("figures/lecture2/recursive/w",str_pad(i,3,pad="0"),".png"),width=1920*.5,height=1080*.5,units="px",dpi=150)
  
}

# this next few lines create a gif from the pre-generated images

## create a list of the png file names in the temp folder
the_list <- paste0("figures/lecture2/recursive/",list.files("figures/lecture2/recursive/"))

## store the graphs as a list in frames
frames <- lapply(the_list,image_read)

## generate a gif
animation <- image_animate(image_join(frames),fps=2)

## save the gif
image_write(animation,"figures/lecture2/recursive.gif")

