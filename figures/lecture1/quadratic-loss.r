library(data.table)
library(ggplot2)
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
    plot.title=element_text(family=base_family,size=rel(1.2),colour="dimgray"),
    plot.caption=element_text(family=base_family,colour="darkgray"),
    plot.margin=margin(.25,.25,.25,.25,"lines"),
    axis.title=element_text(family=base_family,face="bold",size=rel(1.3),colour="dimgray"),
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


set.seed(1)
dt <- data.table(e=rnorm(100))

dev <- c(seq(0,1.5,.02),seq(1.4,-1.5,-.02),seq(-1.4,0,.02))

for(i in 1:length(dev)){
  
  dt[,e1:=e-dev[i]]
  
  dt[,`:=`(loss_quadratic=e1^2,loss_absolute=abs(e1))]
  
  el <- round(mean(dt$loss_quadratic),2)
  
  gg <- ggplot(data=dt)+
    geom_line(aes(x=e1+dev[i],y=loss_quadratic),color="dimgray",linewidth=1)+
    geom_point(aes(x=e1+dev[i],y=loss_quadratic),color="dimgray",fill="white",stroke=.5,shape=21,size=1.5)+
    geom_point(aes(x=e,y=0),color="dimgray",fill="lightgray",stroke=.5,shape=21,size=2)+
    geom_segment(aes(x=dev[i]-.2,xend=dev[i]+.1,y=el,yend=el),color="coral",linewidth=.8,lineend="round")+
    geom_point(aes(x=dev[i],y=loss_quadratic),color="coral",fill="lightgray",stroke=.5,shape=21,size=2)+
    annotate(geom="text",x=dev[i]+.15,y=el,label=sprintf('%.2f',el),hjust=0,vjust=0,color="coral",size=5)+
    coord_cartesian(xlim=c(-3,3),ylim=c(0,16))+
    labs(x=expression(e),y=expression(e^2))+
    theme_eg()
  
  ggsave(paste0("figures/lecture1/quadratic-loss/w",str_pad(i,3,pad="0"),".png"),width=1920*.5,height=1080*.5,units="px",dpi=150)
  
}

# this next few lines create a gif from the pre-generated images

## create a list of the png file names in the temp folder
the_list <- paste0("figures/lecture1/quadratic-loss/",list.files("figures/lecture1/quadratic-loss/"))

## store the graphs as a list in frames
frames <- lapply(the_list,image_read)

## generate a gif
animation <- image_animate(image_join(frames),fps=25)

## save the gif
image_write(animation,"figures/lecture1/quadratic-loss.gif")

