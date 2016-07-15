# make these labels here, instead of constructing and updating them in PPT

library(ggplot2)
library(dplyr)
library(reshape2)
library(llamar)

source('EN_load.R')
# TODO: I'll eventually want to call this from a master script that will 
# update everything.

red <-  '#BA0C2F'  #'#C70F3B'
blue <- '#006789' #'#012A6C'
dgray <- '#6C6463'
mgray <- '#8C8985'

cbind(geo_budget[,1],geo_budget[,c('human_total','dev_total')]/1e6)
cbind(geo_budget[,1],rowSums(geo_budget[,2:3])/1e6)

totals <- geo_budget %>%
  transmute(mission=mission,
            Hum.=human_total/1e6,
            Dev.=dev_total/1e6,
            Total=(human_total+dev_total)/1e6)

make_label <- function(i,width=1.5) {
  # make a country label for row i of totals
  height <- width/1.61803398875
  text_size <- 3*width
  title_size <- text_size/4
  lwidth <- width*0.7
  print(paste('lwidth',lwidth))
  tmp <- totals[i,] %>% 
    melt(id.vars='mission') %>%
    mutate(cols=c(1,2,3),
           y=c(2,1.1,0),
           value=paste('$',round(value),'M',sep='')) %>%
    melt(id.vars=c('mission','cols','y')) %>%
    mutate(x=ifelse(variable=='variable',0,1),
           vjust=y/2,
           cols=as.character(cols))
  mission <- tmp[1,'mission']
  p <- ggplot(tmp,aes(x,y,label=value)) +
    geom_text(aes(hjust=x,vjust=vjust,color=cols),size=text_size)+ 
    ggtitle(mission) +
    geom_hline(yintercept=0.65,size=0.5,color=mgray) +
    scale_color_manual(values=c(red,blue,dgray)) +
    theme(plot.title = element_text(size = rel(title_size),color=dgray),
          axis.title = element_blank(), 
          axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.ticks.length = unit(0, units = "points"), 
          panel.border = element_blank(), 
          panel.grid = element_blank(), panel.background = element_blank(), 
          plot.background = element_blank(), legend.position = "none")
  fname <- paste(mission,'-label.png',sep='')
  print(paste('Writing',fname))
  ggsave(fname,p,bg='white',
         width=width,height=height,units='in')
  p
}

make_label(1,width=1.5)

# generate labels for all countries
