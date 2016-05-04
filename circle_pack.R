# I think it might look cool to add packed circles to illustrate the number of 
# projects in each country. 

library(ggplot2)
library(gridExtra)
library(packcircles)
library(plyr)
library(dplyr)
library(png)
library(grid)

setwd("C:/Users/Craig/Desktop/El Nino")
source('EN_load.R')

###############################################################################

makepng <- function(row,scale='projects') {
  # right now the 'scale' option doesn't do anything; in the future I'll want
  # to be able to scale by either number of projects or total funding level.
  country <- row[1,1]
  row <- row[1,4:21]
  n <- names(row)[row>0]
  s <- row[row>0]
  rad <- sqrt(s)
  hum <- 1:length(n) %in% grep('hum',n)
  color <- ifelse(hum,"#CB654F","#8CBEA3")
  # Adapted from http://www.r-bloggers.com/circle-packing-in-r-again/
  ncircles <- length(n)
  if (scale == 'projects') {
    lsize <- 10
  } else if (scale == 'budget') {
    lsize <- 40000
  } else {
    lsize <- sum(s)
  }
  limits <- c(-lsize,lsize)
  inset <- diff(limits) / 3
  set.seed(123)
  xyr <- data.frame(
    x = runif(ncircles, min(limits) + inset,0)*(2*as.numeric(hum)-1),
    y = runif(ncircles, min(limits) + inset, max(limits) - inset),
    r = rad)
  res <- circleLayout(xyr, limits, limits, maxiter = 1000)
  dat.after <- circlePlotData(res$layout)
  dat.color <- color[dat.after$id]
  labels <- ddply(dat.after,'id',numcolwise(mean))
  labels$text <- n
  notext <- ggplot(dat.after) +
    geom_polygon(aes(x, y, group=id), colour=dat.color, fill=dat.color, alpha=0.3) +
    coord_equal(xlim=limits, ylim=limits) +
    theme_classic() +
    theme(text=element_text(size=20),
          axis.title.x=element_blank(),
          axis.line.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  get_r <- function(i) {
    dx <- dat.after[dat.after$id==i,'x'] - labels[i,'x']
    dy <- dat.after[dat.after$id==i,'y'] - labels[i,'y']
    (dx^2 + dy^2) %>% sqrt() %>% mean()
  }
  labels$r <- sapply(labels$id,get_r)
  labels$xmin <- labels$x - labels$r/sqrt(2)
  labels$xmax <- labels$x + labels$r/sqrt(2)
  labels$ymin <- labels$y - labels$r/sqrt(2)
  labels$ymax <- labels$y + labels$r/sqrt(2)
  
  labels[grep('^food_',labels$text),'fname'] <- 'icon_food.png'
  labels[grep('^nutrition_',labels$text),'fname'] <- 'icon_nutrition.png'
  labels[grep('^wash_',labels$text),'fname'] <- 'icon_WASH.png'
  labels[grep('^ag_',labels$text),'fname'] <- 'icon_ag.png'
  labels[grep('^health_',labels$text),'fname'] <- 'icon_health.png'
  labels[grep('^ed_',labels$text),'fname'] <- 'icon_education.png'
  labels[grep('^shelter_',labels$text),'fname'] <- 'icon_shelter.png'
  labels[grep('^protection_',labels$text),'fname'] <- 'icon_protection.png'
  wicons <- notext 
  for (i in labels[!is.na(labels$fname),'id']) {
    img <- readPNG(labels[labels$id==i,'fname'])
    g <- rasterGrob(img, interpolate=TRUE)
    wicons <- wicons + annotation_custom(g, 
                                         xmin=labels[labels$id==i,'xmin'], 
                                         xmax=labels[labels$id==i,'xmax'], 
                                         ymin=labels[labels$id==i,'ymin'], 
                                         ymax=labels[labels$id==i,'ymax'])
    
  }
  fname=paste(country,scale,'0503.png',sep='_')
  print(paste('Writing',fname,'...'))
  ggsave(fname,wicons,bg='transparent',
         width=3,height=3,units='in')
  wicons
}

###############################################################################

makepng(geo_budget[1,],scale='budget')

makepng(geo_projects[1,])

