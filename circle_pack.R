# I think it might look cool to add packed circles to illustrate the number of 
# projects in each country. 

library(ggplot2)
library(gridExtra)
library(packcircles)
library(plyr)
library(dplyr)
library(png)
library(grid)

setwd("C:/Users/Craig/Desktop/Live Projects/El Nino/El_Nino")
source('EN_load.R')
setwd("C:/Users/Craig/Desktop/Live Projects/El Nino/El_Nino")

###############################################################################

makepng <- function(row,scale='projects',hum_color='#fc8d59',
                    dev_color='#91bfdb',icons=TRUE) {
  # right now the 'scale' option doesn't do anything; in the future I'll want
  # to be able to scale by either number of projects or total funding level.
  country <- row[1,1]
  row <- row[1,4:21]
  n <- names(row)[row>0]
  s <- row[row>0]
  rad <- sqrt(s)
  hum <- 1:length(n) %in% grep('hum',n)
  color <- ifelse(hum,hum_color,dev_color)
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
  if (ncircles == 1) { 
    xyr=data.frame(x=0,y=0,r=rad)
    dat.after <- circlePlotData(xyr)
    print('Just one circle.')
  } else {
    inset <- (diff(limits) - sqrt(sum(row)))/2
    if (inset < 0) {
      inset <- diff(limits) / 3
    }
  set.seed(123)
    xyr <- data.frame(
      x = runif(ncircles, min(limits) + inset,0)*(2*as.numeric(hum)-1),
      y = runif(ncircles, min(limits) + inset, max(limits) - inset),
      r = rad)
    res <- circleLayout(xyr, limits, limits, maxiter = 2000)
    #cat(res$niter, "iterations performed")
    dat.after <- circlePlotData(res$layout)
  }
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
  
  labels[grep('^food_',labels$text),'fname'] <- '../icons/gray_food.png'
  labels[grep('^nutrition_',labels$text),'fname'] <- '../icons/gray_nutrition.png'
  labels[grep('^wash_',labels$text),'fname'] <- '../icons/gray_WASH.png'
  labels[grep('^ag_',labels$text),'fname'] <- '../icons/gray_ag.png'
  labels[grep('^health_',labels$text),'fname'] <- '../icons/gray_health.png'
  labels[grep('^ed_',labels$text),'fname'] <- '../icons/gray_education.png'
  labels[grep('^shelter_',labels$text),'fname'] <- '../icons/gray_shelter.png'
  labels[grep('^protection_',labels$text),'fname'] <- '../icons/gray_protection.png'
  labels[grep('^other_',labels$text),'fname'] <- '../icons/gray_other.png'
  wicons <- notext 
  if (icons) {
    for (i in labels[!is.na(labels$fname),'id']) {
      img <- readPNG(labels[labels$id==i,'fname'])
      g <- rasterGrob(img, interpolate=TRUE)
      wicons <- wicons + annotation_custom(g, 
                                           xmin=labels[labels$id==i,'xmin'], 
                                           xmax=labels[labels$id==i,'xmax'], 
                                           ymin=labels[labels$id==i,'ymin'], 
                                           ymax=labels[labels$id==i,'ymax'])
    }
  }
  fname=paste(country,scale,'0523.png',sep='_')
  print(paste('Writing',fname,'...'))
  ggsave(fname,wicons,bg='transparent',
         width=3.5,height=3.5,units='in')
  wicons
}

###############################################################################

for (i in 1:5) {
  makepng(geo_budget[i,],scale='budget')
  makepng(geo_projects[i,])
}

# Make legend bubbles for budget and projects
m <- as.matrix(geo_budget[,4:21])
m[m==0] <- NA
quantile(m,na.rm=TRUE)
# So I want bubbles for $100k, $1M, $10M, $100M

m <- as.matrix(geo_projects[,4:21])
m[m==0] <- NA
quantile(m,na.rm=TRUE)
# Make bubbles for 1,2,5,10 projects

legend_budget <- geo_budget[1:4,]
legend_budget$mission <- c('100k','1M','10M','100M')
legend_budget[,2:21] <- 0
legend_budget$other_dev <- c(1.5,1e6,1e7,1e8)

legend_projects <- legend_budget
legend_projects$mission <- c('1','2','5','10')
legend_projects$other_dev <- c(1,2,5,10)

for (i in 1:4) {
  makepng(legend_budget[i,],scale='budget',icons=FALSE)
  makepng(legend_projects[i,],icons=FALSE)
}

red <- legend_projects[1,]
red$mission <- 'humanitarian'
red[1,c('other_dev','other_hum')] <- c(0,1)
makepng(red,icons=FALSE)
