setwd("C:/Users/Craig/Desktop/Live Projects/El Nino/El_Nino")
source('EN_load.R')
setwd("C:/Users/Craig/Desktop/Live Projects/El Nino/El_Nino")

library(ggplot2)
library(reshape)
library(dplyr)

# Colors
blue <- '#6CAFCC'
yellow <- '#EBE85D'

# Labels
labels <- c('Food','Nutrition','WASH','Agriculture','Health','Education',
            'Shelter','Protection','Other')

# Aggregate by sector
project_totals <- colSums(geo_projects[,4:21])
budget_totals <- colSums(geo_budget[,4:21])


# Plotting function
myplot <- function(d) {
  d_reshape <- data.frame(label=labels,dev=d[2*1:9-1],
                               hum=d[2*1:9]) 
  label_sort <- as.character(d_reshape$label[order(d_reshape$dev + d_reshape$hum)])
  label_sort <- c('Other',label_sort[label_sort != 'Other'])
  d_reshape <-  melt(d_reshape,id='label')
  d_reshape$label <- factor(d_reshape$label,levels=label_sort)
  
  ggplot(d_reshape,aes(x=label,y=value,fill=variable)) +
    geom_bar(stat='identity',alpha=0.5) +
    theme_classic() +
    scale_fill_manual(values=c(blue,yellow)) +
    coord_flip() 
}

clear_theme <- theme(axis.ticks = element_blank(), 
                     axis.text = element_blank(),
                     axis.title = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     plot.background = element_blank(),
                     legend.position='none')

# Budget plot
budget <- myplot(budget_totals)
budget
budget_clean <- budget + clear_theme
ggsave('budget_bars.png',budget_clean,bg='transparent',
       width=6,height=11,units='cm')

# Sector totals
budget_totals[2*1:9-1] + budget_totals[2*1:9]

# Country totals
cbind(geo_budget[,1],rowSums(geo_budget[,2:3]))
  

# Project plot
project <- myplot(project_totals)
project
project_clean <- project + clear_theme
ggsave('project_bars.png',project_clean,bg='transparent',
       width=6,height=11,units='cm')

# Sector totals
project_totals[2*1:9-1] + project_totals[2*1:9]

# Country totals
cbind(geo_projects[,1],rowSums(geo_projects[,4:21]))

