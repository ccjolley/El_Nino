library(ggplot2)
library(reshape)
library(plyr)
library(dplyr)


# Labels
labels <- c('Food','Nutrition','WASH','Agriculture','Health','Education',
            'Shelter','Protection','Other')

setwd("C:/Users/Craig/Desktop/Live Projects/El Nino/El_Nino")
source('EN_load.R')
setwd("C:/Users/Craig/Desktop/Live Projects/El Nino/El_Nino")
# Aggregate by sector
budget_totals <- colSums(geo_budget[,4:21])

# Plotting function
myplot <- function(d,hum_color='#ba1f30',dev_color='#64a2d3',alpha=0.6) {
  d_reshape <- data.frame(label=labels,dev=d[2*1:9-1],
                               hum=d[2*1:9]) 
  label_sort <- as.character(d_reshape$label[order(d_reshape$dev + d_reshape$hum)])
  label_sort <- c('Other',label_sort[label_sort != 'Other'])
  d_reshape <-  melt(d_reshape,id='label')
  d_reshape$label <- factor(d_reshape$label,levels=label_sort)
  
  ggplot(d_reshape,aes(x=label,y=value,fill=variable)) +
    geom_bar(stat='identity',alpha=alpha) +
    theme_classic() +
    scale_fill_manual(values=c(dev_color,hum_color)) +
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
# TODO: Somehow this step is broken now.

add_us <- data.frame(label=labels,x=0,y=1:length(labels))

budget
budget_clean <- budget + clear_theme

budget_clean + 
  geom_text(data=add_us,aes(x=x,y=y,label=label))

z=ggplot(data=add_us,aes(x=x,y=y,label=label)) + geom_text()

z+budget_clean
# Add sector labels
# Add icons
# Add total budget figures


ggsave('budget_bars.png',budget_clean,bg='transparent',
       width=8,height=11,units='cm')

