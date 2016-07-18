# Add in the legend with sector names, icons, and bars

library(ggplot2)
library(reshape)
library(plyr)
library(dplyr)
library(png)
library(grid)


# Labels
labels <- c('Food','Nutrition','WASH','Agriculture','Health','Education',
            'Shelter','Protection','Other')

setwd("C:/Users/Craig/Desktop/Live Projects/El Nino/El_Nino")
source('EN_load.R')
setwd("C:/Users/Craig/Desktop/Live Projects/El Nino/El_Nino")
# Aggregate by sector
d <- colSums(geo_budget[,4:21])

hum_color <- '#ba0c2f'
dev_color <- '#0067b9'
dkred <- '#651d32'
dkblue <- '#002F6C'
rblack <- '#212721'

clear_theme <- theme(axis.ticks = element_blank(), 
                     axis.text = element_blank(),
                     axis.title = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     plot.background = element_blank(),
                     legend.position='none')

fnames <- data.frame(
  label=c('Food','Nutrition','WASH','Agriculture','Health','Education',
          'Shelter','Protection','Other'),
  file=c('../icons/gray_food.png','../icons/gray_nutrition.png',
         '../icons/gray_WASH.png','../icons/gray_ag.png',
         '../icons/gray_health.png','../icons/gray_education.png',
         '../icons/gray_shelter.png','../icons/gray_protection.png',
         '../icons/gray_other.png'),
  stringsAsFactors = FALSE
)


d_reshape <- data.frame(label=labels,dev=d[2*1:9-1],
                        hum=d[2*1:9]) %>%
  mutate(total=dev+hum)
label_sort <- as.character(d_reshape$label[order(d_reshape$dev + d_reshape$hum)])
label_sort <- c('Other',label_sort[label_sort != 'Other'])
d_reshape <- d_reshape %>% 
  melt(id=c('label','total')) %>%
  mutate(label=factor(label,levels=label_sort),
         text=paste('$',round(value/1e6),'M',sep=''),
         text=ifelse(value<1e7,
                     paste('$',0.1*round(value/1e5),'M',sep=''),text),
         text=ifelse(text=='$0M','$0',text),
         total_text=paste('$',round(total/1e6),'M',sep=''),
         total_text=ifelse(total<1e7,
                     paste('$',0.1*round(total/1e5),'M',sep=''),total_text),
         icon=fnames[match(label,fnames$label),'file'])
maxval <- max(d_reshape$total)

p <- ggplot(d_reshape,aes(x=label,y=value,fill=variable)) +
  geom_bar(stat='identity',alpha=0.6) +
  scale_fill_manual(values=c(med_blue,usaid_red)) +
  geom_text(aes(x=label,y=0.01*maxval,label=label,hjust=0),color=r_black) +
  geom_text(aes(x=label,y=0.3*maxval,label=text,hjust=0),
            data=d_reshape %>% filter(variable=='dev'),color=usaid_blue) +
  geom_text(aes(x=label,y=0.55*maxval,label=text,hjust=0),
            data=d_reshape %>% filter(variable=='hum'),color=dk_red) +
  geom_text(aes(x=label,y=0.8*maxval,label=total_text,hjust=0),
            data=d_reshape %>% filter(variable=='dev'),color=r_black) +
  annotate('text',label='Sector',x=10,y=0.01*maxval,color=r_black,hjust=0,vjust=1) +
  annotate('text',label='Dev.',x=10,y=0.3*maxval,color=usaid_blue,hjust=0,vjust=1) +
  annotate('text',label='Hum.',x=10,y=0.55*maxval,color=dk_red,hjust=0,vjust=1) +
  annotate('text',label='Total',x=10,y=0.8*maxval,color=r_black,hjust=0,vjust=1) +
  scale_y_continuous(limits=c(-0.05*maxval,maxval)) +
  coord_flip() +
  clear_theme 

for (i in 1:9) {
  j <-  which(fnames$label==label_sort[i])
  img <- readPNG(fnames[j,'file'])
  g <- rasterGrob(img,interpolate=TRUE)
  bottom <- 0.4 + (i-1)*1.02222
  top <- 0.4 + i*1.02222
  p <- p + annotation_custom(g,
                             xmin=bottom,
                             xmax=top,
                             ymax=0)
}

p
  
ggsave('legend.png',p,bg='white',
       width=3.5,height=4.5,units='in')
