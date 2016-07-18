# Make a budget legend at the same scale as the circle plots

s <- c(100,10,1)*1e6 # millions of $
limits <- c(-40000,40000) # same scale used in circle_pack.R
rad <- sqrt(s)
gap <- 1000
y <- c(0,
       rad[1]+gap+rad[2],
       rad[1]+2*rad[2]+2*gap+rad[3])
xyr=data.frame(x=0,y=y,r=rad)
cpd <- circlePlotData(xyr)

bl <- ggplot(cpd) +
  geom_polygon(aes(x, y, group=id), colour=med_blue, fill=med_blue, alpha=0.6) +
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
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  annotate('text',label='$100M',x=rad[1]+gap,y=y[1],hjust=0,size=4.5,color=med_gray) +
  annotate('text',label='$10M',x=rad[1]+gap,y=y[2],hjust=0,size=4.5,color=med_gray)+
  annotate('text',label='$1M',x=rad[1]+gap,y=y[3],hjust=0,size=4.5,color=med_gray) +
  annotate('text',label='Budget',x=0,y=y[3]+7*gap,size=6)

ggsave('bubble_legend.png',bl,bg='white',
       width=3,height=3,units='in')  # use same size as in circle_pack.R
