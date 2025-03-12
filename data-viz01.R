library(survival)
gbsg <- survival::gbsg
gbsg <- gbsg[,c("meno", "size", "nodes")]
knitr::kable(head(gbsg, n=10))

gbsg$dot_col <- with(gbsg, ifelse(meno==1, "#df8f44", "#374e55"))
gbsg$fill_col <- with(gbsg, ifelse(meno==1, "#df8f44", "white"))
gbsg <- subset(gbsg, size<=100 & nodes<=50)
gbsg <- gbsg[1:400,]

with(gbsg, plot(size, nodes, col=dot_col))

library(ggplot2)
ggplot(data=gbsg, aes(x=size, y=nodes, colour=meno)) + geom_point() 

gg <- ggplot(data=gbsg, aes(x=size, y=nodes, colour=dot_col)) + geom_point()
gg
gg2 <- gg + theme_minimal()
gg2
gg3 <- gg2 + theme_minimal() + theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank())
gg3
gg4 <- gg3 + theme(text=element_text(family="Corbel", colour="black"), 
                   plot.title = element_text(size=20),
                   axis.text.x=element_text(size=20, colour="black"), 
                   axis.text.y=element_text(size=20, colour="black"), 
                   axis.title.x = element_text(size=20), 
                   axis.title.y = element_text(size=20))
gg4
gg5 <- gg4 + theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
                   axis.ticks = element_line(colour = "black", linewidth = 0.25),
                   axis.ticks.length = unit(4, "pt"),
                   axis.minor.ticks.length = unit(2, "pt"))
gg5
gg6 <- gg5 + coord_cartesian(xlim=c(0,100), ylim=c(0,50), clip = "off")
gg6
gg7 <- gg6 + scale_x_continuous(breaks=seq(0,100,by=20),
                                labels=seq(0,100,by=20),
                                minor_breaks = seq(0,100,by=10),
                                expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,50,by=10),
                     labels=seq(0,50,by=10),
                     minor_breaks = seq(0,50,by=5),
                     expand = c(0,0))
gg7
gg8 <- gg7 + guides(x = guide_axis(minor.ticks = TRUE),
                    y = guide_axis(minor.ticks = TRUE))

gg8
gg9 <- gg8 +
  annotate(geom="segment", x=0, xend=100, y=seq(0,50,by=10), yend=seq(0,50,by=10), 
           col="#E3E4E5") +
  scale_colour_identity() +
  xlab("Tumor size (mm)") + ylab("") + labs(title="No. of positive lymph nodes")
gg9

theme_boers <- function(){
  theme(text=element_text(family="Corbel", colour="black"), 
        #define font 
        plot.margin = margin(0.2,1,0,0,"cm"), 
        #prevent x axis labels from being cut off
        plot.title = element_text(size=20),
        #text size of the title
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #we do not want automatic grid lines in the background
        axis.text.x=element_text(size=20, colour="black"), 
        axis.text.y=element_text(size=20, colour="black"), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        #define the size of the tick labels and axis titles
        axis.line = element_line(colour = 'black', linewidth = 0.25),
        axis.ticks = element_line(colour = "black", linewidth = 0.25),
        #specify thin axes
        axis.ticks.length = unit(4, "pt"), 
        axis.minor.ticks.length = unit(2, "pt")) 
  #minor ticks should be shorter than major ticks
}

gbsg$size_new <- gbsg$size + rnorm(n=nrow(gbsg), mean=0, sd=0.3)
#scatter the x position slightly

ggplot(data=gbsg, aes(x=size_new, y=nodes, colour=dot_col, fill=fill_col)) + 
  theme_minimal() + theme_boers() +
  annotate(geom="segment", x=0, xend=100, y=seq(0,50,by=10), yend=seq(0,50,by=10), 
           col="#E3E4E5") +
  coord_cartesian(xlim=c(0,100), ylim=c(0,50), clip = "off") +
  scale_x_continuous(breaks=seq(0,100,by=20),
                     labels=seq(0,100,by=20),
                     minor_breaks = seq(0,100,by=10),
                     expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,50,by=10),
                     labels=seq(0,50,by=10),
                     minor_breaks = seq(0,50,by=5),
                     expand = c(0,0)) +
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) +
  scale_colour_identity() + scale_fill_identity() +
  geom_point(shape=21, alpha=0.5) +
  xlab("Tumor size (mm)") + ylab("") + labs(title="No. of positive lymph nodes")

