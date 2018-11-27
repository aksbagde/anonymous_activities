baregister <- geordi[which(geordi$userID=="(anonymous)"),]
regsummary <- ddply(baregister, c("new.category","compare_tstamps"), summarise,
                    count = length(new.category)
)
regsummary <- regsummary[which(regsummary$compare_tstamps=="1" | regsummary$compare_tstamps=="0"), ]
regsummary <- regsummary[-c(3, 18, 19, 28, 41), ]


# Stacked
regsummary$compare_tstamps <- factor(regsummary$compare_tstamps)
ggplot(regsummary, aes(x = regsummary$new.category, y = regsummary$count, fill =regsummary$compare_tstamps)) + 
  geom_bar(stat = "identity") + scale_fill_manual(labels = c("After Registration", "Before Registration"),values = c("deepskyblue4","orangered1")) + coord_flip() +
  labs(title="Activities Before/After Registration", x="Activities",y="Number of Activities") +
  guides(fill=guide_legend(title="Legend")) + 
  scale_x_discrete(NULL, expand = waiver(), position = "bottom") +
  scale_y_discrete("Cut")

#convert regsummary$count to numeric
class(regsummary$count)
regsummary$count <- as.numeric(regsummary$count)

regsummary1 <- regsummary[-c(17, 18), ]

# Stacked 2
ggplot(regsummary1, aes(x = regsummary1$new.category, y = regsummary1$count, fill =regsummary1$compare_tstamps)) + 
  geom_bar(stat = "identity") + scale_fill_manual(labels = c("After Registration", "Before Registration"),values = c("deepskyblue4","orangered1")) + coord_flip() +
  labs(title="Activities Before/After Registration", x="Activities",y="Number of Activities") +
  guides(fill=guide_legend(title="Legend"))

regsummary2 <- regsummary1[-c(1,2), ]

# Stacked 3
ggplot(regsummary2, aes(x = regsummary2$new.category, y = regsummary2$count, fill =regsummary2$compare_tstamps)) + 
  geom_bar(stat = "identity") + scale_fill_manual(labels = c("After Registration", "Before Registration"),values = c("deepskyblue4","orangered1")) + coord_flip() +
  labs(title="Activities Before/After Registration", x="Activities",y="Number of Activities") +
  guides(fill=guide_legend(title="Legend"))

regsummary3 <- regsummary2[-c(19,20),]

# Stacked 4
ggplot(regsummary3, aes(x = regsummary3$new.category, y = regsummary3$count, fill =regsummary3$compare_tstamps)) + 
  geom_bar(stat = "identity") + scale_fill_manual(labels = c("After Registration", "Before Registration"),values = c("deepskyblue4","orangered1")) + coord_flip() +
  labs(title="Activities Before/After Registration", x="Activities",y="Number of Activities") +
  guides(fill=guide_legend(title="Legend"))

# Stacked 4 BarPlot Sorted
regsummary3$new.category <- factor(regsummary3$new.category, levels=unique(as.character(regsummary3$new.category)) )
regsummary3 <- transform(regsummary3, x=reorder(regsummary3$new.category, -regsummary3$count) ) 
ggplot(regsummary3, aes(regsummary3$x, regsummary3$count, fill =regsummary3$compare_tstamps)) + geom_bar(stat = "identity") + coord_flip() + 
    scale_fill_manual(labels = c("After Registration", "Before Registration"),values = c("deepskyblue4","orangered1"))+
  labs(title="Activities Before/After Registration", x="Activities",y="Number of Activities") +
  guides(fill=guide_legend(title="Legend")) + theme(
    
    
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14)
    
  )


# Stacked 3 BarPlot Sorted
regsummary2$new.category <- factor(regsummary2$new.category, levels=unique(as.character(regsummary2$new.category)) )
regsummary2 <- transform(regsummary2, x=reorder(regsummary2$new.category, -regsummary2$count) ) 
ggplot(regsummary2, aes(regsummary2$x, regsummary2$count, fill =regsummary2$compare_tstamps)) + geom_bar(stat = "identity") + coord_flip() + 
  scale_fill_manual(labels = c("After Registration", "Before Registration"),values = c("deepskyblue4","orangered1"))+
  labs(title="Activities Before/After Registration", x="Activities",y="Number of Activities") +
  guides(fill=guide_legend(title="Legend"))

# Stacked 2 BarPlot Sorted
regsummary1$new.category <- factor(regsummary1$new.category, levels=unique(as.character(regsummary1$new.category)) )
regsummary1 <- transform(regsummary1, x=reorder(regsummary1$new.category, -regsummary1$count) ) 
ggplot(regsummary1, aes(regsummary1$x, regsummary1$count, fill =regsummary1$compare_tstamps)) + geom_bar(stat = "identity") + coord_flip() + 
  scale_fill_manual(labels = c("After Registration", "Before Registration"),values = c("deepskyblue4","orangered1"))+
  labs(title="Activities Before/After Registration", x="Activities",y="Number of Activities") +
  guides(fill=guide_legend(title="Legend"))

# Stacked1 BarPlot Sorted
regsummary$new.category <- factor(regsummary$new.category, levels=unique(as.character(regsummary$new.category)) )
regsummary <- transform(regsummary, x=reorder(regsummary$new.category, -regsummary$count) ) 
ggplot(regsummary, aes(regsummary$x, regsummary$count, fill =regsummary$compare_tstamps)) + geom_bar(stat = "identity") + coord_flip() + 
  scale_fill_manual(labels = c("After Registration", "Before Registration"),values = c("deepskyblue4","orangered1"))+
  labs(title="Activities Before/After Registration", x="Activities",y="Number of Activities") +
  guides(fill=guide_legend(title="Legend"))


#Scaling
?scale
scale(regsummary3$count, center = TRUE,scale = TRUE)

+ scale_x_continuous(name="Speed of cars", limits=c(0, 30)) +
  scale_y_continuous(name="Stopping distance", limits=c(0, 150))

regsummary$count=as.numeric(levels(regsummary$count))[regsummary$count]

