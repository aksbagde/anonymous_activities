#library(dplyr)

join_date <- ddply(geordi,"userID",summarise,firstrecord = min(time))
join_date <- join_date[which(join_date$userID!="(anonymous)"), ]
View(join_date)

geordi <- merge(geordi, joindate, by.x = "userID_a", by.y = "userID", all.x = TRUE)
geordi$compare_tstamps <- ifelse(geordi$time < geordi$firstclass , 1 , 0)

table(geordi$new.category)
table(geordi$new.category[which(geordi$userID=="(anonymous)")])

#requires plyr
new_categories <- count(geordi$new.category[which(geordi$userID=="(anonymous)")])
new_categories

new_categories1 <- new_categories[new_categories$freq<=50000, ]
new_categories1


#BarPlot
ggplot(new_categories1, aes(x, freq)) + geom_bar(stat = "identity", fill="dodgerblue4") + coord_flip() + 
  labs(title="Count of all Activities", x="Categories",y="Frequency")
#BarPlot Sorted
new_categories1$x <- factor(new_categories1$x, levels=unique(as.character(new_categories1$x)) )
new_categories1 <- transform(new_categories1, x=reorder(x, -freq) ) 
ggplot(new_categories1, aes(x, freq)) + geom_bar(stat = "identity", fill="dodgerblue4") + coord_flip() + 
  labs(title="Count of all Activities", x="Categories",y="Frequency") + theme(
    
    #axis.title.x=element_text(size=6),
    
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12)
  )

#Scatter
ggplot(new_categories1) + geom_point(aes(x=freq, y=x, color=freq)) + 
  labs(title="Visualisation",x="Categories",y="Frequency")

#BarPlot 2
new_categories1$type <- ifelse(new_categories1$freq > 30, "Yes","No")
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar( stat="identity")

#Extra
new_categories1$type <- ifelse(new_categories1$freq > 30, "Yes","No")
new_categories1 <- new_categories1[-1,]
