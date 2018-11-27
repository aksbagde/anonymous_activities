#Find the first record of a specific user
joindate <- ddply(geordi,"userID",summarize,firstclass = min(time))
joindate <- joindate[which(joindate$userID!="(anonymous)"),]

View(joindate)

#Merge First record with geordi to compare the dates
geordi <- merge(geordi, joindate, by.x = "userID_a", by.y = "userID", all.x = TRUE)
geordi$compare_tstamps <- ifelse(geordi$firstclass < geordi$time , 1 , 0)
register_time <- geordi
register_time <- register_time[which(register_time$userID=="(anonymous)"),]
register_time <- register_time[which(register_time$compare_tstamps==0),]
register_time <- register_time[which(register_time$time!=register_time$firstclass), ]
#register_time$compare_tstamps <- ifelse(register_time$firstclass < register_time$time , 0 , 1)
register_time$new.categories <- NULL
register_time$clientIP <- NULL
register_time$id <- NULL
register_time$userID <- NULL
register_time$type.x <- NULL
register_time$subjectID <- NULL
register_time$relatedID <- NULL
register_time$data <- NULL
register_time$type.y<- NULL
register_time$new.category <- NULL
register_time$relatedID <- NULL
register_time$description <- NULL
register_time$compare_tstamps <- NULL
register_time <- unique(register_time)

#Convert time and first classification into POSIXct
register_time$time_posix <- as.POSIXct(register_time$time, format="%d-%m-%Y %H:%M")
register_time$firstclass_posix <- as.POSIXct(register_time$firstclass, format="%d-%m-%Y %H:%M")

#remove reduntant columns
register_time$time <- NULL
register_time$firstclass <- NULL
#remove redundant rows
register_time_plot <- aggregate(register_time$time_posix ~ register_time$userID_a+register_time$firstclass_posix, data = register_time, min)
names(register_time_plot) <-c("UserID_a" , "firstclass_posix" , "time_posix")
#time taken to register
register_time_plot$time_to_register <- register_time_plot$firstclass_posix-register_time_plot$time_posix
register_time_plot<- register_time_plot[register_time_plot$time_to_register>=0, ]
#Bins
#Create Bins (1-2,3-5,..)
register_time_plot$bin <- ifelse(register_time_plot$time_to_register == 1,"1",
                            ifelse(register_time_plot$time_to_register == 2,"2",
                              ifelse(register_time_plot$time_to_register == 3,"3",
                                 ifelse(register_time_plot$time_to_register == 4,"4",
                                        ifelse(register_time_plot$time_to_register >= 5,"5+","Error")))))

register_time_plot$UserID_a <- as.numeric(register_time_plot$UserID_a)


#remove redundant columns
register_time_plot$firstclass_posix <- NULL
register_time_plot$time_posix <-NULL

#Create a dataframe to find number of activities pre-registration
register_time_plot_count<-as.data.frame(table(register_time_plot$bin))
names(register_time_plot_count) <-c("TimeTaken" , "Users")


#Sort the plot binwise
register_time_plot_count <- within(register_time_plot_count, 
                              register_time_plot_count$TimeTaken <- factor(register_time_plot_count$TimeTaken, 
                                                            levels=names(sort(table(register_time_plot_count$TimeTaken), 
                                                                            decreasing=FALSE))))
#BarPlot
ggplot(register_time_plot_count, aes(x = register_time_plot_count$TimeTaken, y = register_time_plot_count$Users)) + geom_bar(stat = "identity", fill="dodgerblue4") + labs(title="Time taken before Registering", x="Time (Mins)",y="No of Users")+
  coord_flip() + theme(
    
    
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14)
    
  )

#Histogram
hist(register_time_plot_count, 
     main="Histogram for Air Passengers", 
     xlab="Passengers", 
     border="blue", 
     col="green",
     xlim=c(100,700),
     las=1, 
     breaks=5)


#BarPlot Sorted According to No of users
register_time_plot_count$Users <- factor(register_time_plot_count$Users, levels=unique(as.character(register_time_plot_count$Users)) )
register_time_plot_count <- transform(register_time_plot_count, x=reorder(register_time_plot_count$Users, -register_time_plot_count$TimeTaken) )
ggplot(register_time_plot_count, aes(register_time_plot_count$TimeTaken, register_time_plot_count$x)) + geom_bar(stat = "identity" , fill="dodgerblue4") + coord_flip() + 
  labs(title="Activities Before/After Registration", x="Activities",y="Number of Activities")


#BarPlot extra
ggplot(register_time_plot, aes(x = register_time_plot$time_to_register, y = register_time_plot$bin)) + geom_bar(stat = "identity", fill="dodgerblue4") + labs(title="Time taken before Registering", x="Time (Mins)",y="No of Users")+
  coord_flip()
