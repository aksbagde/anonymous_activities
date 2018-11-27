activities <- count(geordi_anonymous$clientIP)
#Remove Classify from dataframe
new_categories1 <- new_categories[new_categories$freq<=50000, ]
#Get records with timestamps=1 from geordi
activities_pre_registration <- geordi[geordi$compare_tstamps==1, ]
length(unique(activities_pre_registration$userID_a))

#Create a dataframe to find number of activities pre-registration
table(activities_pre_registration$userID_a)
activities_count <- count(activities_pre_registration$userID_a)
activities_count <- data.frame(table(activities_count$freq))
names(activities_count) <-c("Activities" , "Users")

#Create Bins (1-2,3-5,..)
activities_count$Activities <- as.numeric(activities_count$Activities)
activities_count$bin <- ifelse(activities_count$Activities <= 2,"1-2",
                               ifelse(activities_count$Activities > 2 & activities_count$Activities <= 5,"3-5",
                                      ifelse(activities_count$Activities >= 6 & activities_count$Activities < 10,"6-9",
                                             ifelse(activities_count$Activities >= 10,"10+","Error"))))

#Sort the plot binwise
activities_count <- within(activities_count, 
                           activities_count$bin <- factor(activities_count$bin, 
                                                          levels=names(sort(table(activities_count$bin), 
                                                                            decreasing=FALSE))))
#BarPlot
ggplot(activities_count, aes(x = activities_count$bin, y = activities_count$Users)) + geom_bar(stat = "identity", fill="dodgerblue4") + labs(title="Number of Activities Before Registering", x="Activities",y="Users")+
      coord_flip() + theme(
        
        
      axis.text.x=element_text(size=14),
      axis.text.y=element_text(size=14)
        
      )
