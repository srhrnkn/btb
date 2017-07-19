b<-ggplot(btb,aes(interviewee.gender.use,author.gender.use))
b + geom_jitter()



b<-ggplot(table(btb$interviewee.gender.use,btb$author.gender.use),aes(interviewee.gender.use,author.gender.use))
b + geom_jitter()

#authormentions by gender raw counts
ggplot(btb,aes(x=interviewee.gender.use,fill=author.gender.use)) + geom_bar() + xlab("Interviewee gender") + ylab("authors mentioned")  + scale_fill_manual(values = c("darkgrey","lightgrey"),name="Author gender",labels=c("female","male")) + theme_bw() + scale_x_discrete(labels=paste0(levels(btb$interviewee.gender.use)," (",c(sum(byint$interviewee.gender.use=="f"), sum(byint$interviewee.gender.use=="m")),")"))


#percentages

ggplot(data.frame(sweep(x = table(btb$author.gender.use,btb$interviewee.gender.use,dnn = c("authors","interviewees")),MARGIN = 2,STATS = margin.table(table(btb$author.gender.use,btb$interviewee.gender.use),2),FUN = "/")),aes(x=interviewees,y=Freq,fill=authors)) + geom_bar(stat = "identity") + xlab("Interviewee gender") + scale_fill_manual(values = c("darkgrey","lightgrey"),name="Author gender",labels=c("male","female")) + theme_bw() + scale_x_discrete(labels=paste0(levels(btb$interviewee.gender.use)," (",c(sum(byint$interviewee.gender.use=="f"), sum(byint$interviewee.gender.use=="m")),")"))


#percent female authors by interviewee (joyplot)
ggplot(byint,aes(x=percF,y = interviewee.gender.use)) + geom_joy() + xlab("Women as percent of authors mentioned") + ylab("Gender of interviewee") 

#jitter
ggplot(byint,aes(y=percF,x = interviewee.gender.use)) + geom_jitter(width = .2) + ylab("Women as percent of authors mentioned") + xlab("Gender of interviewee") 

#age
#joy
ggplot(byint,aes(x=age,y = interviewee.gender.use)) + geom_joy() + xlab("Age") + ylab("Gender of interviewee") 
#jitter
ggplot(byint,aes(x=age,y = interviewee.gender.use)) + geom_jitter() + xlab("Age") + ylab("Gender of interviewee") 

#number of authors mentioned
ggplot(byint,aes(x=countauth,y=interviewee.gender.use)) + geom_joy()

#higher frequency by date mentioned
ggplot(highfreqauthbydate,aes(x=Date,y=author)) + geom_point(aes(color=interviewee.gender.use)) + theme(legend.position="none") #+ scale_y_discrete(breaks=1:length(levels(highfreqauthbydate$author)),labels=unique(paste0(highfreqauthbydate$author," (",highfreqauthbydate$countment,")")))

#barplot of counts of interviewees
ggplot(btb %>% mutate(intervieweelevels=factor(interviewee.GRname,levels=byint$interviewee.GRname[order(byint$countauth)])), aes(intervieweelevels)) + geom_bar(aes(fill = author.gender.use))  + theme(legend.position="none") + xlab("interviewee") + coord_flip()