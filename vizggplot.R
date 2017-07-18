b<-ggplot(btb,aes(interviewee.gender.use,author.gender.use))
b + geom_jitter()



b<-ggplot(table(btb$interviewee.gender.use,btb$author.gender.use),aes(interviewee.gender.use,author.gender.use))
b + geom_jitter()

#raw counts
b<-ggplot(btb,aes(x=interviewee.gender.use,fill=author.gender.use))
b + geom_bar() + xlab("Interviewee gender") + scale_fill_manual(values = c("darkgrey","lightgrey"),name="Author gender",labels=c("male","female")) + theme_bw()


#percentages
b<-ggplot(data.frame(sweep(x = table(btb$author.gender.use,btb$interviewee.gender.use,dnn = c("authors","interviewees")),MARGIN = 2,STATS = margin.table(table(btb$author.gender.use,btb$interviewee.gender.use),2),FUN = "/")),aes(x=interviewees,y=Freq,fill=authors))
b + geom_bar(stat = "identity") + xlab("Interviewee gender") + scale_fill_manual(values = c("darkgrey","lightgrey"),name="Author gender",labels=c("male","female")) + theme_bw()


#percent female authors by interviewee
gmixbyint<-btb %>% group_by(interviewee.GRname, interviewee.gender.use) %>% summarise(countF=rsum(author.gender.use=="f"),countauth=countN(author.gender.use=="f")) %>% mutate(percF=countF/countauth) %>% mutate(varf=(1-percF)/countauth) %>% arrange(percF)
b<-ggplot(gmixbyint,aes(x=percF,y = interviewee.gender.use))
b + geom_joy() + xlab("Women as percent of authors mentioned") + ylab("Gender of interviewee") 

#jitter
b<-ggplot(gmixbyint,aes(y=percF,x = interviewee.gender.use))
b + geom_jitter(width = .2) + ylab("Women as percent of authors mentioned") + xlab("Gender of interviewee") 

#age
library("lubridate", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
bdays<-btb %>% distinct(interviewee.GRname,interviewee.birthdate, interviewee.gender.use) 
bdays$year<-year(bdays$interviewee.birthdate)
ggplot(bdays,aes(x=year,y = interviewee.gender.use)) + geom_joy() + xlab("Year of birth") + ylab("Gender of interviewee") 

#number of authors mentioned
ggplot(gmixbyint,aes(x=countauth,y=interviewee.gender.use)) + geom_joy()

#barplot of counts of interviewees
ggplot(gmixbyint,aes(x=interviewee.gender.use,fill=author.gender.use)) + geom_bar(stat = "identity") + xlab("Interviewee gender") + scale_fill_manual(values = c("darkgrey","lightgrey"),name="Author gender",labels=c("male","female")) + theme_bw()