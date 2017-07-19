library("lubridate", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

#summarize by interviewee: counts by gender, percents, bday, age
byint<-btb %>% group_by(interviewee.GRname,interviewee.id, interviewee.gender.use,interviewee.birthdate) %>% summarise(countF=rsum(author.gender.use=="f"),countM=rsum(author.gender.use=="m"),countauth=countN(author.gender.use=="f")) %>% mutate(percF=countF/countauth,age=as.period(interval(interviewee.birthdate,Sys.Date()))$year) %>% mutate(varf=(1-percF)/countauth) %>% arrange(percF)
byint<-merge(byint,interviewees[,c("GRID","Date")],by.x="interviewee.id",by.y="GRID")

#summarize by author: mentiongs, bday, age
byauth<-btb %>% group_by(author.GRname, author.gender.use,author.birthdate) %>% summarise(countFint=rsum(interviewee.gender.use=="f"),countMint=rsum(interviewee.gender.use=="m"),countment=countN(interviewee.gender.use)) %>% mutate(percF=countFint/countment,age=as.period(interval(author.birthdate,Sys.Date()))$year) %>% arrange(desc(countment))

#authors with more than three mentions by date of interview
highfreqauthbydate<-btb %>%  filter(author.GRname %in% byauth$author.GRname[byauth$countment>3]) %>% select(author.GRname, interviewee.GRname, Date, interviewee.gender.use) %>% left_join(byauth[,c("author.GRname","countment")]) %>% arrange(desc(countment),author.GRname,Date) %>% mutate(author=factor(author.GRname,levels = rev(unique(author.GRname))))