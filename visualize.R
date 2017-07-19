#visualizations of btb data
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("xml2", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("lubridate", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

#number of interviewees
btb %>% distinct(interviewee.GRname) %>% nrow()


#number of unique authors
btb %>% distinct(author.GRname) %>% nrow()

#count author mentions (descending)
aggregate(interviewee.name~author.GRname,btb,countN)[rev(order(aggregate(interviewee.name~author.GRname,btb,countN)[,2])),][1:100,]

#plot author mention counts
hist(aggregate(interviewee.name~author.GRname,btb,countN)[rev(order(aggregate(interviewee.name~author.GRname,btb,countN)[,2])),2])

#authors per interviewee

(btb %>% distinct(interviewee.GRname,author.GRname) %>% nrow())/(btb %>% distinct(interviewee.GRname) %>% nrow())

#count interviewees by gender
btb %>% distinct(interviewee.GRname,interviewee.gender.use)  %>% summarise(f=rsum(interviewee.gender.use=="f"),m=rsum(interviewee.gender.use=="m"))

#count recommended authors by gender
btb %>% distinct(author.GRname) %>% summarise(f=rsum(author.gender.use=="f"),m=rsum(author.gender.use=="m"))

#perc recommended authors by gender
(btb %>% distinct(author.GRname) %>% summarise(m=rsum(author.gender.use=="m"),f=rsum(author.gender.use=="f")))/(btb %>% distinct(author.GRname) %>% nrow())

#barplot
barplot(table(btb$author.gender.use,btb$interviewee.gender.use,dnn = c("authors","interviewees")/(btb %>% distinct(author.GRname) %>% nrow())),beside = T,legend.text = c("f","m"))

#barplot by percent of gender of interviewee
barplot(sweep(x = table(btb$author.gender.use,btb$interviewee.gender.use,dnn = c("authors","interviewees")),MARGIN = 2,STATS = margin.table(table(btb$author.gender.use,btb$interviewee.gender.use),2),FUN = "/"),names.arg = c("female interviewees","male interviewees"),legend.text = c("female authors","male authors"),axes = F)

#age of interviewees
btb %>% distinct(interviewee.GRname)  %>% mutate(age=trunc(new_interval(start = interviewee.birthdate, end = Sys.Date()) / duration(num = 1, units = "years"))) %>% group_by(interviewee.gender.use) %>% summarise(rmean(age))

#animate a barplot as interviewes are added?


##percent female recs by interviewee
btb %>% group_by(interviewee.GRname) %>% summarise(percf=rsum(author.gender.use=="f")/countN(author.gender.use=="f"))

#counts with variance

btb %>% group_by(interviewee.GRname) %>% summarise(countf=rsum(author.gender.use=="f"),countauth=countN(author.gender.use=="f")) %>% mutate(percf=countf/countauth) %>% mutate(varf=sqrt(percf*(1-percf)/countauth))

