#visualizations of btb data
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("xml2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

#number of interviewees
btb %>% distinct(interviewee.GRname) %>% nrow()


#number of unique authors
btb %>% distinct(author.GRname) %>% nrow()

#count author mentions (descending)
aggregate(interviewee.name~author.GRname,btb,countN)[rev(order(aggregate(interviewee.name~author.GRname,btb,countN)[,2])),]

#plot author mention counts
hist(aggregate(interviewee.name~author.GRname,btb,countN)[rev(order(aggregate(interviewee.name~author.GRname,btb,countN)[,2])),2])

#authors per interviewee

btb %>% distinct(interviewee.GRname,author.GRname) %>% nrow()

#count interviewees by gender
btb %>% distinct(interviewee.GRname)  %>% summarise(f=rsum(author.gender.use=="f"),m=rsum(author.gender.use=="m"))

#count recommended authors by gender
btb %>% distinct(author.GRname) %>% summarise(f=rsum(author.gender.use=="f"),m=rsum(author.gender.use=="m"))

#perc recommended authors by gender
(btb %>% distinct(author.GRname) %>% summarise(m=rsum(author.gender.use=="m"),f=rsum(author.gender.use=="f")))/(btb %>% distinct(author.GRname) %>% nrow())

#barplot
barplot(table(btb$author.gender.use,btb$interviewee.gender.use,dnn = c("authors","interviewees")/(btb %>% distinct(author.GRname) %>% nrow())),beside = T,legend.text = c("f","m"))

#barplot by percent of gender of interviewee
barplot(sweep(x = table(btb$author.gender.use,btb$interviewee.gender.use,dnn = c("authors","interviewees")),MARGIN = 2,STATS = margin.table(table(btb$author.gender.use,btb$interviewee.gender.use),2),FUN = "/"))

#animate a barplot as interviewes are added?