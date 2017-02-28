#steps to add
#bring in new version of log
#look for new interviewees
#for each new interviewee, bring in authors
#hit goodreads for new interviewees and new authors

#add goodreads to GRdata


#data:
#individuals with info about them: name, gender, birthdate, town, interviewee status/recommendation status
#interviewee/author crosswalk

#bring in new version of log
intervieweesnew<-read.csv("logscripted.csv")
intervieweesnew$Date<-as.Date(x = intervieweesnew$Date,format = "%b. %d, %Y")
intervieweesnew<-intervieweesnew[which(!intervieweesnew$Subject %in% interviewees$Subject),]

authorsnew<-data.frame(name=character(),gender=character(),interviewee=character())
for(i in 1: length(intervieweesnew$Subject)){
  tempauths<-read.csv(paste0(intervieweesnew$Subject[i],"multi.csv"))
  authorsnew<-rbind(authorsnew,cbind(tempauths[,1:2],interviewee=intervieweesnew$Subject[i]))
}
#get rid of entries that aren't actually authors (ones where gender is blank)
authorsnew<-authorsnew[which(authorsnew$gender!=""),]
#refactor to get rid of blanks
authorsnew$name<-factor(authorsnew$name)
authorsnew$gender<-factor(authorsnew$gender)


#loop through interviewees to get data
for (i in 1:length(intervieweesnew$Subject)){
  if(!intervieweesnew$Subject[i] %in% GRdata$name){
    tempdetails<-authdetails(intervieweesnew$Subject[i])
    tempdetails$birthdate<-as.Date(tempdetails$birthdate,format = "%Y/%m/%d")
    GRdata<-rbind(GRdata,cbind(tempdetails,is.interviewee=T,is.author=NA,input.gender=intervieweesnew$Subject.gender[i],gender.use=ifelse(test = is.na(tempdetails$gender)|tempdetails$gender==""|substr(tempdetails$gender,1,1)==intervieweesnew$Subject.gender[i],yes = intervieweesnew$Subject.gender[i],no = substr(tempdetails$gender,1,1))))
  } else {
    GRdata$is.interviewee[which(GRdata$name==intervieweesnew$Subject[i])]<-T
  }
}



#loop through authors to get data
authorstemp<-unique(authorsnew$name[which(!authorsnew$name %in% GRdata$name[which(GRdata$is.author)])])
for (i in 1:length(authorstemp)){
  if(!authorstemp[i] %in% GRdata$name){
    tempdetails<-authdetails(authorstemp[i])
    tempdetails$birthdate<-as.Date(tempdetails$birthdate,format = "%Y/%m/%d")
    GRdata<-rbind(GRdata,cbind(tempdetails,is.interviewee=NA,is.author=T,input.gender=authorsnew$gender[match(authorstemp[i],authorsnew$name)],gender.use=ifelse(test = is.na(tempdetails$gender)|tempdetails$gender==""|substr(tempdetails$gender,1,1)==authorsnew$gender[match(authorstemp[i],authorsnew$name)],yes = authorsnew$gender[match(authorstemp[i],authorsnew$name)],no = substr(tempdetails$gender,1,1))))
  } else{
    GRdata$is.author[which(GRdata$name==authorstemp[i])]<-T
  }
}

interviewees<-rbind(interviewees,intervieweesnew)
authors<-rbind(authors,authorsnew)
