#steps to add
#bring in new version of log
#look for new interviewees
#for each new interviewee, bring in authors
#hit goodreads for new interviewees and new authors
#add goodreads to GRdata
#create new btb analysis dataset

library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("xml2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("stringdist", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")


#data:
#individuals with info about them: name, gender, birthdate, town, interviewee status/recommendation status
#interviewee/author crosswalk

#bring in new version of log - this happens before Rparse
intervieweesnew<-read.csv("logscripted.csv")
intervieweesnew$Date<-as.Date(x = intervieweesnew$Date,format = "%b. %d, %Y")
intervieweesnew<-intervieweesnew[which(!intervieweesnew$Subject %in% interviewees$Subject),]

# old way of creating authors new
authorsnew<-data.frame(name=character(),gender=character(),interviewee=character())
for(i in 1: length(intervieweesnew$Subject)){
  tempauths<-read.csv(paste0(intervieweesnew$Subject[i],"multi.csv"))
  authorsnew<-rbind(authorsnew,cbind(tempauths[,1:2],interviewee=intervieweesnew$Subject[i]))
}
for(i in 1: length(intervieweesnew$Subject)){
  tempauths<-read.csv(paste0(intervieweesnew$Subject[i],"single.csv"))
  authorsnew<-rbind(authorsnew,cbind(tempauths[,1:2],interviewee=intervieweesnew$Subject[i]))
}
#get rid of entries that aren't actually authors (ones where gender is blank)
authorsnew<-authorsnew[which(authorsnew$gender!=""),]
#refactor to get rid of blanks
authorsnew$name<-factor(authorsnew$name)
authorsnew$gender<-factor(authorsnew$gender)


#loop through interviewees to get data
#gender.use - use gender from authors if it matches GR data or if GR data is blank; else use GR data
for (i in 1:length(intervieweesnew$Subject)){
  if(!intervieweesnew$Subject[i] %in% GRdata$name){
    tempdetails<-authdetails(intervieweesnew$Subject[i])
    tempdetails$birthdate<-as.Date(tempdetails$birthdate,format = "%Y/%m/%d")
    GRdata<-rbind(GRdata,cbind(tempdetails,is.interviewee=T,is.author=NA,input.gender=intervieweesnew$Subject.gender[i],gender.use=ifelse(test = is.na(tempdetails$gender)|tempdetails$gender==""|substr(tempdetails$gender,1,1)==intervieweesnew$Subject.gender[i],yes = as.character(intervieweesnew$Subject.gender[i]),no = substr(tempdetails$gender,1,1)),stringdistance=stringdist(a = tempdetails$name,b = tempdetails$GRname),matchOK=ifelse(test = stringdist(a = tempdetails$name,b = tempdetails$GRname)<3,yes = T,no = NA)))
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
    GRdata<-rbind(GRdata,cbind(tempdetails,is.interviewee=NA,is.author=T,input.gender=authorsnew$gender[match(authorstemp[i],authorsnew$name)],gender.use=ifelse(test = is.na(tempdetails$gender)|tempdetails$gender==""|substr(tempdetails$gender,1,1)==authorsnew$gender[match(authorstemp[i],authorsnew$name)],yes = as.character(authorsnew$gender[match(authorstemp[i],authorsnew$name)]),no = substr(tempdetails$gender,1,1)),stringdistance=stringdist(a = tempdetails$name,b = tempdetails$GRname),matchOK=ifelse(test = stringdist(a = tempdetails$name,b = tempdetails$GRname)<3,yes = T,no = NA)))
  } else{
    GRdata$is.author[which(GRdata$name==authorstemp[i])]<-T
  }
}

interviewees<-rbind(interviewees,intervieweesnew)
authors<-rbind(authors,cbind(authorsnew, GRID=GRdata$id[match(authorsnew$name,GRdata$name)]))

#create new analysis dataset
# btb<-merge(authors[,c(1,3)],GRdata[,c(1:2,4:5,9,10)],by = "name",all.x = T,all.y = F)
# names(btb)[c(1,3:7)]<-paste0("author.",names(btb)[c(1,3:7)])
# names(btb)[2]<-"interviewee.name"
# btb<-merge(btb,GRdata[,c(1:2,4:5,9,10)],by.x = "interviewee.name",by.y = "name",all.x = T,all.y = F)
# names(btb)[8:12]<-paste0("interviewee.",names(btb)[8:12])


#diff method by GR ID (omits anyone who wasn't a GR hit)
btb<-merge(authors[,c(3,4)],GRdata[which(GRdata$matchOK==T),c(1:2,4:5,9,10)],by.x = "GRID",by.y = "id")
names(btb)[c(1,3:7)]<-paste0("author.",names(btb)[c(1,3:7)])
names(btb)[2]<-"interviewee.name"
btb<-merge(btb,GRdata[,c(1:2,4:5,9,10)],by.x = "interviewee.name",by.y = "name",all.x = T,all.y = F)
names(btb)[8:12]<-paste0("interviewee.",names(btb)[8:12])


##quality control: find the sketchy GR matches:

#one cause is spaces between initials, so can fix that before running GR lookups:
authorsnew$name[grep("[A-Z]\\. [A-Z]\\.",authorsnew$name,perl = T)]<-str_replace(string = authorsnew$name[grep("[A-Z]\\. [A-Z]\\.",authorsnew$name,perl = T)],pattern = " ",replacement = "" )

#calc distance
GRdata$stringdistance<-stringdist(a = GRdata$name,b = GRdata$GRname)
GRdata$matchOK<-NA
GRdata$matchOK[GRdata$stringdistance<3]<-T
mismatches<- GRdata %>% filter(is.na(matchOK)) %>% select(name,id,GRname,stringdistance) %>%  data.frame()
mismatches<-mismatches[rev(order(mismatches$stringdistance)),]
mismatches$matchOK<-NA
#then manually went through and checked for mismatches, using this code
i<-3 #starts at 3 - lower ok
mismatches[which(mismatches$stringdistance==i),] #look at mismatches at that level
mismatches$matchOK[which(mismatches$name=="")]<-F #set any that are wrong to F
mismatches$matchOK[which(mismatches$stringdistance==i&is.na(mismatches$matchOK))]<-T #set the rest at that level to T
i<-i+1 #increment up
#Then bring these designations back into GRdata
GRdata$matchOK[which(GRdata$name %in% mismatches$name)]<-mismatches$matchOK[match( GRdata$name[GRdata$name %in% mismatches$name],mismatches$name)]
#at some point clean these up, for now, throw them out at merge

data.frame(cbind(GRdata[which(GRdata$name!=GRdata$GRname),c("name","id","GRname")],dist=stringdist(a = as.character(unlist(GRdata[which(GRdata$name!=GRdata$GRname),"name"])),b = as.character(unlist(GRdata[which(GRdata$name!=GRdata$GRname),"GRname"])))))[rev(order(stringdist(a = as.character(unlist(GRdata[which(GRdata$name!=GRdata$GRname),"name"])),b = as.character(unlist(GRdata[which(GRdata$name!=GRdata$GRname),"GRname"]))))),]
