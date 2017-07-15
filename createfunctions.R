library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("xml2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
nF<-function(x) {ifelse(is.na(x), F,x)}
countN<-function(x) {as.integer(sum(!is.na(x)))}
#function to:
#search for author
#grab author id
#archive search page
#grab author page
#archive author page
#extract gender, birthdate, hometown and return as data frame row
authdetails<-function(authname){
  #search for author's name to get goodreads ID
  searchgr<-read_xml(paste0("https://www.goodreads.com/search/index.xml?key=",grkey,"&q=",str_replace_all(string = authname,pattern = " ",replacement = "%20"),"&search[field]=author"))
  ids<-data_frame(id=as.numeric(xml_text(xml_find_all(searchgr,"//author//id"))),name=xml_text(xml_find_all(searchgr,"//author//name")))
#if the search comes up with nothing, nrow for ids is 0, set authID and modalauthname to 0; else pick one that matches name entered or most frequently appearing one; if the latter return warning
  if(nrow(ids)==0){
    authID<-NA
    modalauthname<-NA} else{
    modalauthname<-aggregate(id~name,ids,countN)[which.max(aggregate(id~name,ids,countN)[,2]),1]
    if(length(unique(ids$id[which(ids$name==authname)]))==0){
      warning(paste0(authname," no exact match; ",modalauthname," used"))
      authID<-unique(ids$id[which(ids$name==modalauthname)])
    } else{
      authID<-unique(ids$id[which(ids$name==authname)]) 
    }
  }  
  #rest to prevent querying API too rapidly
  Sys.sleep(1.5)
  #get author page
  if(!is.na(authID)){
    authgr<-read_xml(paste0("https://www.goodreads.com/author/show/",authID,"?format=xml&key=SshlDssD6Elfx3hxbeo8g"))
    authgend<-xml_text(xml_find_all(authgr,"//author//gender"))
    authbirth<-xml_text(xml_find_all(authgr,"//author//born_at"))
    authtown<-xml_text(xml_find_all(authgr,"//author//hometown"))
    Sys.sleep(1.5)
  } else {
    authgr<-NA
    authgend<-NA
    authbirth<-NA
    authtown<-NA
    Sys.sleep(1.5)
  }
  #archive both - archiving as character; if need to use will have to use read_xml again
assign(x = paste0("searchgr",str_replace_all(string = authname,pattern = " ",replacement = "_")),value = as.character(searchgr),envir = .GlobalEnv)
assign(x = paste0("authgr",str_replace_all(string = authname,pattern = " ",replacement = "_")),value = as.character(authgr),envir = .GlobalEnv) 
  #return data frame
  data_frame(name=authname,id=authID, gender=authgend, birthdate=authbirth, town=authtown, GRname=ifelse(length(unique(ids$id[which(ids$name==authname)]))==0,modalauthname,as.character(authname)))
  
}

#create data frame - first time only
GRdata<-data_frame(name=character(),id=numeric(), gender=character(), birthdate=character(), town=character(),is.interviewee=logical(),is.author=logical())

#loop through interviewees to get data
for (i in 1:length(interviewees$Subject)){
  if(!interviewees$Subject[i] %in% GRdata$name){
  GRdata<-rbind(GRdata,cbind(authdetails(interviewees$Subject[i]),is.interviewee=T,is.author=NA))
  } else {
      GRdata$is.interviewee[which(GRdata$name==interviewees$Subject[i])]<-T
  }
}



#loop through authors to get data
authorstemp<-unique(authors$name[which(!authors$name %in% GRdata$name[which(GRdata$is.author)])])
for (i in 1:length(authorstemp)){
  if(!authorstemp[i] %in% GRdata$name){
    GRdata<-rbind(GRdata,cbind(authdetails(authorstemp[i]),is.interviewee=NA,is.author=T))
  } else{
    GRdata$is.author[which(GRdata$name==authorstemp[i])]<-T
  }
}

GRdata$gender<-as.factor(GRdata$gender)
GRdata$birthdate<-as.Date(GRdata$birthdate,format = "%Y/%m/%d")

#add hand coded gender to GR data
GRdata$input.gender<-authors$gender[match(GRdata$name,authors$name)]
GRdata$input.gender[is.na(GRdata$input.gender)]<-interviewees$Subject.gender[match(GRdata$name[is.na(GRdata$input.gender)],interviewees$Subject)]


#reconcile - pick GR data if conflicting
GRdata$gender.use<-GRdata$input.gender
GRdata$gender.use[GRdata$input.gender!=str_sub(GRdata$gender,start = 1,end = 1)&!is.na(GRdata$gender)&GRdata$gender!=""]<-str_sub(GRdata$gender,start = 1,end = 1)[GRdata$input.gender!=str_sub(GRdata$gender,start = 1,end = 1)&!is.na(GRdata$gender)&GRdata$gender!=""]

#create analysis dataset
btb<-merge(authors[,c(1,3)],GRdata[,c(1:2,4:5,9)],by = "name",all.x = T,all.y = F)
names(btb)[c(1,3:6)]<-paste0("author.",names(btb)[c(1,3:6)])
names(btb)[2]<-"interviewee.name"
btb<-merge(btb,GRdata[,c(1:2,4:5,9)],by.x = "interviewee.name",by.y = "name",all.x = T,all.y = F)
names(btb)[7:10]<-paste0("interviewee.",names(btb)[7:10])



