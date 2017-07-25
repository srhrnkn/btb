library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("xml2", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
nF<-function(x) {ifelse(is.na(x), F,x)}
countN<-function(x) {as.integer(sum(!is.na(x)))}
rmean<-function(x){mean(x,na.rm=T)}
rmeanr<-function(x){round(mean(x,na.rm=T),2)}
rmedian<-function(x){median(x,na.rm=T)}
rsum<-function(x) {sum(x,na.rm=T)}
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
    authgr<-read_xml(paste0("https://www.goodreads.com/author/show/",authID,"?format=xml&key=",grkey))
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