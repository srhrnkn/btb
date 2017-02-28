library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("xml2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

#bring in log
interviewees<-read.csv("logscripted.csv")
interviewees$Date<-as.Date(x = interviewees$Date,format = "%b. %d, %Y")
authors<-data.frame(name=character(),gender=character(),interviewee=character())
for(i in 1: length(interviewees$Subject)){
  tempauths<-read.csv(paste0(interviewees$Subject[i],"multi.csv"))
  authors<-rbind(authors,cbind(tempauths[,1:2],interviewee=interviewees$Subject[i]))
}
#get rid of entries that aren't actually authors (ones where gender is blank)
authors<-authors[which(authors$gender!=""),]
#refactor to get rid of blanks
authors$name<-factor(authors$name)
authors$gender<-factor(authors$gender)



#older data
oldlog<-read.csv("olderlog.csv")
oldlog$Date<-as.Date(oldlog$Date,format = "%m/%d/%y")

#goodreads api key
grkey<-"SshlDssD6Elfx3hxbeo8g"



#loop through authors
#store info on how much time each call takes
elapsed<-numeric()
authgenders<-character()
authbirthdates<-character()
authtowns<-character()
for(i in 3:15){
#find author id on goodreads by searching name
#ptm<-proc.time()
authname<-authors$name[i]
#search for author's name to get goodreads ID
searchgr<-read_xml(paste0("https://www.goodreads.com/search/index.xml?key=",grkey,"&q=",str_replace_all(string = authname,pattern = " ",replacement = "%20"),"&search[field]=author"))
ids<-data_frame(id=as.numeric(xml_text(xml_find_all(searchgr,"//author//id"))),name=xml_text(xml_find_all(searchgr,"//author//name")))
#need to account for case where author name is not found
authID<-ifelse(length(unique(ids$id[which(ids$name==authname)]))==0,NA,unique(ids$id[which(ids$name==authname)]))
#elapsed<-c(elapsed,(proc.time()-ptm)[3])
Sys.sleep(1.5)

#find author gender on goodreads by loading page with Id and extracting gender
#ptm<-proc.time()
if(!is.na(authID)){
authgr<-read_xml(paste0("https://www.goodreads.com/author/show/",authID,"?format=xml&key=SshlDssD6Elfx3hxbeo8g"))
authgend<-xml_text(xml_find_all(authgr,"//author//gender"))
authgenders<-c(authgenders,authgend)
authbirth<-xml_text(xml_find_all(authgr,"//author//born_at"))
authbirthdates<-c(authbirthdates,authbirth)
authtown<-xml_text(xml_find_all(authgr,"//author//hometown"))
authtowns<-c(authtowns,authtown)
#elapsed<-c(elapsed,(proc.time()-ptm)[3])
Sys.sleep(1.5)
assign(x = paste0("searchgr",str_replace_all(string = authname,pattern = " ",replacement = "_")),value = searchgr)
assign(x = paste0("authgr",str_replace_all(string = authname,pattern = " ",replacement = "_")),value = authgr)
} else {
  authgr<-NA
  authgend<-NA
  authgenders<-c(authgenders,authgend)
  authbirth<-NA
  authbirthdates<-c(authbirthdates,authbirth)
  authtown<-NA
  authtowns<-c(authtowns,authtown)
  elapsed<-c(elapsed,(proc.time()-ptm)[3])
  Sys.sleep(1.5)
  assign(x = paste0("searchgr",str_replace_all(string = authname,pattern = " ",replacement = "_")),value = searchgr)
  assign(x = paste0("authgr",str_replace_all(string = authname,pattern = " ",replacement = "_")),value = authgr)  
}


}

authids<-numeric()
for(i in 1:15){
  ids<-data_frame(id=as.numeric(xml_text(xml_find_all(get(paste0("searchgr",str_replace_all(string = authors$name[i],pattern = " ",replacement = "_"))),"//author//id"))),name=xml_text(xml_find_all(get(paste0("searchgr",str_replace_all(string = authors$name[i],pattern = " ",replacement = "_"))),"//author//name")))
  #need to account for case where author name is not found
  authID<-ifelse(length(unique(ids$id[which(ids$name==authors$name[i])]))==0,NA,unique(ids$id[which(ids$name==authors$name[i])]))  
  authids<-c(authids,authID)
}


GRdata<-rbind(GRdata,data.frame(name=authors$name[1:15],id=authids,gender=authgenders,birthdate=authbirthdates,town=authtowns,is.interviewee=NA,is.author=T))