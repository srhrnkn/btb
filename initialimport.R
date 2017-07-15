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

#add in single
singleauthors<-data.frame(name=character(),gender=character(),interviewee=character())
for(i in 1: length(interviewees$Subject)){
  tempauths<-read.csv(paste0(interviewees$Subject[i],"single.csv"))
  singleauthors<-rbind(singleauthors,cbind(tempauths[,1:2],interviewee=interviewees$Subject[i]))
}
#get rid of entries that aren't actually authors (ones where gender is blank)
singleauthors<-singleauthors[which(singleauthors$gender!=""),]
#refactor to get rid of blanks
singleauthors$name<-factor(singleauthors$name)
singleauthors$gender<-factor(singleauthors$gender)

#get IDs where already avail
singleauthors$GRID<-GRdata$id[mapply(FUN = function(x){min(grep(x,GRdata$name))},singleauthors$name)]
singleauthors$GRname<-GRdata$name[mapply(FUN = function(x){min(grep(x,GRdata$name))},singleauthors$name)]
singleauthors$multGR<-mapply(FUN = function(x){sum(grepl(x,GRdata$name))},singleauthors$name)>1




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

#one time fix for single authors
#add to GRdata - get full name
#add to authors crosswalk

#get GR data for new - problem is that authdetails requires match of name input and GR name, for partial names it won't work
authorstemp<-unique(singleauthors$name[which(is.na(singleauthors$GRname))])
for (i in 1:length(authorstemp)){
  if(!authorstemp[i] %in% GRdata$name){
    tempdetails<-authdetails(authorstemp[i])
    tempdetails$birthdate<-as.Date(tempdetails$birthdate,format = "%Y/%m/%d")
    GRdata<-rbind(GRdata,cbind(tempdetails,is.interviewee=NA,is.author=T,input.gender=authorsnew$gender[match(authorstemp[i],authorsnew$name)],gender.use=ifelse(test = is.na(tempdetails$gender)|tempdetails$gender==""|substr(tempdetails$gender,1,1)==authorsnew$gender[match(authorstemp[i],authorsnew$name)],yes = as.character(authorsnew$gender[match(authorstemp[i],authorsnew$name)]),no = substr(tempdetails$gender,1,1))))
    singleauthors$GRID[match(authorstemp[i],singleauthors$name)]<-tempdetails$id
    singleauthors$GRname[match(authorstemp[i],singleauthors$name)]<-tempdetails$GRname
  } else{
    GRdata$is.author[which(GRdata$name==authorstemp[i])]<-T
  }
}
#fix errors
#Elisheba Johnson isn't coming up in search - set to NA
GRdata[which(GRdata$name=="Elisheba Johnson"),c(2:5,10)]<-NA
singleauthorerrors<-data.frame(name=c("Hawthorne","Fuchs","DuBois","Tea Obreht"),GRdataRow=match(c("Hawthorne","Fuchs","DuBois","Tea Obreht"),GRdata$name),better=c("Nathaniel Hawthorne","Jurgen Fuchs","W.E.B. DuBois","Téa Obreht"))

for(i in 1:4){
  tempdetails<-authdetails(singleauthorerrors$better[i])
  tempdetails$birthdate<-as.Date(tempdetails$birthdate,format = "%Y/%m/%d")
  GRdata[singleauthorerrors$GRdataRow[i],c(2:5,10)]<-tempdetails[2:6]
}

#update singleauthors
singleauthors$GRID[which(is.na(singleauthors$GRID))]<-GRdata$id[mapply(FUN = function(x){min(grep(x,GRdata$name))},singleauthors$name[which(is.na(singleauthors$GRID))])]
singleauthors$GRname[which(is.na(singleauthors$GRID))]<-GRdata$GRname[mapply(FUN = function(x){min(grep(x,GRdata$name))},singleauthors$name[which(is.na(singleauthors$GRID))])]
singleauthors$multGR[which(is.na(singleauthors$GRID))]<-mapply(FUN = function(x){sum(grepl(x,GRdata$name))},singleauthors$name[which(is.na(singleauthors$GRID))])>1
singleauthors$GRname[is.na(singleauthors$GRname)]<-GRdata$GRname[match(singleauthors$GRID[is.na(singleauthors$GRname)],GRdata$id)]


GRdata$input.gender[474:517]<-singleauthors$gender[match(GRdata$id[474:517],singleauthors$GRID)]
GRdata$gender.use[474:517]<-ifelse(test = is.na(GRdata$gender[474:517])|GRdata$gender[474:517]==""|substr(GRdata$gender[474:517],1,1)==GRdata$input.gender[474:517],yes = as.character(GRdata$input.gender[474:517]),no = substr(GRdata$gender[474:517],1,1))

#add to authors but use GR name rather than name bc more likely to match previously used author name
authors<-rbind(authors,data.frame(name=singleauthors$GRname,gender=singleauthors$gender,interviewee=singleauthors$interviewee))

#add GR id to authors
authors$GRID<-GRdata$id[match(authors$name,GRdata$name)]
authors$GRID[is.na(authors$GRID)]<-GRdata$id[match(authors$name[is.na(authors$GRID)],GRdata$GRname)]


#one time fix for GR data that was coming out null because GR name didn't match btb name
GRdata$GRname<-GRdata$name
GRnas<-as.data.frame(GRdata[which(is.na(GRdata$id)),"name"])[,1]
for(i in 1:length(GRnas)){
  tempname<-as.character(GRnas[i])
  tempdetails<-authdetails(tempname)
  GRdata[which(GRdata$name==tempname),"id"]<-tempdetails$id
  GRdata[which(GRdata$name==tempname),"gender"]<-tempdetails$gender
  GRdata[which(GRdata$name==tempname),"birthdate"]<-as.Date(tempdetails$birthdate,format = "%Y/%m/%d")
  GRdata[which(GRdata$name==tempname),"town"]<-tempdetails$town
  GRdata[which(GRdata$name==tempname),"GRname"]<-tempdetails$GRname
}

