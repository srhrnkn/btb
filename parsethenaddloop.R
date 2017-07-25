#this script looks for new BTB interviews and adds them to the dataset

#General outline:
#bring in new version of interviewee log
#hit against current list to look for new interviewees
#for each new interviewee
  #bring in text file 
  #look for potential author names
  #loop through names and enter gender, flag errors
  #loop through errors & fix
#wind up with list of author names, genders, interviewees
#hit goodreads for data re new interviewees and new authors, add to GRdata
#error checking for erroneous hits - look for cases where name and GR name are too far apart, manually fix
#merge GRdata and author/interview listing to create new btb analysis dataset with interviewee & author 

#load libraries ####
library("xml2", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("lubridate", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("stringdist", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

#create functions ####
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
#!!requires object grkey - API authorization for GoodReads!!##
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

# bring in new version of log, find new interviewee names ####
intervieweesnew<-read.csv("logscripted.csv")
intervieweesnew$Date<-as.Date(x = intervieweesnew$Date,format = "%b. %d, %Y")
intervieweesnew<-intervieweesnew[which(!intervieweesnew$Subject %in% interviewees$Subject),]

# read in text files & parse ####
#for each interviewee:
#read in file 
#use regex to find potential author names
#loop through author names and record genders
#store issues for further look
#wind up with list of author names, genders, interviewees

authorsnew<-data.frame(name=character(),gender=character(),interviewee=character())
for(i in 1:length(intervieweesnew$Subject)){
  #read in file 
  interviewee<-as.character(intervieweesnew$Subject[i])
  btblines<-readLines(con = paste0(interviewee,'.txt'),n = -1, encoding='UTF-8')
  
  #use regex to find potential author names
  
  #var to store regex results looking for multiple initial cap words in a row
  multi<-character()
  #var to store regex results looking for single initial cap words that weren't in multi
  single<-character()
  
  #first look for multi - store only if something is found in the line
  print(paste0("New interviewee: ",interviewee))
  print("type gender; add slash at end if unsure or error in string")
  for(x in 1:length(btblines)){
    tempmulti<-str_extract_all(string = btblines[x],pattern = "((?<![“])([:upper:]{1}(\\. )?)+[:lower:]+(?=([ \\’\\'-][:upper:]{1}(\\. )?)+)(?:[\\s\\’\\'-][:upper:]{1}(\\. )?[[:upper:]{1}([:lower:]\\'+)-]+)+)")[[1]]
    tempmulti<-str_replace(string = tempmulti,pattern = "\\.$",replacement = "")
    multi<-c(multi,tempmulti)
  }
  #now look for single but only store if they weren't in multi
  for(x in 1:length(btblines)){
    tempsingle<-str_extract_all(string = btblines[x],pattern = "((?<![“])[A-Z][A-Z#(\\w+)]+)")[[1]]
    tempsingle<-str_replace(string = tempsingle,pattern = "\\.$",replacement = "")
    single<-c(single,tempsingle[unlist(lapply(X = tempsingle,FUN = function(x){max(grepl(pattern = x,x = multi))}))==0])
  }
  
  #var to store genders
  multigend = character()
  
  #loop through multi and register genders
  for(x in 1:length(multi)){
    tempmultigend<-readline(prompt = paste0(multi[x]," "))
    multigend<-c(multigend, tempmultigend)
  }
  
  #bring together names, genders
  multiframe<-data.frame(name=multi,gender=multigend,stringsAsFactors = F)
  multiframe<-multiframe[which(multiframe$gender!=""),]
  
  #for single just view and tag any that are real
  singlegend = character()
  print(single)
  singleindices<-readline(prompt = "Which items are authors? enter index numbers separated by spaces")
  singleindices<-as.integer(unlist(str_split(string = singleindices," ")))
  for(x in 1:length(singleindices)){
    tempsinglegend<-readline(prompt = paste0(single[singleindices[x]]," "))
    singlegend<-c(singlegend, tempsinglegend)
  }
  
  singleframe<-data.frame(name=single[singleindices],gender=singlegend,stringsAsFactors = F)
  authorsnew<-rbind(authorsnew, rbind(cbind(multiframe,interviewee=interviewee),cbind(singleframe,interviewee=interviewee)))
}

#fix flagged

#find rownums of flagged entries
fixnums<-grep("/",authorsnew$gender)
#loop through flagged entries and prompt for name,gender string - separated by comma, no quotes
print("for each author enter correct name and gender, separated by comma, no quotes. if not an author just enter comma")
for(i in 1:length(fixnums)){
  tempfix<-readline(prompt = paste(authorsnew[fixnums[i],],collapse = " "))
  authorsnew[fixnums[i],1:2]<-unlist(str_split(string = tempfix,pattern = ","))
}
#delete any blank lines
authorsnew<-authorsnew[authorsnew$name!=""&authorsnew$gender!="",]

#hit goodreads for data re new interviewees and new authors, add to GRdata ####

#record row count of GRdata so can look only at new entries later. If GRdata doesn't exist yet (ie, starting from scratch), create it
if(exists("GRdata")){
GRdatarowcountold<-nrow(GRdata)} else{
  GRdata<-data_frame(name=character(),id=numeric(), gender=factor(levels = c("","female","male")), birthdate=as.Date(x = integer(0), origin = "1970-01-01"), town=character(),is.interviewee=logical(),is.author=logical(),input.gender=factor(levels=c("f","m")),gender.use=factor(levels=c("f","m")), GRname=character(),stringdistance=numeric(),matchOK=logical())
}


#loop through interviewees to get GR data
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

#loop through authors to get GR data
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

# quality control: find the sketchy GR matches (do this before btb merge) ####
mismatches<-GRdata %>% filter(row(GRdata[,1])>GRdatarowcountold&GRdata$stringdistance>0)  %>% data.frame

#if not too many, just eyeball and designate appropriately
GRdata[which(GRdata$name %in% mismatches$name),"matchOK"]<-c()

#or can use this:
# #go through each level of distance and look for mismatches
# i<-3 #starts at 3 - lower ok
# mismatches[which(mismatches$stringdistance==i),] #look at mismatches at that level
# mismatches$matchOK[which(mismatches$name=="")]<-F #set any that are wrong to F
# mismatches$matchOK[which(mismatches$stringdistance==i&is.na(mismatches$matchOK))]<-T #set the rest at that level to T
# i<-i+1 #increment up
# #Then bring these designations back into GRdata
# GRdata$matchOK[which(GRdata$name %in% mismatches$name)]<-mismatches$matchOK[match( GRdata$name[GRdata$name %in% mismatches$name],mismatches$name)]
# #at some point clean these up, for now, throw them out at merge

#add new interviewee and author names to crosswalk ####
interviewees<-rbind(interviewees,cbind(intervieweesnew,GRID=GRdata$id[match(intervieweesnew$Subject,GRdata$name)]))
authors<-rbind(authors,cbind(authorsnew, GRID=GRdata$id[match(authorsnew$name,GRdata$name)]))


#create dataset of authors & interviewees merging by GR ID (omits anyone who wasn't a GR hit) ####
#could prob redo this with dplyr
btb<-merge(authors[,c(3,4)],GRdata[which(GRdata$matchOK==T),c(2,4:5,9,10)],by.x = "GRID",by.y = "id")
names(btb)[c(1,3:6)]<-paste0("author.",names(btb)[c(1,3:6)])
names(btb)[2]<-"interviewee.name"
btb<-merge(btb,GRdata[,c(1:2,4:5,9,10)],by.x = "interviewee.name",by.y = "name",all.x = T,all.y = F)
names(btb)[7:11]<-paste0("interviewee.",names(btb)[7:11])
btb<-merge(btb,interviewees[,c("GRID","Date")],by.x="interviewee.id",by.y="GRID")
#dedupe by GRID
btb<-unique(btb)

#save btb so as to load in rmd file.  ####
#save GRdata just for safekeeping
save(btb,file = "btb.Rdata")
save(GRdata,file="GRdata.Rdata")  



