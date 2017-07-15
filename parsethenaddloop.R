#combined Rparse and addnewdata

#steps to add
#bring in new version of log
#look for new interviewees
#for each new interviewee
  #bring in text files (*need to turn this into loop)
  #bring in authors
#hit goodreads for new interviewees and new authors
#add goodreads to GRdata
#create new btb analysis dataset

library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("xml2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("stringdist", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")


#####bring in new version of log, find new interviewee names
intervieweesnew<-read.csv("logscripted.csv")
intervieweesnew$Date<-as.Date(x = intervieweesnew$Date,format = "%b. %d, %Y")
intervieweesnew<-intervieweesnew[which(!intervieweesnew$Subject %in% interviewees$Subject),]

#### read in text files & parse - need to turn this section into loop##
#for each interviewee:
#read in file 
#use regex to find potential author names
#loop through author names and record genders
#store issues for further look
#wind up with list of author names, genders, interviewees

authorsnew<-data.frame(name=character(),gender=character(),interviewee=character())
#for(i in 1:length(intervieweesnew$Subject[i]))
for(i in 2:3){
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
    tempmulti<-str_extract_all(string = btblines[x],pattern = "((?<![“])[A-Z][\\. ]?\\w+(?=[ \\’\\'-][A-Z][\\. ]?)(?:[\\s\\’\\'-][A-Z][\\. ]?[A-Z\\.?(\\w\\'+)-]+)+)")[[1]]
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
authorsnew<-authorsnew[authorsnew$name!="",]


# create data frame for use in next step


## not sure this is helping
# #clean up initials - NYT uses space, GR does not
# #one cause is spaces between initials, so can fix that before running GR lookups:
# authorsnew$name[grep("[A-Z]\\. [A-Z]\\.",authorsnew$name,perl = T)]<-str_replace(string = authorsnew$name[grep("[A-Z]\\. [A-Z]\\.",authorsnew$name,perl = T)],pattern = " ",replacement = "" )

#record row count of GRdata so can look only at new entries later
GRdatarowcountold<-nrow(GRdata)

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

#add new interviewee and author names to crosswalk
interviewees<-rbind(interviewees,intervieweesnew)
authors<-rbind(authors,cbind(authorsnew, GRID=GRdata$id[match(authorsnew$name,GRdata$name)]))

##quality control: find the sketchy GR matches (do this before btb merge)


mismatches<-GRdata %>% filter(row(GRdata[,1])>GRdatarowcountold&GRdata$stringdistance>0)  %>% data.frame


#then manually went through and checked for mismatches, using this code. or can just eyeball if not too many
i<-3 #starts at 3 - lower ok
mismatches[which(mismatches$stringdistance==i),] #look at mismatches at that level
mismatches$matchOK[which(mismatches$name=="")]<-F #set any that are wrong to F
mismatches$matchOK[which(mismatches$stringdistance==i&is.na(mismatches$matchOK))]<-T #set the rest at that level to T
i<-i+1 #increment up
#Then bring these designations back into GRdata
GRdata$matchOK[which(GRdata$name %in% mismatches$name)]<-mismatches$matchOK[match( GRdata$name[GRdata$name %in% mismatches$name],mismatches$name)]
#at some point clean these up, for now, throw them out at merge

data.frame(cbind(GRdata[which(GRdata$name!=GRdata$GRname),c("name","id","GRname")],dist=stringdist(a = as.character(unlist(GRdata[which(GRdata$name!=GRdata$GRname),"name"])),b = as.character(unlist(GRdata[which(GRdata$name!=GRdata$GRname),"GRname"])))))[rev(order(stringdist(a = as.character(unlist(GRdata[which(GRdata$name!=GRdata$GRname),"name"])),b = as.character(unlist(GRdata[which(GRdata$name!=GRdata$GRname),"GRname"]))))),]


#diff method by GR ID (omits anyone who wasn't a GR hit)
btb<-merge(authors[,c(3,4)],GRdata[which(GRdata$matchOK==T),c(1:2,4:5,9,10)],by.x = "GRID",by.y = "id")
names(btb)[c(1,3:7)]<-paste0("author.",names(btb)[c(1,3:7)])
names(btb)[2]<-"interviewee.name"
btb<-merge(btb,GRdata[,c(1:2,4:5,9,10)],by.x = "interviewee.name",by.y = "name",all.x = T,all.y = F)
names(btb)[8:12]<-paste0("interviewee.",names(btb)[8:12])





