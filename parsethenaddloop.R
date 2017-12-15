#this script looks for new BTB interviews and adds them to the dataset
#simplified to stop manually inputting gender info - just asks if it's an author or not, then gets the gender data from wikipedia (with final manual input step)
#altered beginning to automate process of getting data from NYTimes site

#General outline:
#visit btb page at NYTimes.com, get names and links
#hit against current list to look for new interviewees
#for each new interviewee
#scrape interview page
#look for potential author names
#loop through names and flag if real name, flag text errrs
#loop through errors & fix
#wind up with list of author names, interviewees
#hit goodreads for data re new interviewees and new authors, add to GRdata
#error checking for erroneous hits - look for cases where name and GR name are too far apart, manually fix
#hit wikipedia for residual gender data, birthdate data
#error check for data still missing
#merge GRdata and author/interview listing to create new btb analysis dataset with interviewee & author 

#load libraries ####
library("xml2", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("lubridate", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("stringdist", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rvest", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("WikipediR", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("tidytext", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

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

#function wikigetdata - get birthdate and gender info from wikipedia, based on a vector of names (assumes GR names)

wikigetdata<-function(namevector){
  wikidata<-data.frame(GRID=numeric(),GRname=character(),wikibirthdate=as.Date(x = integer(0), origin = "1970-01-01"),wikigender=character(),wikimsg=character())
  for(i in 1:length(namevector)){
    name<-namevector[i]
    GRID<-NA
    birthdate<-NA
    gender<-NA
    msg<-NA
    try({
      wikifull<- page_content(language = "en",project = "wikipedia",page_name = name)
    })
    if(!exists("wikifull")){
      msg<-"no wiki page"} else{
        if(wikifull$parse$text$`*` %>% read_html() %>% xml_text() %>% str_detect("This disambiguation page")){
          msg<-"disambiguation"
        } else{
          tables<- wikifull$parse$text$`*` %>% read_html() %>%  xml2::xml_find_all(".//table")
          if(length(tables)==0){
            msg<-"no tables"
          } else {
            if(is.na(wikibiotablenum(wikifull))) {
              msg<-"no bio table"
            }
            else {
              table<-tables[[wikibiotablenum(wikifull)]] %>%  html_table(fill = T)
              table<-table[,1:2]
              names(table)<-c("item","data")
              birthdate<-table %>% filter(item=="Born") %>% select(data) %>% str_extract(pattern = "\\d+-\\d+-\\d+") %>% parse_date_time("ymd") %>% as.Date()
            }
          }
        }
        
        assign(x = paste0("wikifull",str_replace_all(string = name,pattern = " ",replacement = "_")),value = wikifull,envir = .GlobalEnv)
        assign(x = paste0("wikitable",str_replace_all(string = name,pattern = " ",replacement = "_")),value = table,envir = .GlobalEnv)
        
        temptext<-data.frame(line=1, text=wikifull$parse$text$`*` %>% read_html %>% xml_text(),stringsAsFactors = F)
        temptokens<-temptext %>% unnest_tokens(output = word,input = text)
        temptokens$row<-as.numeric(rownames(temptokens))
        shehercount<-temptokens %>% filter(word %in% c("she","She","her","Her")) %>% count() %>% pull()
        hehimcount<-temptokens %>% filter(word %in% c("he","He","him","Him")) %>% count() %>% pull()
        if(nz(shehercount)==nz(hehimcount)){
          gender<-NA
        } else if(nz(shehercount)>nz(hehimcount)){
          gender<-("f")
        } else {
          gender<-("m")
        }
      }
    
    tempdetails<-data.frame(GRID=GRdata$id[match(name,GRdata$GRname)],GRname=name,wikibirthdate=birthdate,wikigender=gender,wikimsg=msg,stringsAsFactors = F)
    wikidata<-rbind(wikidata,tempdetails)
    rm(wikifull)
  }
  return(wikidata)
}


#find the bio table within a wikipedia page - used in above function
wikibiotablenum<- function(pagecontent){
  try(findborn<-unlist(lapply(X = (pagecontent$parse$text$`*` %>% read_html() %>%  xml2::xml_find_all(".//table") %>%  html_table(fill = T)),FUN = function(x){max(grepl("Born",x[[1]]))})))
  if(exists("findborn")){
    if(sum(findborn)==0){
      biotablenum<-NA 
    } else {
      biotablenum<-which.max(findborn)
    }
    return(biotablenum)
  } else {
    return(NA)
  }
}






#look at main btb page, get new interviews and links
btbcolumn<-read_html("https://www.nytimes.com/column/by-the-book")
headlines <- btbcolumn %>% html_nodes(css = ".headline") %>% html_text(trim = T)
dates <- btbcolumn %>% html_nodes(css = ".dateline") %>% html_text(trim = T)
dates <-as.Date(dates , format = "%b. %d, %Y")
links <- btbcolumn %>% html_nodes(css = ".story-link") %>% html_attr(name = "href")
btbnames<-str_replace(headlines,": By the Book","")
isnew<-!btbnames %in% interviewees$Subject
btbnewcols<-data.frame(Date=dates,Link=links, Subject=btbnames,isnew)
btbnewcols<-unique(btbnewcols[(btbnewcols$isnew),])
#order by date so adding older interviews first
btbnewcols<-btbnewcols[order(btbnewcols$Date),]

# read in text files & parse ####
#for each interviewee:
#read in file 
#use regex to find potential author names
#loop through author names and record: is a name (y), need attention (/), not a name (enter)
#store issues for further look
#wind up with list of author names, genders, interviewees

##make intervieweesnew data frame because that was already used in the code. Need to loop through and add gender because that had been added in the excel file. Other fields aren't being used but are left over - could clean this up at some point.

intervieweesnew<-data.frame(btbnewcols[,c("Date","Subject")],Subject.gender = NA, Count.Authors = NA,Count.Female.Authors= NA, Count.Male.Authors= NA, printoronline = "online",Notes = NA)
print("New interviewees: type f or m")
for(i in 1:nrow(intervieweesnew)){
  intervieweesnew[i,3]<-readline(prompt = paste0(intervieweesnew[i,2]," "))
}

#now back on track, except this code is changed to hit the link directly and to save to a text file

authorsnew<-data.frame(name=character(),isauth=character(),interviewee=character())

for(i in 1:length(intervieweesnew$Subject)){
  #read in file & save
  interviewee<-as.character(intervieweesnew$Subject[i])
  btblines<-read_html(as.character(btbnewcols$Link[i]))
  btblines<-btblines %>% html_nodes(css = ".story-body-text") %>% html_text()
  writeLines(text = btblines, con = paste0(btbnewcols$Subject[i],".txt"))
  
  #use regex to find potential author names
  
  #var to store regex results looking for multiple initial cap words in a row
  multi<-character()
  #var to store regex results looking for single initial cap words that weren't in multi
  single<-character()
  
  #first look for multi - store only if something is found in the line
  print(paste0("New interviewee: ",interviewee))
  print("type y if is an author name; type slash if unsure or error in string")
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
  
  #var to store input data: is this an author name?
  isauth = character()
  
  #loop through multi and register authorness
  for(x in 1:length(multi)){
    tempmulti<-readline(prompt = paste0(multi[x]," "))
    isauth<-c(isauth, tempmulti)
  }
  
  #bring together names, genders
  multiframe<-data.frame(name=multi,isauth,stringsAsFactors = F)
  multiframe<-multiframe[which(multiframe$isauth!=""),]
  
  #for single just view and tag any that are real
  singleisauth = character()
  print(single)
  singleindices<-readline(prompt = "Which items are authors? enter index numbers separated by spaces ")
  singleindices<-as.integer(unlist(str_split(string = singleindices," ")))
  print("Enter y if author name ok, / if needs editing")
  for(x in 1:length(singleindices)){
    tempsingle<-readline(prompt = paste0(single[singleindices[x]]," "))
    singleisauth<-c(singleisauth, tempsingle)
  }
  
  singleframe<-data.frame(name=single[singleindices],isauth=singleisauth,stringsAsFactors = F)
  authorsnew<-rbind(authorsnew, rbind(cbind(multiframe,interviewee=interviewee),cbind(singleframe,interviewee=interviewee)))
}

#fix flagged

#find rownums of flagged entries
fixnums<-grep("/",authorsnew$isauth)
#loop through flagged entries and prompt for name,gender string - separated by comma, no quotes
print("for each author enter correct name, no quotes")
for(i in 1:length(fixnums)){
  tempfix<-readline(prompt = paste0(authorsnew$name[fixnums[i]], " "))
  authorsnew$name[fixnums[i]]<-tempfix
  authorsnew$isauth[fixnums[i]]<-"y"
}
#delete any blank lines
authorsnew<-authorsnew[authorsnew$name!=""&authorsnew$isauth!="",]

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
    GRdata<-rbind(GRdata,cbind(tempdetails,is.interviewee=NA,is.author=T,input.gender=NA,gender.use=NA,stringdistance=stringdist(a = tempdetails$name,b = tempdetails$GRname),matchOK=ifelse(test = stringdist(a = tempdetails$name,b = tempdetails$GRname)<3,yes = T,no = NA)))
  } else{
    GRdata$is.author[which(GRdata$name==authorstemp[i])]<-T
  }
}



# quality control: find the sketchy GR matches (do this before btb merge) ####
mismatches<-GRdata %>% filter(row(GRdata[,1])>GRdatarowcountold&GRdata$stringdistance>0)  %>% data.frame

#if not too many, just eyeball and designate appropriately
GRdata[which(GRdata$name %in% mismatches$name),"matchOK"]<-c()

#manually fix the ones where the search just went wonky
fixname<-#name to fix
newlookup<-#new string to use for lookup
GRdata[which(GRdata$name==fixname),c("id","gender","birthdate","town","GRname")]<-authdetails(newlookup)[,2:6]
GRdata$matchOK[which(GRdata$name==fixname)]<-T

#get wikipedia gender###
wikinew<-wikigetdata(namevector = GRdata$GRname[(GRdatarowcountold+1):length(GRdata$GRname)])


#add missing genders
#find rownums of flagged entries
fixnums<-which(is.na(wikinew$wikigender))
#loop through flagged entries and prompt for name,gender string - separated by comma, no quotes
print("for each author enter m, f, or nothing")
for(i in 1:length(fixnums)){
  tempfix<-readline(prompt = paste0(wikinew$GRname[fixnums[i]]," "))
  wikinew$wikigender[fixnums[i]]<-tempfix
}

#add gender and bday data to GRnames
#for bday only the ones where GR data doesn't have bday
GRdata$birthdate[which((GRdata$id %in% wikinew$GRID)&is.na(GRdata$birthdate))]<-wikinew$wikibirthdate[match(GRdata$id[which((GRdata$id %in% wikinew$GRID)&is.na(GRdata$birthdate))],wikinew$GRID)]
#for gender, the wiki data takes the place of input gender, then evaluate
GRdata$input.gender[which(GRdata$id %in% wikinew$GRID)]<-wikinew$wikigender[match(GRdata$id[which(GRdata$id %in% wikinew$GRID)],wikinew$GRID)]
GRdata$gender.use[which(GRdata$id %in% wikinew$GRID)]<-case_when(GRdata$gender[which(GRdata$id %in% wikinew$GRID)]=="" ~ as.character(GRdata$input.gender[which(GRdata$id %in% wikinew$GRID)]), T ~ substr(GRdata$gender[which(GRdata$id %in% wikinew$GRID)],1,1))


#add new interviewee and author names to crosswalk ####
interviewees<-rbind(interviewees,cbind(intervieweesnew,GRID=GRdata$id[match(intervieweesnew$Subject,GRdata$name)]))
authorsnew$gender<-NA
authors<-rbind(authors,cbind(authorsnew[,c(1,3:4)], GRID=GRdata$id[match(authorsnew$name,GRdata$name)]))


#create dataset of authors & interviewees merging by GR ID (omits anyone who wasn't a GR hit) ####
#could prob redo this with dplyr
btb<-merge(authors[,c(3,4)],GRdata[which(GRdata$matchOK==T&!is.na(GRdata$gender.use)),c(2,4:5,9,10)],by.x = "GRID",by.y = "id")
names(btb)[c(1,3:6)]<-paste0("author.",names(btb)[c(1,3:6)])
names(btb)[2]<-"interviewee.name"
btb<-merge(btb,GRdata[,c(1:2,4:5,9,10)],by.x = "interviewee.name",by.y = "name",all.x = T,all.y = F)
names(btb)[7:11]<-paste0("interviewee.",names(btb)[7:11])
btb<-merge(btb,interviewees[,c("GRID","Date")],by.x="interviewee.id",by.y="GRID")
#dedupe by GRID
btb<-unique(btb)

#save btb so as to load in rmd file.  ####
#save GRdata, authgr, searchgr, wiki files just for safekeeping - iterate update
if(exists("savenum")){
  savenum<-savenum+1} else {
    savenum<-1
  }

save(btb,file = "btb.Rdata")
save(btb,file = paste0("btb",as.character(savenum),".Rdata"))
save(GRdata,file = "GRdata.Rdata")  
save(GRdata,file= paste0("GRdata",as.character(savenum),".Rdata")) 
save(authors,file = "authors.Rdata")  
save(authors,file= paste0("authors",as.character(savenum),".Rdata"))  
save(wikinew,file= paste0("wikinew",as.character(savenum),".Rdata")) 
save(list = apropos("authgr"),file = paste0("authgr",as.character(savenum),".Rdata"))
save(list = apropos("searchgr"),file = paste0("searchgr",as.character(savenum),".Rdata"))
save(list = apropos("wikifull"),file = paste0("wikifull",as.character(savenum),".Rdata"))
save(list = apropos("wikitable"),file = paste0("wikitable",as.character(savenum),".Rdata"))
rm(list = apropos("authgr"))
rm(list = apropos("searchgr"))
rm(list = apropos("wikifull"))
rm(list = apropos("wikitable"))



