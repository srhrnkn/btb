#refactor (?) python code into R - this then got merged into the parsethenaddloop file

#readline() - prompts for input
#readLines() - reads character strings from a file


# coding=utf-8
#read in all lines
#find words starting with upper case
#drop everything else
#keep all strings of upper case words as unique elements
#surrounded by quotes more likely to be titles



#set name of author - need to run the beginning of addnewdata first
interviewee<-as.character(intervieweesnew$Subject[i])
btblines<-readLines(con = paste0(interviewee,'.txt'),n = -1, encoding='UTF-8')
authorsnew<-data.frame(name=character(),gender=character(),interviewee=character())

#var to store regex results looking for multiple initial cap words in a row
multi<-character()
#var to store regex results looking for single initial cap words that weren't in multi
single<-character()

#first look for multi - store only if something is found in the line
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

multigend = character()

for(i in 1:length(multi)){
  tempmultigend<-readline(prompt = paste0(multi[i]," "))
  multigend<-c(multigend, tempmultigend)
}

multiframe<-data.frame(name=multi,gender=multigend,stringsAsFactors = F)
#find rownums of flagged entries
fixnums<-grep("/",multiframe$gender)
#loop through flagged entries and prompt for name,gender string - separated by comma, no quotes
for(i in 1:length(fixnums)){
  tempfix<-readline(prompt = paste(multiframe[fixnums[i],],collapse = " "))
  multiframe[fixnums[i],]<-unlist(str_split(string = tempfix,pattern = ","))
}

multiframe<-multiframe[which(multiframe$gender!=""),]

#for single just view and tag any that are real
singlegend = character()
print(single)
singleindices<-readline(prompt = "Which items are authors? ")
singleindices<-as.integer(unlist(str_split(string = singleindices," ")))
for(i in 1:length(singleindices)){
  tempsinglegend<-readline(prompt = paste0(single[singleindices[i]]," "))
  singlegend<-c(singlegend, tempsinglegend)
}

singleframe<-data.frame(name=single[singleindices],gender=singlegend,stringsAsFactors = F)

#data frame for use in next step

authorsnew<-rbind(authorsnew,rbind( cbind(multiframe,interviewee=interviewee),cbind(singleframe,interviewee=interviewee)))



