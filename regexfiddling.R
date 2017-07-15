#stab 1
str_extract_all(string = "Robert Lawrence was there",pattern = "((?<![“])[:upper:][\\. ]?[:lower:]+(?=[ \\’\\'-][:upper:][\\. ]?)(?:[\\s\\’\\'-][:upper:][\\. ]?[A-Z\\.?(\\w\\'+)-]+)+)")[[1]]

#stab 2
str_extract_all(string = "Robert Galbraith. He and Ron Smith were there but not J. K. Rowling. Hm.",pattern = "((?<![“])[:upper:]{1}(\\. )?[:lower:]+(?=[ \\’\\'-][:upper:]{1}(\\. )?)(?:[\\s\\’\\'-][:upper:]{1}(\\. )?[[:upper:]{1}(\\w\\'+)-]+)+)")[[1]]

#stab 3 - swapping this into live code
str_extract_all(string = "Robert Galbraith. He and Ron Q. Smith and Jane McDougal and Liz O'Connor were there but not J. K. Rowling. Hm.",pattern = "((?<![“])([:upper:]{1}(\\. )?)+[:lower:]+(?=([ \\’\\'-][:upper:]{1}(\\. )?)+)(?:[\\s\\’\\'-][:upper:]{1}(\\. )?[[:upper:]{1}([:lower:]\\'+)-]+)+)")[[1]]



#works to get at initials only
str_extract_all(string = "Robert Galbraith. He and Ron Smith were there but not J. K. Rowling. Hm.",pattern = "([:upper:]{1}(\\. )+){2}")[[1]]


#currently using
str_extract_all(string = "Robert Galbraith. He and Ron Q. Smith and Jane McDougal and Liz O'Connor were there but not J. K. Rowling. Hm.",pattern = "((?<![“])[A-Z][\\. ]?\\w+(?=[ \\’\\'-][A-Z][\\. ]?)(?:[\\s\\’\\'-][A-Z][\\. ]?[A-Z\\.?(\\w\\'+)-]+)+)")[[1]]
