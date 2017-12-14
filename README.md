# btb
Mining and visualizing btb interviews.  
  
The New York Times Book Review has a weekly interview feature called <a href="https://www.nytimes.com/column/by-the-book">By the Book</a>. Authors are interviewed about their past, current, and planned reading habits. This repository contains code to search the text of those interviews, look for the authors mentioned by each interviewee, extract the author names, determine the gender of each author mentioned, and access the Goodreads API and Wikipedia for more information about each author mentioned. 

* `parsethenaddloop.R`: Loops through text files, extracts author names (interactive), queries GR API & wikipedia, adds to dataset
* `createfunctions.R`: defines functions
* `summaryobjects.R`: creates summary objects for visualization
* `btb.Rmd`: RMarkdown to generate summary discussion & visualization
* `btb.md`: <a href="https://github.com/srhrnkn/btb/blob/master/btb.md">summary discussion & visualization</a>

