New York Times "By The Book" Interviews
---------------------------------------

Interview dates: **January 5, 2017 - December 28, 2017**

Total interviews: **52**

Total unique authors mentioned: **1199**

The New York Times Book Review's "By the Book" feature is a weekly interview exploring reading habits past, present, and planned. The interviews yield author recommendations - an average of **31** authors mentioned by name in each interview. The dataset analyzed below is a result of code that looks at the text of each interview, identifies strings of text that look like author names, and verifies them by looking for corresponding names in the Goodreads author database. The gender and birthdate information comes from the data in the Goodreads database, with missing data filled in from the authors' Wikipedia pages.

### Gender

There were a total of **22** female interviewees and **30** male interviewees.

![](btb_files/figure-markdown_github/unnamed-chunk-2-1.png)

Male interviewees are more likely to mention male authors. Female interviewees mention men and women at about the same rate.

![](btb_files/figure-markdown_github/unnamed-chunk-3-1.png)

But there is plenty of variation among interviewees in the total number of authors mentioned & in the gender split.

![](btb_files/figure-markdown_github/unnamed-chunk-4-1.png)

Older interviewees are both more likely to be male (**62%** of male interviewees were born before 1960, vs **47%** of female interviewees) and slightly more likely to have recommended a lower percentage of female authors.

![](btb_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Frequently mentioned authors

This data has a long tail - there are **1199** unique authors mentioned across the **52** interviews, and **979** of them are only mentioned once. The most frequently mentioned authors are mentioned by a fifth of all interviewees. The chart below shows the most frequently mentioned authors, the number of times they were mentioned, and the dates they were mentioned.

![](btb_files/figure-markdown_github/unnamed-chunk-6-1.png)

Below we can see instances where two interviewees mentioned the same author. Note that although some interviewees mentioned dozens of authors, the maximum overlap in authors mentioned between any two interviewees was **5**.

![](btb_files/figure-markdown_github/unnamed-chunk-8-1.png)

We can also see how many times pairs of authors were mentioned together in the same interview.

![](btb_files/figure-markdown_github/unnamed-chunk-9-1.png)
