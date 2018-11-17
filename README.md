# WebCrawlerProject
Webscraping the data from the Internet and using Topic modeling to find the relevance between the seeds. The result is visualized using an R shiny app.

I chose https://en.wikipedia.org as the website for web scraping. And by searching the keyword, the website will return a list of the results. The top result is used as input to the Topic Modeling.

Based on the frequency of the word, it can generate the word cloud and histogram. And it can also give the output interactive network graph from the topic modeling model. The user can choose which keyword to search, the minimum frequency to generate the wordcloud and top word numbers for the LDA model.
