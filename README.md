# WebCrawlerProject
Webscraping the data from the Internet and using Topic modeling to find the relevance between the seeds. The result is visualized using an R shiny app.

I chose https://en.wikipedia.org as the website for web scraping. And by searching the keyword, the website will return a list of the results. The top result is used as input to the Topic Modeling.

The result returned from the Topic Modeling will be used as keyword input for the next search. Finally, the result will be visualized by the network nodes and links.

We have two keyword input, and the final visualization will be a graph of connected network including two keywords if a path is found after the given iteration times. If not, then it will display two connected networks.

For example, "car" and "steel" are not connected if we set iteration =1 and links=2. So the result is two networks. But "car" and "fire" with the same parameter will be connected.
