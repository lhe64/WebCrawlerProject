plotNetwork<-function(keyword,topTerms){
  library(rvest)
  library(tm)
  library(SnowballC)
  k<-2
  url1<-paste('https://en.wikipedia.org/w/index.php?search=',keyword,'&title=Special%3ASearch&profile=default&fulltext=1',sep='')
  webpage <- read_html(url1)
  nodeSum<-'.mw-search-result:nth-child(1) a'
  hyperLink<-webpage %>% html_nodes(nodeSum) %>% html_attr("href")
  article <- read_html(paste('https://en.wikipedia.org',hyperLink,sep=""))
  nodeTxt<-'p'
  txt<-paste(html_text(html_nodes(article,nodeTxt)), collapse = '')
  docs <- Corpus(VectorSource(txt))
  #Remove punctuation - replace punctuation marks with " "
  docs <- tm_map(docs, removePunctuation)
  #Transform to lower case
  docs <- tm_map(docs,content_transformer(tolower))
  #Strip digits
  docs <- tm_map(docs, removeNumbers)
  #Remove stopwords from standard stopword list 
  docs <- tm_map(docs, removeWords, stopwords("english"))
  #Strip whitespace (cosmetic?)
  docs <- tm_map(docs, stripWhitespace)
  #Stem document to ensure words that have same meaning or different verb forms of the same word arent duplicated 
  #define and eliminate all custom stopwords
  myStopwords <- c("'s",keyword, "'t",'can', 'say','one','way','use',
                   'also','howev','tell','will',
                   'much','need','take','tend','even',
                   'like','particular','rather','said',
                   'get','well','make','ask','come','end',
                   'first','two','help','often','may',
                   'might','see','someth','thing','point',
                   'post','look','right','now','think','ve',
                   're ','anoth','put','set','new','good',
                   'want','sure','kind','larg','yes,','day','etc',
                   'quit','sinc','attempt','lack','seen','awar',
                   'littl','ever','moreov','though','found','abl',
                   'enough','far','earli','away','achiev','draw',
                   'last','never','brief','bit','entir','brief',
                   'great','lot')
  docs <- tm_map(docs, removeWords, myStopwords)
  
  #Create document-term matrix
  dtm <- DocumentTermMatrix(docs)
  
  freq <- colSums(as.matrix(dtm))
  
  #create sort order (descending)
  ord <- order(freq,decreasing=TRUE)
  #List all terms in decreasing order of freq and write to disk
  library(wordcloud)
  set.seed(42)
  #.add color
  wordCloud<-wordcloud(names(freq),freq,min.freq=20,colors=brewer.pal(6,'Dark2'))
  wf=data.frame(term=names(freq),occurrences=freq)
  library(ggplot2)
  p <- ggplot(subset(wf, freq>50), aes(term, occurrences))
  p <- p + geom_bar(stat="identity")
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
  library(topicmodels)
  #Run Latent Dirichlet Allocation (LDA) using Gibbs Sampling
  #set burn in
  burnin <-4000
  #set iterations
  iter<-2000
  #thin the spaces between samples
  thin <- 500
  #set random starts at 5
  nstart <-5
  #use random integers as seed 
  seed <-list(2003,5,63,100001,765)
  # return the highest probability as the result
  best <-TRUE
  #run the LDA model
  ldaOut <- LDA(dtm,k, method="Gibbs", control=
                  list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  #docs to topics
  ldaOut.topics <- as.matrix(topics(ldaOut))
  
  #top 10 terms in each topic
  ldaOut.terms <- as.matrix(terms(ldaOut,topTerms))
  ldaOut.terms
  topicnodes<-c("topic1","topic2",c(rbind(ldaOut.terms[,1],ldaOut.terms[,2])))
  topicnodes
  links<-cbind(c("topic1","topic2"),topicnodes)
  links[1,1]<-keyword
  links[2,1]<-keyword
  nodes<-c(keyword,topicnodes)
  library("visNetwork") 
  vis.nodes<-as.data.frame(nodes)
  colnames(vis.nodes) = c("id")
  vis.nodes$shape  <- "dot"  
  vis.nodes$shadow <- TRUE # Nodes will drop shadow
  vis.nodes$title  <- vis.nodes$id # Text on click
  vis.nodes$label  <- vis.nodes$id # Node label
  #vis.nodes$size   <- vis.nodes$audience.size # Node size
  vis.nodes$borderWidth <- 2 # Node border width
  #vis.nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
  vis.nodes$color.border <- "black"
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"
  vis.links<-as.data.frame(links)
  colnames(vis.links) = c("from","to")
  #vis.links$width <- 1+links$weight/8 # line width
  vis.links$color <- "gray"    # line color  
  vis.links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
  vis.links$smooth <- FALSE    # should the edges be curved?
  vis.links$shadow <- FALSE    # edge shadow
  networkgraph<-visNetwork(vis.nodes,vis.links, width="100%", height="400px")
  #topicProbabilities <- as.data.frame(ldaOut@gamma)
  #topicProbabilities
  #lapply(1:nrow(dtm),function(x) sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])
  return(networkgraph)
  
}


