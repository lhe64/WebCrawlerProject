plotNetwork<-function(keyword,k,topTerms){
  library(rvest)
  library(tm)
  url <-paste("https://www.politico.com/search?adv=true&userInitiated=true&s=&q=",keyword,"&pv=&c=0000014b-324d-d4f3-a3cb-f3ff415e0035&r=&start=&start_submit=&end=&end_submit=",sep="")
  webpage <- read_html(url)
  nodeTxt<-'.story-text p'
  nodeSum<-'.fig-graphic a , .lazy-loaded'
  hyperLink<-webpage %>% html_nodes(nodeSum) %>% html_attr("href")
  txt<-c()
  for (i in 1:length(hyperLink)){
    article <- read_html(hyperLink[i])
    txt[i]<-paste(html_text(html_nodes(article,nodeTxt)), collapse = '')
  }
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
  docs <- tm_map(docs,stemDocument)
  #define and eliminate all custom stopwords
  myStopwords <- c("'s","'t",'can', 'say','one','way','use',
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
  return(list(wordCloud,p,ldaOut.terms))
  
}



