processWordcloud<-function(keyword,minfreq){
  library(rvest)
  library(tm)
  library(SnowballC)
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
  return(wordcloud(names(freq),freq,min.freq=minfreq,colors=brewer.pal(6,'Dark2')))
}



