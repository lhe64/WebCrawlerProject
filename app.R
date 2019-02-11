library(shiny)
library(visNetwork)
library(rvest)
library(tm)
library(SnowballC)
library(topicmodels)


connectTopic<-function(keyword1,keyword2,topTerms,iterTimes){

  fulllink<-findLink(keyword1,topTerms)
  for (i in 1:iterTimes){
    fulllink<-findFulllink(fulllink,topTerms)
  }
  fulllink2<-findLink(keyword2,topTerms)
  for (i in 1:iterTimes){
    fulllink2<-findFulllink(fulllink2,topTerms)
  }
  fulllinks<-rbind(fulllink,fulllink2)
  nodes<-unique(as.vector(fulllinks))
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
  vis.links<-as.data.frame(fulllinks)
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

findFulllink<-function(fulllink,num){
  fulllink2<-fulllink
  for (j in 1:length(fulllink[,2])){
    linkB<-findLink(fulllink[j,2],num)
    fulllink2<-rbind(fulllink2,linkB)
  }  
  return(fulllink2)
}

findLink<-function(keyword,topTerms){

  k<-2
  url1<-paste('https://en.wikipedia.org/w/index.php?search=',keyword,'&title=Special%3ASearch&profile=default&fulltext=1',sep='')
  webpage <- read_html(url1)
  nodeSum<-'.mw-search-result:nth-child(1) a'
  hyperLink<-webpage %>% html_nodes(nodeSum) %>% html_attr("href")
  if (length(paste('https://en.wikipedia.org',hyperLink,sep=""))!=1){
    return(NULL)
    break
  }
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
  myStopwords <- c("'s",keyword, "'t",'used','can', 'say','one','way','use',
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
  topicnodes<-as.vector(ldaOut.terms)
  links<-cbind(keyword,topicnodes)
  return(links)
}


ui <- htmlTemplate("index.html", 
  text1 = textInput(inputId = "keyword1",label<-"keyword1",value<-"car"),
  text2 = textInput(inputId = "keyword2",label<-"keyword2",value<-"steel"),
  numeric1 = numericInput(inputId = "topTerms1",label<-"number of top links x2",value<-2),
  numeric2 = numericInput(inputId = "iterTimes1",label<-"iteration times",value<-1),
  submit1 = submitButton("Update View", icon("refresh")),
  network1 = visNetworkOutput("network1",width = "1000px", height = "1000px")
  
)

server <- function(input, output) {
  

  output$network1 <-  renderVisNetwork(connectTopic(input$keyword1,input$keyword2,input$topTerms1,input$iterTimes1))
  
  output$network2 <-  renderVisNetwork(connectTopic(input$keyword3,input$keyword4,input$topTerms2,input$iterTimes2))
}

shinyApp(ui = ui, server = server)