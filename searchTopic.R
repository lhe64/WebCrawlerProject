searchTopic<-function(keyword,topTerms,iterTimes){
  library(rvest)
  library(tm)
  library(SnowballC)
  fulllink<-findLink(keyword,topTerms)
  for (i in 1:iterTimes){
    fulllink<-findFulllink(fulllink,topTerms)
  }
  fulllink2<-fulllink
  nodes<-unique(as.vector(fulllink2))
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
  vis.links<-as.data.frame(fulllink2)
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


