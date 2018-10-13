library(rvest)
library(tm)
keyword<-"trump"
url <-paste("https://www.politico.com/search?adv=true&userInitiated=true&s=&q=",keyword,"&pv=&c=0000014b-324d-d4f3-a3cb-f3ff415e0035&r=&start=&start_submit=&end=&end_submit=",sep="")
webpage <- read_html(url)
nodeTxt<-'.story-text p'
nodeSum<-'.fig-graphic a , .lazy-loaded'
hyperLink<-webpage %>% html_nodes(nodeSum) %>% html_attr("href")
txt<-c()
for (i in 1:length(hyperLink)){
  download.file(hyperLink[i], destfile =as.character(i))
  article <- read_html(as.character(i))
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
myStopwords <- c("'s","'t",keyword,'can', 'say','one','way','use',
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
#inspect a document as a check
writeLines(as.character(docs[[1]]))
#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
dtm
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],'word_freq.csv')
#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freq),freq, min.freq=20)
#.add color
wordcloud(names(freq),freq,min.freq=20,colors=brewer.pal(6,'Dark2'))
#hist
wf=data.frame(term=names(freq),occurrences=freq)
library(ggplot2)
p <- ggplot(subset(wf, freq>50), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
rownames(dtm) <- filenames
#Load Topic models
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
#set number of topics 
k <-4
#run the LDA model
ldaOut <- LDA(dtm,k, method="Gibbs", control=
                list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste('LDAGibbs',k,'DocsToTopics.csv'))

#top 10 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))
write.csv(ldaOut.terms,file=paste('LDAGibbs',k,'TopicsToTerms.csv'))


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste('LDAGibbs',k,'TopicProbabilities.csv'))


#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


#write to file
write.csv(topic1ToTopic2,file=paste('LDAGibbs',k,'Topic1ToTopic2.csv'))
write.csv(topic2ToTopic3,file=paste('LDAGibbs',k,'Topic2ToTopic3.csv'))
terms(ldaOut,10)
ldaOut.terms <- as.matrix(terms(ldaOut,10))
#view the topic assignment for each document
topics(ldaOut)
ldaOut.topics <-as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics2.csv"))
#Find probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma) 
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
#investigate topic probabilities data.frame
summary(topicProbabilities)
str(topicProbabilities)

