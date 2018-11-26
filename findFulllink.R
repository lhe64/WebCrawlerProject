findFulllink<-function(fulllink,num){
  fulllink2<-fulllink
  for (j in 1:length(fulllink[,2])){
    linkB<-findLink(fulllink[j,2],num)
    fulllink2<-rbind(fulllink2,linkB)
  }  
  return(fulllink2)
}
