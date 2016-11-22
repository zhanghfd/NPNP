
get_fpr<-function(yy,dd,thres){
  nn1<-sum(dd)
  nn0<-length(dd)-nn1
  temp<-outer(yy[dd==0],thres,'>=')
  fpr<-colSums(temp)/nn0
  fpr
}
