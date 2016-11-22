
get_fnr<-function(yy,dd,thres){
  nn1<-sum(dd)
  nn0<-length(dd)-nn1
  temp<-outer(yy[dd==1],thres,'<')
  fnr<-colSums(temp)/nn1
  fnr
}
