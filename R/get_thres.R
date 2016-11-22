get_thres<-function(yy,dd,fpr){
  nn1<-sum(dd)
  nn0<-length(dd)-nn1
  temp<-ceiling(nn0-nn0*fpr)
  thres<-sort(yy[dd==0])[temp]
  thres
}
