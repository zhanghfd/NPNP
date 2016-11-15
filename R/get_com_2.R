get_com_2<-function(y,a,b,r){
  K<-ncol(y)
  n<-nrow(y)
  com<-rep(0,n)
  for (i in 1:n){
    temp<-as.vector(y[i,][2:K]-y[i,][1:K-1]%*%t(r))
    temp0<-y[i,1]^2+sum(temp^2)
    temp1<-((y[i,1]-a[1])/b[1])^2+sum(((temp-a[2:K])/b[2:K])^2)
    com[i]<-temp0-temp1
  }
  com
}
