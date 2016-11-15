NPNP_training<-function(train_y,train_d,transformation='simultaneous'){
  K<-ncol(train_y)
  nsort<-rep(0,K)
  for (i in 1:K){
    sorty<-sort_0.1(train_y[,i])$sort.x
    nsort[i]<-length(sorty)
  }

  if(transformation=='simultaneous'){
    H1<-matrix(0,K,max(nsort)+1)
    a1<-rep(0,K)
    b1<-rep(0,K)
    ######################################################trans model 1
    for(i in 1:K){
      tempy<-get_es_1(train_y[,i],train_d)
      sorty<-sort_0.1(train_y[,i])$sort.x
      H1[i,1:(length(sorty)+1)]<-tempy$H
      a1[i]<-tempy$a
      b1[i]<-tempy$b
    }
    return(list(H=H1,a=a1,b=b1,transformation=transformation));
  }else{
    ######################################################trans model 2
    H2<-matrix(0,K,max(nsort)+1)
    r<-matrix(0,K-1,K-1)
    a2<-rep(0,K)
    b2<-rep(0,K)
    Hy<-matrix(0,K,nrow(train_y))
    sorty1<-sort_0.1(train_y[,1])$sort.x
    Hy[1,]<-get_H(train_y[,1],sorty1,H1[1,1:(length(sorty1)+1)])
    a2[1]<-a1[1]
    b2[1]<-b1[1]
    H2[1,1:(length(sorty1)+1)]<-H1[1,1:(length(sorty1)+1)]

    i<-2
    tempy<-get_es_2(train_y[,i],train_d,t(Hy[1:i-1,]))
    sorty<-sort_0.1(train_y[,i])$sort.x
    Hy[i,]<-get_H(train_y[,i],sorty,tempy$H)
    H2[i,1:(length(sorty)+1)]<-tempy$H
    a2[i]<-tempy$a
    b2[i]<-tempy$b
    r[i-1,1:i-1]<-t(tempy$r)

    if (K>2){
      for (i in 3:K){
        tempy<-get_es_2(train_y[,i],train_d,Hy[1:i-1,])
        sorty<-sort_0.1(train_y[,i])$sort.x
        Hy[i,]<-get_H(train_y[,i],sorty,tempy$H)
        H2[i,1:(length(sorty)+1)]<-tempy$H
        a2[i]<-tempy$a
        b2[i]<-tempy$b
        r[i-1,1:i-1]<-t(tempy$r)
      }
    }
    return(list(H=H2,r=r,a=a2,b=b2,transformation=transformation));
  }
}
