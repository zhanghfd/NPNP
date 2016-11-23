NPNP2_com<-function(test,train,train_d,trained_result,transformation='sequential'){
  H2<-trained_result$H;
  a2<-trained_result$a;
  b2<-trained_result$b;
  r<-trained_result$r;

  K<-ncol(test)
  n_train<-nrow(train)
  n_test<-nrow(test)
  train_Hy_2<-matrix(0,n_train,K)
  test_Hy_2<-matrix(0,n_test,K)
  for (i in 1:K){
    sorty<-sort_0.1(train[,i])$sort.x
    train_Hy_2[,i]<-get_H(train[,i],sorty,H2[i,1:(length(sorty)+1)])
  }
  for (i in 1:K){
    sorty<-sort_0.1(train[,i])$sort.x
    test_Hy_2[,i]<-get_H(test[,i],sorty,H2[i,1:(length(sorty)+1)])
  }
  optim_com_2<-get_com_2(test_Hy_2,a2,b2,r)
  list(com=optim_com_2,transformation='sequential')
}
