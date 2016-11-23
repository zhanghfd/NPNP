NPNP1_com<-function(test,train,train_d,trained_result,transformation='simultaneous'){
  H1<-trained_result$H;
  a1<-trained_result$a;
  b1<-trained_result$b;
  K<-ncol(test)
  n_train<-nrow(train)
  n_test<-nrow(test)
  train_Hy_1<-matrix(0,n_train,K)
  test_Hy_1<-matrix(0,n_test,K)
  for (i in 1:K){
    sorty<-sort_0.1(train[,i])$sort.x
    train_Hy_1[,i]<-get_H(train[,i],sorty,H1[i,1:(length(sorty)+1)])
  }
  for (i in 1:K){
    sorty<-sort_0.1(train[,i])$sort.x
    test_Hy_1[,i]<-get_H(test[,i],sorty,H1[i,1:(length(sorty)+1)])
  }
  optim_com_1<-get_com_1(train_Hy_1,train_d,a1,b1,test_Hy_1)
  list(com=optim_com_1,transformation='simultaneous')
}
