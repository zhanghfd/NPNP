
NPNP_prediction<-function(test_y,test_com,NPNP_training_results,fpr_thre){
  # use optimal combination & (threshold,fnr,fpr) results from NPNP_training
  # to predict disease status
  train_points<-NPNP_training_results$train_points
  H_value<-NPNP_training_results$H_value
  thres<-NPNP_training_results$thres
  fnr<-NPNP_training_results$fnr
  fpr<-NPNP_training_results$fpr
  fnr_fpr<-NPNP_training_results$fr

  # trans_test: transformed value of test_y
  M<-length(train_points)
  trans_test<-test_y
  temp_result<-vector('list',ncol(test_y))
  for (i in 1:ncol(test_y)){
    temp_result[[i]]<-matrix(0,nrow(test_y),M)
  }
  for (i in 1:ncol(test_y)){
    for (m in 1:M){
      sorty<-sort_0.1(train_points[[m]][,i])$sort.x
      temp_result[[i]][,m]<-get_H(test_y[,i],sorty,H_value[[m]][i,1:(length(sorty)+1)])
    }
    trans_test[,i]<-rowMeans(temp_result[[i]])
  }
  # disease status prediction
  if(missing(fpr_thre)){
    thres_optim<-thres[which.min(fnr_fpr)]
    predict_d<-as.numeric(test_com>=thres_optim)
    method="minimize FPR+FNR"
  }else{
    thres_optim<-thres[which.min(abs(fpr-fpr_thre))]
    predict_d<-as.numeric(test_com>=thres_optim)
    method=paste0("FPR=",fpr_thre,sep='')
  }
  return(predict_d=predict_d);
}
