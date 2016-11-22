
NPNP = function(train_y,train_d,tv_ratio=0.5,M=5,test_d=NULL,transformation='simultaneous'){
  
  trained_parameter<-parameter_from_training(train_y,train_d,transformation);
  
  NPNP_training_results<-NPNP_training(tv_ratio,M,trained_parameter,train_y,train_d)
  
  fpr=NPNP_training_results$fpr;
  fnr=NPNP_training_results$fnr;
  auc=NPNP_training_results$auc;
  
  if(is.null(test_d)){
    
    return(list(fpr=fpr,fnr=fnr,auc=auc));
    
  }else{
    
    NPNP_result<-NPNP_diagnosis(test_y,train_y,train_d,trained_parameter)
    
    optimal_com<-NPNP_result$com
    
    test_prediction<-NPNP_prediction(test_y,optimal_com,NPNP_training_results)
    
    return(list(fpr=fpr,fnr=fnr,auc=auc,predict_d=test_prediction$predict_d));
    
  }
}
