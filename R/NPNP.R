
NPNP = function(train_validation,test_biomarker=NULL,tv_ratio=1,n.validation=5,fpr_thre=NULL,transformation='simultaneous'){

  train_d = train_validation[,1];
  train_y = train_validation[,-1];
  test_y = test_biomarker;

  trained_parameter<-parameter_from_training(train_y,train_d,transformation);

  NPNP_training_results<-NPNP_training(tv_ratio,n.validation,trained_parameter,train_y,train_d)

  fpr=NPNP_training_results$fpr;
  fnr=NPNP_training_results$fnr;
  auc=NPNP_training_results$auc;

  if(is.null(test_y)){

    return(list(fpr=fpr,fnr=fnr,auc=auc));

  }else{

    NPNP_result<-NPNP_diagnosis(test_y,train_y,train_d,trained_parameter)

    optimal_com<-NPNP_result$com

    test_prediction<-NPNP_prediction(test_y,optimal_com,NPNP_training_results,fpr_thre)

    return(list(fpr=fpr,fnr=fnr,auc=auc,predict_d=test_prediction));

  }
}
