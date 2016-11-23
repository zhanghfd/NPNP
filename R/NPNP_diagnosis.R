NPNP_diagnosis <- function(test,train,train_d,trained_result){
  if(trained_result$transformation=='simultaneous'){
    return(NPNP1_com(test,train,train_d,trained_result));
  }else{
    return(NPNP2_com(test,train,train_d,trained_result));
  }
}
