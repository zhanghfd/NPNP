NPNP_training<-function(tv_ratio,M,trained_result,train_y,train_d){
  # use training data to obtain thresholds at fpr = 0.01,...,0.99
  trans<-trained_result$transformation
  com<-NPNP_diagnosis(train_y,train_y,train_d,trained_result)$com
  fpr<-seq(0.01,0.99,by=0.01)
  thres<-get_thres(com,train_d,fpr)
  thres_low<-min(thres)-(max(thres)-min(thres))/10
  thres_high<-max(thres)+(max(thres)-min(thres))/10
  thres_v<-seq(thres_low,thres_high,by=(thres_high-thres_low)/99)
  thres<-floor(thres_v*10)/10
  if (min(diff(thres))==0) {thres<-floor(thres_v*100)/100}
  
  fpr_m<-matrix(0,M,length(thres))
  fnr_m<-matrix(0,M,length(thres))
  
  ratio<-tv_ratio/(tv_ratio+1)
  
  train_y_d<-train_y[train_d==1,]
  train_y_n<-train_y[train_d==0,]
  n01<-sum(train_d)
  n00<-length(train_d)-sum(train_d)
  train_points<-vector('list',M)
  H_value<-vector('list',M)
  
  for (m in 1:M){
    # Split data into training data (tt) and validation data (tv)
    # validation sample size = size(full_train)*ratio
    tt_id_d<-sample(1:n01,floor(n01*ratio),replace=FALSE)
    tt_id_n<-sample(1:n00,floor(n00*ratio),replace=FALSE)
    tt_y<-rbind(train_y_n[tt_id_n,],train_y_d[tt_id_d,])
    tt_d<-c(rep(0,length(tt_id_n)),rep(1,length(tt_id_d)))
    tv_y<-rbind(train_y_n[-tt_id_n,],train_y_d[-tt_id_d,])
    tv_d<-c(rep(0,n00-length(tt_id_n)),rep(1,n01-length(tt_id_d)))
    
    train_result<-parameter_from_training(tt_y,tt_d,trans)
    NPNP_result<-NPNP_diagnosis(tv_y,tt_y,tt_d,train_result)$com
    fnr_m[m,]<-get_fnr(NPNP_result,tv_d,thres)
    fpr_m[m,]<-get_fpr(NPNP_result,tv_d,thres)
    H_value[[m]]<-train_result$H
    train_points[[m]]<-tt_y
  }
  fnr=colMeans(fnr_m);
  fpr=colMeans(fpr_m);
  fr = fnr + fpr;
  
  o = order(fpr);
  nn = length(o);
  o.m.spec = c(fpr[o],1);
  sens = c(1-fnr[o],1);
  
  auc = spec[1]*sens[1]/2+sum((o.m.[-1]-o.m.[-nn])*(sens[-1]+sens[-nn]))/2;
  
  list(thres=thres,fnr=fnr,fpr=fpr,fr=fr,auc=auc,train_points=train_points,H_value=H_value)
}
