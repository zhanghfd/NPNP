get_com_1<-function(train,train_d,alpha,beta,x,d){
  K<-length(alpha)
  alpha_matrix<-train_d%*%t(alpha)
  beta_matrix<-train_d%*%t(beta)+(1-train_d)%*%t(rep(1,K))
  xi_cov<-cor((train-alpha_matrix)/beta_matrix)
  mu_n<-rep(0,K)
  mu_d<-alpha
  sigma_n<-xi_cov
  sigma_d<-beta%*%t(beta)*xi_cov

  n1<-sum(d)
  n0<-length(d)-sum(d)
  x_d<-matrix(0,n1,K)
  x_n<-matrix(0,n0,K)
  for (i in 1:K){
    x_d[,i]<-x[,i][d==1]
    x_n[,i]<-x[,i][d==0]
  }
  d<-c(rep(0,n0),rep(1,n1))
  x_d_com<-diag(x_d%*%(pd.solve(sigma_n)-pd.solve(sigma_d))%*%t(x_d))+2*as.vector((t(mu_d)%*%pd.solve(sigma_d)-t(mu_n)%*%pd.solve(sigma_n))%*%t(x_d))+t(mu_n)%*%pd.solve(sigma_n)%*%mu_n-t(mu_d)%*%pd.solve(sigma_d)%*%mu_d-log(det(sigma_d)/det(sigma_n))
  x_n_com<-diag(x_n%*%(pd.solve(sigma_n)-pd.solve(sigma_d))%*%t(x_n))+2*as.vector((t(mu_d)%*%pd.solve(sigma_d)-t(mu_n)%*%pd.solve(sigma_n))%*%t(x_n))+t(mu_n)%*%pd.solve(sigma_n)%*%mu_n-t(mu_d)%*%pd.solve(sigma_d)%*%mu_d-log(det(sigma_d)/det(sigma_n))
  x_com<-c(x_n_com,x_d_com)
  list(com=x_com,d=d)

}
