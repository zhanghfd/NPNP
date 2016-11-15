trans_coordinate<-function(H,a,b,r,y,sorty,d,x){
  quadratic<-function(a,b,c){(sqrt(b^2-4*a*c)-b)/(2*a)}
  n<-length(y)
  nn<-length(sorty)
  r<-as.matrix(r)
  x<-as.matrix(x)
  K<-length(x[,1])
  g<-(1/(b^2)-1)*d+1
  index<-outer(y,sorty,">=")
  A<-cbind(array(1,n),index)
  count<-colSums(index)  ## count[r]=#{y_i|y_i >= y^r}
  count_diff<-c(-diff(count),count[nn])
  distance<-1
  M<-1
  L<-array(0,1000)
  while(distance>10^-4){
    result_temp<-c(H,a,b,r)
    H[1]<-(a*t(d)%*%g+t(r)%*%x%*%g-t(g)%*%index%*%H[-1])/(sum(g))
    for(i in 1:nn){
      H[i+1]<-quadratic(t(g)%*%index[,i],colSums((g*index[,i])*A[,-(i+1)])%*%H[-(i+1)]-sum(d*g*index[,i])*a-t(r)%*%x%*%(g*index[,i]),-count_diff[i])
    }
    a<-(t(d*g)%*%A%*%H-t(r)%*%x%*%(d*g))/(sum(d*g));
    b<-sqrt(sum((A%*%H-t(x)%*%r-a*d)^2*d)/(sum(d)))
    g<-(1/(b^2)-1)*d+1
    for(k in 1:K){
      r[k]<-(t(x[k,]*g)%*%A%*%H-a*t(d)%*%(x[k,]*g)-t(r[-k])%*%x[-k,]%*%(x[k,]*g))/(sum(g*x[k,]^2))
    }
    distance<-sum((c(H,a,b,r)-result_temp)^2)
    L[M]<-sum((A%*%H-t(x)%*%r-a*d)^2*g)/2-t(count_diff)%*%log(H[-1])+sum(d)*log(b)
    M<-M+1
  }
  list(H=H,a=a,b=b,r=r,L=L[1:M-1],dif_L=max(diff(L[1:M-1])),M=M-1,distance=distance)
}
