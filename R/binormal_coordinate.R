binormal_coordinate<-function(H,a,b,x,sortx,d){
  quadratic<-function(a,b,c){(sqrt(b^2-4*a*c)-b)/(2*a)}
  n<-length(x)
  nn<-length(sortx)
  g<-(1/(b^2)-1)*d+1
  index<-outer(x,sortx,">=")
  A<-cbind(array(1,n),index)
  count<-colSums(index)  ## count[r]=#{y_i|y_i >= y^r}
  count_diff<-c(-diff(count),count[nn])
  distance<-1
  k<-1
  L<-array(0,1000)
  while(distance>5*10^-6){
    result_temp<-c(H,a,b)
    H[1]<-(a*t(d)%*%g-t(g)%*%index%*%H[-1])/(sum(g))
    for(i in 1:nn){
      H[i+1]<-quadratic(t(g)%*%index[,i],colSums((g*index[,i])*A[,-(i+1)])%*%H[-(i+1)]-sum(d*g*index[,i])*a,-count_diff[i])
    }
    a<-(t(d*g)%*%A%*%H)/(sum(d*g))
    b<-sqrt(sum((A%*%H-a*d)^2*d)/(sum(d)))
    g<-(1/(b^2)-1)*d+1
    distance<-sum((c(H,a,b)-result_temp)^2)
    L[k]<-sum((A%*%H-a*d)^2*g)/2-t(count_diff)%*%log(H[-1])+sum(d)*log(b)
    k<-k+1
  }
  list(H=H,a=a,b=b,L=L[1:k-1],k=k-1)
}
