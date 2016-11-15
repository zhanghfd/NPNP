get_es_1<-function(y,d){
  sorty<-sort_0.1(y)$sort.x
  temp_H<-c(sorty[1]-0.1,0.1,diff(sorty))
  initial_Hy<-(temp_H-min(temp_H))*24/(max(temp_H)-min(temp_H))-10
  a<-(mean(y[d==1])-mean(y[d==0]))/sd(y[d==0])
  b<-sqrt(sd(y[d==1])/sd(y[d==0]))
  binormal_coordinate(initial_Hy,a,b,y,sorty,d)
}
