sort_0.1<-function(x){
  res = floor(x*10)/10;
  res[res==0]=0.1;
  sort.x = sort(unique(res));
  list(x=res,sort.x=sort.x)
}
