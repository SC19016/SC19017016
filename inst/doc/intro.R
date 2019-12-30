## -----------------------------------------------------------------------------
npboy<-function(a,b,c,miu,sigma){
  n_day<-numeric(365)
  revenue<-function(x,guest){
    if(guest<=x)
      r<-(a-b)*guest-(b-c)*(x-guest)
    else
      r<-(a-b)*x
    return(r)
  }
  for (i in 1:365) {
    guest<-rnorm(1,miu,sigma)
    temp<-numeric(6*sigma+1)
    paper<-seq(from=miu-3*sigma,to=miu+3*sigma,by=1)
    for(j in miu-3*sigma:miu+3*sigma){
      temp[j]<-revenue(j,guest)
    }
    n_day<-paper[which.max(temp)]
  }
  return(mean(n_day))
}


## -----------------------------------------------------------------------------
steel<-function(L,sigma,pre){
  n<-10000
  loss<-function(l){
    temp<-numeric(length(l))
    for (i in 1:length(l)) {
      if(l[i]>L)
        temp[i]<-l[i]-L
      else
        temp[i]<-l[i]
    }
    return(temp)
  }
  tr<-seq(from=L,to=L+3*sigma,by=pre)
  true_loss<-numeric(length(tr))
  for (i in 1:length(tr)) {
    l<-rnorm(n,tr[i],sigma)
    temp<-loss(l)
    true_loss[i]<-sum(temp)/sum(l>L)
  }
  return(tr[which.min(true_loss)])
}


## -----------------------------------------------------------------------------
a<-3
b<-2
c<-0.8
miu<-500
sigma<-60
optimal_number<-npboy(a,b,c,miu,sigma)

## -----------------------------------------------------------------------------
L<-120
sigma<-5
pre<-0.5
optimal_length<-steel(L,sigma,pre)

