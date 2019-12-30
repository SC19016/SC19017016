#' @title Waste in steel rolling
#' @description A function to solve the problem about wasting in steel rolling
#' @param L Required steel's size
#' @param sigma Accuracy of instrument
#' @return The mean of finished rolled steel should be set \code{l}
#' @examples
#' \dontrun{
#' L<-100
#' sigma<-2
#' pre<-0.5
#' optimal_L<-steel(L,sigma,pre)
#' @export
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
