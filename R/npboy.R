#' @title npboy
#' @description A function to solve the newspaper-boy-problem in mathematical modeling. 
#' @param a Newspaper retail price
#' @param b Newspaper purchase price
#' @param c Newspaper rebate(a>b>c)
#' @param miu The expected number of daily newspaper buyers
#' @param sigma The variance of the number of daily newspaper buyers
#' @return The optimum number of newspapers a newspaper boy should buy each day \code{n}
#' @examples
#' \dontrun{
#' a<-2
#' b<-1.5
#' c<-1.2
#' miu<-200
#' sigma<-50
#' npboy(a,b,c,miu,sigma)
#' @export
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
