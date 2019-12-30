
#include <Rcpp.h>
using namespace Rcpp;



//' @title rcpp_add
//' @description The sum of two integers.This is a simple function to test the integrity of Rcpp package
//' @param the first parameter x (int)
//' @param the second parameter y (y)
//' @return the sum of two integers \code{n}
//' @examples
//' \dontrun{
//' x<-3
//' y<-4
//' z<-rcpp_add(3,4)
//' }
//' @export
// [[Rcpp::export]]
int rcpp_add(int x,int y) {
  int z;
  z=x+y;
  return z ;
}
