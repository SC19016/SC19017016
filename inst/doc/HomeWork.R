## -----------------------------------------------------------------------------
# Directly assign a string of charaters to x
x <- "Hello World!";x
# Use the c() function to connect the two strings
y <-c(x,"Welcome to the RStudio!");y
# Display the properties of x
mode(x)
# Dispaly the length of y
length(y)

## -----------------------------------------------------------------------------
# Directly assign number to x1
x1 <- 10
# Use the seq() function to generate a sequence of regular numbers
y1 <- seq(1,10,by=1)
# generate a matrix 
z <- matrix(1:9,nrow = 3,ncol = 3)
# Display x1,y1 and z
x1;y1;z
# Determine whether z is a array
is.array(z)

## -----------------------------------------------------------------------------
# Generate the informations of ID,Age and Score.
ID<-seq(19017001,19017040,by=1)
Age<-floor(rnorm(40,mean = 23,sd=1))
Score<-floor(rnorm(40,mean=80,sd=10))
my_table<-data.frame(ID,Age,Score);my_table

## -----------------------------------------------------------------------------
# generate a collection of Sigma
sigma<-c(1,2,4,10,20,100)
# set the format of output picture
# use circular structure to generate the random numbers in different parameter
for (i in 1:length(sigma)) {
  # set the seed to guarantee the consistency of Pseudo-Random numbers 
  set.seed(i)
  # the title of histogram
  title<-c("Histogram of Rayleigh","parameter is",sigma[i])
  # generate the random numbers of normal distribution
  x<-rnorm(1000,0,sigma[i])
  y<-rnorm(1000,0,sigma[i])
  # generate the random numbers of Rayleigh distribution
  z<-sqrt(x^2+y^2)
  #diagram the histogram and check it 
  hist(z,prob=TRUE,breaks = seq(0,6*sigma[i],length.out = 20)
       ,main = title,col = "grey")
  # plot the Rayleigh density function 
  x1<-seq(0,6*sigma[i],length.out = 100000)
  y1<-(x1/sigma[i]^2)*exp(-(x1^2)/(2*sigma[i]^2))
  lines(x1,y1,col="red")
}

## -----------------------------------------------------------------------------
# set the size
size<-1000
#generate the normal random numbers
x<-rnorm(size,0,1)
y<-rnorm(size,3,1)
# when the p=0.75
r<-sample(c(0,1),size,replace = TRUE,prob = c(0.75,0.25))
# generate the normal location mixture
z<-r*x+(1-r)*y
# diagram the histogram of z
hist(z,prob=TRUE,breaks = seq(-4,8,length.out = 20),main = "Normal Location Mixture when p=0.75")
p<-seq(0.1,0.9,by=0.1)
# set the format
for (i in 1:length(p)) {
  # set the seed to guarantee the consistency of Pseudo-Random numbers
  set.seed(i)
  # generate the random numbers which respectively subjects to the Normal         distribution
  x<-rnorm(size,0,1)
  y<-rnorm(size,3,1)
  # set the title of histogram
  title<-c("Normal Location Mixture in p=",p[i])
  # generate the p
  r<-sample(c(0,1),size,replace = TRUE,prob = c(p[i],1-p[i]))
  # generate the normal location mixture
  z<-r*x+(1-r)*y
  # plot the histogram
  hist(z,prob=TRUE,breaks = seq(-4,8,length.out = 20),main = title)
}

## -----------------------------------------------------------------------------
# function of Wishart
Wishart<-
  function(sigma,n){
    # choleski decomposition of sigma
    Q<-chol(sigma)
    # dimension of sigma
    d<-sqrt(length(sigma))
    # generate the Bartlett's decomposition matrix
    # generate the zero matrix which will be used to save the value
    # t is the lower triangular d*d random matrix
    t<-matrix(numeric(d*d),nrow = d,ncol = d)
    for (i in 1:d) {
      j<-1
      while(j<i){
        # T(i,j) should be sample from standard normal distribution
        t[i,j]<-rnorm(1,mean = 0,sd =1)
        # lower triangular matrix
        j<-j+1
      }
      # T(i,i) should be a random number from chi-squared distribution
      t[i,i]<-rchisq(1,n-i+1)
    }
    # accroding to the formula,calculate the matrix which subject to tht           Wishart distribution
    A<-t%*%t(t)
    x<-Q%*%A%*%t(Q)
    # display x
    x
  }

## -----------------------------------------------------------------------------
# generate the empty set to save the value
test<-numeric(1000)
# generate random numbers subject to Wishart distribution
for (i in 1:1000) {
  temp<-Wishart(1,1)
  test[i]<-temp
}
# generate random numbers subject to Chi-square distribution
test2<-rchisq(1000,1)
# plot QQ-plot
qqplot(test,test)

## -----------------------------------------------------------------------------
m<-1e4
## save the theta.hat
theta.hat=numeric(3)
## method one 
set.seed(12345)
T<-runif(m,min = 0,max = pi/3)
## define g(x)
g1<-function(t){
  (3/pi)*sin(t)
}
## MC method
theta.hat[1]<-mean(g1(T))
## method two
set.seed(12345)
X<-runif(m,0,1)
## define g(x)
g2<-function(x){
  (pi/3)*sin((pi*x)/3)
}
theta.hat[2]<-mean(g2(X))
## exact value
theta.hat[3]<-0.5
## show the theta.hat
theta.hat

## -----------------------------------------------------------------------------
## Monte carlo Antithetic Variables
m<-10000
## save the theta.hat and variance
theta.hat<-numeric(2)
Var_thata.hat<-numeric(2)
temp_u<-runif(m/2,0,1)
## generate the antithetic variables
v<-1-temp_u
u1<-c(temp_u,v)
## define g(x)
g<-function(x){
  exp(x)/(1+x^2)
}
theta.hat[1]<-mean(g(u1))
## calculate the variance
Var_thata.hat[1]<-var(g(u1))
## without variance reduction
u2<-runif(m,0,1)
theta.hat[2]<-mean(g(u2))
Var_thata.hat[2]<-var(g(u2))
# show the theta.hat and variance
theta.hat
Var_thata.hat
## percentage of the variance without variance reduction.
(Var_thata.hat[1]-Var_thata.hat[2])/Var_thata.hat[2]

## -----------------------------------------------------------------------------
m<-10000
## save the value
theta.hat<-numeric(2)
Var_thata.hat<-numeric(2)
## define the function
g<-function(x){
  exp(-x)/(1+x^2)*(x>0)*(x<1)
}
## the importance sampling
set.seed(12345)
u<-runif(m,0,1)
x<--log(1-u*(1-exp(-1)))
fg<-g(x)/(exp(-x)/(1-exp(-1)))
theta.hat[1]<-mean(fg)
Var_thata.hat[1]<-sd(fg)
## the Stratified importance sampling
## the number of grouop
j<-5
## the samples of every group
r<-m/j
##save the value
theta_j<-numeric(j)
var_theta1<-numeric(j)
sub_interval<-c(0,0.1351603,0.291487,0.4768629,0.7046056,1)
for (i in 1:5) {
  set.seed(i)
  # in the subinterval,we generate the random number
  u<-runif(r,min =sub_interval[i],max=sub_interval[i+1])
  # inverse transformation
  x<--log(exp(-sub_interval[i])-u/5*(1-exp(-1)))
  # assignment theta_j
  theta_j[i]<-mean(g(x)/(5*exp(-x)/(1-exp(-1))))
  # calculate the variance
  var_theta1[i]<-var(g(x)/(5*exp(-x)/(1-exp(-1))))
}
theta.hat[2]<-sum(theta_j)
Var_thata.hat[2]<-sum(var_theta1)
# show the theta and variance
theta.hat
Var_thata.hat

## -----------------------------------------------------------------------------
#number of replicate
m<-1e4
# number of samples
n<-20
# confidence level
alpha<-0.05
# define the CI of Student's T-distribution to estimate the mean
# when x subject to the Normal-distribution
t_CI_N<-function(n,alpha){
  x<-rnorm(n,mean = 0,sd=2)
  ci<-matrix(c(mean(x)-qt(1-alpha/2,n-1)*sqrt(var(x)/n),mean(x)+qt(1-alpha/2,n-1)*sqrt(var(x)/n)),nrow = 1,ncol = 2)
}
# define the CI of Student's T-distribution to estimate the mean
# when x subject to the Chi-square-distribution
t_CI_chisq<-function(n,alpha){
  x<-rchisq(n,2)
  ci<-matrix(c(mean(x)-qt(1-alpha/2,n-1)*sqrt(var(x)/n),mean(x)+qt(1-alpha/2,n-1)*sqrt(var(x)/n)),nrow = 1,ncol = 2)
}
# define the CI of Chi-square to estimate the variance
# when x subject to the Normal-distribution
Chisq_CI_N <- function(n, alpha) {
  x <- rnorm(n, mean = 0, sd = 2)
  return((n-1) * var(x) / qchisq(alpha, df = n-1))
}
# define the CI of Chi-square to estimate the variance
# when x subject to the Chi-square distribution
Chisq_CI_chisq <- function(n, alpha) {
  x <- rchisq(n,2)
  return((n-1) * var(x) / qchisq(alpha, df = n-1))
}
# counter
counter1<-0
# generate the CI of mean and evaluate the result
# x subject to the normal distribution 
# and Chi-Square distribution repspectively
for (i in 1:m) {
  temp1<-t_CI_N(n,alpha)
  if (temp1[1,1]<0 && temp1[1,2]>0)
    counter1<-counter1+1
}
counter2<-0
for (i in 1:m) {
  temp2<-t_CI_chisq(n,alpha)
  if (temp2[1,1]<2 && temp2[1,2]>2)
    counter2<-counter2+1
}
# generate the CI of variance
# x subject to the normal distribution
# and Chi-Square distribution respectively
CI_CHI_N<-replicate(m,expr = Chisq_CI_N(n,alpha))
CI_CHI_CHI<-replicate(m,expr = Chisq_CI_chisq(n,alpha))
# evaluate the result of variance 
r1<-mean(CI_CHI_N > 4)
r2<-mean(CI_CHI_CHI > 4)
# visualize the result
result<-c(counter1/m,counter2/m,r1,r2)
process<-c("CI_mean_X~N","CI_mean_X~ChiS","CI_Var_X~N","CI_Var_X~ChiS")
final_result<-data.frame(process,result)
final_result


## -----------------------------------------------------------------------------
# number of replicatioin
m<-1e4
# empty array to save the value
sk<-numeric(m)
# generate the a set of skewness values 
library(moments)
for (i in 1:m) {
  x<-rnorm(1e4,mean = 0,sd = 1)
  sk[i]<-skewness(x)
}
# quantiles to be calculated
q<-c(0.025,0.05,0.95,0.975)
quantile_1<-quantile(sk,probs = q)
# calculate the variance of sample quantile
var_sk<-numeric(4)
for (i in 1:4) {
  var_sk[i]<-quantile_1[i]*(1-quantile_1[i])/(m*dnorm(quantile_1[i]))
}
Mark<-c("Var_SK_0.025","Var_SK_0.05","Var_SK_0.95","Var_SK_0.975")
result_var_sk<-data.frame(Mark,var_sk)
result_var_sk


## -----------------------------------------------------------------------------
# significance level
alpha<-0.05
#number of replicates
m<-1e5
# storage of skewness
sk_1<-sk_2<-numeric(m)
sk_beta<-function(beta,n){
  x<-rbeta(n,beta,beta)
  xbar<-mean(x)
  m3 <- mean((x-xbar)^3)
  m2 <- mean((x-xbar)^2)
  return(m3/m2^1.5)
}

# parameter of Beta distribution
para_beta<-10
# number of sample
n<-30
#critical value for the skewness test
cv <- qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
# Monte Carlo
for (i in 1:m) {
  temp<-sk_beta(para_beta,n)
  if(abs(temp)>cv)
    sk_1[i]<-1
}
power_beta<-mean(sk_1)


# when the distribution is T-stribution
sk_t<-function(n,d){
  x<-rt(n,d)
  xbar<-mean(x)
  m3 <- mean((x-xbar)^3)
  m2 <- mean((x-xbar)^2)
  return(m3/m2^1.5)
}
# free degree of T-distribution
para_t<-3
for (i in 1:m) {
  temp<-sk_t(n,para_t)
  if(abs(temp)>cv)
    sk_2[i]<-1
}
power_t<-mean(sk_2)
distribution<-c("power_beta","power_t")
power<-c(power_beta,power_t)
result<-data.frame(distribution,power)
result
# draw the scatter plot
new_beta<-c(seq(3,20,1))
new_t<-c(seq(1,20,1))
# set parameter
plot_beta<-numeric(length(new_beta))
plot_t<-numeric(length(new_t))
# generate power of beta
for (i in 1:length(new_beta)) {
  storage<-numeric(m)
  for (j in 1:m) {
      temp<-sk_beta(new_beta[i],n)
  if(abs(temp)>cv)
      storage[j]<-1  
  }
  plot_beta[i]<-mean(storage)  
}
# generate power of T
for (i in 1:length(new_t)) {
  storage<-numeric(m)
  for (j in 1:m) {
      temp<-sk_t(n,new_t[i])
  if(abs(temp)>cv)
      storage[j]<-1  
  }
  plot_t[i]<-mean(storage)
}

plot(plot_beta)
plot(plot_t)

## -----------------------------------------------------------------------------
# number of replicates
m<-1e4
# significance level
alpha<-0.05
# number of sample
n<-30
# the null hypothesis is true
mu0<-1

# function to calculate the empirical Type I error rate

chi<-function(n,m){
  # MC method  
  result<-numeric(m)
for (i in 1:m){
  x<-rchisq(n,1)
  t_test <- t.test(x, alternative = "two.sided", mu = mu0)
  # reject H0 if p<significant level alpha
  if(t_test$p.value<alpha)
    result[i]<-1
}
    return(mean(result))

}

uniform<-function(n,m){
  # MC method  
  result<-numeric(m)
for (i in 1:m){
  x<-runif(n,0,2)
  t_test <- t.test(x, alternative = "two.sided", mu = mu0)
  # reject H0 if p<significant level alpha
  if(t_test$p.value<alpha)
    result[i]<-1
}
  return(mean(result))
}

exp<-function(n,m){
  # MC method  
  result<-numeric(m)
for (i in 1:m){
  x<-rexp(n,1)
  t_test <- t.test(x, alternative = "two.sided", mu = mu0)
  # reject H0 if p<significant level alpha
  if(t_test$p.value<alpha)
    result[i]<-1
}
  return(mean(result))
}

# calling function
k<-100
temp_r1<-temp_r2<-temp_r3<-numeric(k)
# MC method
for (i in 1:k) {
  temp_r1[i]<-chi(n,m)
  temp_r2[i]<-uniform(n,m)
  temp_r3[i]<-exp(n,m)

}

# Type I error rate
r1<-mean(temp_r1)
r2<-mean(temp_r2)
r3<-mean(temp_r3)
# standard deviation
sd1<-sd(temp_r1)
sd2<-sd(temp_r2)
sd3<-sd(temp_r3)
Type_I_error<-c(r1,r2,r3)
sd_Type_I_error<-c(sd1,sd2,sd3)
Three_Distribution<-c("Chisq","Uniform","Exp")
MC_result<-data.frame(Three_Distribution,Type_I_error,sd_Type_I_error)
MC_result


## ----eval=FALSE---------------------------------------------------------------
#  # library the package
#  library(corrplot)
#  # load data
#  data(scor,package = "bootstrap")
#  # draw the scatter plot
#  plot(scor)
#  # correlation matrix
#  M<-cor(scor)
#  M
#  

## ----eval=FALSE---------------------------------------------------------------
#  # use the Bootstrap to estimate the standard errors
#  mydata<-scor
#  B<-1e4
#  n<-40
#  # method 1 with the function-sample
#  rho<-function(i,j){
#    mycorr<-numeric(B)
#    for (b in 1:B) {
#      temp<-sample(1:nrow(scor),nrow(scor),replace = TRUE)
#      x<-scor[temp,i]
#      y<-scor[temp,j]
#      mycorr[b]<-cor(x,y)
#    }
#    return(sd(mycorr))
#  }
#  
#  # method 2 with the package boot
#  f<-function(x,i){
#    cor(x[i,1],x[i,2])
#  }
#  d12<-scor[,1:2]
#  d34<-scor[,3:4]
#  d35<-matrix(c(scor[,3],scor[,5]),ncol = 2,nrow = nrow(scor))
#  d45<-scor[,4:5]
#  # ues bootstrap generate data
#  r12<-boot(data=d12,statistic=f,R=B)
#  r34<-boot(data=d34,statistic=f,R=B)
#  r35<-boot(data=d35,statistic=f,R=B)
#  r45<-boot(data=d45,statistic=f,R=B)
#  nr12<-r12$t
#  nr34<-r34$t
#  nr35<-r35$t
#  nr45<-r45$t
#  result_2<-c(sd(nr12),sd(nr34),sd(nr35),sd(nr45))
#  
#  # generate the rho
#  subscript<-matrix(c(1,3,3,4,2,4,5,5),nrow = 4,ncol = 2)
#  result_1<-numeric(4)
#  for (i in 1:4) {
#    result_1[i]<-rho(subscript[i,1],subscript[i,2])
#  }
#  # show the result
#  Method_1<-c("se_rho_12","se_rho_34","se_rho_35","se_rho_45")
#  Method_2<-c("se_rho_12","se_rho_34","se_rho_35","se_rho_45")
#  data.frame(Method_1,result_1,Method_2,result_2)

## ----eval=FALSE---------------------------------------------------------------
#  # library pacage
#  library(moments)
#  
#  # simulation times of MC
#  m<-100
#  # times of Bootstrap
#  B<-100
#  # number of sample
#  n<-50
#  # function of skewness
#  sk<-function(x,i){
#    skewness(x[i])
#  }
#  ci.basic_N<-ci.perc_N<-matrix(NA,m,2)
#  # loop
#  # calculate the CI of N
#  for (i in 1:m) {
#    temp_data<-rnorm(n,mean = 0,sd=1)
#    de<-boot(data=temp_data,statistic=sk,R=B)
#    ci<-boot.ci(de,type=c("basic","perc"))
#    ci.basic_N[i,]<-ci$basic[4:5]
#    ci.perc_N[i,]<-ci$percent[4:5]
#  }
#  ci.basic_Chi<-ci.perc_Chi<-matrix(NA,m,2)
#  # calculate the CI of Chisq
#  for (i in 1:m) {
#    temp_data<-rchisq(n,5)
#    de<-boot(data=temp_data,statistic=sk,R=B)
#    ci<-boot.ci(de,type=c("basic","perc"))
#    ci.basic_Chi[i,]<-ci$basic[4:5]
#    ci.perc_Chi[i,]<-ci$percent[4:5]
#  }
#  # calculate the coverage rates
#  r_Normal_Basic<-mean(ci.basic_N[,1]<0 & ci.basic_N[,2]>0)
#  r_Normal_perc<-mean(ci.perc_N[,1]<0 & ci.perc_N[,2]>0)
#  r_Chisq_Basic<-mean(ci.basic_Chi[,1]>0)
#  r_Chisq_perc<-mean(ci.perc_Chi[,1]>0)
#  # show the result
#  Lable<-c("r_Normal_Basic","r_Normal_perc","r_Chisq_Basic","r_Chisq_perc")
#  result<-c(r_Normal_Basic,r_Normal_perc,r_Chisq_Basic,r_Chisq_perc)
#  final_result_1<-data.frame(Lable,result)
#  final_result_1
#  
#  # calculate the miss rate on left and right
#  
#  r1_N_L<-mean(ci.basic_N[,1]>0)
#  r1_N_R<-mean(ci.basic_N[,2]<0)
#  r2_N_L<-mean(ci.perc_N[,1]>0)
#  r2_N_R<-mean(ci.perc_N[,2]<0)
#  
#  r1_Chi_L<-mean(ci.basic_Chi[,1]<0)
#  r2_Chi_L<-mean(ci.perc_Chi[,1]<0)
#  
#  
#  Lable_2<-c("Normal_SK_LeftMiss_Basic","Normal_SK_RightMiss_Basic","Normal_SK_LeftMiss_perc","Normal_SK_LeftMiss_perc","Chisq_SK_LeftMiss_Basic","Chisq_SK_LeftMiss_Basic")
#  result_2<-c(r1_N_L,r1_N_R,r2_N_L,r2_N_R,r1_Chi_L,r2_Chi_L)
#  final_result_2<-data.frame(Lable_2,result_2)
#  final_result_2
#  

## -----------------------------------------------------------------------------
# data set
data(scor,package = "bootstrap")
# number of loop
n<-nrow(scor)
# define theta
theta<-function(x){
  return(x[1]/sum(x))
}
# calculate the covariance
sigma<-cov(scor)
e_value<-eigen(sigma)
theta_hat<-theta(e_value$values)
# storage of theta
theta_jk<-numeric(n)
# the loop
for (i in 1:n) {
  new_data<-scor[-i,]
  temp<-cov(new_data)
  evalue_temp<-eigen(temp)
  theta_jk[i]<-theta(evalue_temp$values)
}
# calculate the bias
theta_bias<-(n-1)*(mean(theta_jk)-theta_hat)
# calculate the standard error
theta_sd<-sqrt((n-1)*mean((theta_jk-mean(theta_jk))^2))
# show the result
result<-c(theta_bias,theta_sd)
lable<-c("theta_bias","theta_sd")
data.frame(lable,result)

## -----------------------------------------------------------------------------
# define the cout 5 test function
count5test <- function(x, y) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))

# return 1 (reject) or 0 (do not reject H0)
return(as.integer(max(c(outx, outy)) > 5))
}
# set the parameters
# the number of tow samples are different
n1<-20 
n2<-50
u1<-u2<-0
sigma1<-sigma2<-1
# generate the sample
x<-rnorm(n1,u1,sigma1)
y<-rnorm(n2,u2,sigma2)
z<-c(x,y)
# cycle times
R<-1e4
n<-(n1+n2)/2
pool<-c(1:(n1+n2))
# storage of result
temp_result<-numeric(R)
# loop
for (i in 1:R) {
  k<-sample(pool,size=n,replace = FALSE)
  x<-z[k]
  y<-z[-k]
  temp_result[i]<-count5test(x,y)
}
# show the result
alpha_hat<-mean(temp_result)

# MC method
mm<-1e3
MC_method_result<-numeric(mm)
for (i in 1:mm) {
  xx<-rnorm(n1,u1,sigma1)
  yy<-rnorm(n2,u2,sigma2)
  zz<-c(xx,yy)
  temp_r2<-numeric(R)
  for (j in 1:R) {
  k<-sample(pool,size=n,replace = FALSE)
  nx<-zz[k]
  ny<-zz[-k]
  temp_r2[j]<-count5test(nx,ny)
  }
  MC_method_result[i]<-mean(temp_r2)
}
MC_alpha_hat<-mean(MC_method_result)
lable<-c("permutation for one time","permutation in MC method")
result<-c(alpha_hat,MC_alpha_hat)
data.frame(lable,result)


## -----------------------------------------------------------------------------
# define the function of standard Laplace
dLaplace<-function(x){
  if(x>0) return((1/2)*exp(-x))
  if(x==0) return(1/2)
  if(x<0) return((1/2)*exp(x))
}

# define the random walk function
rw.Metropolis<-function(sigma,x0,N){
  x<-numeric(N)
  x[1]<-x0
  u <- runif(N)
  k<-0
  for (i in 2:N) {
    y<-rnorm(1, x[i-1], sigma)
    if (u[i] <= (dLaplace(y) / dLaplace(x[i-1])))
      x[i] <- y else {
          x[i] <- x[i-1]
          k <- k + 1
      }
  }
  return(list(x=x, k=k))
}
# iteration times
N<-2000
# differnet variance
sigma<-c(0.1,0.25,0.5,1,2,4,16)
# initial position
x0<-0
index<-seq(from=1,to=2000,by=1)
accep_rate<-numeric(length(sigma))


## -----------------------------------------------------------------------------
# define the sk(a) function
s_k_a<-function(a,k){
  temp<-sqrt(a^2*(k-1)/(k-a^2))
  sk<-1-pt(temp,k-1)
  return(sk)
}
k<-c(seq(from=4,to=25,by=1),100,500,1000)
AK_without_uniroot<-AK_with_uniroot<-numeric(length(k))
# without the 'uniroot'-function
for (j in 1:length(k)) {
  a<-seq(from=0,to=(sqrt(k[j])),by=0.01)
  temp1<-temp2<-numeric(length(a))
  for (i in 1:length(a)) {
    temp1[i]<-s_k_a(a[i],k[j])
    temp2[i]<-s_k_a(a[i],k[j]+1)
  }
  temp<-which.min(abs(temp1[-1]-temp2[-1]))
  AK_without_uniroot[j]<-a[-1][temp]
}
# use the 'uniroot'
for (j in 1:length(k)) {
  ff<-function(a) pt(sqrt(a^2*(k[j]-1)/(k[j]-a^2)),k[j]-1)-pt(sqrt(a^2*(k[j])/(k[j]+1-a^2)),k[j])
  temp<-uniroot(ff,c(0.0001,sqrt(k[j])-0.001))
  AK_with_uniroot[j]<-temp$root
}
# show the result
result<-cbind(AK_without_uniroot,AK_with_uniroot)
result

## -----------------------------------------------------------------------------
# set seed
set.seed(543)
# define the parameter
N<-10000
nAO<-nBO<-10
nA<-28
nB<-24
nOO<-41
nAB<-70
n<-nA+nB+nOO+nAB
tol <- .Machine$double.eps^0.5
L.old<-c(0.3,0.4,0.3)
counter<-1
p_pro<-numeric(N)
q_pro<-numeric(N)
# start the EM method without 'mle'-function
for (i in 1:N) {
  lambda1<-(2*(nA-nAO)+nAO+nAB)/(2*(nA-nAO)+2*nOO+2*nAO+nBO+nAB)
  lambda2<-(2*(nB-nBO)+nBO+nAB)/(2*(nB-nBO)+2*nOO+2*nBO+nAO+nAB)
  p<-lambda1*(1-lambda2)/(1-lambda1*lambda2)
  q<-lambda2*(1-lambda1)/(1-lambda1*lambda2)
  nAO<-2*p*(1-p-q)*n
  nBO<-2*q*(1-p-q)*n
  L<-c(p,q,1-p-q)
  counter<-counter+1
  p_pro[i]<-p
  q_pro[i]<-q
  if(sum(abs(L - L.old)/L.old) < tol) break
  L.old<-L
}
lable<-c("p","q","r")
pro<-L.old
data.frame(lable,pro)
counter
plot(c(1:(counter-1)),p_pro[1:counter-1],xlab = "iteration",ylab='p_pro',type="b",col='red',main="The interation plot of probability of 'p'")
plot(c(1:(counter-1)),q_pro[1:counter-1],xlab = "iteration",ylab='q_pro',type="b",col='blue',main="The interation plot of probability of 'q'")

## -----------------------------------------------------------------------------

formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)
## loop
for (i in seq_along(formulas)) {
    lm(formulas[[i]],data=mtcars)
}



## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i){
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
  })
## loop

for (i in seq_along(bootstraps)) {
  lm(mpg~disp,data = bootstraps[[i]])
}


## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared
## EX.3
result_EX.3<-lapply(lapply(formulas,function(x){lm(formula = x,data=mtcars)})
,rsq)
## EX.4
result_EX.4<-lapply(lapply(bootstraps,function(x){lm(mpg~disp,x)}),rsq)




## cleaning data  
## show the result
result_EX.3<-as.numeric(result_EX.3)
result_EX.4<-as.numeric(result_EX.4)
lable_EX.3<-c("R^2_formula_1","R^2_formula_2","R^2_formula_3","R^2_formula_4")
lable_EX.4<-c("Result_1","Result_2","Result_3","Result_4","Result_5","Result_6","Result_7","Result_8","Result_9","Result_10")
data.frame(lable_EX.3,result_EX.3)
data.frame(lable_EX.4,result_EX.4)

## -----------------------------------------------------------------------------
## an anonymous function 
p.value <- function(mod) mod$p.value
## t-test for 100 times
trials <- replicate(100,t.test(rpois(10, 10), rpois(7, 10)),simplify = FALSE)
## use sapply to extract p_value
p_value1<-sapply(trials,p.value)

## -----------------------------------------------------------------------------
# This is the Rcpp file
# insert the cpp
library(Rcpp)
Rcpp::sourceCpp('C:/Users/YE/Desktop/StatisticalComputing/homework/HW11/randomwalk.cpp')

# define the function to compare

for (i in 1:length(sigma)) {
  x<-RandomWalk_Cpp(sigma[i])
  plot(index,x,type = "l",main = ,ylab = "x")
}

