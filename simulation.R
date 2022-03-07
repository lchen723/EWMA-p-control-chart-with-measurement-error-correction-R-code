######UCL######
RL1 = function(data,lambda,p,L,v,n){
  m = length(data)
  s = rep(NA,m)
  t = rep(NA,m)
  s[1] = lambda * data[1] + (1 - lambda) * p
  t[1] = p+L*sqrt(lambda*(1-(1-lambda)^2)*v/(2-lambda)/n)
  i = 1
  while(s[i]<t[i]){
    i = i+1
    s[i] = lambda * data[i] + (1 - lambda) * s[i-1];
    t[i] = p+L*sqrt(lambda*(1-(1-lambda)^(2*(i)))*v/(2-lambda)/n)
    if (i == m){break}
  }
  return(i)
}
RRL1F = function(lambda,p,L,v,n){
  hat_p = rbinom(5000,n,p)/n
  return(RL1(hat_p,lambda,p,L,v,n))
}

RARL1F = function(lambda,p,L,v,n){
  a = replicate(10000,RRL1F(lambda,p,L,v,n))
  return(list(ARL = mean(a),MRL = median(a),SDRL = sd(a)))
}

L1andARLf = function(lambda,p,v,n,ARL){
  af = function(x){
    RARL1F(lambda,p,x,v,n)$ARL-ARL
  }
  L = uniroot(af,lower = 0,upper = 3, extendInt = "yes")
  ans = RARL1F(lambda,p,L$root,v,n)
  hat_ARL0 = ans$ARL
  ARL0_error = abs(hat_ARL0-ARL)
  while (ARL0_error>0.5){
    L = uniroot(af,lower = 0,upper = 3, extendInt = "yes")
    ans = RARL1F(lambda,p,L$root,v,n)
    hat_ARL0 = ans$ARL
    ARL0_error = abs(hat_ARL0-ARL)}
  return(list(L1 = L$root, hat_ARL = ans$ARL, 
              hat_MRL = ans$MRL, hat_SDRL = ans$SDRL))
}
######
lambda = 0.05 #0.1 #0.2
ps = 0.1 #0.2 #0.3 #0.4 #0.5 #0.6 #0.7 #0.8 #0.9 
pi = 1 #0.99 #0.95
pss = (ps+pi-1)/(1-2*(1-pi))
v = ps*(1-ps)/(1-2*pi)^2
n = 5 #10 #15 #20 #25
L1andARLf(lambda,pss,v,n,ARL)
######LCL######
RL2 = function(data,lambda,p,L,v,n){
  m = length(data)
  s = rep(NA,m)
  t = rep(NA,m)
  s[1] = lambda * data[1] + (1 - lambda) * p
  t[1] = p-L*sqrt(lambda*(1-(1-lambda)^2)*v/(2-lambda)/n)
  i = 1
  while(s[i]>t[i]){
    i = i+1
    s[i] = lambda * data[i] + (1 - lambda) * s[i-1];
    t[i] = p-L*sqrt(lambda*(1-(1-lambda)^(2*(i)))*v/(2-lambda)/n)
    if (i == m){break}
  }
  return(i)
}
RRL2F = function(lambda,p,L,v,n){
  hat_p = rbinom(5000,n,p)/n
  return(RL2(hat_p,lambda,p,L,v,n))
}

RARL2F = function(lambda,p,L,v,n){
  a = replicate(10000,RRL2F(lambda,p,L,v,n))
  return(list(ARL = mean(a),MRL = median(a),SDRL = sd(a)))
}

L2andARLf = function(lambda,p,v,n,ARL){
  af = function(x){
    RARL2F(lambda,p,x,v,n)$ARL-ARL
  }
  L = uniroot(af,lower = 0,upper = 3, extendInt = "yes")
  ans = RARL2F(lambda,p,L$root,v,n)
  hat_ARL0 = ans$ARL
  ARL0_error = abs(hat_ARL0-ARL)
  while (ARL0_error>0.5){
    L = uniroot(af,lower = 0,upper = 3, extendInt = "yes")
    ans = RARL2F(lambda,p,L$root,v,n)
    hat_ARL0 = ans$ARL
    ARL0_error = abs(hat_ARL0-ARL)}
  return(list(L2 = L$root, hat_ARL = ans$ARL, 
              hat_MRL = ans$MRL, hat_SDRL = ans$SDRL))
}
######
lambda = 0.05 #0.1 #0.2
ps = 0.1 #0.2 #0.3 #0.4 #0.5 #0.6 #0.7 #0.8 #0.9 
pi = 1 #0.99 #0.95
pss = (ps+pi-1)/(1-2*(1-pi))
v = ps*(1-ps)/(1-2*pi)^2
n = 5 #10 #15 #20 #25
L2andARLf(lambda,pss,v,n,ARL)
######EWMA######
library(Rcpp)
cppFunction("NumericVector ewmaRcpp(Rcpp::NumericVector data, double lambda, double p){
       int n = data.length();
       Rcpp::NumericVector s(n);
       s[0] = lambda * data[0] + (1 - lambda) * p;
       if (n > 1) {
         for (int i = 1; i < n; i++) {
           s[i] = lambda * data[i] + (1 - lambda) * s[i-1];
         }
       }
       return s;
     }")