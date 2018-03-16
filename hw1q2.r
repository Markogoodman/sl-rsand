mle_update <- function(mu, s, n, x){
  n = n + 1
  mu_old = mu
  #update
  mu = mu + (x - mu) / n
  new_s = (x - mu) %*% t(x - mu) / n +
          (s * (n - 1) / n) +
          (x - mu_old) %*% t(x - mu_old) * (n-1) / (n^3)
  ret = list(mu=mu,
             s=new_s,
             n=n)
  return(ret)
}
set.seed(1223)
nobs = 3 
nfeature = 7
rawdata=matrix(runif(nobs*nfeature), nrow=nobs, ncol=nfeature)
data1 = rawdata[1:(nobs-1),] 
xn = rawdata[nobs,] 
cov1 = cov(data1)*(nrow(data1)-1) / nrow(data1)
mu1 = colMeans(data1) 

out1 = mle_update(mu1, cov1, nrow(data1), xn)
print(out1$mu[1:3])
print(out1$s[1:3,1:3])
print(out1$n)
