load(file='rtb2_train.rdata')
#########################
nfeat=20
rtb3 = rtb2_train[1:(nfeat+1)]
y=as.matrix(rtb3[,1])
xmat = model.matrix(paying_price~., data=rtb3)
#################
nfeat=seq(1, length(rtb2_train), by = 50)
rtb3 = rtb2_train[1:10000,nfeat]
y=as.matrix(rtb3[,1])
xmat = model.matrix(paying_price~., data=rtb3)
###################
nfeat=seq(1, length(rtb2_train), by = 40)
rtb3 = rtb2_train[nfeat]
y=as.matrix(rtb3[,1])
xmat = model.matrix(paying_price~., data=rtb3)
###############################
lm_evmax <- function(y=NULL, xmat=NULL){
  if(is.null(y) ||  is.null(xmat)){
    return(NULL)
  }
  N = nrow(xmat)
  lambda = 0.001 * N
  tmp = t(xmat) %*% xmat
  diag(tmp) = diag(tmp) + lambda
  mN = solve(tmp) %*% t(xmat) %*% y
  e = y - xmat %*% mN
  beta = as.numeric(N / (t(e) %*% e))
  alpha = lambda * beta
  
  while(TRUE){
    A = beta * (t(xmat) %*% (xmat))
    diag(A) = diag(A) + alpha
    mN = beta * solve(A) %*% t(xmat) %*% y
    eigen.values = eigen(beta * (t(xmat) %*% (xmat)))$values
    gamma = 0
    for(i in eigen.values){
      gamma = gamma + (i / (alpha + i))
    }
    alpha.new = as.numeric(gamma / (t(mN) %*% mN))
    e = y - (xmat %*% mN)
    beta.new = as.numeric((N - gamma) / (t(e) %*% e))
    
    if((abs(alpha - alpha.new) + abs(beta - beta.new)) < 10**(-5)){
      break
    }
    alpha = alpha.new
    beta = beta.new
  }
  
  ret=list(mN=mN, 
           mNsd=sqrt(diag(solve(A))), 
           alpha=alpha, 
           beta=beta)
  return(ret)
}

lmev1 = lm_evmax(y, xmat)
lmev1$mN
lmev1$mNsd
lmev1$alpha
lmev1$beta
