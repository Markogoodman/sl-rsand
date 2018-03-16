load("rtb1_train.rdata")

gen_utagmat <- function(utagvec=NULL, y=NULL){
  # cal t-value
  reg_tvalue = function(y, x) {
    if(length(y) != length(x)) {
      stop("Inconsistent length of y and x")
    }
    y=matrix(y, ncol=1)
    xmat=matrix(1, ncol=2, nrow=length(y))
    xmat[,2] = x  

    bhead = solve(t(xmat)%*%xmat, t(xmat)%*%y)
    yhead = xmat %*% bhead
    e1 = y - yhead
    var1 = sum(e1 * e1) / (length(e1)-2)    
    sigma2 = solve(t(xmat)%*%xmat) * var1
    t1=bhead[2]/sqrt(sigma2[2,2])    
    return(t1)
  }
  
  tag_lists <- sapply(utagvec, FUN=function(x)strsplit(x, ','))
  count = c(NA)
  for(tag_list in tag_lists){
    count = c(count, as.numeric(tag_list))
  }
  count = table(count)
  # remove cols that have only one unique value
  count = count[count!=length(tag_lists)]
  count = sort(count, decreasing = T)
  threshold = 5
  index = count >= 5
  count = count[index]
  nfeature = length(count)
  featureNames = names(count)
  allt = rep(NA, nfeature)
  names(allt) = names(count)

  for(id in 1:nfeature) {

    feat = featureNames[id]
    if(is.na(feat)){
      break
    }
    x=c()
    for(tag_list in tag_lists){
      x = c(x, as.numeric(feat %in% tag_list))
    }
    allt[id] = reg_tvalue(y,x)
    
  }
  ord = order(abs(allt), decreasing = T)
  count = count[ord]
  allt = allt[ord]
  index = (abs(allt) >= 1)
  count = count[index]
  nfeature = length(names(count))
  featureNames = names(count)
  dummy_matrix = matrix(0, nrow=length(utagvec), ncol= nfeature + 1)
  colnames(dummy_matrix) = c('constant', featureNames)
  dummy_matrix[,"constant"] = 1
  i = 2
  for(name in featureNames){
    x = c()
    for(tag_list in tag_lists){
      x = c(x, as.numeric(name %in% tag_list))
    }
    dummy_matrix[,name] = x
    colnames(dummy_matrix)[i] = paste("user", name, sep="_")
    i = i + 1
  }
  return(dummy_matrix)
}

t = rtb1_train[1:5,]
umat1 = gen_utagmat(t$user_tags, t$paying_price)
head(umat1)
y = t$paying_price
w = solve(t(umat1) %*% umat1, t(umat1) %*% y)
print(w)

for(i in 6:1500){
  t = rtb1_train[1:i,]
  umat1 = gen_utagmat(t$user_tags, t$paying_price)
}
