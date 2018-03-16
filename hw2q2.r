load("rtb1_train.rdata")

gen_uagentmat <- function(uagentvec=NULL, y=NULL){
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
  pattern <- "([A-Za-z][A-Za-z0-9]{1,})"
  #agent_lists <- sapply(uagentvec, FUN=function(x)strsplit(x, ','))
  agent_lists <- uagentvec
  agent_lists = regmatches(agent_lists, gregexpr(pattern, agent_lists))
  #keep only unique words in each row. 
  agent_lists = lapply(agent_lists, unique)
  count = c(NA)
  for(agent_list in agent_lists){
    count = c(count, as.character(agent_list))
  }

  count = table(count)

  # remove cols that have only one unique value
  count = count[count!=length(agent_lists)]
  count = sort(count, decreasing = T)
  threshold = 10
  index = (count >= threshold )
  count = count[index]
  index = count <= floor(length(agent_lists)*0.5)
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
    for(agent_list in agent_lists){
      x = c(x, as.numeric(feat %in% agent_list))
    }
    allt[id] = reg_tvalue(y,x)
    
  }
  print(length(count))
  ord = order(abs(allt), as.character(names(count)), decreasing = T)
  count = count[ord]
  allt = allt[ord]
  index = (abs(allt) >= 1)
  count = count[index]
  nfeature = length(names(count))
  featureNames = names(count)
  dummy_matrix = matrix(0, nrow=length(uagentvec), ncol= nfeature + 1)
  colnames(dummy_matrix) = c('constant', featureNames)
  dummy_matrix[,"constant"] = 1
  i = 2
  for(name in featureNames){
    x = c()
    for(agent_list in agent_lists){
      x = c(x, as.numeric(name %in% agent_list))
    }
    dummy_matrix[,name] = x
    colnames(dummy_matrix)[i] = paste("agent", name, sep="_")
    i = i + 1
  }
  return(dummy_matrix)
}

t = rtb1_train[1:1500,]
umat1 = gen_uagentmat(t$user_agent, t$paying_price)
View(head(umat1))
print(head(sort(colSums(umat1), decreasing=TRUE), n=10))
qr1 = qr(umat1, tol =1e-7)
ind3 = qr1$pivot[1:qr1$rank]
rank0 = ncol(umat1)
if(qr1$rank < rank0){ 
  cat("There are", rank0, "columns, but rank is only", qr1$rank, "\n")
  toremove = qr1$pivot[(qr1$rank+1):rank0]
  cat("list of features removed", toremove,"\n")
  tokeep = qr1$pivot[1:qr1$rank]
  umat1 = umat1[,tokeep]
}
y = t$paying_price
w = solve(t(umat1) %*% umat1, t(umat1) %*% y)
print(w)
# 
# for(i in 6:1500){
#   t = rtb1_train[1:i,]
#   umat1 = gen_utagmat(t$user_agent, t$paying_price)
# }
