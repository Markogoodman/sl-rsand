df1_train = read.csv('df1_train.csv')
df1_test1 = read.csv('df1_test1.csv')
df1_test1y = read.csv('df1_test1y.csv')
options(scipen=10)

gpredict <- function(dftrain=NULL, dftest=NULL){
  if(is.null(dftrain) ){
    return(NULL)
  }
  num_xa = 1
  num_xb = ncol(dftrain) - num_xa
  if(!is.null(dftest) && ncol(dftest) != num_xb){
    return(NULL)
  }
  #training part
  training_mean = apply(dftrain, 2, mean)
  mua = training_mean[1]
  mub = training_mean[2:length(training_mean)]
  s = cov(dftrain)
  s[is.na(s)] = 0
  s_ab = s[1:num_xa, (1+num_xa):ncol(s)]
  s_ab = matrix(s_ab, nrow=num_xa, ncol=num_xb)
  s_bb = s[(num_xa+1):nrow(s), (1+num_xa):ncol(s)]
  s_bb = matrix(s_bb, nrow=num_xb, ncol=num_xb)
  s_bb_inverse = solve(s_bb)
  #prediction part
  if(!is.null(dftest) && ncol(dftest) == num_xb){
    
    predict = mua + (s_ab %*% s_bb_inverse %*% t(sweep(dftest, 2, mub, "-")))
    predict[is.na(predict)] = mua
    predict = as.numeric(predict)
  } else {
    predict = NULL
  }
  ret=list(mua=mua, 
           mub=mub, 
           s_ab=s_ab, 
           s_bb=s_bb, 
           predict=predict)
  return(ret)
}

out1 = gpredict(df1_train[1:200,],df1_test1)
print(out1$mua)
print(out1$mub[1:5])
print(out1$s_ab[1:5])
print(out1$s_bb[1:5,1:5])
print(out1$predict)
mae1a = mean(abs(df1_test1y[,1] - out1$pred))
cat("MAE1a=", mae1a, "\n")

