ols <- function(y, X){
  
  X <- cbind(1, X)
  
  # getting the coefficients
  coefs <- solve(t(X)%*%X) %*% (t(X)%*%y)
  
  # and now for the SEs...
  # residuals
  e <- y - X %*% coefs
  # estimated sigma squared
  hat_sigma2 <- sum(t(e)%*%e) / (nrow(X)-length(coefs))
  # and we can find estimated standard errors from $\hat{\sigma^2}(X'X)^{-1}$.
  varcovmat <- hat_sigma2 * solve((t(X)%*%X)) 
  SEs <- sqrt(diag(varcovmat))
  
  # t-stats
  tstat <- coefs/SEs
  
  # p-values
  df <- nrow(X) - ncol(X)
  pvals <- pt(abs(tstat), df=df, ncp=0, lower.tail=FALSE) * 2
  
  
  # output
  out <- round(as.matrix(cbind(coefs, SEs, tstat, pvals)), 5)
  colnames(out) <- c("coefficient", "SEs", "t-statistic", "p-values")
  
  return(out)
}