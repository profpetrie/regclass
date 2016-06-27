summarize.tree <-
function(TREE) {
  
  
  if( sum( class(TREE)%in%c("rpart") )==1 ) {
    y <- TREE$y
    n <- length(y)
    pred.y <- predict(TREE)
    k <- max(1+(TREE$cptable)[,2])
    SSE <-  sum( (y-pred.y)^2 )
    RMSE <- sqrt( SSE/( n-k-1))
    AIC <- n*log(SSE)-n*log(n)+2*k
    return(list(rmse=RMSE,aic=AIC,importance=TREE$var)) }
    
    
    
    
    #Random forest
    if( sum(class(TREE) %in% c("randomForest"))==1 )  {
      IMP <- TREE$importance
      if(TREE$type=="regression") {
        IMP <- IMP[order(-IMP[,ncol(IMP)]),]
      }
        if(TREE$type=="classification") {
          IMP <- IMP[order(-IMP[,ncol(IMP)]),]
          
        }

     return(list(importance=IMP))
    }
    
}
