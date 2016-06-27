extrapolation.check <-
function(M,newdata) {
    X <- data.frame( model.matrix(M)[,1+which( attr(M$terms,"order") == 1 )] )
    names(X) <- attr(M$terms,"term.labels")[ which( attr(M$terms,"order") == 1 ) ]
    if(missing(newdata)) { newdata <- X }
    y.name <- names(M$model)[1]
   if( !all( names(newdata) %in% names(M$model)[-1]  ) ) { 
      stop(paste( names(newdata)[which(names(newdata) %in% names(X)==FALSE)],"not found in",eval(parse(newdata)),"\n"))
   }
    
    NEW.X <- X[rep(row.names(X[1,]),nrow(newdata)),]; rownames(NEW.X) <- NULL

    for (i in 1:ncol(X)) {
      NEW.X[,i] <- newdata[,which(names(newdata)==names(X)[i])]
    }
    
    k <- ncol(X)
    COV <- cov(X)
    MEAN <- apply(X,2,mean)
    all.MD <- mahalanobis(X,center=MEAN,cov=COV)
 
    MD <- mahalanobis(NEW.X,center=MEAN,cov=COV)
    
    percentiles <- unlist(lapply(MD,function(x)length(which(all.MD<=x))/length(all.MD)))
    #cat("Note:  analysis assumes the predictor data cloud is roughly elliptical.\n\n")
    #cat("Percentiles are the fraction of observations used in model that are CLOSER to\n")
    #cat("the center than the point(s) in question.  If Percentile is about 99% or higher\n")
    #cat("you may be extrapolating.\n")

    RESULTS <- data.frame(Observation=1:length(percentiles),Percentile=round(100*percentiles,digits=1))
    row.names(RESULTS) <- NULL
    return(RESULTS)
}
