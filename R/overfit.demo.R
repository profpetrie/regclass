overfit.demo <-
function(DF,y=NA,seed=NA) {
  if(is.na(y)) { stop(paste("Need to specify y variable in quotes\n")) }
  if(!is.na(seed)) { set.seed(seed) }
  n <- nrow(DF)
  selected <- sample(n,n/2,replace=TRUE)
  training <- DF[selected,]
  holdout <- DF[-selected,]
  
  form1 <- formula( paste(y,"~1") )
  form2 <- formula( paste(y,"~.^2") )
  
  null.model <- lm(form1,data=training)
  full.model <- lm(form2,data=training)
  
  best.model <- step(null.model,scope=list(lower=null.model,upper=full.model),direction="forward",trace=0)
  M <- step(null.model,scope=list(lower=null.model,upper=full.model),direction="forward",trace=0,steps=1)
  
  y.pos <- which(names(holdout)==y)
  y.holdout <- holdout[,y.pos]
  pred.holdout <- predict(M,newdata=holdout)
  RMSE.holdout <- sqrt(mean( (y.holdout-pred.holdout)^2 ))
  aic.train <- AIC(M)
  
  for (i in 2:30) { 
    	M <- step(M,scope=list(lower=null.model,upper=full.model),direction="both",steps=1,trace=0,k=.001)
    	pred.holdout <- predict(M,newdata=holdout)
    	RMSE.holdout[i] <- sqrt(mean( (y.holdout-pred.holdout)^2 ))
    	aic.train[i] <- AIC(M)
    	}
  
  RMSE.holdout <- (RMSE.holdout-min(RMSE.holdout))/(max(RMSE.holdout)-min(RMSE.holdout))
  aic.train <- (aic.train-min(aic.train))/(max(aic.train)-min(aic.train))
  
  plot( 1:length(aic.train),RMSE.holdout,xlab="# variables added",ylab="Relative Performance",type="l",lwd=2,yaxt="n")
         lines( 1:length(aic.train),aic.train,lwd=2,lty=2)
         legend("top",c("RMSE(holdout)","AIC(training)"),lwd=2,lty=1:2)

  
}
