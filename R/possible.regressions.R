possible.regressions <-
function(M,permutations=100,sse=TRUE,reduction=TRUE) {
  
  
  y.label <- names(M$model)[1]
  y <- M$model[,1]
  x.label <- names(M$model)[2]
  x <- M$model[,2]
  
  if(class(M)[1]=="lm") {
    
    if(sse) { par(mfrow=c(1,2)) } else { par(mfrow=c(1,1)) }
    plot(y~x,xlab=x.label,ylab=y.label,pch=20,cex=0.8)
    COEF <- data.frame(intercept=rep(0,permutations),slope=rep(0,permutations))
    if(sse) { SSE <- rep(0,permutations+1); SSE[1] <- sum(M$resid^2) }
    
    
    for (i in 1:permutations) {
      M <- lm(sample(y)~x)
      COEF[i,] <- as.numeric(coef(M))
      abline(M,col=grey(.9))
      if(sse) { SSE[i+1] <- sum(M$resid^2) }
    }
    
    pos <- which.max(COEF[,2])
    neg <- which.min(COEF[,2])
    abline(COEF[pos,1],COEF[pos,2],col="black",lwd=1)
    abline(COEF[neg,1],COEF[neg,2],col="black",lwd=1)
    abline(lsfit(x,y),col="red",lwd=5)
    points(x,y,pch=20,cex=0.8)
    
    if(sse) {
      if(reduction) { SSE <- sum((y-mean(y))^2) - SSE; lab<-"Chance reductions in SSE" } else {
        lab <- "SSE by chance" }
      hist(SSE[-1],,xlab=lab,ylab="",xlim=c(min(SSE),max(SSE)),main="")
      abline(v=SSE[1],col="red",lwd=4)
      legend("topright",paste("p=",round(length(which(SSE[-1]>=SSE[1]))/permutations,2)))
    }
    par(mfrow=c(1,1))
  }
  
  
  if(class(M)[1]=="glm") {
    M.orig <- M
    visualize.model(M)
    for (i in 1:permutations) {
      M <- glm(sample(y)~x,family=binomial)
      pars <- as.numeric(coef(M))
      xs <- seq(min(x),max(x),length=50)
      ps <- exp( pars[1] + pars[2]*xs ) / (1 + exp( pars[1] + pars[2]*xs ) )
      lines(xs,ps,col=grey(.9))
    }
    pars <- as.numeric(coef(M.orig))
    xs <- seq(min(x),max(x),length=50)
    ps <- exp( pars[1] + pars[2]*xs ) / (1 + exp( pars[1] + pars[2]*xs ) )
    lines(xs,ps,lwd=2)
    
    
  }
}
