choose.order <-
function(M,max.order=6,sort=FALSE,loc="topleft",...) {
    if(class(M)!="lm") { cat("Argument must be a fitted model\n"); return() }
    V <- names(M$coef)
    if(length(V)>2) { stop(cat("This function is valid only for simple linear regression\n")) }
    
    DATA <- M$model
    DATA <- DATA[order(DATA[,2]),]
    y <- DATA[,1]
    x <- DATA[,2]
    n <- nrow(DATA)
    R2adj <- c()
    A <- c()
    par(mfrow=c(1,1))
    plot(y~x,xlab=V[2],ylab=V[3],...)
    for (i in 1:max.order) {
        M <- lm(y~poly(x,i))
        lines(x,fitted(M),col=i,lwd=2)
        k <- i+1
        A[i] <- AIC(M)+ 2*k +2*k*(k+1)/(n-k-1)
        R2adj[i] <- summary(M)$adj.r.sq }
    D <-data.frame(order=1:max.order,R2adj=R2adj,AICc=A)
    if(sort==TRUE | sort=="R2adj" | sort=="r2adj" | sort=="r2" | sort=="R2") { D <- D[order(D$R2adj,decreasing=TRUE),] }
    if(sort=="aic" | sort=="AIC" | sort=="AICc" | sort=="aicc" | sort=="AICC") { D <- D[order(D$AICc),] }
    print(D,row.names=FALSE)
    legend(loc,paste("Order",1:max.order),col=1:max.order,lty=1,lwd=2,cex=0.7) 
}
