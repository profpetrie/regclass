find.transformations <-
function(M,powers=seq(from=-3,to=3,by=.25),...) {
    if(class(M)!="lm") { cat("Argument needs to be a fitted simple linear regression model using lm()\n"); return(0); }
    V <- names(M$coef)
    DATA <- M$model
    if(length(V)>2) { cat("This function is valid only for simple linear regression\n"); return(0); }
    D <- names(DATA)
    
    x <- DATA[,2]
    y <- DATA[,1]
    
    cat(paste("y is",D[1],"\n"))
    cat(paste("x is",D[2],"\n"))
       
    RESULTS <- data.frame(x.power=rep(0,length(powers)^2 ),y.power=rep(0,length(powers)^2),rsquared=rep(0,length(powers)^2))
    z<-1
    bads <- c()
    for( i in 1:length(powers)) {
        for(j in 1:length(powers)) {
            if( powers[i]<=0 & min(x)<=0 ) { bads <- c(bads,z) }
            if( powers[j]<=0 & min(y)<=0 ) { bads <- c(bads,z) }
            if( powers[i] == 0) { x.new <- log10(x) } else { x.new <- x^powers[i] }
            if( powers[j] == 0) { y.new <- log10(y) } else { y.new <- y^powers[j] }
            collect <- c(powers[i],powers[j],round( cor(x.new,y.new)^2, digits=3) )
            if( powers[i] == 0) { collect[1] <- "log10" }
            if( powers[j] == 0) { collect[2] <- "log10" }
            RESULTS[z,] <- collect
            z<-z+1
        }}
    if(length(bads)>0) { RESULTS <- RESULTS[-bads,] }
    RESULTS <- RESULTS[order(RESULTS$rsquared,decreasing=TRUE),]
    
    par(mfrow=c(2,2))
    plot(DATA[,2],DATA[,1],xlab=names(DATA)[2],ylab=names(DATA)[1],main="No Transformation",...)
    
    
    for (i in 1:3) {
    if(RESULTS[i,1]!="log10") { 
      xx <- DATA[,2]^as.numeric(RESULTS[i,1])
      xl <- paste(names(DATA)[2],"raised to",RESULTS[i,1]) } else { 
        xx <- log10(DATA[,2]) 
        xl <- paste("log10(",names(DATA)[2],")",sep="")
      }

    if(RESULTS[i,2]!="log10") { 
      yy <- DATA[,1]^as.numeric(RESULTS[i,2]) 
      yl <- paste(names(DATA)[1],"raised to",RESULTS[i,2]) } else { 
        yy <- log10(DATA[,1])
        yl <-  paste("log10(",names(DATA)[1],")",sep="")
      }
    if(i==1) { mt <- "Highest R2"} else{ if (i==2) { mt <- "2nd highest R2"} else { mt <- "3rd highest R2"}}
    plot(yy~xx,xlab=xl,ylab=yl,main=mt,...)
          }
    par(mfrow=c(1,1))
    cat(paste("No transformation yields rsquared of",round(cor(x,y)^2,digits=3),"\n\n"))
    selected <- which(as.numeric(RESULTS$rsquared) >= max(as.numeric(RESULTS$rsquared)-.02))
    RESULTS <- RESULTS[selected,]
    print(RESULTS,row.names=FALSE)
    
}
