visualize.model <-
function(M,loc="topleft",level=0.95,cex.leg=0.7,...) {
  
  COLORS <- c("blue","black","red")
  
  if(class(M)[1]=="lm") {
    y.label <- names(M$model)[1]
    y <- M$model[,1]
    
    TERMS <- as.character(attr(terms(M),"variables"))[-(1:2)]
    
    if(length(TERMS)==1) { 
      par(mfrow=c(1,1))
      x.label <- names(M$model)[2]
      x <- M$model[,2]
      if( !(class(x) %in% c("integer","numeric") )) { stop(cat("Error:  the model cannot have I(), poly(), factors, or transformations like x^2 in its formulation\n")) } 
      new <- data.frame(sort(x))
      names(new) <- x.label
      conf.int <- predict(M,newdata=new,interval="confidence",level)$fit
      pred.int <- predict(M,newdata=new,interval="prediction",level)$fit
      plot(y~x,xlab=x.label,ylab=y.label,ylim=c(min(pred.int[,2]),max(pred.int[,3])),...)
      title("Scatterplot, fitted line, and confidence/prediction intervals",cex.main=0.9)
      abline(lsfit(x,y),col="red",lwd=3)
      lines(new[,1],conf.int[,2],lwd=2)
      lines(new[,1],conf.int[,3],lwd=2)
      lines(new[,1],pred.int[,2],lwd=2,lty=2)
      lines(new[,1],pred.int[,3],lwd=2,lty=2)
      legend(loc,c("Confidence Interval","Prediction Interval"),lty=1:2,lwd=2,cex=cex.leg)
    } 
    
    if(length(TERMS)==2) {
      
      x1.label <- names(M$model)[2]
      x2.label <- names(M$model)[3]
      x1 <- M$model[,2]
      x2 <- M$model[,3]
      x1.q <- ifelse(class(M$model[,2])%in%c("integer","numeric"),TRUE,FALSE)
      x2.q <- ifelse(class(M$model[,3])%in%c("integer","numeric"),TRUE,FALSE)
      
      #Both predictors are quantitative
      if(!x1.q&!x2.q) { stop(cat("Error:  the model requires at least one quantitative predictor\n")) }
      if(x1.q&x2.q) {
        par(mfrow=c(1,2))
        COLORS <- c("blue","black","red")
        P <- list()
        
        z <- 1
        for(q in c(1-level,.5,level)) {
          new <- data.frame(x1=x1,x2=rep(quantile(x2,q),length(x1)))
          names(new) <- TERMS
          new <- new[order(new[,1]),]
          P[[z]] <- predict(M,newdata=new)
          z <- z+1
        }
        
        plot(0,0,xlab=x1.label,ylab=y.label,xlim=range(x1),ylim=range(unlist(P)),col="white",
             main=paste("Implicit lines relating",y.label,"to",x1.label,"\nfor various values of",x2.label),cex.main=0.9)
        
        
        for(z in 1:3) { lines(sort(x1),P[[z]],col=COLORS[z]) } 
        legend(loc,paste(x2.label,c("small","median","large")),lty=1,col=COLORS,cex=cex.leg)
        
        z <- 1
        for(q in c(1-level,.5,level)) {
          new <- data.frame(x1=rep(quantile(x1,q),length(x2)),x2=x2)
          names(new) <- TERMS
          new <- new[order(new[,2]),]
          P[[z]] <- predict(M,newdata=new)
          z <- z+1
        }
        
        plot(0,0,xlab=x2.label,ylab=y.label,xlim=range(x2),ylim=range(unlist(P)),col="white",
             main=paste("Implicit lines relating",y.label,"to",x2.label,"\nfor various values of",x1.label),cex.main=0.9)
        
        for(z in 1:3) { lines(sort(x2),P[[z]],col=COLORS[z]) } 
        legend(loc,paste(x1.label,c("small","median","large")),lty=1,col=COLORS,cex=cex.leg)
        
        if(max(attr(terms(M),"order"))==2) { 
          cat(paste("\nInteraction term has p-value",prettyNum(unlist(drop1(M,test="F")[6])[2],digits=4,format="g"),"\n"))
        }
        
        
        par(mfrow=c(1,1))
        
      } else { 
        
        P <- list()
        
        if(x1.q) { each.level <- levels(x2) } else { each.level <- levels(x1) } 
        
        z <- 1
        for(q in each.level) {
          new <- data.frame(x1=x1,x2=x2)
          if(x1.q) { new[,2] <- factor(q,levels=each.level) } else {
            new[,1] <- factor(q,levels=each.level) }
          names(new) <- TERMS
          if(x1.q) { new <- new[order(new[,1]),] } else { new <- new[order(new[,2]),] }
          P[[z]] <- predict(M,newdata=new)
          z <- z+1
        }
        
        cat.label <- ifelse(x1.q,x2.label,x1.label)
        if(x1.q) { plot.label <- x1.label; plot.range <- range(x1) } else { plot.label <- x2.label; plot.range <- range(x2) }
        if(x1.q) { 
          plot(x1,y,xlab=plot.label,ylab=y.label,xlim=plot.range,ylim=range(unlist(P)),pch=20,col=as.numeric(x2)) }
        if(x2.q) { 
                plot(x2,y,xlab=plot.label,ylab=y.label,xlim=plot.range,ylim=range(unlist(P)),pch=20,col=as.numeric(x1)) }
        title(paste("Implicit lines relating",y.label,"to",plot.label,"\nfor each level of",cat.label),cex.main=0.9)
        
        
        for(z in 1:length(each.level)) { 
          to.plot <- 
            if(x1.q) { lines(sort(x1),P[[z]],col=z) } else { lines(sort(x2),P[[z]],col=z) } }
        legend(loc,legend=each.level,lty=1,col=1:length(each.level),cex=cex.leg) 
        
        if(max(attr(terms(M),"order"))==2) { 
          cat(paste("\nEffect test for interaction with",cat.label,"has p-value",prettyNum(unlist(drop1(M,test="F")[6])[2],digits=4,format="g"),"\n"))
        } else {
            sel.row <- which( drop1(M,test="F")$Df >= 2)
            cat(paste("\nEffect test for",cat.label,"has p-value",prettyNum(unlist(drop1(M,test="F")[6])[sel.row],digits=4,format="g"),"\n"))
       
        }
        
        
        
        
        
        
      }
      
      
      
      
    }
  }
  
  if(class(M)[1]=="glm") {
    
    opts <- list(...)
    if(length(opts$xlim)>0) { x.plot <- seq(opts$xlim[1],opts$xlim[2],length=100)  } 
    y.label <- names(M$model)[1]
    y <- M$model[,1]
    interest <- levels(y)[2]
    TERMS <- as.character(attr(terms(M),"variables"))[-(1:2)]
    
    
    
    if(length(TERMS)>3) { stop(cat("Error:  model can have at most two predictors\n")) } 
    
    #Simple logistic regression first
    if(length(TERMS)==1) {
      par(mfrow=c(1,1))
      x.label <- names(M$model)[2]
      x <- M$model[,2]
      if( !(class(x) %in% c("integer","numeric") )) { stop(cat("Error:  the model cannot have I(), poly(), factors, or transformations like x^2 in its formulation\n")) } 
      b0 <- M$coef[1]
      b1 <- M$coef[2]
      if(length(opts$xlim)==0) { x.plot <- seq(min(x),max(x),length=100) }
      y.plot <- exp(b0+b1*x.plot)/(1+exp(b0+b1*x.plot))
      plot(x.plot,y.plot,xlab=x.label,ylab=paste("Probability of ",interest),ylim=c(0,1),type="l",...) 
      title("Fitted logistic curve")
    } 
    
    if(length(TERMS)==2) {
      x1.label <- names(M$model)[2]
      x2.label <- names(M$model)[3]
      x1 <- M$model[,2]
      x2 <- M$model[,3]
      x1.q <- ifelse(class(M$model[,2])%in%c("integer","numeric"),TRUE,FALSE)
      x2.q <- ifelse(class(M$model[,3])%in%c("integer","numeric"),TRUE,FALSE)
      
      #Both predictors are quantitative
      if(!x1.q&!x2.q) { stop(cat("Error:  the model requires at least one quantitative predictor\n")) }
      if(x1.q&x2.q) {
        par(mfrow=c(1,2))
        COLORS <- c("blue","black","red")
        P <- list()
        
        if(length(opts$xlim)==0) { x.plot <- seq(min(x1),max(x1),length=100) }
        z <- 1
        for(q in c(1-level,.5,level)) {
          new <- data.frame(x1=x.plot,x2=rep(quantile(x2,q),length(x.plot)))
          names(new) <- TERMS
          new <- new[order(new[,1]),]
          P[[z]] <- predict(M,newdata=new,type="response")
          z <- z+1
        }
        
        plot(0,0,xlab=x1.label,xlim=range(x.plot),ylim=c(0,1),col="white",
             ylab=paste("Probability of",interest),
             main=paste("Implicit lines relating probability of",interest,"to",x1.label,"\nfor various values of",x2.label),cex.main=0.9)
        
        
        for(z in 1:3) { lines(x.plot,P[[z]],col=COLORS[z]) } 
        legend(loc,paste(x2.label,c("small","median","large")),lty=1,col=COLORS,cex=cex.leg)
        
        
        if(length(opts$xlim)==0) { x.plot <- seq(min(x2),max(x2),length=100) }
        z <- 1
        for(q in c(1-level,.5,level)) {
          new <- data.frame(x1=rep(quantile(x1,q),length(x.plot)),x2=x.plot)
          names(new) <- TERMS
          new <- new[order(new[,2]),]
          P[[z]] <- predict(M,newdata=new,type="response")
          z <- z+1
        }
        
        plot(0,0,xlab=x2.label,xlim=range(x.plot),ylim=c(0,1),col="white",
             ylab=paste("Probability of",interest),
             main=paste("Implicit lines relating probability of",interest,"to",x2.label,"\nfor various values of",x1.label),cex.main=0.90)
        
        for(z in 1:3) { lines(x.plot,P[[z]],col=COLORS[z]) } 
        legend(loc,paste(x1.label,c("small","median","large")),lty=1,col=COLORS,cex=cex.leg)
        
        if(max(attr(terms(M),"order"))==2) { 
          cat(paste("\nInteraction term has p-value",prettyNum(unlist(drop1(M,test="Chisq"))[10],digits=4,format="g"),"\n"))
        }
        
        par(mfrow=c(1,1))
        
      } else {
        P <- list()
        
        if(x1.q) { each.level <- levels(x2) } else { each.level <- levels(x1) } 
        
        if(length(opts$xlim)==0) {
          if(x1.q) { x.plot <- seq(min(x1),max(x1),length=100) } else { x.plot <- seq(min(x2),max(x2),length=100) }
        }
        
        z <- 1
        
        
        for(q in each.level) {
          if(x1.q) { new <- data.frame(x1=x.plot,x2=factor(q,levels=each.level)) } else {
            new <- data.frame(x1=factor(q,levels=each.level),x2=x.plot) } 
          names(new) <- TERMS
          P[[z]] <- predict(M,newdata=new,type="response")
          z <- z+1
        }
        
        cat.label <- ifelse(x1.q,x2.label,x1.label)
        if(x1.q) { plot.label <- x1.label } else { plot.label <- x2.label }
        plot(x.plot,x.plot,xlab=plot.label,ylab=paste("Probability of",interest),ylim=c(0,1),col="white") 
        title(paste("Implicit lines relating probability of",interest,"to",plot.label,"\nfor each level of",cat.label),cex.main=0.90)
        
        
        for(z in 1:length(each.level)) { lines(x.plot,P[[z]],col=z) }
        legend(loc,legend=each.level,lty=1,col=1:length(each.level),cex=cex.leg) 
        
        if(max(attr(terms(M),"order"))==2) { 
          cat(paste("\nEffect test for interaction with",cat.label,"has p-value",prettyNum(unlist(drop1(M,test="Chisq"))[10],digits=4,format="g"),"\n"))
        } else {
            sel.row <- which( drop1(M,test="Chisq")$Df >= 2)
            cat(paste("\nEffect test for",cat.label,"has p-value",prettyNum(unlist(drop1(M,test="Chisq")[5])[sel.row],digits=4,format="g"),"\n"))

        }
        
        
        
      }
      
    }
    
  }
  
  if(class(M)[1]=="rpart") {
    levs <- colnames(predict(M))
    if(M$method=="anova") { 
      node.fun <- function(x,labs,digits,varlen) {
        a <- x$frame$yval
        paste(prettyNum(a,digits=3,format="g"),"\n\nn=",x$frame$n)
      }}
    if(M$method=="class") { 
      node.fun <- function(x,labs,digits,varlen) {
        p.interest <- x$frame$yval2[,5]
        cl <- ifelse(p.interest>=0.5,levs[2],levs[1])
        paste(cl,"\n",levs[1],round(1-p.interest,digits=3),"\n",levs[2],round(p.interest,digits=3),"\n\nn =",x$frame$n)
      } }
    prp(M,node.fun=node.fun,tweak=0.8,varlen=0)
  }
  
  par(mfrow=c(1,1))
}
