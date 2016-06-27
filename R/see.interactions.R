see.interactions <-
function(M,pos="bottomright",many=FALSE,level=0.95,...) {
    if(class(M)!="lm") { cat("First argument needs to be a fitted linear regression model using lm()\n"); return(0); }
    
    DATA <- M$model
    V <- names(M$coef)
    D <- names(DATA)
    
    int <- attr(terms(M),"order")
    
    if(max(int)==1) { stop("No interactions fitted in model\n")  }
    terms.selected <- which(int==2)
    
    int <- attr(terms(M),"term.labels")[terms.selected]
    
    L <- matrix(0,nrow=length(int),ncol=2)
    
    selected <- c()
    to.put <- 0
    pn <- 1
    
    for (i in 1:length(int)) {
        v1 <- unlist(strsplit(int[i],split=":"))[1]
        v2 <- unlist(strsplit(int[i],split=":"))[2]
        if(class(DATA[,v1]) %in% c("integer","numeric") | class(DATA[,v2]) %in% c("integer","numeric") ) { 
          selected <- c(selected,i)
          L[i,] <- c(pn,pn+1)
          if( (class(DATA[,v1]) == "factor"| class(DATA[,v2]) == "factor") ) { 
            if(i==1) { L[i,] <- c(1,0); to.put <- 1 } else{
              L[i,] <- c(0,0)
              if( to.put>0 ) { L[to.put,2] <- pn; to.put <- 0 } else  {  L[i,1]<-pn;  to.put <- i } 
            }
            pn <- pn+1
         } else { pn <- pn + 2}
        } 
        
        }
    
    temp <- apply(L,1,sum)
    take.out <- which(temp==0)
    if(length(take.out)>0) { L <- L[-take.out,] }
    if(sum(L)==1) { L <- matrix(1,nrow=1,ncol=1) } 
    
    types <- unlist(lapply((DATA),class))
    cats <- which(types=="factor")

    if(many==FALSE) {  layout(L) } else { par(mfrow=c(1,2)) }
    
    par(mar=c(4,4,1,1))
    for (i in selected) {
        var1 <- unlist(strsplit(int[i],split=":"))[1]
        var2 <- unlist(strsplit(int[i],split=":"))[2]
        whichvar1 <- which(D==var1)
        whichvar2 <- which(D==var2)
        x1 <- DATA[,var1]
        x2 <- DATA[,var2]
        
        if(class(x1)%in%c("integer","numeric")&class(x2)%in%c("integer","numeric")) {
        min.x1 <- quantile(x1,1-level)
        min.x2 <- quantile(x2,1-level)
        max.x1 <- quantile(x1,level)
        max.x2 <- quantile(x2,level)
        
        new <- DATA
        new[,whichvar2] <- min.x2
        new[,setdiff(1:ncol(DATA),c(cats,whichvar1,whichvar2))] <- 0
        if(length(cats)>0) {
          for (j in cats) {
            new[,j] <- factor(names(table(new[,j]))[1])
          }
        }
        y.low <- predict(M,newdata=new)
        low.data <- data.frame(x1,y.low)
        low.data <- low.data[order(low.data$x1),]
        
        new <- DATA
        new[,whichvar2] <- max.x2
        new[,setdiff(1:ncol(DATA),c(cats,whichvar1,whichvar2))] <- 0
        if(length(cats)>0) {
          for (j in cats) {
            new[,j] <- factor(names(table(new[,j]))[1])
          }
        }
        y.high <- predict(M,newdata=new)
        high.data <- data.frame(x1,y.high)
        high.data <- high.data[order(high.data$x1),]
        
        new <- DATA
        new[,whichvar2] <- median(x2)
        new[,setdiff(1:ncol(DATA),c(cats,whichvar1,whichvar2))] <- 0
        if(length(cats)>0) {
          for (j in cats) {
            new[,j] <- factor(names(table(new[,j]))[1])
          }
        }
        y.med <- predict(M,newdata=new)
        med.data <- data.frame(x1,y.med)
        med.data <- med.data[order(med.data$x1),]
        
        
        plot(low.data,type="l",xlab=var1,ylab=names(DATA)[1],ylim=c(min(c(y.low,y.high)),max(c(y.low,y.high))),lwd=2,yaxt='n')
        lines(med.data,lty=2,lwd=2)
        lines(high.data,lty=3,lwd=2)
        
        legend(pos,c(paste(var2,"= Small"), paste(var2,"= Median"), paste(var2,"= Large") ),lty=1:3,...)
        
        new <- DATA
        new[,whichvar1] <- min.x1
        new[,setdiff(1:ncol(DATA),c(cats,whichvar1,whichvar2))] <- 0
        if(length(cats)>0) {
          for (j in cats) {
            new[,j] <- factor(names(table(new[,j]))[1])
          }
        }
        y.low <- predict(M,newdata=new)
        low.data <- data.frame(x2,y.low)
        low.data <- low.data[order(low.data$x2),]
        
        new <- DATA
        new[,whichvar1] <- max.x1
        new[,setdiff(1:ncol(DATA),c(cats,whichvar1,whichvar2))] <- 0
        if(length(cats)>0) {
          for (j in cats) {
            new[,j] <- factor(names(table(new[,j]))[1])
          }
        }
        y.high <- predict(M,newdata=new)
        high.data <- data.frame(x2,y.high)
        high.data <- high.data[order(high.data$x2),]
        
        new <- DATA
        new[,whichvar1] <- median(x1)
        new[,setdiff(1:ncol(DATA),c(cats,whichvar1,whichvar2))] <- 0
        if(length(cats)>0) {
          for (j in cats) {
            new[,j] <- factor(names(table(new[,j]))[1])
          }
        }
        y.med <- predict(M,newdata=new)
        med.data <- data.frame(x2,y.med)
        med.data <- med.data[order(med.data$x2),]
        
        
        plot(low.data,type="l",xlab=var2,ylab=names(DATA)[1],ylim=c(min(c(y.low,y.high)),max(c(y.low,y.high))),lwd=2,yaxt='n')
        lines(med.data,lty=2,lwd=2)
        lines(high.data,lty=3,lwd=2)
        legend(pos,c(paste(var1,"= Small"), paste(var1,"= Median"), paste(var1,"= Large") ),lty=1:3,...)
        if(many==TRUE & i < max(selected)) {
            cat ("\nPress [enter] to continue to see next set of interactions or q (then Enter) to quit\n")
            line <- readline()
            if(line=="q") {
                par(mfrow=c(1,1))
                par(mar=c(5, 4, 4, 2) + 0.1)
                return("Command completed"); }}
        
        } else {
         
          new <- DATA
          new[,setdiff(1:ncol(DATA),c(cats,whichvar1,whichvar2))] <- 0
          
          if(class(x1)=="factor") { x.c <- 1 } else { x.c <- 2 }
          if(x.c==1) {
            F <- list()
            for (z in 1:nlevels(x1)) {
              new[,whichvar1] <- levels(x1)[z]     
              if(length(cats)>1) { 
                for(k in setdiff(cats,whichvar1)) {
                  new[,k] <- levels(DATA[,k])[1]
                }
                }
              y.new <- predict(M,newdata=new)
              f.data <- data.frame(x2,y.new)
              F[[z]] <- f.data[order(f.data$x2),]
            }
          }
          if(x.c==2) {
            F <- list()
            for (z in 1:nlevels(x2)) {
                  new[,whichvar2] <- levels(x2)[z]
                  if(length(cats)>1) { 
                    for(k in setdiff(cats,whichvar2)) {
                      new[,k] <- levels(DATA[,k])[1]
                    }
                  }
                  
                  y.new <- predict(M,newdata=new)
                  f.data <- data.frame(x1,y.new)
                  F[[z]] <- f.data[order(f.data$x1),]
            }
          }
            
          ymin <- min(unlist(lapply(F,function(x)min(x[,2]))))
          ymax <- max(unlist(lapply(F,function(x)max(x[,2]))))
          if(many==TRUE) { par(mfrow=c(1,1)) } 
          plot(F[[1]],xlab=ifelse(x.c==1,var2,var1),ylab=names(DATA)[1],col="white",ylim=c(ymin,ymax),yaxt='n')

          for (z in 1:ifelse(x.c==1,nlevels(x1),nlevels(x2))) {
            lines(F[[z]],col=z) 
          }
          if(x.c==1) { lnames <- levels(x1) } 
          if(x.c==2) { lnames <- levels(x2) }
          legend(pos,legend=lnames,col=1:length(lnames),lty=1,... )
          if(many==TRUE & i < max(selected)) {
            par(mfrow=c(1,2))
            cat ("\nPress [enter] to continue to see next set of interactions or q (then Enter) to quit\n")
            line <- readline()
            if(line=="q") {
              par(mfrow=c(1,1))
              par(mar=c(5, 4, 4, 2) + 0.1)
              return("Command completed"); }}
          
        }
        
    }
    par(mar=c(5, 4, 4, 2) + 0.1)
    par(mfrow=c(1,1))
    
}
