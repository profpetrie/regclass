check.regression <-
function(M,extra=FALSE,tests=TRUE,simulations=500,n.cats=10,seed=NA,prompt=TRUE) {
  
  
  if(length(intersect(class(M),c("lm","glm")))==0) {
    stop(cat("First argument needs to be a fitted linear regression model using lm() or logistic regression using glm()\n"))
  }
  
  
  #Linear Regression
  if(class(M)[1]=="lm") {

    DATA <- M$model
    
    #Residuals plot, histogram of residuals, and QQ plot of residuals
    par(mfrow=c(1,3))
    plot(fitted(M),residuals(M),xlab="Predicted Values",ylab="Residuals",main="Residuals Plot",pch=20,cex=.6)
    abline(h=0)
    box(which="figure",col="grey",lwd=2)
    hist(residuals(M),main="",xlab="Residuals",ylab="Relative Frequency",freq=FALSE)
    curve(dnorm(x,0,sd(residuals(M))),col="red",add=TRUE)
    box(which="figure",col="grey",lwd=2)
    qq <- function(x)  {
      x <- sort(x)
      n <- length(x)
      P <- ppoints(n)
      z <- qnorm(P,mean(x),sd(x))
      plot(z, x, xlab = "Values of residuals if Normal", ylab = "Observed values of residuals",pch=20,cex=.8)
      Q.x <- quantile(x, c(0.25, 0.75))
      Q.z <- qnorm(c(0.25, 0.75), mean(x),sd(x) )
      b <- as.numeric( (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1]) )
      a <- as.numeric( Q.x[1] - b * Q.z[1] )
      abline(a, b, lwd=1,col="red")
      conf <-  0.95
      zz <- qnorm(1 - (1 - conf)/2)
      SE <- (b/dnorm(z,mean(x),sd(x)) ) * sqrt(P * (1 - P)/n)
      fit.value <- a + b * z
      upper <- fit.value + zz * SE
      lower <- fit.value - zz * SE
      lines(z, upper, lty = 2,col="red",lwd=1)
      lines(z, lower, lty = 2,col="red",lwd=1)
    } 
    qq(residuals(M))
    box(which="figure",col="grey",lwd=2)
    
    
    
    y <- DATA[,1]
    MM <- model.matrix(M)
    X <- MM[,-1]
    if(class(X)!="matrix") { X <- matrix(X,ncol=1); colnames(X) <- colnames(MM)[2] }
    
    #Statistical Tests of assumptions
    
    if(tests==TRUE) {
    cat(paste("\nTests of Assumptions: ( sample size n =",nrow(DATA),"):\n"))
    
    cat(paste("Linearity\n"))
    
    #Linearity Tests
    
    
    for ( i in 1:ncol(X)) {
      x <- X[,i]
      if(length(unique(x))<=2) { LIN <- "NA (categorical or only 1 or 2 unique values)" } else {
        if(length(x)!=length(unique(x))) {
          M1 <- lm(y~x)
          M2 <- lm(y~as.factor(x))
          LIN <- round( anova(M1,M2)$"Pr(>F)"[2],digits=4)  } else { LIN <- "NA (no duplicate values)" }} 
      cat(paste("   p-value for",colnames(X)[i],":",LIN,"\n")) 
    }
    
    #Overall test of linearity
    
    if(sum(duplicated(X))>1) {
      combo <- c()
      for (i in 1:nrow(X)) { 
        entries <- as.numeric(unlist(X[i,]))
        vars <- entries[1]
        if(length(entries)>1) { for (j in 2:length(entries)) { vars <- paste(vars,entries[j])  } }
        combo <- c(combo,vars) }
      
        xf <- factor(combo)
        M.sat <- lm(y~xf)
        ND <- data.frame(y,xf)
        colnames(ND) <- c( colnames(DATA)[1],"combo")
        
        form <- formula(paste(colnames(DATA)[1],"~combo"))
        M.sat <- lm(form,data=ND)
        linearity.pval <- round( anova(M,M.sat)$"Pr(>F)"[2],digits=4)
        cat(paste("   p-value for overall model",":",linearity.pval,"\n"))
      }  else { cat(paste("   p-value for overall model",":","NA (not enough duplicate rows)\n")) }
      
 
    
    
    #Equal Spread Test
    bptest <- function(formula, varformula = NULL, studentize = TRUE, data = list()) {
      dname <- paste(deparse(substitute(formula)))
      if (!inherits(formula, "formula")) {
        X <- if (is.matrix(formula$x))
          formula$x
        else model.matrix(terms(formula), model.frame(formula))
        y <- if (is.vector(formula$y))
          formula$y
        else model.response(model.frame(formula))
        Z <- if (is.null(varformula))
          X
        else model.matrix(varformula, data = data)
      }
      else {
        mf <- model.frame(formula, data = data)
        y <- model.response(mf)
        X <- model.matrix(formula, data = data)
        Z <- if (is.null(varformula))
          X
        else model.matrix(varformula, data = data)
      }
      if (!(all(c(row.names(X) %in% row.names(Z), row.names(Z) %in%
                    row.names(X))))) {
        allnames <- row.names(X)[row.names(X) %in% row.names(Z)]
        X <- X[allnames, ]
        Z <- Z[allnames, ]
        y <- y[allnames]
      }
      k <- ncol(X)
      n <- nrow(X)
      resi <- lm.fit(X, y)$residuals
      sigma2 <- sum(resi^2)/n
      if (studentize) {
        w <- resi^2 - sigma2
        fv <- lm.fit(Z, w)$fitted
        bp <- n * sum(fv^2)/sum(w^2)
        method <- "studentized Breusch-Pagan test"
      }
      else {
        f <- resi^2/sigma2 - 1
        fv <- lm.fit(Z, f)$fitted
        bp <- 0.5 * sum(fv^2)
        method <- "Breusch-Pagan test"
      }
      names(bp) <- "BP"
      df <- ncol(Z) - 1
      names(df) <- "df"
      RVAL <- list(statistic = bp, parameter = df, method = method,
                   p.value = pchisq(bp, df, lower.tail = FALSE), data.name = dname)
      class(RVAL) <- "htest"
      return(RVAL)
    }

    equal.spread <- bptest(M)$p.value
    
    cat(paste("Equal Spread:  p-value is",round(equal.spread,digits=4),"\n"))
    
    
    #Normality Tests
    if( length(nrow(DATA)) <= 5000 ) {
      normality <- shapiro.test(residuals(M))$p.value } else {
        normality <- ks.test(residuals(M),"pnorm",0,sd(residuals(M)))$p.value
      }
    cat(paste("Normality:  p-value is",round(normality,digits=4),"\n"))
    
    
    
    #Reminders
    
    cat("\nAdvice:  if n<25 then all tests must be passed.\n")
    cat("If n >= 25 and test is failed, refer to diagnostic plot to see if violation is severe\n or is small enough to be ignored.\n")
    
    }
    
    #Add predictor vs residuals plots if requested
    if(extra==TRUE & ncol(X)>1) { 
      par(mar=c(4,4,0.4,0.4))
      
      if(prompt==TRUE) {
      cat (paste("\nPress [enter] to continue to Predictor vs. Residuals plots or q (then Return) to quit (",ncol(X),"plots to show )\n"))
      line <- readline()
      if(line=="q" | line=="Q") { par(mfrow=c(1,1));   par(mar=c(5, 4, 4, 2) + 0.1); cat("Command completed\n"); return(invisible(1)); }
      }
      
      
      if(ncol(X)<=3) { par(mfrow=c(1,ncol(X))) }
      if(ncol(X) > 3 & ncol(X) <=6) { par(mfrow=c(2,3)) }
      if(ncol(X)==4) { par(mfrow=c(2,2))}
      if(ncol(X) <=6) { 
        for (i in 1:ncol(X)) { 
          plot(X[,i],residuals(M),cex=0.5,pch=20,xlab=colnames(X)[i],ylab="Residuals")
          abline(h=0)
          box(which="figure",col="grey",lwd=2)
          
        } }
      if(ncol(X)>6) {
        par(mfrow=c(1,3))
        zz <- 1
        for(i in 1:3) { 
          plot(X[,i],residuals(M),cex=0.5,pch=20,xlab=colnames(X)[zz],ylab="Residuals")
          zz <- zz+1
          abline(h=0)
          box(which="figure",col="grey",lwd=2)
          
        }
        nits <- ceiling(ncol(X)/3)
        for (z in 2:nits) {
          if(prompt==TRUE) {
          cat (paste("\nPress [enter] to continue to Predictor vs. Residuals plots or q (then Return) to quit (",nits-z+1,"sets of plots to go )\n"))
          line <- readline()
          if(line=="q" | line=="Q") { par(mfrow=c(1,1)); cat("Command completed\n"); return(invisible(1)); }
          }
           for(i in 1:3) {
            if(3*(z-1)+i>ncol(X)) { break }
            plot(X[,3*(z-1)+i],residuals(M),cex=0.5,pch=20,xlab=colnames(X)[zz],ylab="Residuals")
            zz <- zz+1
            abline(h=0)
            box(which="figure",col="grey",lwd=2)
            
          } 
          }
          
          
        }
        
        
        
        }
       
    par(mfrow=c(1,1))
    par(mar=c(5, 4, 4, 2) + 0.1)
      }
    
  if(class(M)[1]=="glm"){
    
    if(!is.na(seed)) { set.seed(seed) }
    #Method 1:  comparing observed values to simulated values if model was correct
    actual <- factor(as.numeric(M$model[,1])-1)
    predicted <- factor(ifelse(fitted(M)>0.5,1,0),levels=levels(actual))
    if( sum( table(predicted) %in% 0 == 1 ) )
      {
      cat("Method 1 unavailable (model predicts all cases to have the majority level)\n");      
      } else {
    observed.chi <- chisq.test(actual,predicted)$stat
    correct.chi <- c()
    bads <- 0
    for (i in 1:simulations) { 
      newsample <- factor(rbinom(length(actual),1,fitted(M)),levels=levels(actual))
      if( sum( table(newsample) %in% 0 == 1 ) ) { bads <- bads+1 }
      correct.chi[i] <- chisq.test(newsample,predicted)$stat
    }
    pval.method1 <- length(which(correct.chi>= observed.chi))/simulations
    
    cat("Method 1 (comparing each observation with simulated results given model is correct; not very sensitive)\n")
    cat(paste("  p-value of goodness of fit test is approximately",pval.method1))
    if(bads>0) {
      cat(paste("  Note:  this p-value is not reliable since",bads,"artificial sample had all cases belong to one level\n"))
    }
      }
    #Method 2:  Hosmer-Lemeshow test
    
    T <- data.frame(actual=as.numeric(M$model[,1])-1,predicted=fitted(M))
    T <- T[order(T$predicted),]
    
    
    n.inside <- floor(nrow(T)/n.cats)
    x.cat <- rep(n.inside,n.cats)
    extra <- nrow(T)-n.inside*n.cats
    x.cat <- x.cat + sample(c(rep(1,extra),rep(0,n.cats-extra)))
    x.breaks <- c(1,cumsum(x.cat))
    
    observed.chi <- 0
    expecteds <- rep(0,n.cats)
    observeds <- rep(0,n.cats)
    for (i in 1:n.cats) { 
      O <- sum(T$actual[x.breaks[i]:x.breaks[i+1]])
      E <- sum(T$predicted[x.breaks[i]:x.breaks[i+1]])
      expecteds[i] <- E
      observeds[i] <- O
      observed.chi <- observed.chi+(O-E)^2/E
    }
    
    correct.chi <- c()
    for (z in 1:simulations) { 
      T$actual <- rbinom(length(actual),1,T$predicted)
      D <- 0
      for (i in 1:n.cats) { 
        O <- sum(T$actual[x.breaks[i]:x.breaks[i+1]])
        D <- D + (O-expecteds[i])^2/expecteds[i]
      }
      correct.chi[z] <- D
    }
      
    pval.method2 <- length(which(correct.chi>= observed.chi))/simulations
    
    cat(paste("\n\nMethod 2 (Hosmer-Lemeshow test with",n.cats,"categories; overly sensitive for large sample sizes) \n"))
    cat(paste("  p-value of goodness of fit test is approximately",pval.method2))
    
    
    
      
    }
  
   par(mfrow=c(1,1))
   
}
