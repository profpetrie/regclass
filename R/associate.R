associate <-
function(formula,data,permutations=500,seed=NA,plot=TRUE,classic=FALSE,cex.leg=0.7,n.levels=NA,prompt=TRUE,color=TRUE,...) {
  
  #Make sure permutations parameter is a positive integer.  If 0, report classic results.
  if( is.numeric(permutations) == FALSE ) {
    stop(paste("Error:  permutations parameter must be a positive integer.  You entered:",permutations,"\n"))
  }
  if (permutations < 0) {
    stop(paste("Error:  permutations parameter must be a positive integer.  You entered:",permutations,"\n"))
  }
  if (permutations==0) { classic <- TRUE }
  
  #If permutations isn't an integer, round it to be one
  permutations <- round(permutations)
  
  
  #Determine if plots must be made.  If new plot, set to default parameters
  if( (classic==FALSE & permutations <=0) | plot==FALSE ) { plot <- FALSE } else { 
    par(mfrow=c(1,1))
    par(mar=c(5, 4, 4, 2) + 0.1) }
  
  op <- options() 
  
  FORM <- formula(formula)
  variables <- as.character(attr(terms(FORM), "variables"))[-1]
  if(length(variables)==1) { variables <- c(variables,variables) }
  x.label <- variables[2]
  y.label <- variables[1]
  
  
  #Define x and y, checking data first before the existing environment 
  if( class(try(eval(parse(text=variables[2]),envir=data),silent=TRUE))=="try-error" ) {
    x <- eval(parse(text=variables[2]))
  } else { x <- eval(parse(text=variables[2]),envir=data) }
  
  if( class(try(eval(parse(text=variables[1]),envir=data),silent=TRUE))=="try-error" ) {
    y <- eval(parse(text=variables[1]))
  } else { y <- eval(parse(text=variables[1]),envir=data) }
  
  
  
  #Make sure x and y are things we can work with.  If x/y is "AsIs", convert to numerical
  if( length(setdiff(c(class(x)[1],class(y)[1]),c("character","factor","numeric","integer","logical","AsIs","ordered")))>0) {
    stop(paste("Error:  both x and y need to be numeric vectors, character vectors, or factors.\n Currently, x is",class(x),"and y is",class(y),"\n"))
  }
  if(class(x)[1]=="AsIs") { x <- as.numeric(x) }
  if(class(y)[1]=="AsIs") { y <- as.numeric(y) }
  
  
  
  #Make sure there's enough data
  if( length(x) < 2 | length(y) < 2) { stop(paste("Error:  need at least 2 observations to proceed.  Currently, only have",length(x),"\n")) }
  
  
  #If simulations are to be performed, set the random number seed if necessary
  if(!is.na(seed)) { set.seed(seed) }
  
  #If a large number of simulations is requested, make sure it is desired (and default to classic if not)
  if(permutations>5000 & prompt==TRUE) {
    cat(paste("You requested",permutations,"permutations to approximate the p-values.  This may take a while.\n"))
    cat(paste("If you are sure you want to continue, type y then enter/return  \n"))
    line <- readline()
    if( line %in% c("yes","y","Y","Yes","YES","ye","YE","Ye") == FALSE ) { permutations <-0; classic=TRUE }
  }
  
  #If a large number of cases are involved, make sure it is desired (and default to classic if not)
  n.cases <- length( intersect( which(complete.cases(x)),which(complete.cases(y)) ))
  if( n.cases >5000 & prompt==TRUE) {
    cat(paste("You have",n.cases,"observations.  This may take a while.\n"))
    cat(paste("If you are sure you want to continue, type y then enter/return  \n"))
    line <- readline()
    if( line %in% c("yes","y","Y","Yes","YES","ye","YE","Ye") == FALSE ) { permutations <-0; classic=TRUE }
  }
  
  #Define important functions to avoid loading in unnecessary libraries
  
  
  #Brown-Forsythe test (Levene's test) for equal spread of y between levels of x
  levene.test <- function(y,x) {
    level.names <- levels(x)
    for (i in 1:length(level.names)) {
      selected <- which(x==level.names[i])
      y[selected] <- abs(y[selected]-median(y[selected])) 
    }
    return( (anova(lm(y~x))[5])[1,1] )
  }
  
  #White test of constant variance:  this is verbatim from Package bstats version 1.1-11-5
  white.test <- function(lmobj, squares.only = FALSE)
  {
    stopifnot(class(lmobj) == "lm")
    mydata <- lmobj$model
    mydata[, 1] <- lmobj$residual^2
    fml <- lmobj$call$formula
    formula1 <- paste(fml[2], fml[1], fml[3])
    pvs <- attr(lmobj$terms, "term.labels")
    k <- length(pvs)
    n <- length(lmobj$fit)
    for (i in 1:k) {
      tmp <- NULL
      if (substr(pvs[i], 1, 2) == "I(") {
        tmp2 <- substr(pvs[i], 3, nchar(pvs[i]) - 1)
      }
      else {
        tmp2 <- pvs[i]
      }
      for (j in 1:nchar(tmp2)) {
        tmp1 <- substr(tmp2, j, j)
        if (tmp1 == ":")
          tmp <- paste(tmp, "*", sep = "")
        else tmp <- paste(tmp, tmp1, sep = "")
      }
      pvs[i] <- tmp
    }
    formula2 <- paste(fml[2], fml[1])
    for (i in 1:k) {
      if (i > 1)
        formula2 <- paste(formula2, "+", sep = "")
      formula2 <- paste(formula2, "I(", pvs[i], ")", sep = "")
      if (squares.only) {
        formula2 <- paste(formula2, "+I(", pvs[i], "*", pvs[i],
                          ")", sep = "")
      }
      else {
        for (j in i:k) formula2 <- paste(formula2, "+I(",
                                         pvs[i], "*", pvs[j], ")", sep = "")
      }
    }
    method <- ifelse(squares.only, "White test for constant variance, squares only",
                     "White test for constant variance")
    out <- lm(as.formula(formula2), data = mydata)
    if (summary(out)$r.squared == 1) {
      RVAL <- NULL
      warning("Test failed.  Possible reasons:\n\t (1) collinearity, or (2) sample size is not big enough for the White's test.")
    }
    else {
      LM = summary(out)$r.squared * n
      names(LM) <- "White"
      df <- out$rank - 1
      names(df) <- "df"
      RVAL <- list(statistic = LM, parameter = df, method = method,
                   p.value = pchisq(LM, df, lower.tail = FALSE), data.name = NULL)
      class(RVAL) <- "htest"
    }
    return(RVAL)
  }
  
  #Breusch-Pagan Test test of constant variance:  this is verbatim from Package bstats version 1.1-11-5
  bptest <- function (formula, varformula = NULL, studentize = TRUE, data = list())
  {
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
  
  #Shapiro-Wilk test of multivariate normality.  Lifted from package mvnormtest version 0.1-9
  mshapiro.test <- function(U)
  {
    if (!is.matrix(U))
      stop("U[] is not a matrix with number of columns (sample size) between 3 and 5000")
    n <- ncol(U)
    if (n < 3 || n > 5000)
      stop("sample size must be between 3 and 5000")
    rng <- range(U)
    rng <- rng[2] - rng[1]
    if (rng == 0)
      stop("all `U[]' are identical")
    Us <- apply(U, 1, mean)
    R <- U - Us
    M.1 <- solve(R %*% t(R), tol = 1e-18)
    Rmax <- diag(t(R) %*% M.1 %*% R)
    C <- M.1 %*% R[, which.max(Rmax)]
    Z <- t(C) %*% U
    return(shapiro.test(Z))
  }
  
  
  
  
  
  
  
  CX <- class(x)[1]
  CY <- class(y)[1]
  
  
  #Determine what we're looking at:  case=0 both numeric, case=1 y numeric/x categorical, case=2 y categorical/x numeric; case3=both categorical
  case <- sum( c( CX=="character" | CX=="factor" | CX == "logical" | CX == "ordered", CY=="character" | CY=="factor" | CY == "logical" | CY=="ordered" ))
  
  
  
  
  #x and y are both numeric
  if(case==0) { 
    
    #Consider only complete cases
    complete.x <- intersect( which(!is.na(x)), which(is.finite(x)))
    complete.y <- intersect( which(!is.na(y)), which(is.finite(y)))
    complete.cases <- intersect(complete.x,complete.y)
    x <- x[complete.cases]
    y <- y[complete.cases]
    
    
    if(plot==TRUE) {
      xrange <- c(min(x),max(x))
      yrange <- c(min(y),max(y))
      xhist <- hist(x,plot = FALSE)
      yhist <- hist(y,plot = FALSE)
      top <- max(c(xhist$counts, yhist$counts))
      if(classic==TRUE & permutations > 0) { 
        nf <- layout(matrix(c(2,2,2,0,1,1,1,3,1,1,1,3,1,1,1,3,4,4,5,5,4,4,5,5,6,6,7,7,6,6,7,7),ncol=4,byrow=TRUE)) } else { 
          nf <- layout(matrix(c(2,2,2,0,1,1,1,3,1,1,1,3,1,1,1,3,4,4,5,5,4,4,5,5),ncol=4,byrow=TRUE)) }
      par(mar = c(4.4,4.2,1,1))
      plot(x, y, xlim = xrange, ylim = yrange, xlab=x.label, ylab=y.label,...)
      par(mar = c(0,4,1,1))
      barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0); legend("topleft",x.label,cex=cex.leg)
      par(mar = c(4,0,1,1))
      barplot(yhist$counts, axes = FALSE, xlim = c(0, top), space = 0, horiz = TRUE); legend("bottomright",y.label,cex=cex.leg)
      
      #Include QQ plots if classic tests are requested
      if(classic==TRUE) {
        par(mar = c(4.4,4.2,1,1))
        qq(x,x.label)
        box(which="figure",col="grey",lwd=2)
        qq(y,y.label)
        box(which="figure",col="grey",lwd=2)
      }
    }
    
    pearson <- cor(x,y)
    pearson.pvalue <- cor.test(x,y)$p.value
    spearman <- cor(x,y,method="spearman")
    spearman.pvalue <- cor.test(x,y,method="spearman")$p.value
    
    
    cat(paste("Association between",x.label,"(numerical) and ",y.label,"(numerical)\n using",length(x),"complete cases\n"))
    
    
    #Obtain approximate permutation p-values and plot sampling distributions when no association exists.
    #Range of p-values quoted is a 95% confidence interval
    
    if(permutations>0) {
      perm.cor <- rep(0,permutations)
      perm.spear <- rep(0,permutations)
      
      for (i in 1:permutations) {
        x.perm <- x
        y.perm <- sample(y)
        perm.cor[i] <- cor(x.perm,y.perm)
        perm.spear[i] <- cor(x.perm,y.perm,method="spearman") }
      
      if(plot==TRUE) { 
        par(mar=c(4.4,1,1,1))
        xlimit <- max(c(max(abs(perm.cor)),max(abs(perm.spear)),abs(pearson),abs(spearman)))
        ylimit <- max(hist(perm.cor,breaks=20,plot=FALSE)$counts)
        hist(perm.cor,breaks=20,ylab="",xlab="Chance values of Pearson",xlim=c(-xlimit,xlimit),main="",axes=FALSE)
        axis(1)
        abline(v=pearson,lwd=4,col="red")
        abline(v=abs(pearson),lwd=1,lty=2,col="red")
        abline(v=-abs(pearson),lwd=1,lty=2,col="red")
        box(which="figure",lwd=2,col="grey")
        
        
        ylimit <- max(hist(perm.spear,breaks=20,plot=FALSE)$counts)
        hist(perm.spear,breaks=20,axes=FALSE,ylab="",xlab="Chance values of Spearman",xlim=c(-xlimit,xlimit),main="")
        axis(1)
        abline(v=spearman,lwd=4,col="red")  
        abline(v=abs(spearman),lwd=1,lty=2,col="red")
        abline(v=-abs(spearman),lwd=1,lty=2,col="red")
        box(which="figure",lwd=2,col="grey")
        
        
      }
      
      cor.extreme <- length(which(abs(perm.cor)>=abs(pearson)))
      cor.pvalue <- cor.extreme/permutations
      spear.extreme <- length(which(abs(perm.spear)>=abs(spearman)))
      spear.pvalue <- spear.extreme/permutations
      RESULTS <- matrix(c(pearson,spearman,cor.pvalue,spear.pvalue),nrow=2)
      rownames(RESULTS) <- c("Pearson's r","Spearman's rank correlation")
      colnames(RESULTS) <- c("Value","Estimated p-value")
      cat("Permutation procedure:\n")
      print(RESULTS)
      cat(paste("With",permutations,"permutations, we are 95% confident that:\n","the p-value of Pearson's correlation (r) is between",round(binom.test(cor.extreme,permutations)$conf.int[1],digits=3),
                "and",round(binom.test(cor.extreme,permutations)$conf.int[2],digits=3),"\n"))
      cat(paste(" the p-value of Spearman's rank correlation is between",
                round(binom.test(spear.extreme,permutations)$conf.int[1],digits=3),"and",
                round(binom.test(spear.extreme,permutations)$conf.int[2],digits=3),"\n"))
      cat("Note:  If 0.05 is in this range, increase the permutations= argument.\n\n")
    }
    
    if(classic==TRUE) {
      M <- lm(y~x)
      if(length(unique(x))<length(x)) {
        FULL <- lm(y~0+as.factor(x))
        linearity <- anova(FULL,M)$"Pr(>F)"[2]
      } else {linearity <- NA}
      equalspread <- as.numeric(bptest(M)$p.value)
      twomoment <- as.numeric(white.test(M)$p.value)
      
      if( length(x) <= 5000 ) {
        normality.x <- shapiro.test(x)$p.value
        normality.y <- shapiro.test(y)$p.value
        mvnormality <- mshapiro.test(matrix(c(x,y),nrow=2,byrow=TRUE))$p.value
      } else {
        normality.x <- ks.test(x,"pnorm",mean(x),sd(x))$p.value
        normality.y <- ks.test(y,"pnorm",mean(y),sd(y))$p.value
        mvnormality <- NA
      }
      
      
      RESULTS2 <- matrix(
        c(pearson,spearman,
          pearson.pvalue,spearman.pvalue),nrow=2)
      RESULTS2 <- data.frame(RESULTS2)
      rownames(RESULTS2) <- c("Pearson's r","Spearman's rank correlation")            
      colnames(RESULTS2) <- c("Value","Approximate p-value")
      
      assumption.pvalues <- c(linearity,equalspread,twomoment,normality.x,normality.y,mvnormality)
      ASSUMPTIONS <- data.frame(Test=c("Linearity","Equal Spread","Linearity+Spread",paste("Normality",x.label),paste("Normality",y.label),"Bivariate Normality"),pvalue=assumption.pvalues,Pass=assumption.pvalues>.05,row.names=NULL)
      
      cat("Classic approach (must check assumptions):\n")
      print(RESULTS2)
      
      cat("\nTests of assumptions:\n\t-matters for Classic approach\n\t-can be overly strict, so use graphics and judgment if Pass is FALSE\n")
      print(ASSUMPTIONS,row.names=FALSE) }
    
    
    cat("\n\nAdvice: If stream of points is well described by an ellipse, use Pearson's r.\nOtherwise, as long as stream is monotonic, use Spearman's rank correlation\nor try logs, e.g. associate( log10(y)~log10(x) )\n")
    
  }
  
  
  #one of x/y is numeric while the other is categorical
  if(case==1) {
    
    
    #If y is the numeric variable, we are comparing averages for each level of x, otherwise try logistic regression
    
    if( class(y)[1]=="numeric" | class(y)[1]=="integer" ) { subcase <- 1 } else { subcase <- 2}
    
    
    #Tackle the y numerical and x categorical case first
    if(subcase==1)  {
      
      
      #Consider only complete cases
      complete.x <- which(!is.na(x))
      complete.y <- intersect( which(!is.na(y)), which(is.finite(y)))
      complete.cases <- intersect(complete.x,complete.y)
      x <- x[complete.cases]
      y <- y[complete.cases]
      
      #Coerce x to be a factor if it is not, and store the level names
      if(class(x)[1]!="ordered") { x <- factor(x) }
      n.levels <- length(unique(x))
      level.names <- sort(unique(x))
      
      #Make side-by-side boxplots, histograms, QQ plots, and sampling distributions.
      if(plot==TRUE) {
        
        if(classic==TRUE & permutations > 0) {       
          if(n.levels >=3) { 
            ML <- matrix(0,nrow=n.levels,ncol=4)
            fill <- c(rep(1,n.levels),2:(1+2*n.levels),sort(rep(seq(2*n.levels+2,by=1,length=3),floor(n.levels/3))))
          } else {
            ML <- matrix(0,ncol=4,nrow=6)
            fill <- c(rep(1,6),sort(rep(2:5,3)),sort(rep(6:8,2)))          
          }
        }    
        if(classic==TRUE & permutations==0) {
          ML <- matrix(0,nrow=n.levels,ncol=3)
          if(n.levels >=3) { fill <- c(rep(1,n.levels),2:(1+2*n.levels)) } else {
            fill <- c(rep(1,2),2,3,4,5) }
        }
        if(classic==FALSE & permutations>0) {
          if(n.levels >=3) { 
            ML <- matrix(0,nrow=n.levels,ncol=4)
            fill <- c(rep(1,n.levels),2:(1+2*n.levels),sort(rep(seq(2*n.levels+2,by=1,length=3),floor(n.levels/3))))
          } else {
            ML <- matrix(0,ncol=4,nrow=6)
            fill <- c(rep(1,6),sort(rep(2:5,3)),sort(rep(6:8,2)))          
          }
        }
        
        ML[1:length(fill)] <- fill
        ML <- t(ML)
        layout(ML)
        
        par(mar=c(4,4.2,0.4,0.4))
        plot(y~x,xlab="",ylab=y.label,...)
        points(1:n.levels,aggregate(y,by=list(x),mean)[,2],pch=8)
        box(which="figure",lwd=2,col="grey")
        for (i in 1:n.levels) {
          if(i==1) { 
            hist(y[x==as.character(level.names[i])],ylab="Frequency",xlab=y.label,main="") } else {
            hist(y[x==as.character(level.names[i])],xlab=y.label,ylab="",main="") }
          legend("topleft",as.character(level.names[i]),cex=cex.leg)
          box(which="figure",lwd=2,col="grey") 
        }
        
        #If classic tests are desired, make sure to include QQ plots
        for (i in 1:n.levels) {
          qq(y[x==as.character(level.names[i])],y.label,as.character(level.names[i]))
          box(which="figure",lwd=2,col="grey") 
        } 
        
      }
      
      sizes <- as.numeric(table(x))
      means <- as.numeric(tapply(y,x,mean))
      ranks <- as.numeric(tapply(order(y),x,mean))
      medians <- as.numeric(tapply(y,x,median))
      med <- median(y)
      O <- tapply(y,x,function(x)length(which(x>=med)))
      n.tot <- tapply(y,x,length)
      MM <- matrix(c(O,n.tot-O),nrow=length(O),byrow=FALSE)
      
      mean.stat <- as.numeric(unlist(anova(lm(y~x))[4]))[1]
      median.test <- chisq.test(MM)$p.val
      median.test.stat <- chisq.test(MM)$stat
      
      kruskal <- kruskal.test(y~x)$p.val
      rank.stat <- kruskal.test(y~x)$stat
      
      if(permutations>0) {
        
        perm.avg <- rep(0,permutations)
        perm.rank <- rep(0,permutations)
        perm.median <- rep(0,permutations)
        
        for (i in 1:permutations) {
          x.new <- sample(x) 
          perm.avg[i] <- as.numeric(unlist(anova(lm(y~x.new))[4]))[1]
          perm.rank[i] <- kruskal.test(y~x.new)$stat
          O <- tapply(y,x.new,function(x)length(which(x>=med)))
          MM <- matrix(c(O,n.tot-O),nrow=length(O),byrow=FALSE)
          perm.median[i] <- chisq.test(MM)$stat
        }
        
        more.extreme.mean <- length(which(perm.avg>=mean.stat))
        mean.pvalue <- more.extreme.mean/permutations
        more.extreme.rank <- length(which(perm.rank>=rank.stat))
        rank.pvalue <- more.extreme.rank/permutations
        more.extreme.median <- length(which(perm.median>=median.test.stat))
        median.pvalue <- more.extreme.median/permutations
        
        if(plot==TRUE) { 
          xlimit <- max(c(perm.avg,mean.stat))
          ylimit <- max(hist(perm.avg,breaks=50,plot=FALSE)$counts)
          hist(perm.avg,breaks=25,xlab="Chance values of Discrepancy",xlim=c(0,xlimit),main="",ylab="",axes=FALSE)
          axis(1)
          abline(v=mean.stat,lwd=2,col="red")
          legend("center","Averages",cex=cex.leg)
          
          xlimit <- max(c(perm.rank,rank.stat))
          ylimit <- max(hist(perm.rank,breaks=50,plot=FALSE)$counts)
          hist(perm.rank,breaks=25,xlab="Chance values of Discrepancy",xlim=c(min(c(perm.rank,rank.stat)),max(c(perm.rank,rank.stat))),main="",ylab="",axes=FALSE)
          axis(1)
          abline(v=rank.stat,lwd=2,col="red")
          legend("center","Mean Ranks",cex=cex.leg)
          
          
          xlimit <- max(c(perm.median,median.test.stat))
          ylimit <- max(hist(perm.median,breaks=50,plot=FALSE)$counts)
          hist(perm.median,breaks=25,xlab="Chance values of Discrepancy",xlim=c(0,xlimit),main="",ylab="",axes=FALSE)
          axis(1)
          abline(v=median.test.stat,lwd=2,col="red")
          legend("center","Medians",cex=cex.leg)
          
          
        }
        
        RESULTS <- matrix(prettyNum(c(means,mean.stat,mean.pvalue,ranks,rank.stat,rank.pvalue,medians,median.test.stat,median.pvalue),drop0trailing=TRUE,digits=4,format="e"),nrow=3,byrow=TRUE)
        RESULTS <- data.frame(RESULTS)        
        rownames(RESULTS) <- c("Averages (ANOVA)","Mean Ranks (Kruskal)","Medians")
        colnames(RESULTS) <- c(as.character(level.names),"Discrepancy","Estimated p-value")
        
        
      }
      
      if(classic==TRUE) {
        
        anova.pval <- as.numeric(unlist(anova(lm(y~x)))[9])
        RESULTS2 <- matrix(prettyNum(c(
          means,anova.pval,ranks,kruskal,medians,median.test),digits=4,format="e",drop0trailing=TRUE),nrow=3,byrow=TRUE)
        RESULTS2 <- data.frame(RESULTS2)
        rownames(RESULTS2) <- c("Averages (ANOVA)","Mean Ranks (Kruskal)","Medians")
        colnames(RESULTS2) <- c(as.character(level.names),"Approximate p-value")
        var.test <- levene.test(y,x)
        if( max(sizes) <= 5000 ) {
          norm.test <- aggregate(y,list(x),function(x)shapiro.test(x)$p.value)[,2]
        } else { norm.test <- aggregate(y,list(x),function(x)ks.test(x,"pnorm",mean(x),sd(x))$p.value)[,2] }
        
        
        assumption.pvalues <- c(var.test,norm.test)
        ASSUMPTIONS <- data.frame(Test=c("Equal Variance",paste("Normality",level.names) ),pvalue=assumption.pvalues,Pass=assumption.pvalues>.05)
        
        
      }
      
      
      
      cat(paste("Association between",x.label,"(categorical) and ",y.label,"(numerical)\n using",length(x),"complete cases\n"))
      cat("\nSample Sizes")
      print(table(x))
      
      if(permutations>0) {
        cat("\nPermutation procedure:\n")
        print(RESULTS)
        CI <- round(binom.test(more.extreme.mean,permutations)$conf.int,digits=3)
        cat(paste("With",permutations,"permutations, we are 95% confident that\n"))
        cat(paste(" the p-value of ANOVA (means) is between",CI[1],"and",CI[2],"\n"))
        CI <- round(binom.test(more.extreme.rank,permutations)$conf.int,digits=3)
        cat(paste(" the p-value of Kruskal-Wallis (ranks) is between",CI[1],"and",CI[2],"\n"))
        CI <- round(binom.test(more.extreme.median,permutations)$conf.int,digits=3)            
        cat(paste(" the p-value of median test is between",CI[1],"and",CI[2],"\n"))
        cat("Note:  If 0.05 is in a range, change permutations= to a larger number\n\n") 
      }
      
      if(classic==TRUE) {
        cat("Classic approach (must check assumptions):\n")
        print(RESULTS2);
        cat("\nTests of assumptions:\n\t-matters for Classic approach\n\t-can be overly strict, use graphics and judgment if FALSE)\n")
        print(ASSUMPTIONS,row.names=FALSE)
        cat("\nFor classic p-values to be reliable, check the sample sizes below:\n")
        print(table(x))
        cat("  If n < 10, samples must pass test for Normality and Equal Variance\n  If n < 25, distribution must be roughly symmetric and pass test of Equal Variance \n  If n < 100, distribution can't be extremely skewed \n  If n > 100, p-values are reliable\n ")
      }
      
      cat("\n\nAdvice: If it makes sense to compare means (i.e., no extreme outliers and the \ndistributions aren't too skewed), use the ANOVA.  If there there are \nsome obvious extreme outliers but the distributions are roughly symmetric, use \nRank test.  Otherwise, use the Median test or rerun the test using, e.g., log10(y) \ninstead of y\n")
      
      
      
    }
    
    #y categorical and x numerical
    if(subcase==2)  {   
      
      
      if(plot==TRUE) { ML <- matrix(1:3,nrow=3,byrow=TRUE) 
      layout(ML)
      par(mar=c(4,4.2,0.4,0.4)) }
      
      
      #Consider only complete cases
      if(class(y)[1]!="ordered") { y <- factor(y) }
      complete.x <- intersect( which(!is.na(x)), which(is.finite(x)))
      complete.y <- intersect( which(!is.na(y)), which(y!="") )
      complete.cases <- intersect(complete.x,complete.y)
      x <- x[complete.cases]
      xx <- x
      y <- droplevels(y[complete.cases])
      level.names <- levels(y)
      ny.levels <- length(levels(y))
      D.temp <- data.frame(x=x,y=y);  D.temp <- D.temp[order(D.temp$x),]
      n <- nrow(D.temp)
      
      cat("\nSample Sizes")
      print(table(y))
      
      if(plot==TRUE) {
        FOREST <- randomForest(y~x,data=D.temp)
        PROBS <- FOREST$votes
        xf.values <- D.temp$x
      }
      
      if(classic==TRUE) { 
        fit1 <- vglm(y~x,data=D.temp,multinomial)
        fit2 <- vglm(y~1,data=D.temp,multinomial)
        CHI <- -2*logLik(fit2) + 2*logLik(fit1)
        classic.pval <- 1-pchisq(CHI,ny.levels)
        FIT <- data.frame( fitted(fit1) )
        FIT$x <- D.temp$x
      }
      
        if(is.na(n.levels)) { n.cats <- min(6,ceiling(n/length(levels(y))/10)) } else { n.cats <- n.levels } 
        n.inside <- floor(n/n.cats)
        x.cat <- rep(n.inside,n.cats)
        extra <- length(x)-n.inside*n.cats
        x.cat <- x.cat + sample(c(rep(1,extra),rep(0,n.cats-extra)))
        x.breaks <- c(1,cumsum(x.cat))
        new.x <- c()
        xlevel.names <- c()
        for (i in 1:n.cats) { 
          x.name <- paste(prettyNum(D.temp$x[x.breaks[i]],drop0trailing=TRUE,digits=3,format="e"),"to",prettyNum(D.temp$x[x.breaks[i+1]],drop0trailing=TRUE,digits=3,format="e"))
          new.x <- c(new.x,rep(x.name,x.cat[i]))
          xlevel.names <- c(xlevel.names,x.name)
        }
        
        D.temp$x <- ordered(factor(new.x,levels=xlevel.names,ordered=TRUE))
        x <- D.temp$x;  y <- D.temp$y
        nx.levels <- length(levels(x))
        names(D.temp) <- c(x.label,y.label)
        
        cat(paste("\nAnalysis proceeds by grouping",x.label,"into",n.cats,"categories:\n"))
        CN <- paste(levels(x)[1],levels(x)[2],sep=", ")
        if(n.cats>2) { 
        for (i in 3:n.cats) { CN <- paste(CN,levels(x)[i],sep=", ")  } }
        cat(paste(CN,"\n\n"))
        
        #Make sure there's enough levels
        if( nx.levels < 2 | ny.levels < 2) { 
          stop(paste("Error:  need at least 2 levels to proceed.  x has",nx.levels,"and y has",ny.levels)) }    
        
        xlevel.names <- levels(x);  ylevel.names <- levels(y)
        
        if(permutations>0) {
          
        test.pval <- chisq.test(x,y,simulate.p.value=TRUE,B=permutations)$p.value
        
      }
      
      if (color==TRUE) {
        if(ny.levels>8) { COLORS <- rainbow(2*ny.levels-1)[seq(1,by=2,length=ny.levels)] } else { 
          COLORS <- 1:ny.levels }
      } else {
        COLORS <- grey(seq(.1,.7,length=ny.levels));   }
        
      
      
      if(plot==TRUE) {
        plot(0,0,col="white",xlim=c(0,1.2),ylim=c(-.05,1),xlab=x.label,ylab=y.label,main="",axes=FALSE)
        O<-matrix(table(x,y),nrow=nx.levels,ncol=ny.levels)
        marginal.y <- apply(O,2,sum)/n
        break.y <- c(0,cumsum(marginal.y))
        for (i in 1:ny.levels) {
          rect(1.01,break.y[i],1.11,break.y[i+1],col=COLORS[i])
          text(1.12,(break.y[i+1]+break.y[i])/2,ylevel.names[i],srt=0,pos=4)   }
        
        marginal.x <- apply(O,1,sum)/n
        break.x <- c(0,cumsum(marginal.x))
        for(i in 1:nx.levels) {
          text( (break.x[i+1]+break.x[i])/2,-.05,xlevel.names[i],cex=0.8)
          marginal.y <- O[i,]/sum(O[i,])
          break.y <- c(0,cumsum(marginal.y))
          for (j in 1:ny.levels) { rect(break.x[i],break.y[j],break.x[i+1],break.y[j+1],col=COLORS[j]) }
        }
        
        
        plot(0,0,col="white",xlim=c(0,1.2),ylim=c(-.05,1),xlab=x.label,ylab="Probability",main="",axes=FALSE)
        axis(2)
        abline(h=0)
        puts <- c()
        for(i in 1:nx.levels) { puts <- c(puts,(break.x[i+1]+break.x[i])/2) }
        for(i in 1:nx.levels) { text( puts[i],-.05,xlevel.names[i],cex=0.8 ) }
        
        for(i in 1:ny.levels) {
          lines(puts,O[,i]/apply(O,1,sum),col=COLORS[i] )
        }
        legend("right",level.names,col=COLORS,lty=1,cex=cex.leg) 
        
        plot(xx,xx,ylim=c(0,1),xlab=x.label,ylab="Probability",col="white")
        for (i in 1:ny.levels) { 
              points(xf.values,PROBS[,i],pch=20,cex=0.4,col=COLORS[i])
            }
        if(classic==TRUE) {
          for (i in 1:ny.levels) { 
            lines(FIT$x,FIT[,i],col=COLORS[i]) 
          }
        }
        legend("right",level.names,col=COLORS,lty=1,cex=cex.leg)  
        
      }
      
    
      RESULTS <- c()
      
      if(classic==TRUE & permutations >0) { 
        RESULTS <- matrix(c(test.pval,classic.pval),nrow=2,ncol=1)
        RESULTS <- data.frame(RESULTS)
        rownames(RESULTS) <- c("Splitting into Categories","Using Logistic Curve")
        colnames(RESULTS) <- c("Estimated p-value") }
      if(classic==TRUE & permutations<=0) { 
        RESULTS <- matrix(classic.pval,nrow=1,ncol=1)
        RESULTS <- data.frame(RESULTS)
        rownames(RESULTS) <- c("Using Logistic Curve")
        colnames(RESULTS) <- c("Estimated p-value")  }
      if(classic==FALSE & permutations>0) { 
        RESULTS <- matrix(c(test.pval),nrow=1,ncol=1)
        RESULTS <- data.frame(RESULTS)
        rownames(RESULTS) <- c("Splitting into Categories")
        colnames(RESULTS) <- c("Estimated p-value") }
      
      
      print(RESULTS)
      
      if(permutations>0) { 
        more.extreme <- round(permutations*test.pval)
        CI <- binom.test(more.extreme,permutations)$conf.int
        cat(paste("\nWith",permutations,"permutations, we are 95% confident that the p-value (obtained by splitting\n",x.label,"into categories) is between",round(CI[1],digits=3),"and",round(CI[2],digits=3),"\n"))
        cat(paste("Note: If 0.05 is in this range, change permutations= to a larger number\n")) }
      if(classic==TRUE) { 
        cat("\n\nNote:  p-value using the logistic curve requires that the curve provides an\n adequate description of how the probabilities change.\n") }
      
      
    }
    
    
    
  }
  
  
  #x and y are both categorical
  
  
  if(case==2) {
    
    complete.x <- which(!is.na(x))
    complete.y <- which(!is.na(y))
    complete.cases <- intersect(complete.x,complete.y)
    x <- x[complete.cases]
    y <- y[complete.cases]
    
    if(class(x)[1]!="ordered") { x <- factor(x) }
    if(class(y)[1]!="ordered") { y <- factor(y) }
    
    n <- length(x)
    nx.levels <- length(unique(x))
    ny.levels <- length(unique(y))
    
    #Make sure there's enough levels
    if( nx.levels < 2 | ny.levels < 2) { 
      stop(paste("Error:  need at least 2 levels to proceed.  x has",nx.levels,"and y has",ny.levels)) }
    
    if(plot==TRUE) { ML <- matrix(1,ncol=1) }
    if(plot==TRUE & permutations > 0) { ML <- matrix(1:2,ncol=1) }
    if(plot==TRUE) {  layout(ML);       par(mar = c(4,4.2,1,1))  }
        
    xlevel.names <- levels(x)
    ylevel.names <- levels(y)
    
    CONT.TAB <- table(x,y)
    CONT.TAB <- addmargins(CONT.TAB)
    rownames(CONT.TAB)[nx.levels+1] <- "Total"
    colnames(CONT.TAB)[ny.levels+1] <- "Total"
    
    O<-matrix(table(x,y),nrow=nx.levels,ncol=ny.levels)
    E<-(apply(O,1,sum) %o% apply(O,2,sum))/n
    
    chisq.stat <- as.numeric( chisq.test(x,y)$stat )
    chisq.pval <- chisq.test(x,y)$p.value
    
    chisq.vals <- rep(0,permutations)
    if(permutations>0) {
      for (i in 1:permutations) { chisq.vals[i] <- as.numeric(chisq.test(sample(x),y)$stat) }
      more.extreme <- length(which(chisq.vals>=chisq.stat))
      chisq.sim <- more.extreme/permutations
    }
    
    
    TABLE <- table(x,y)
    for (i in 1:nx.levels) {
      TABLE[i,] <- TABLE[i,]/sum(TABLE[i,]) }
    TABLE <- rbind(TABLE,table(y)/length(y))
    rownames(TABLE)[dim(TABLE)[1]] <- "Marginal"
    
    if(plot==TRUE) {
      par(mar=c(4,4.2,0.4,2))
      plot(0,0,col="white",xlim=c(0,1.3),ylim=c(-.05,1),xlab=x.label,ylab=y.label,main="",axes=FALSE)
      axis(1,at=seq(0,1,.05))
      axis(2,at=seq(0,1,.05))
      
      if (color==TRUE) {
        if(ny.levels>8) { COLORS <- rainbow(2*ny.levels-1)[seq(1,by=2,length=ny.levels)] } else { 
          COLORS <- 1:ny.levels }
      } else {
        COLORS <- grey(seq(.1,.7,length=ny.levels));   }
      
      marginal.y <- apply(O,2,sum)/n
      break.y <- c(0,cumsum(marginal.y))
      for (i in 1:ny.levels) {
        rect(1.01,break.y[i],1.11,break.y[i+1],col=COLORS[i])
        text(1.10,(break.y[i+1]+break.y[i])/2,ylevel.names[i],srt=0,pos=4,cex=1)   }
      
      marginal.x <- apply(O,1,sum)/n
      break.x <- c(0,cumsum(marginal.x))
      for(i in 1:nx.levels) {
        text( (break.x[i+1]+break.x[i])/2,-.05,xlevel.names[i] )
        marginal.y <- O[i,]/sum(O[i,])
        break.y <- c(0,cumsum(marginal.y))
        for (j in 1:ny.levels) { rect(break.x[i],break.y[j],break.x[i+1],break.y[j+1],col=COLORS[j]) }
      }
      if(permutations>0) {
        hist(chisq.vals,xlab="Chance value of Discrepancy",ylab="",main="",breaks=25,axes=FALSE,xlim=c(0,max(c(chisq.vals,chisq.stat))))
        axis(1)
        abline(v=chisq.stat,col="red",lwd=2)
      }
    }
    
    
    
    
    
    rownames(E)<-xlevel.names
    colnames(E)<-ylevel.names
    
    ASSUMPTIONS <- round(E,digits=1)
    
    
    cat(paste("Association between",x.label,"(categorical) and ",y.label,"(categorical):\n\n using",length(x),"complete cases\n"))
    cat("Contingency table:\n")
    print(CONT.TAB)
    cat("\n Table of Expected Counts:\n")
    print(ASSUMPTIONS,row.names=FALSE)
    cat(paste("\nConditional distributions of y (",y.label,") for each level of x (",x.label,"):\nIf there is no association, these should look similar to each other and\n similar to the marginal distribution of y\n",sep=""))
    print(TABLE)
    
    if(permutations>0) {
      RESULTS <- matrix(c(chisq.stat,chisq.sim),nrow=1,ncol=2)
      RESULTS <- data.frame(RESULTS) 
      colnames(RESULTS) <- c("Discrepancy","Estimated p-value") 
      rownames(RESULTS) <- " "
      cat("\nPermutation procedure:\n")
      print(RESULTS)
      CI <- binom.test(more.extreme,permutations)$conf.int
      cat(paste("With",permutations,"permutations, we are 95% confident that:\n","the p-value is between",round(CI[1],digits=3),"and",round(CI[2],digits=3),"\nIf 0.05 is in this range, change permutations= to a larger number\n")) }
    
    
    
    if(classic==TRUE) {
      RESULTS <- matrix(c(chisq.stat,chisq.pval),nrow=1,ncol=2)
      RESULTS <- data.frame(RESULTS)
      
      rownames(RESULTS) <- " "
      colnames(RESULTS) <- c("Discrepancy","Estimated p-value") 
      cat("\nClassic approach (must check assumptions):\n")
      print(RESULTS)
      cat("\nReminder:  Classic approach requires most expected counts >= 5.  No requirements\n for permutation approach.\n")
      
    }
    
    
  }
  
  
  
  layout(1)
  par(mar=c(5, 4, 4, 2) + 0.1)
  options(op)
  
}
