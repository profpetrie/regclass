build.model <-
function(form,data,type="predictive",Kfold=5,repeats=10,prompt=TRUE,seed=NA,holdout=NA,...) {
  
  FORM <- as.formula(form)
  form <- as.character(FORM)
  
  yname <- form[2]
  
  if(!(yname %in% names(data))) { stop(paste("Error:",yname,"not found in",eval(parse(data)),"\n")) }
  
  y.pos <- which(names(data) %in% yname)
  if(nlevels(data[,y.pos])>2 ) { stop(cat(paste("Error:  y has more than 2 levels\n"))) }
  if(nlevels(data[,y.pos])==2) { model.family <- "binomial" } else { model.family <- "gaussian" }

  if(model.family=="gaussian") { M <- lm(FORM,data=data) } 
  if(model.family=="binomial") { M <- glm(FORM,data=data,family="binomial") }
  
  Xy <- data.frame(model.matrix(M)[,-1],data[,y.pos])
  if(model.family=="binomial") { Xy[,ncol(Xy)] <- as.numeric(Xy[,ncol(Xy)])-1 }
  names(Xy)[ncol(Xy)] <- yname
  
  
  if(!missing(holdout)) { 
    if(model.family=="gaussian") { M.h <- lm(FORM,data=holdout) }
    if(model.family=="binomial") { M.h <- glm(FORM,data=holdout,family="binomial") }
    Xy.h <- data.frame(model.matrix(M.h)[,-1],holdout[,y.pos])
    if(model.family=="binomial") { Xy.h[,ncol(Xy.h)] <- as.numeric(Xy.h[,ncol(Xy.h)])-1  }
    names(Xy.h)[ncol(Xy.h)] <- yname
    }
    
  
  if(prompt==TRUE) { 
  if( (type=="predictive" & 2^(ncol(Xy)-1) > 5000) | (type=="descriptive" & 2^(ncol(Xy)-1) > 50000) ) {
      
    cat(paste("Warning:  this may take a long time time.\n"))
    cat(paste("There are",nrow(Xy),"rows in the data and",ncol(Xy)-1,"potential predictors\n"))
    cat(paste("There are up to",formatC(2^(ncol(Xy)-1),digits=4,format="e"),"possible models to consider.\n"))
    cat(paste("Try cutting down on the number of interactions (if present).\n"))
    cat(paste("Enter 'y' to continue (you may have to force-quit R to end to task otherwise if it takes too long)\n"))
    desire <- readline()
    if(desire!="y") { stop("Aborted due to user termination\n") } else { cat("Continuing...  (you may have to force quit R if this does not termintate)\n") }
  }
  
  if( nrow(Xy) > 5000  ) {
    
    cat(paste("Warning:  there are",nrow(Xy),"rows in the data and",ncol(Xy)-1,"potential predictors\n"))
    cat(paste("Enter 'y' to continue (you may have to force-quit R to end to task otherwise if it takes too long)\n"))
    desire <- readline()
    if(desire!="y") { stop("Aborted due to user termination\n") } else { cat("Continuing...\n") }
  }
  }
  
  if(!is.na(seed)) { set.seed(seed) }
  
  if(type=="predictive") {

    if(model.family=="gaussian") { MODELS <- bestglm(Xy,family=gaussian,IC="CV",CVArgs=list(Method="HTF",K=Kfold,REP=repeats),...) }
    if(model.family=="binomial") { MODELS <- bestglm(Xy,family=binomial,IC="CV",CVArgs=list(Method="HTF",K=Kfold,REP=repeats),...) }

    RESULTS <- MODELS$Subsets
    USED <- RESULTS[,1:ncol(Xy)]
    USED <- USED[,-1]
    
    
    if(model.family=="binomial") {
        mc.train <- length(which(Xy[,ncol(Xy)]==names( which.min(table(Xy[,ncol(Xy)])  )) ))/nrow(Xy)
        x.temp <- Xy[,which(USED[2,]==1)]
        M.temp <- glm(Xy[,ncol(Xy)]~x.temp,family="binomial") 
        Xy.temp <- data.frame(x.temp=Xy[,which(USED[2,]==1)])
        pred.temp <- ifelse( predict(M.temp,type="response",newdata=Xy.temp) < 0.5,0,1)
        mc.train <- c( mc.train, sum( pred.temp != Xy[,ncol(Xy)] ) / nrow(Xy) ) 
        if( nrow(USED) > 2 ){ 
          for (i in 3:nrow(USED)){
            M.temp <- glm(Xy[,ncol(Xy)]~.,data=Xy[,which(USED[i,]==1)],family="binomial" )
            pred.temp <- ifelse( predict(M.temp,type="response",newdata=Xy) < 0.5,0,1)
            mc.train <- c(mc.train, sum( pred.temp != Xy[,ncol(Xy)] )/nrow(Xy) )
          } }
      }
      
    if(!missing(holdout)) {
      
      if(model.family=="gaussian") { 
        RMSE.holdout <- sqrt( mean( (Xy.h[,ncol(Xy.h)] - mean(Xy[,ncol(Xy)]) )^2 ) )
        x.temp <- Xy[,which(USED[2,]==1)]
        M.temp <- lm(Xy[,ncol(Xy)]~x.temp )
        Xy.temp <- data.frame(x.temp=Xy.h[,which(USED[2,]==1)])
        
        RMSE.holdout <- c(RMSE.holdout, sqrt( mean( (Xy.h[,ncol(Xy.h)] - predict(M.temp,newdata=Xy.temp) )^2 ) ))
        if( nrow(USED) > 2 ){ 
          for (i in 3:nrow(USED)) {
            M.temp <- lm(Xy[,ncol(Xy)]~.,data=Xy[,which(USED[i,]==1)] )
            RMSE.holdout <- c(RMSE.holdout, sqrt( mean( (Xy.h[,ncol(Xy.h)] - predict(M.temp,newdata=Xy.h) )^2 ) ))
          } }
        }
      
      if(model.family=="binomial") { 
        mc.holdout <- length(which(Xy.h[,ncol(Xy.h)]==names( which.min(table(Xy[,ncol(Xy)])  )) ))/nrow(Xy.h)
        x.temp <- Xy[,which(USED[2,]==1)]
        M.temp <- glm(Xy[,ncol(Xy)]~x.temp,family="binomial") 
        Xy.temp <- data.frame(x.temp=Xy.h[,which(USED[2,]==1)])
        pred.temp <- ifelse( predict(M.temp,type="response",newdata=Xy.temp) < 0.5,0,1)
        mc.holdout <- c( mc.holdout, sum( pred.temp != Xy.h[,ncol(Xy.h)] ) / nrow(Xy.h) ) 
        if( nrow(USED) > 2 ){ 
          for (i in 3:nrow(USED)){
            M.temp <- glm(Xy[,ncol(Xy)]~.,data=Xy[,which(USED[i,]==1)],family="binomial" )
            pred.temp <- ifelse( predict(M.temp,type="response",newdata=Xy.h) < 0.5,0,1)
            mc.holdout <- c(mc.holdout, sum( pred.temp != Xy.h[,ncol(Xy.h)] )/nrow(Xy.h) )
          } }
        RMSE.holdout <- mc.holdout 
      }
      
    }   
    
    

    PRED <- list()
    for (i in 2:(nrow(RESULTS))) {
      PRED[[i-1]] <- (names(RESULTS)[RESULTS[i,]==TRUE])[-1]
    }
    
    CVs <- RESULTS$CV
    SDs <- RESULTS$sdCV
    
    CVtable <- matrix(c(0:(nrow(RESULTS)-1),CVs,SD=SDs),ncol=3)
    if(model.family=="gaussian") { colnames(CVtable) <- c("k","SquaredEstGenErr","SD") }
    if(model.family=="binomial") { 
      CVtable <- cbind(CVtable,mc.train)
      colnames(CVtable) <- c("k","EstGenErr","SD","EstGenMisclass") 
    }
    
    
    rownames(CVtable)<-rep(" ",nrow(CVtable))
    selected.model <- which.min(CVs)[1]
    rownames(CVtable)[selected.model] <- "*"
    selected.vars <- names(RESULTS)[which(RESULTS[selected.model,]==TRUE)]
    selected.vars <- setdiff(selected.vars,c("(Intercept)","Intercept"))
    cat("\n")
    cat(paste("Model with lowest estimated generalization error has:\n"))
    cat(paste(selected.vars))
    bf <- paste(yname,"~",
                paste( unlist( lapply( strsplit(selected.vars,split=".",fixed=TRUE), function(x)paste(x,collapse = ":")) ),collapse="+"))
    cat(paste("\n Closest Formula:",bf))
    cat("\n")
    selected.model2 <- min(which(CVs<=min(CVs)+SDs[which.min(CVs)[1]]))
    if(selected.model==selected.model2) { rownames(CVtable)[selected.model2] <- "+*"} else {
    rownames(CVtable)[selected.model2] <- "+" }
    
    selected.vars <- names(RESULTS)[which(RESULTS[selected.model2,]==TRUE)]
    selected.vars <- setdiff(selected.vars,c("(Intercept)","Intercept"))
    cat("\n")
    cat(paste("Model selected with one standard deviation rule has:\n"))
    cat(paste(selected.vars))
    bf <- paste(yname,"~",
                paste( unlist( lapply( strsplit(selected.vars,split=".",fixed=TRUE), function(x)paste(x,collapse = ":")) ),collapse="+"))
    cat(paste("\n Closest Formula:",bf))
    cat("\n")
    cat("\n")
    

    lowers <- CVs-SDs
    uppers <- CVs+SDs
    pchs <- rep(20,nrow(MODELS$Subsets))
    pchs[selected.model] <- 8
    pchs[selected.model2] <- 10
    cols <- rep("black",nrow(MODELS$Subsets))
    cols[selected.model] <- "blue"
    cols[selected.model2] <- "blue"
    cexs <- rep(1,nrow(MODELS$Subsets))
    cexs[selected.model] <- 2.2
    cexs[selected.model2] <- 2.2
    
    if(!missing(holdout)) { par(mfrow=c(1,2)) } else { par(mfrow=c(1,1)) }
    
    if(model.family=="gaussian") {
      
      plot(0:(nrow(MODELS$Subsets)-1),sqrt(CVs),xlab="# Vars in Model",ylab="Estimated Gen. Error",pch=pchs,cex=cexs,col=cols,ylim=c(min(sqrt(lowers[-1])),max(sqrt(uppers[-1]))))
      lines(0:(nrow(MODELS$Subsets)-1),sqrt(CVs),col="red")
      for(j in 1:length(CVs)) { lines( c(j-1,j-1),c(sqrt(lowers[j]),sqrt(uppers[j])) ) }
      abline(h=sqrt( min(CVs) ) )
      abline(h= sqrt(  uppers[which.min(CVs)] ),lty=2 )
      points(0:(nrow(MODELS$Subsets)-1),sqrt(CVs),pch=pchs,cex=cexs,col=cols)
      
      EstGenErr <- sqrt(CVtable[,2])
      CVtable <- cbind(CVtable,EstGenErr)
      
      if(!missing(holdout)) { 
        plot(0:(nrow(MODELS$Subsets)-1),RMSE.holdout,pch=pchs,col=cols,cex=cexs,
             ylim=c( min(sqrt(CVs),RMSE.holdout),max(RMSE.holdout)),
             xlab="# Vars in Model",ylab="RMSE Holdout" )
        lines(0:(nrow(MODELS$Subsets)-1),RMSE.holdout )
        points(0:(nrow(MODELS$Subsets)-1),EstGenErr,col="red",pch=20,cex=0.5 )
        legend("topright",c("Holdout","Estimated"),pch=20,col=c("black","red"),cex=0.5)
        abline(h=sqrt( min(CVs) ) )
        abline(h= sqrt(  uppers[which.min(CVs)] ),lty=2 )
        
        CVtable <- cbind(CVtable,RMSE.holdout)
        colnames(CVtable)[ncol(CVtable)] <- "RMSEholdout" 
        
        holdout.predictions <- predict(MODELS$BestModel,newdata=Xy.h)
        par(mfrow=c(1,1))
        return(list(bestformula=as.formula(bf),bestmodel=MODELS$BestModel,predictors=PRED,CVtable=CVtable,predictions=holdout.predictions))  
      }
      return(list(bestformula=as.formula(bf),bestmodel=MODELS$BestModel,predictors=PRED,CVtable=CVtable))  
      
    }
    
    if(model.family=="binomial") {
      plot(0:(nrow(MODELS$Subsets)-1),CVs,xlab="# Vars in Model",ylab="Est. Generalization Error",pch=pchs,cex=cexs,col=cols,ylim=c(min((lowers)),max((uppers))))
      lines(0:(nrow(MODELS$Subsets)-1),(CVs),col="red")
      for(j in 1:length(CVs)) { lines( c(j-1,j-1),c((lowers[j]),(uppers[j])) ) }
      abline(h=( min(CVs) ) )
      abline(h= (  uppers[which.min(CVs)] ),lty=2 )
      points(0:(nrow(MODELS$Subsets)-1),(CVs),pch=pchs,cex=cexs,col=cols)
      
      if(!missing(holdout)) { 
        CVtable <- cbind(CVtable,RMSE.holdout)
        colnames(CVtable)[ncol(CVtable)] <- "Misclassholdout" 
        holdout.predictions <- predict(MODELS$BestModel,newdata=Xy.h,type="response")
        holdout.predictions <- ifelse(holdout.predictions>0.5,levels( data[,y.pos])[2],levels( data[,y.pos])[1] )
        plot(0:(nrow(MODELS$Subsets)-1),RMSE.holdout,pch=pchs,col=cols,cex=cexs,
             ylim=c( min((mc.train),min(RMSE.holdout)),max(RMSE.holdout)),
             xlab="# Vars in Model",ylab="Misclass Rate Holdout" )
        lines(0:(nrow(MODELS$Subsets)-1),RMSE.holdout )
        points(0:(nrow(MODELS$Subsets)-1),mc.train,pch=pchs,col="red",cex=0.5*cexs )
        legend("topright",c("Holdout","Estimated"),col=c("black","red"),pch=20,cex=0.5)
        par(mfrow=c(1,1))
        return(list(bestformula=as.formula(bf),bestmodel=MODELS$BestModel,predictors=PRED,CVtable=CVtable,predictions=holdout.predictions))  
      }
      return(list(bestformula=as.formula(bf),bestmodel=MODELS$BestModel,predictors=PRED,CVtable=CVtable))  
      
    }
    
 
  }
  
  
  
  
  
  if(type=="descriptive") {
    if(model.family=="binomial") { MODELS <- bestglm(Xy,family=binomial,IC="AIC",...) }
    if(model.family=="gaussian") { MODELS <- bestglm(Xy,family=gaussian,IC="AIC",...) }
    RESULTS <- MODELS$Subsets
    RESULTS$AIC <- RESULTS$AIC + 2
    PRED <- list()
    for (i in 2:(nrow(RESULTS))) {
      PRED[[i-1]] <- (names(RESULTS)[RESULTS[i,]==TRUE])[-1]
    }
    
    plot(0:(nrow(MODELS$Subsets)-1),RESULTS[,"AIC"],xlab="# Vars in Model",ylab="AIC",pch=20,cex=2,col="red")
    lines(0:(nrow(MODELS$Subsets)-1),RESULTS[,"AIC"],col="red")
    abline(h=min(RESULTS$AIC)+2,lty=2)
    abline(v= which.min(RESULTS$AIC)[1])
    selected.model <- which.min(RESULTS$AIC)[1]
    selected.vars <- names(RESULTS)[which(RESULTS[selected.model,]==TRUE)]
    selected.vars <- setdiff(selected.vars,c( "(Intercept)","Intercept") )
    
    cat("\n")
    cat(paste("Model with lowest AIC uses:\n"))
    cat(paste(selected.vars))
    bf <- paste(yname,"~",
                paste( unlist( lapply( strsplit(selected.vars,split=".",fixed=TRUE), function(x)paste(x,collapse = ":")) ),collapse="+"))
    cat(paste("\n Closest Formula:",bf))
    cat("\n")
    
    
    
    selected.model <- min(which(RESULTS$AIC <= min(RESULTS$AIC)+2 ) )
    abline(v= selected.model,lty=2)
    selected.vars <- names(RESULTS)[which(RESULTS[selected.model,]==TRUE)]
    selected.vars <- setdiff(selected.vars,c( "(Intercept)","Intercept") )
    cat("\n")
    cat(paste("Simplest model with AIC within 2 of lowest uses:\n"))
    cat(paste(selected.vars))
    bf <- paste(yname,"~",
                paste( unlist( lapply( strsplit(selected.vars,split=".",fixed=TRUE), function(x)paste(x,collapse = ":")) ),collapse="+"))
    cat(paste("\n Closest Formula:",bf))
    cat("\n")
    cat("\n")
    
    
    AICs <- RESULTS$AIC
    AICtable <- matrix(c(0:(nrow(RESULTS)-1),AIC=AICs),ncol=2)
    colnames(AICtable) <- c("k","AIC")
    rownames(AICtable)<-rep(" ",nrow(AICtable))
    selected.model <- which.min(AICs)[1]
    rownames(AICtable)[selected.model] <- "*"
    selected.model2 <- min(which(AICs<=min(AICs)+2))
    if(selected.model2==selected.model) {rownames(AICtable)[selected.model2] <- "+*" } else {
    rownames(AICtable)[selected.model2] <- "+"}
    
    
    return(list(bestformula=as.formula(bf),bestmodel=MODELS$BestModel,predictors=PRED,AICtable=AICtable ))  
    
    
  }

}
