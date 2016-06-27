see.models <-
function(ALLMODELS,report=0,aicc=FALSE,reltomin=FALSE) {
    if(class(ALLMODELS)!="regsubsets") { cat("Input must be the saved results of performing regsubsets()\n") }
    RESULTS <- summary(ALLMODELS)$which
    n <- ALLMODELS$d[1]
    k <- apply(summary(ALLMODELS)$which,1,sum);
    aic <- log(summary(ALLMODELS)$rss/n)*n + 2*k
    if(aicc) { aic <- aic + 2*k*(k+1)/(n-k-1)}
    best.model <- which.min(aic)
    if(reltomin==TRUE) { aic <- aic-min(aic) }
    terms <- c()
    for (i in 1:length(aic)) {
        terms[i] <- ""
        vars <- colnames(RESULTS)[1+which(RESULTS[i,-1]==TRUE)]
        for (j in 1:length(vars)) { terms[i] <- paste(terms[i],vars[j]   ) }
    }
    results <- data.frame(AIC=round(aic,digits=1),NumVars=k-1,Terms=terms)
    if(aicc) { names(results)[1] <- "AICc" }
    results <- results[order(results$AIC),]
    if(report==0) { cat("Reporting all models with AIC within 4 of the lowest value\n") }
    if(report>0) { cat(paste("Reporting the",report,"models with the lowest AICs\n"))}
    if(report==0) { report <- max(which(results$AIC <=4+min(results$AIC))) }
    results <- results[1:report,]
    rownames(results)<-NULL
    return(results)
}
