all_correlations <-
function(X,type="pearson",interest=NA,sorted="none") {
    if(class(X)!="data.frame") { 
      stop(paste("This function only accepts data frames.",deparse(substitute(X)),"is a",class(X),"\n"))
    }
    if(type!="pearson" & type != "spearman") { 
      stop(paste("type argument must be 'pearson' or 'spearman', you entered",type,"\n")) }
    
    if( !is.na(interest) & length(intersect(interest,names(X)))==0) { 
      stop(paste(deparse(substitute(interest)),"not found in the data frame",deparse(substitute(X)),"\n")) }
    
    RESULTS <- data.frame(var1=names(X),var2=names(X),correlation=rep(0,length(names(X))),pval=rep(rep(0,length(names(X))) ))
    nvar <- ncol(X)
    z<-1
    for (i in 1:(nvar-1)) {
        for (j in (i+1):nvar) {
            if( (class(X[,i])=="numeric" | class(X[,i]) =="integer") & (class(X[,j])=="numeric" | class(X[,j]) =="integer"))      {
                RESULTS[z,] <- list(names(X)[i],names(X)[j],cor.test(X[,i],X[,j],method=type)$est, cor.test(X[,i],X[,j],method=type)$p.val)
                z<-z+1
            }
        }}
    RESULTS <- RESULTS[1:(z-1),]
    if( !is.na(interest) ) {
        selected <- union(which(RESULTS[,1]==interest),which(RESULTS[,2]==interest))
        unselected <- setdiff(1:(z-1),selected)
        RESULTS <- RESULTS[c(selected),]
    }
    if(sorted=="strength") {
        RESULTS <- RESULTS[order(RESULTS$correlation),]
    }
    if(sorted=="significance") {
      RESULTS <- RESULTS[order(RESULTS$pval),]
    }
    rownames(RESULTS)<-NULL
    return(RESULTS)
}
