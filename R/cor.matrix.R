cor.matrix <-
function(X,type="pearson") {
    if(type!="pearson" & type != "spearman") { stop(paste(cat("type must be 'pearson' or 'spearman', you entered",type,"\n"))) }
    selected <- c()
    for (i in 1:ncol(X)) { if( class(X[,i])=="numeric" | class(X[,i]) =="integer" ) { selected <- c(selected,i) } }
    COR <- matrix(0,nrow=length(selected),ncol=length(selected))
    for (i in selected) {
        for (j in selected) {
            COR[which(selected==i),which(selected==j)] <- round(cor(X[,i],X[,j],method=type),digits=3) }}
    rownames(COR) <- colnames(COR) <- names(X)[selected]
    return(COR)
}
