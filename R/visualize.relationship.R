visualize.relationship <- function(TREE,interest,on,smooth=TRUE,marginal=TRUE,nplots=5,seed=NA,pos="topright") {
  if(!is.na(seed)) { set.seed(seed) } 
  if(missing(interest)) { stop("Must specify x variable in the interest= argument") }
  if(missing(on)) { stop("Must give name of a dataframe in the on= argument")}
  selected.column <- which(names(on)==interest)
  if (length(selected.column)==0) { stop(paste(interest,"not found in dataframe",on)) }
  
  y.name <- unlist(strsplit(as.character( formula(TREE)[2] ),"\\(" ) )
  y.val <- on[,which(names(on)==y.name)]
  if(class(y.val)=="factor") { y.values <- as.numeric(y.val) - 1 } else { y.values <- y.val }
  x.val <- on[,selected.column]
  x.u <- sort( unique(x.val) )
  
  n <- c()
  for (x in x.u ) {
    selected <- which(x.val==x)
    n <- c(n,length(selected))
  }
  
  if(marginal==TRUE) {
    if(class(y.val)=="factor") { y.pred <- predict(TREE,newdata=on,type="prob")[,1] } else {
      y.pred <- predict(TREE,newdata=on) }
    
    y.avg <- c()
    
    for (x in x.u ) {
      selected <- which(x.val==x)
      y.avg <- c(y.avg,mean(y.pred[selected]))
    }
    plot(y.values~x.val,xlab=interest,ylab=paste("Predicted",y.name),pch=20,cex=0.7 )
    points(y.avg~x.u,pch=20,cex=0.3,col="red" )
    #SS <- smooth.spline(x.u, y.avg, w = n,df=smooth*length(x.u)); lines(SS)
    if(smooth==TRUE) {
      L <- loess(y.avg~x.u, weights = n)
      xl <- seq(min(x.u),max(x.u), (max(x.u) - min(x.u))/1000)
      lines(xl, predict(L,xl), col='red', lwd=4) 
    } else {
      lines(y.avg~x.u)
    }
    legend(pos,c("Observed","Predicted"),col=c("black","red"),pch=20,cex=0.5)
  }
  if(marginal==FALSE) {
    plot(y.values~x.val,xlab=interest,ylab=paste("Predicted",y.name),pch=20,cex=0.7 )
    
    cols <- rainbow(1.2*nplots)[1:nplots]
    for (i in 1:nplots) {
      on.expanded <- on[rep(row.names(on)[sample(1:nrow(on),1)],length(x.u)), ]
      row.names(on.expanded) <- NULL
      on.expanded[,selected.column] <- x.u
      if(class(y.val)=="factor") { py <- predict(TREE,newdata=on.expanded,type="prob")[,1] } else {
        py <- predict(TREE,newdata=on.expanded)
      }
      
      if(smooth==TRUE) {
        L <- loess(py~x.u, weights = n)
        xl <- seq(min(x.u),max(x.u), (max(x.u) - min(x.u))/1000)
        lines(xl, predict(L,xl), col=cols[i], lwd=3) 
      } else {
        
        
        lines(py~x.u,lwd=3,col=cols[i]) }
    }
  }
}
