build.tree <-
function(form,data,minbucket=5,seed=NA,holdout) {
  if(!is.na(seed)) { set.seed(seed) }
  FORM <- formula(form)
  TREE <- rpart(FORM,data,control=rpart.control(cp=0,xval=10,minbucket=minbucket))
  CPs <- TREE$cptable
  colnames(CPs) <- c("cp","nsplit","rel error","Est Gen Error","SD Gen Error")
  
  if(!missing(holdout)) {
    y.name <- strsplit(as.character(FORM),split="~")[[2]]
    y.actual <- holdout[,which(names(holdout)==y.name)]
    y.actualtrain <- data[,which(names(holdout)==y.name)]
    rmse.holdout <- c()
    rmse.train <- c()
    for (cp in CPs[,1]) {
      TREE.temp <- rpart(FORM,data,control=rpart.control(cp=cp,xval=10,minbucket=minbucket))
      pred.train <- predict(TREE.temp,newdata=data)
      pred.temp <- predict(TREE.temp,newdata=holdout)
      if(class(y.actual)=="factor") { 
        rmse.holdout <- c( rmse.holdout, 1-sum( colnames(pred.temp)[ apply(pred.temp,1,which.max) ] == y.actual )/length(y.actual))
        rmse.train <- c( rmse.train, 1-sum( colnames(pred.train)[ apply(pred.train,1,which.max) ] == y.actualtrain )/length(y.actualtrain))
        
        } else {
        rmse.holdout <- c( rmse.holdout, sqrt( mean( (pred.temp-y.actual)^2 )) ) 
        rmse.train <- c( rmse.train, sqrt( mean( (pred.train-y.actualtrain)^2 )) ) }
  }
    CPs <- cbind(CPs, rmse.train)
    CPs <- cbind(CPs, rmse.holdout)
    colnames(CPs) <- c("cp","nsplit","rel error","Est Gen Error","SD Gen Error","Train Error","Holdout Error")
  }
  
  CPs <- CPs[,-3]
  
  
  
  rownames(CPs) <- rep("",nrow(CPs))
  xerror <- CPs[,"Est Gen Error"]
  splits <- CPs[,"nsplit"]
  cps <- CPs[,"cp"]
  sds <- CPs[,"SD Gen Error"]
  bests <- which(xerror<=min(xerror)+min(sds[which.min(xerror)]))
  rownames(CPs)[which.min(xerror)] <- "min*"
  rownames(CPs)[min(bests)] <- "1SD*"
  print(CPs)
  cat(paste("\nSuggested cp =",prettyNum(cps[which.min(xerror)]),"for the tree with the lowest generalization error (",splits[which.min(xerror)],"splits )"))
  cat(paste("\nSuggested cp =",prettyNum(cps[min(bests)]),"for the simplest tree according to the one standard deviation rule (",splits[min(bests)],"splits )"))
  par(mfrow=c(1,1))
  plot(xerror~splits,xlab="# Splits",ylab="Relative crossvalidation error",pch=20,cex=1,col="blue",ylim=c(min(xerror-sds),max(xerror+sds)))
  abline(h=min(xerror)+sds[which.min(xerror)])
  abline(v=splits[which.min(xerror)],lty=2)
  abline(v=splits[min(bests)])
  for (i in 1:length(splits)) {
    lines(c(splits[i],splits[i]),c(xerror[i]-sds[i],xerror[i]+sds[i]),col="red",lwd=2)
  }
  legend("topright",c("1 SD rule","lowest error"),lty=1:2)
  
}
