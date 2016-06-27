getcp <-
function(TREE) {
    CPs <- TREE$cptable
    rownames(CPs) <- rep("",nrow(CPs))
    xerror <- CPs[,"xerror"]
    splits <- CPs[,"nsplit"]
    cps <- CPs[,"CP"]
    sds <- CPs[,"xstd"]
    bests <- which(xerror<=min(xerror)+min(sds[which.min(xerror)]))
    rownames(CPs)[which.min(xerror)] <- "min*"
    rownames(CPs)[min(bests)] <- "1SD*"
    print(CPs)
    cat(paste("\nSuggested cp =",prettyNum(cps[which.min(xerror)]),"for the tree with the lowest generalization error (",splits[which.min(xerror)],"splits )"))
    cat(paste("\nSuggested cp =",prettyNum(cps[min(bests)]),"for the simplest tree according to the one standard deviation rule (",splits[min(bests)],"splits )"))
    par(mfrow=c(1,1))
    plot(xerror~splits,xlab="# Splits",ylab="Relative crossvalidation error",pch=20,cex=1,col="blue",ylim=c(min(xerror-sds),max(xerror+sds)))
    abline(h=min(xerror)+2*sds[which.min(xerror)])
    abline(v=splits[which.min(xerror)],lty=2)
    abline(v=splits[min(bests)])
    for (i in 1:length(splits)) {
      lines(c(splits[i],splits[i]),c(xerror[i]-sds[i],xerror[i]+sds[i]),col="red",lwd=2)
    }
    legend("topright",c("1 SD rule","lowest error"),lty=1:2)
       }
