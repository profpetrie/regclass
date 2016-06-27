segmented.barchart <-
function(x) {
    xlabel <- deparse(substitute(x))
    par(mfrow=c(1,1))
    par(mar=c(5,2,4,4))
    offset <- table(x)/length(x)/2
    fs <- c(0,cumsum(table(x)/length(x)) )[1:length(offset)]
    par(mgp = c(0.5, 1, 0))
    plot(factor(x)~factor(rep(" ",length(x))),xlab=xlabel,ylab="",axes=FALSE,col=grey(seq(.3,.9,length=length(offset))))
    axis(2)
    text(0.5,fs+offset,labels=levels(factor(x)))
    axis(4,at=fs+offset,labels=prettyNum(table(x)/length(x),drop0trailing=FALSE,digits=2,format="e"),las=1)
    par(mgp=c(3, 1, 0))
}
