influence_plot <-
function(M,large.cook) {
    if(class(M)!="lm") { stop(cat("Argument must be a fitted model using lm()\n")) }
    par(mfrow=c(1,1))
    par(mar=c(5,4,4,2)+0.1)
    plot( hatvalues(M), rstudent(M), type="n", xlab="Leverage", ylab="deleted Studentized residuals", main="",cex.lab=1.2,cex.axis=1.2)
    cook <- cooks.distance(M)
    if(!missing(large.cook)) { big.cooks <- which(cook>large.cook) } else {
        big.cooks <- which(cook>4/length(cook) ) }
    influential <- intersect(which(hatvalues(M)>(2*length(M$coef)/length(M$res))),which(abs(rstudent(M))>2))
    shade <- grey(rep(.4, length(M$res) ))
    shade[big.cooks] <- grey(0)
    sizes <- rep(1,length(M$res))
    sizes[big.cooks] <- 2.5
    border <- rep(1,length(M$res))
    border[big.cooks] <- 2
    points( hatvalues(M), rstudent(M), cex= 10*sqrt(cook)/max(sqrt(cook)),col=shade,lwd=sizes)
    if (length(influential)>0) { text( hatvalues(M)[influential], rstudent(M)[influential], "x") }
    abline(v=2*length(M$coef)/length(M$res))
    abline(h=c(-2,2))
    output<-list(Cooks=big.cooks, Leverage=influential)
    return(output)
}
