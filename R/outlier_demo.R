outlier_demo <-
function() {
    par(mfrow=c(1,1))
    plot(0,0,xlim=c(0,1),col="white",ylim=c(0,1),xlab="x",ylab="y",main="click plot to add points then click END below to stop demo",cex.main=0.9)

    rect(-1,.8,.5,1.05,col="white")
    legend(0,1,c("without red dot","with red dot"),lty=c(1,1),col=c("black","red"),cex=0.8)
    
    x <- c(); y <- c()
    r.old <- 0
    keep.going <- 1
    while (keep.going==1) {
        new.point <- unlist(locator(1))
        if( new.point[1] > .1 | new.point[2] > .05 ) {
            plot(x,y,pch=20,cex=2,col="black",xlim=c(0,1),ylim=c(0,1),xlab="x",ylab="y",main="")
            rect(0,0,.1,.05,col="red");  text(0.05,.025,"END",cex=0.8)
            legend(0,1,c("without red dot","with red dot"),lty=c(1,1),col=c("black","red"),cex=0.8)
            
            if(length(x)>=2) { abline(lsfit(x,y),col="black",lty=1,lwd=2) }
            points(new.point[1],new.point[2],pch=20,cex=2,col="red")
            x <- c(x,new.point[1]);  y <- c(y,new.point[2])
            if(length(x)>2) {      abline(lsfit(x,y),col="red",lty=1,lwd=2) }
        } else { keep.going <- 0 }
    }
}
