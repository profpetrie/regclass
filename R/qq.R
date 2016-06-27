qq <-
function(x,ax=NA,leg=NA,cex.leg=0.8)  {
  if(is.na(ax)) { x.label <- deparse(substitute(x)) } else { x.label <- ax }
  x <- sort(x)
  n <- length(x)
  P <- ppoints(n)
  z <- qnorm(P,mean(x),sd(x))
  plot(z, x, xlab = paste("Values of",x.label,"if Normal"), ylab = "Observed Values",pch=20,cex=.8)
  Q.x <- quantile(x, c(0.25, 0.75))
  Q.z <- qnorm(c(0.25, 0.75), mean(x),sd(x) )
  b <- as.numeric( (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1]) )
  a <- as.numeric( Q.x[1] - b * Q.z[1] )
  abline(a, b, lwd=1,col="red")
  conf <-  0.95
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (b/dnorm(z,mean(x),sd(x)) ) * sqrt(P * (1 - P)/n)
  fit.value <- a + b * z
  upper <- fit.value + zz * SE
  lower <- fit.value - zz * SE
  lines(z, upper, lty = 2,col="red",lwd=1)
  lines(z, lower, lty = 2,col="red",lwd=1)
  if(!is.na(leg)) { legend("topleft",c(leg),cex=cex.leg) }

}
