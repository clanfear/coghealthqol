
pgmm_summary <- function(x){
  if (x$args$transformation=="d") { l <- 1
  } else {
    l <- 0 }
  if (length(x$args$namest) < 4-l) {order<-1
  } else {
    order <- 2
  }
  if (length(x$args$namest) > 2-l) {MTest<-mtest(x,order=order,vcovHC(x))
  } else {
    MTest<-"MTest requires T > 3"
  }
  Sargan<-sargan(x)
  std.err <- sqrt(diag(vcovHC(x)))
  b <- coef(x)
  z <- b/std.err
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  Summary <- cbind(round(b,4), round(std.err,4), round(z,3), round(p,3))
  colnames(Summary) <- c("b","se","z","p")
  ChiSq<-plm:::wald(x, "time", vcovHC(x))
  list<-list(MTest,Sargan,Summary,ChiSq)
  names(list)<-c("MTest","Sargan","Summary","ChiSq")
  list
}

extract_pe_ci <- function(x){
  vals <- c(mean(x), quantile(x, probs=c(.025, .975)))
}
