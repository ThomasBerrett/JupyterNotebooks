library(class)
iris <- iris[iris$Species!='setosa',]
set.seed(1)
x <- as.matrix(iris[,c(1,4)]) %>% jitter(amount=1.)  ## jitter a bit for more interesting plots
x <- scale(x) #; plot(x, col=factor(y))
y <- as.character(iris$Species)

par(mfrow=c(2,2), mai=c(0.1, .1, .4, .1))
for(k in c(1, 5, 15, 31)){
  nt <- 150
  tseq <- seq(-3,3, len=nt); xygrid <- expand.grid(tseq, tseq)
  ypred <- knn(train=x, test=xygrid, cl=y, k=k)
  #
  col <- c(1,4)[factor(y)]
  plot(x, col=col, pch=20, xaxt='n', yaxt='n', main=paste0("K = ", k))
  points(xygrid, pch=20, cex=.05, col=c(1,4)[factor(ypred)])
  contour(tseq, tseq, z=matrix(as.numeric(ypred), nt), drawlabels=FALSE, add=TRUE, levels=1.5)
  #contour(tseq, tseq, z=matrix(as.numeric(ypred), nt), drawlabels=FALSE, add=TRUE, nlevels=1)
}