library(UsingR)
data(father.son)

y<-(father.son$sheight-mean(father.son$sheight))/sd(father.son$sheight) 
x<-(father.son$fheight-mean(father.son$fheight))/sd(father.son$fheight) 
rho<-cor(x,y)

regrplot<-function(x,y){
  plot(x,y, xlab="Father'sheight,normalized", ylab="Son'sheight,normalized", xlim=c(-3,3),ylim=c(-3,3), bg="lightblue",col="black",cex=1.1,pch=21, frame=FALSE)
}


myplot <- function(beta){
  y<-galton$child-mean(galton$child) 
  x<-galton$parent-mean(galton$parent) 
  freqData<-as.data.frame(table(x,y)) 
  names(freqData)<-c("child","parent","freq") 
  plot(
    as.numeric(as.vector(freqData$parent)), 
    as.numeric(as.vector(freqData$child)), 
    pch=21,col="black",bg="lightblue", cex=.15*freqData$freq, 
    xlab="parent", ylab="child"
  )
  abline(0,beta,lwd=3) 
  points(0,0,cex=2,pch=19)
  mse<-mean((y-beta*x)^2) 
  title(paste("beta=",beta,"mse=",round(mse,3)))
}