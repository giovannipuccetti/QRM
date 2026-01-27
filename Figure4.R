## Code for Figure 4
library(ggplot2)
library(gridExtra)
library(extrafont)
alpha=.90 # VaR confidence level
N=100 # number of points in the support of the TAIL approximation of each marginal distribution 
## Build marginals approximation
X=matrix(rep(qnorm(seq(alpha,1,length=N+1)),2),nrow=N+1)
LX=X[1:N,] # approximation of the marginals from below
UX=X[2:(N+1),] # approximation of the marginals from above
# Rearrangement algorithm
epsilon=0.001 # desired accuracy
LVaR=Inf # recursive value for the worst possible VaR
delta=Inf # difference between two consecutive estimates
## Iteratively rearrange each column of LX until the difference between
## two consecutive estimates of the worst VaR is less than epsilon
while(delta >epsilon ){
  LVaRtemp=LVaR # auxiliary variable
  ## Rearrangement loop
  for(j in 1:2) {
    LX[,j]=sort(LX[,j],decreasing=TRUE)[rank(LX[,c(1:2)[-j]])] }
  LVaR= min(rowSums(LX))
  delta=abs(LVaR-LVaRtemp)
}
## Iteratively rearrange each column of UX until the difference between
## two consecutive estimates of the worst VaR is less than epsilon
UVaR=Inf # recursive value for the worst possible VaR
delta2=Inf # difference between two consecutive estimates
## Rearrangement loop
while(delta2 >epsilon ){
  UVaRtemp=UVaR # auxiliary variable
  ## Rearrangement loop
  for(j in 1:2) {
    UX[,j]=sort(UX[,j],decreasing=TRUE)[rank(UX[,c(1:2)[-j]])] }
  UVaR= min(rowSums(UX))
  delta2=abs(UVaR-UVaRtemp)
}
M=as.integer(alpha*N/(1-alpha)) # number of additional points in the left part of the marginals supports
## Generate the joint dependence maximizing VaR
## Left alpha-part of the dependence can be chosen arbitrarily
LXa=matrix(rep(qnorm(seq(0,alpha,length=M)),2),nrow=M) # set as comonotonicity
A=rbind(LXa[2:M,],LX) # final support of the solution
## Generate joint normal distribution with same marginals and same correlation
library(MASS)
set.seed(200) #seed for reproducibility
B = mvrnorm(M+N,c(0,0),cor(A)) # simulations
## Isolate corresponding Gaussian copula
GC=pnorm(B)
## Rearrange previous marginal values with new copula
rk=apply(GC,2,rank) #dependence structure as (inter-)ranks
BB=sapply(1:2, function(j) sort(A[,j])[rk[,j]]) # original data rearrangement according to copula

#plot1

data <- data.frame(A)

plot1 <- ggplot(data, aes(x=X1,y=X2)) +
         geom_point(size=1.5, shape=16) +
         xlim(-3, 3) + ylim (-3,3) +
         xlab("X") +  ylab("Y") +
         ggtitle("Worst VaR distribution") +
         geom_rug(sides ="bl",size=0.5)

#plot2

data <- data.frame(BB)

plot2 <- ggplot(data, aes(x=X1,y=X2)) +
  geom_point(size=1.5, shape=16) +
  xlim(-3, 3) + ylim (-3,3) +
  xlab("X") +  ylab("Y") +
  ggtitle("Bivariate Normal with the same correlation") +
  geom_rug(sides ="bl",size=0.5)

plotfinal=grid.arrange(plot1 + theme(
  text = element_text(size=12, family = "serif"),
  plot.title = element_text(size=14, face="bold",hjust = 0.5),
  axis.title.x = element_text(size=10),
),plot2 + theme(
  text = element_text(size=12, family = "serif"),
  plot.title = element_text(size=14, face="bold",hjust = 0.5),
  axis.title.x = element_text(size=10),),ncol = 2, nrow = 1)

ggsave(plotfinal,file="Figure4.pdf",width=10,height=5)


