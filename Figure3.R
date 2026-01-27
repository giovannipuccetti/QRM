## Code for Figure 3 - Attainable correlations
library(ggplot2)
library(extrafont)
## Parameters of X
meanx=0 # mean of X
varx=1 # variance of X
invx<-function(x){qnorm(x)} # Normal quantile function of X
N=200 #resolution of the correlation plot
dof=seq(2,4,len=N) # points at which to compute correlation
maxcor=seq(0,0,len=N) # vector to store max correlation
mincor=seq(0,0,len=N) # vector to store min correlation
## Loop to compute coordinates to plot
for(i in 1:N) {
  ## Parameters of Y
  meany=0 # mean of Y
  dofy=dof[i] # dof of Y
  vary=dofy/(dofy-2) # variance of Y
  invy<-function(x){qt(x,dofy)} # t quantile function of Y
  ## Compute max/min correlation via numerical integration
  f<-function(u){invx(u)*invy(u)}
  g<-function(u){invx(u)*invy(1-u)}
  maxcor[i]=(as.numeric(integrate(f, lower = 0,
                                  upper=1)[1])-meanx*meany)/(sqrt(varx*vary))
  mincor[i]=(as.numeric(integrate(g, lower = 0,
                                  upper = 1)[1])-meanx*meany)/(sqrt(varx*vary))
}

upper=seq(1,1,len=N)
lower=seq(-1,-1,len=N)
zero=seq(0,0,len=N)
data <- data.frame(dof,maxcor,mincor,upper,lower,zero)
colnames(data) <- c("dof","Maximal Correlation","Minimal Correlation","Upper","Lower","Zero")


## Max/Min correlation plot

MEplot <- ggplot(data, aes(dof)) +
          geom_line(aes(y=maxcor, colour="Maximal Correlation"), linewidth=1.5, linetype = "solid") +
          geom_line(aes(y=mincor, colour="Minimal Correlation"), linewidth=1.5, linetype = "dashed") +
          geom_line(aes(y=upper), linewidth=1, linetype = "dotted") +
          geom_line(aes(y=lower), linewidth=1, linetype = "dotted") +
          geom_line(aes(y=zero),  linewidth=1, linetype = "dotted") +
          xlim(2, 4) + ylim (-1,1) +
          ggtitle("Attainable Correlations between a Normal and a t") +
          xlab("dof") +  ylab(" ") +
          scale_color_manual(name = " ", values = c("Maximal Correlation" = "black", "Minimal Correlation" = "black"))

MEplot + theme(
  text = element_text(size=12, family = "serif"),
  plot.title = element_text(size=14, face="bold",hjust = 0.5),
  axis.title.x = element_text(size=10),
  legend.key.width = unit(3, "line"),
  legend.text=element_text(size=14),
  legend.position=c(.7,.5),
  legend.title = element_blank()
)

ggsave(file="Figure3.pdf",width=10,height=5)


