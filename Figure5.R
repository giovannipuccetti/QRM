## Code for Figure 5
library(ggplot2)
library(extrafont)
set.seed(200) # seed for reproducibility
theta=0.9 # Pareto tail parameter (theta<1 means infinite mean)
N=10^5 # sample size
alpha=seq(0.01,0.99,0.001) # values at which to draw the plot
qp=function(x){(1-x)^(-1/theta)-1} # Pareto quantile function
U1=runif(N) # U(0,1) simulation
U2=runif(N) # U(0,1) simulation
I=qp(U1)+qp(U2) # sum of two independent Pareto
VaR_ind=quantile(I,probs=alpha,type=1) # VaR calculation
C=qp(U1)+qp(U1) # sum of two comonotonic Pareto
VaR_com=quantile(C,probs=alpha,type=1) # VaR calculation


## Create ME-plots for various distributions

data <- data.frame(alpha,VaR_ind,VaR_com)
colnames(data) <- c("alpha","Independence","Comonotonicity")


MEplot <- ggplot(data, aes(alpha)) +
          geom_line(aes(y=VaR_ind, colour="Independence"), size=1, linetype = "solid") +
          geom_line(aes(y=VaR_com, colour="Comonotonicity"), size=1, linetype = "dashed") +
          xlim(0, 1) + ylim (0,40) +
          ggtitle("VaR of the sum of two Pareto risks with infinite mean") +
          xlab(expression(alpha)) +  ylab(" ") +
          scale_color_manual(name = " ", values = c("Independence" = "black", "Comonotonicity" = "black"),breaks=c("Independence", "Comonotonicity"))

MEplot + theme(
  text = element_text(size=12, family = "serif"),
  plot.title = element_text(size=14, face="bold",hjust = 0.5),
  axis.title.x = element_text(size=10),
  legend.key.width = unit(3, "line"),
  legend.text=element_text(size=14),
  legend.position=c(.3,.9),
  legend.title = element_blank()
)

ggsave(file="Figure5.pdf",width=5,height=5)
