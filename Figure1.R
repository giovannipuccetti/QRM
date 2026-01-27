## Code for Figure 1 - Mean Excess (ME) function for some distributions having mean 1.
library(ggplot2)
library(extrafont)
N=100 # resolution of the ME-plot
M=8 # upper x-limit of the ME-plot
u=seq(0,M,length=N) # x-coordinates
y=matrix(,nrow = N,ncol = 4) # y-coordinates
## Loop to compute plot coordinates for various dfs standardized to have mean=1
for (i in 1:N){
  ## Normal ME-plot
  f <- function(x) {1-pnorm(x+u[i]-1)}
  y[i,1]=(as.numeric(integrate(f, lower = 0, upper=M)[1]))/(1-pnorm(u[i]-1))
  ## Exponential
  y[i,2]=1
  ## Student's t ME-plot
  v=2 #dof
  f <- function(x) {1-pt(x+u[i]-1,v)}
  y[i,3]=(as.numeric(integrate(f, lower = 0, upper=M)[1]))/(1-pt(u[i]-1,v))
  ## Pareto
  y[i,4]=1+u[i]
}

## Create ME-plots for various distributions

data <- data.frame(u,y)
colnames(data) <- c("threshold","Normal","Exponential","t","Pareto")


MEplot <- ggplot(data, aes(u)) +
          geom_line(aes(y=Normal, colour="Normal"), size=1, linetype = "solid") +
          geom_line(aes(y=Exponential, colour="Exponential"), size=1, linetype = "dashed") +
          geom_line(aes(y=t, colour="Student's t"), size=1, linetype = "dotted") +
          geom_line(aes(y=Pareto, colour="Pareto"), size=1, linetype = "dotdash") +
          xlim(0, 8) + ylim (0,4) +
          ggtitle("Mean Excess function") +
          xlab("threshold") +  ylab(" ") +
          scale_color_manual(name = " ", values = c("Normal" = "black", "Exponential" = "black", "Student's t"  = "black","Pareto"  = "black" ) ,breaks=c("Pareto", "Student's t", "Exponential", "Normal")) +
          coord_fixed(ratio = 1)

MEplot + theme(
  text = element_text(size=12, family = "serif"),
  plot.title = element_text(size=14, face="bold",hjust = 0.5),
  axis.title.x = element_text(size=10),
  legend.key.width = unit(4, "line"),
  legend.text=element_text(size=14),
  legend.position="right",
)

ggsave(file="Figure1.pdf",width=12,height=5)


