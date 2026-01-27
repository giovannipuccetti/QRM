## Code for Figure 2 - Mean Excess (ME) function for for two data samples
library(ggplot2)
library(extrafont)
library(gridExtra)
set.seed(100) # seed for reproducibility
N=500 # sample size
## Generate samples from X and Y
X=rnorm(N) # Normal sample
Y=rt(N,df=2) # t sample
## Standardize samples
X=(X-mean(X))/sd(X)
Y=(Y-mean(Y))/sd(Y)
## Compute sample mean of X and Y
c(mean(X),mean(Y))
## [1] 3.952394e-17 -1.332268e-17
## Compute sample variance of X and Y
c(var(X),var(Y))
## [1] 1 1
## Compute sample correlation between X and Y
cor(X,Y)

## ME values for the x-sample

cut=3 # cut is the number of largest observations to be excluded.

  data=X
  data=sort(as.numeric(data)); # ordered sample values
  N=length(data); # data sample size
  mef=c();
  for (i in 1:N) {
    mef[i]=mean(data[data>data[i]])-data[i];
    # averages of exceedances at the ordered sample values
  }
  ## Omit from the ME-plot the largest observations
  data_out_x=data[1:(N-cut)];
  mef_out_x=mef[1:(N-cut)];
  ## Create final database
  dataf_x=data.frame(data_out_x,mef_out_x)

  ## ME values for the y-sample
  data=Y
  data=sort(as.numeric(data)); # ordered sample values
  N=length(data); # data sample size
  mef=c();
  for (i in 1:N) {
    mef[i]=mean(data[data>data[i]])-data[i];
    # averages of exceedances at the ordered sample values
  }
  ## Omit from the ME-plot the largest observations
  data_out_y=data[1:(N-cut)];
  mef_out_y=mef[1:(N-cut)];
  ## Create final database
  dataf_y=data.frame(data_out_y,mef_out_y)


## Create ME-plot for x-data

MEplotX <- ggplot(dataf_x, aes(data_out_x)) +
          geom_point(aes(y=mef_out_x), size=2, shape=1) +
          xlim(0, 2.5) + ylim (0,0.8) +
          ggtitle("ME-plot for x-data") +
          xlab("threshold") +  ylab(" ")

MEplotX + theme(
  text = element_text(size=12, family = "serif"),
  plot.title = element_text(size=14, face="bold",hjust = 0.5),
  axis.title.x = element_text(size=10),
)

## Create ME-plot for y-data

MEplotY <- ggplot(dataf_y, aes(data_out_y)) +
  geom_point(aes(y=mef_out_y), size=2, shape=1) +
  xlim(0, 2) + ylim (0,2) +
  ggtitle("ME-plot for y-data") +
  xlab("threshold") +  ylab(" ")

MEplotY + theme(
  text = element_text(size=12, family = "serif"),
  plot.title = element_text(size=14, face="bold",hjust = 0.5),
  axis.title.x = element_text(size=10),
)

MEfinal=grid.arrange(MEplotX + theme(
  text = element_text(size=12, family = "serif"),
  plot.title = element_text(size=14, face="bold",hjust = 0.5),
  axis.title.x = element_text(size=10),
),MEplotY+ theme(
  text = element_text(size=12, family = "serif"),
  plot.title = element_text(size=14, face="bold",hjust = 0.5),
  axis.title.x = element_text(size=10),
), ncol = 2, nrow = 1)

ggsave(MEfinal,file="Figure2.pdf",width=10,height=5)
