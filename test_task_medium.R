library(ggplot2)
library(dirichletprocess)
library(mixR)
library(datasets)
library(dplyr)
library(tidyr)

log_normal_data <- rmixlnorm(1000, c(0.5,0.5), c(2, 5), c(1,1))

plot(log_normal_data)
log_normal_data <- scale(log_normal_data)
dp <- DirichletProcessGaussian(log_normal_data)
dp <- Fit(dp, 1000)

plot(dp)

xGrid <- seq(0, 1, by=0.01)

postEval <- replicate(100, PosteriorFunction(dp)(xGrid))

meanFrame <- data.frame(Mean=rowMeans(postEval), x=xGrid)
quantileFrame <- data.frame(x=xGrid, t(apply(postEval, 1, quantile, prob=c(0.05, 0.95))))

ggplot()  + geom_ribbon(data=quantileFrame, aes(x=x, ymin=X5., ymax=X95.), alpha=0.4) + 
  geom_line(data=meanFrame, aes(x=x, y=Mean, colour="Posterior Mean")) 


data <- faithful 
data <- scale(data)

dp1 <- DirichletProcessMvnormal(data, alphaPriors = c(2,4))
dp1 <- Fit(dp1, 1000)
plot(dp1)

dp2 <- DirichletProcessMvnormal(data, alphaPriors = c(4,8))
dp2 <- Fit(dp2, 1000)
plot(dp2)

alphaFrame <- data.frame(Chain1 = dp1$alphaChain, Chain2 = dp2$alphaChain,
                         Iter=seq_len(1000))
alphaFrame %>% gather(Chain, Value, -Iter) -> alphaFrameTidy
ggplot(alphaFrameTidy, aes(x=Iter, y=Value, colour=Chain)) + geom_line()

