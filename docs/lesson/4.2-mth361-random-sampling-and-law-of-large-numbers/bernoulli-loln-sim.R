# load packages
library(tidyverse)
library(openintro)
library(gganimate)
library(latex2exp)
data(COL)
seed <- 42

# set hyperparameters
set.seed(seed) # for reproducibility
n <- seq(1,1000) # number of trials as a vector

# set parameter and PMF of the Bernoulli r.v
X <- c(0,1) # outcomes ("0"="failure","1"="success")
p <- 0.60 # probability of success
bern_pmf <- c(1-p,p)

# simulate Bernoulli trials while tracking n and the proportions
samples <- sample(X,size=max(n),prob=bern_pmf,replace=TRUE)
props_0 <- c()
props_1 <- c()
for (i in n){
  props_0 <- c(props_0,1-sum(samples[1:i])/i)
  props_1 <- c(props_1,sum(samples[1:i])/i)
}
df <- tibble(n=n,`0`=props_0,`1`=props_1) %>%
  pivot_longer(-n, names_to = "X",values_to = "prop")

# plot simulations
p3 <- ggplot(df,aes(x=n,y=prop,color=X,group=X)) + 
  geom_point(size=2) + 
  geom_line(linewidth=1) + 
  scale_color_manual(values=c(`0`="#ff0080",`1`="#009159")) + 
  geom_hline(aes(yintercept = p), color = "#f1cc54", linetype="dashed", linewidth=1) + 
  geom_text(aes(0,p),label = paste(TeX("E(X)")," = ",p,sep=""), 
            vjust = -1, hjust=-0.75,color="#f1cc54") + 
  ylim(0,1) + 
  xlab("n") + 
  ylab("proportion") + 
  ggtitle(paste("Simulation of Bernoulli Trials (",TeX("$p$")," = ",p,")",sep="")) + 
  theme_minimal()

# animation options
p3_anim <- p3 + 
  transition_reveal(n)

# animate and save as .gif
animate(p3_anim, nframes = round(length(n)/5), 
        renderer = gifski_renderer("bernoulli-loln-sim.gif"),
        height=1000,width=2000,res=300)