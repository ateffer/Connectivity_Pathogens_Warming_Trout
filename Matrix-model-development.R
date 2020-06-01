### Practicing Matrix Population Modeling
#### A.K. Teffer

library(expm)    
library(ggplot2)
library(dplyr)
library(reshape2)

setwd("~/Smith Conservation Fellow/Smith BKT Data/R Smith")
#Load packaages and set directory
library(expm)    
library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/Smith Conservation Fellow/Smith BKT Data/R Smith")

## Create our matrix
mtx <- matrix(
  c(0.0,1.0,0.0,0.4,
    0.4,0.3,0.1,0.3,
    0.0,0.6,0.0,0.9,
    0.2,0.3,0.4,0.5),
  nrow=4)
mtx

## Create vector of age distributions, Pops A&B
pop.age<-c(20,50,40,30)
pop.age

## Multiply matrix by vector for one cycle
result.1cyc <- mtx %*% pop.age
result.1cyc

result.2cyc <- (mtx %^% 2) %*% pop.age
result.2cyc


## Loop by year and store values in matrix

### Create empty matrix to store values per year for 100 years
mat <- matrix(,nrow=4, ncol=100)

### Loop over 100 years
for(i in 1:100){
  mat[,i] = (mtx %^% i) %*% pop.age
}

### Plot age distributions in 2 populations over time
pop.100 <- as.data.frame(mat)
rownames(pop.100) <- c("PopA0", "PopA1", "PopB0", "PopB1")
colnames(pop.100) <- c(1:100)

### Add row names and melt into long form to plot
pop.df <- tibble::rownames_to_column(pop.100, "pop")
pop.df2 <- melt(data = pop.df,
                id.vars = "pop")

### Plot population size change over 100 cycles
ggplot(pop.df2, aes(x=variable, y=value, fill=pop)) + 
  geom_bar(stat = "identity") +
  labs(y="Population size", x="Cycle") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






