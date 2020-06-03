### Practicing Matrix Population Modeling
#### A.K. Teffer

setwd("~/Smith Conservation Fellow/Smith BKT Data/R Smith")

#Load packaages and set directory
library(expm)    
library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/Smith Conservation Fellow/Smith BKT Data/R Smith")

## Define parameters
p <- 0.5
t <- 1.0

### Fecundity by population/movement
Fa <- 1.5
Fb <- 2.5
Fab <- 0.5
Fba <- 0.3

### Survival by population/stage
Sa0 <- function(p,t){0.5 + p * -0.1 + t * -0.2}
Sa1 <- function(p,t){0.6 + p * -0.1 + t * -0.2}
Sb0 <- function(p,t){0.4 + p * -0.1 + t * -0.2}
Sb1 <- function(p,t){0.5 + p * -0.1 + t * -0.2}

### Movement by population/stage
Mab0 <- function(p,t){0.3 + p * -0.1 + t * -0.2}
Mab1 <- function(p,t){0.4 + p * -0.1 + t * -0.2}
Mba0 <- function(p,t){0.35 + p * -0.1 + t * -0.2}
Mba1 <- function(p,t){0.3 + p * -0.1 + t * -0.2}

## Create our matrix
mtx <- function(p,t){
  out <- matrix(
        c(0.0, Sa0(p,t), 0.0, Mab0(p,t),
        Fa, Sa1(p,t), Fab, Mab1(p,t),
        0.0, Mba0(p,t), 0.0, Sb0(p,t),
        Fba, Mba1(p,t), Fb, Sb1(p,t)),
      nrow=4)
}
mtx1 <- mtx(p,t)

## Characterize population attributes
eigen(mtx1)

## Create vector of age distributions, Pops A&B
pop.age 
mat[,1]<-c(20,50,40,30)
pop.age

## Multiply matrix by vector for one cycle
result.1cyc <- mtx %*% pop.age
result.1cyc

result.2cyc <- (mtx %^% 2) %*% pop.age
result.2cyc


## Loop by year and store values in matrix

### Create empty matrix to store values per year for 100 years
mat <- matrix(,nrow=4, ncol=100)


# loop over p and t
for(k in p){
  
  mat[,i]
  ### Loop over 100 years
  for(i in 2:100){
    #mat[,i] = (mtx %^% i) %*% pop.age
    mat[,i] <- mtx(k,j) %*% mat[,i-1]
    #mat[,i,k]
  }
  output
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

### Plot population size change over 100 cycles - log pop size
ggplot(pop.df2, aes(x=variable, y=log10(value), fill=pop)) + 
  geom_bar(stat = "identity") +
  labs(y="Population size", x="Cycle") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




