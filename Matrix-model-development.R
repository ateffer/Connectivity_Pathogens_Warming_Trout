### Practicing Matrix Population Modeling
#### A.K. Teffer

setwd("~/Smith Conservation Fellow/Smith BKT Data/R Smith")

#Load packaages and set directory
library(expm)    
library(ggplot2)
library(dplyr)
library(reshape2)

## Define parameters
p <- 0.5  #pathogen prevalence or likelihood of presence or virulence degree
t <- 10  #temperature

### Fecundity by and between populations (a,b) 
Fa <- 1.5
Fb <- 2.5
Fab <- 0.5
Fba <- 0.3

### Survival by population/stage (0, 1+)
Sa0 <- function(p,t){0.5 + p * -0.1 + t * -.04}
Sa1 <- function(p,t){0.6 + p * -0.1 + t * -.02}
Sb0 <- function(p,t){0.5 + p * -0.1 + t * -.04}
Sb1 <- function(p,t){0.5 + p * -0.1 + t * -.02}

### Movement by population/stage
Mab0 <- function(p,t){0.3 + p * -0.1 + t * -.02}
Mab1 <- function(p,t){0.4 + p * -0.1 + t * -.02}
Mba0 <- function(p,t){0.35 + p * -0.1 + t * -.02}
Mba1 <- function(p,t){0.3 + p * -0.1 + t * -.02}

## Create our matrix
mtx <- function(p,t){
  out <- matrix(
        c(0, Sa0(p,t), 0, Mab0(p,t),
        Fa, Sa1(p,t), Fab, Mab1(p,t),
        0, Mba0(p,t), 0, Sb0(p,t),
        Fba, Mba1(p,t), Fb, Sb1(p,t)),
      nrow=4)
}
mtx1 <- mtx(p,t) #test it
mtx1

## Characterize population attributes (r, stable stage structure)
eigen(mtx1)

### Create empty matrix to store values per year for 100 years
mat <- matrix(, nrow=4, ncol=100)
mat[,1]<-c(20,50,40,30) #initial population structure

### Loop over 100 years
for(i in 2:100){
    mat[,i] <- mtx(p,t) %*% mat[,i-1]
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

## Next - vary the parameter vales along a gradiet and observe how varying magnitude of effects impacts population structure and resilience
# loop over p and t
# e.g., p <- seq(0.2, 0.9, 2.5); for(k in p){...
  
  

