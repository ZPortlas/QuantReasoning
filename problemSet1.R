# Problem Set 1: Single species models in discrete time
# Jan 16 2019
# ZMP

# Question 1
# Parameters
max_time <- 100
r <- .3
K <- 1

N <- vector('numeric',length=max_time)
N[1] <- 0.1

# iterate model
for (t in 1:max_time){
  N[t+1] = N[t] + r*N[t]*(1-N[t]/K)
}

plot(N)

### As you increase r slowly, the population reaches the equilibrium point earlier (lower t) until r gets too high and it doesn't stabilize.

# Question 2
# loop within loop

max_time <- 50
K <- 1
N <- vector('numeric',length=max_time)
N[1] <- 0.1

plot(rep(0.3,10),N[41:50], xlim = c(0,4), ylim = c(0,1.5))

for (r in seq(0.1,4,0.01)) {
  for (t in 1:max_time) {
    N[t+1] = N[t] + r*N[t]*(1-N[t]/K)
  }
points(rep(r,10),N[41:50])
}

# Question 3
cod_pop <- c(1450, 1420, 1050, 2643, 1060, 1080, 1410, 1150, 740, 175, 43, 10, 12, 15, 16, 16, 28, 30, 32, 23, 12, 19, 27)

years <- c(1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005)

par(mfrow=c(1,2))
plot(years,cod_pop,ylim=c(0,3000),las=1,ylab='Population (tonnes)',xlab='Time (years)',pch=16)
plot(cod_pop[1:(length(cod_pop)-1)],cod_pop[2:length(cod_pop)],ylab='N[t+1]',xlab='N[t]',las=1,pch=16)

mtext(text = 'Atlantic cod population - Canada',side = 3,line = -3,outer=T)

# Beverton-Holt Model: N[t+1] = (a*N[t])/(1+b*N[t])

par(mfrow=c(1,1))
max_time <- 30
a <- 1
b <- 1
N <- vector('numeric',length=max_time)
N[1] <- 0.1

for (t in 1:max_time){
N[t+1] = (a*N[t])/(1+b*N[t])
}

plot(N[t+1] ~ N[t])
