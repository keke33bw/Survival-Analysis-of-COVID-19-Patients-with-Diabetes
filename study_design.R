## study_design.R

rm(list = ls())

## number of deaths needed to get a significant logrank test
alpha = 0.05
beta = 0.2
theta = 0.37 ## from figure
p = 0.5
deaths = (qnorm(alpha / 2) + qnorm(beta))^2 / (p * (1 - p) * log(theta)^2)

## get estimated survival curves from digitizer
## https://automeris.io/WebPlotDigitizer/
D = read.csv("D:/Study/Grad/STAT434/Final project/d.csv", header = FALSE)
V = read.csv("D:/Study/Grad/STAT434/Final project/v.csv", header = FALSE)

## make survival functions
S_D = stepfun(D[,1], c(1, pmin(D[,2], 100) / 100))
S_V = stepfun(V[,1], c(1, pmin(V[,2], 100) / 100))

S_D(2)

## marginal survival
S = function(x, p) {
  p * S_D(x) + (1 - p) * S_V(x)
}

## suppose accrual time is 3 months,
## follow-up time is 6 months,
a = 3
f = 6

## probability of death in time a + f
prob = 1 - 1 / 6 * (S(f, p) + 4 * S(0.5 * a + f, p) + S(a + f, p))

## total number of samples necessary
n = deaths / prob
## ROUND UP
