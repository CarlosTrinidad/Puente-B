# libraries
library(sde)

# parameters
T_final <- 2.0
sigma2 <- 0.3
alpha <- -0.1
M <- 10
N <- 2**M
L <- 1000

# simulation parameters
dt <- T_final/(2**M)
drift_sim <- 0.0
sigma_sim <- sqrt(sigma2)
hits <- 0
P_middle <- c()

for (i in  1:L){
  P <- sigma_sim*BBridge(x=0, y=0, t0=0, T=T_final, N=N)
  P_middle <- c(P_middle, P[N/2 + 1])
  P.min <- min(P)
  hits <- hits + ifelse(P.min < alpha, 1, 0)
}

mean_middle <- mean(P_middle)
var_middle <- var(P_middle)
plot(P)
prob.hit <- hits/L
cat("probability below alpha : ", prob.hit)
cat("mean_middle : ", mean_middle)
cat(" var_middle : ", var_middle)