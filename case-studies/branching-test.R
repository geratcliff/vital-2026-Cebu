library(expm)
library(tidyverse)

# Define Rates
r_A = 0.02
r_B = 0.06

# 1. Conversion
p_A_conv = 1-exp(-r_A)
p_B_conv = 1-exp(-r_B)

# 2. Embedding
R = matrix(0,nrow = 3, ncol = 3, dimnames = list(c("Alive","DeadA","DeadB"), c("Alive","DeadA","DeadB")))
R[1,] = c(0,r_A,r_B)
diag(R) = -rowSums(R)
P = expm::expm(R)
P
# Embedded probabilities of A and B
p_A = P[1,2]
p_B = P[1,3]

tibble(approach = c("Conversion","Embedding"), p_A = c(p_A_conv, p_A), p_B = c(p_B_conv, p_B))



