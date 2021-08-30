# "Data" Visualizations 7/20/21
# Spellman Fellow 2021
# Evan Perry

# DESCRIPTION:
# Plug some numbers in and see if this whole thing works.

library(tidyverse)

a <- 0.3
b <- 0.6
c <- 0.2
d <- 1.3

H <- 2000 * 4000
theta <- 1
V <- 25000

prod <- 100
lamd_g <- 1.07
lamd_b <- 1

K <- 100000000
lc <- 100000

c_g <- 8
c_b <- 1

A <- log((theta/V)^(1/a)*H*(1-a)^(1-a))
B_b <- (1/(1-b-c))*log(prod*lamd_b*b^(1-c)*c^c) + log(K)
B_g <- (1/(1-b-c))*log(prod*lamd_g*b^(1-c)*c^c) + log(K)
C_b <- (1/(1-b-c))*log(prod*lamd_b*b^(b)*c^(1-b)) + log(K)
C_g <- (1/(1-b-c))*log(prod*lamd_g*b^(b)*c^(1-b)) + log(K)
F_b <- log((1/(d*c_b))^(1/(d-1)) * lc)
F_g <- log((1/(d*c_g))^(1/(d-1)) * lc)


para_A <- a*b - a*b*d
para_B <- -a*b + a*b*d
para_CF <- 1 - b - c - d + a*b + b*d + c*d - a*b*d
para_DM <- c - d + b*d - a*b*d

p_g <- (para_A * A + para_B * B_g + para_CF*(C_g - F_g))/para_DM
p_b <- (para_A * A + para_B * B_b + para_CF*(C_b - F_b))/para_DM


exp(p_g)
exp(p_b)


# With Respect to A

prod_seq <- seq(70,130, by=2)

A <- log((theta/V)^(1/a)*H*(1-a)^(1-a))
B_b <- (1/(1-b-c))*log((prod_seq^lamd_b)*b^(1-c)*c^c) + log(K)
B_g <- (1/(1-b-c))*log((prod_seq^lamd_g)*b^(1-c)*c^c) + log(K)
C_b <- (1/(1-b-c))*log((prod_seq^lamd_b)*b^(b)*c^(1-b)) + log(K)
C_g <- (1/(1-b-c))*log((prod_seq^lamd_g)*b^(b)*c^(1-b)) + log(K)
F_b <- log((1/(d*c_b))^(1/(d-1)) * lc)
F_g <- log((1/(d*c_g))^(1/(d-1)) * lc)

p_g <- exp((para_A * A + para_B * B_g + para_CF*(C_g - F_g))/para_DM)
p_b <- exp((para_A * A + para_B * B_b + para_CF*(C_b - F_b))/para_DM)

green_ratio <- (p_g)^d / c_g
brown_ratio <- (p_b)^d / c_b

p_ratio <- p_g/p_b

ggplot() +
  geom_line(aes(x = prod_seq, y= green_ratio), color = "red") +
  geom_line(aes(x = prod_seq, y= brown_ratio), color = "blue")

for (i in 1:length(prod_seq)){
  cat('(', prod_seq[i], ",", brown_ratio[i], ')', sep="")
}

for (i in 1:length(prod_seq)){
  cat('(', prod_seq[i], ",", green_ratio[i], ')', sep="")
}



lamd_seq_b <- seq(1,1.5, by=0.025)
lamd_seq_g <- .8* lamd_seq_b * lamd_seq_b



A <- log((theta/V)^(1/a)*H*(1-a)^(1-a))
B_b <- (1/(1-b-c))*log(prod*lamd_seq_b*b^(1-c)*c^c) + log(K)
B_g <- (1/(1-b-c))*log(prod*lamd_seq_g*b^(1-c)*c^c) + log(K)
C_b <- (1/(1-b-c))*log(prod*lamd_seq_b*b^(b)*c^(1-b)) + log(K)
C_g <- (1/(1-b-c))*log(prod*lamd_seq_g*b^(b)*c^(1-b)) + log(K)
F_b <- log((1/(d*c_b))^(1/(d-1)) * lc)
F_g <- log((1/(d*c_g))^(1/(d-1)) * lc)

p_g <- exp((para_A * A + para_B * B_g + para_CF*(C_g - F_g))/para_DM)
p_b <- exp((para_A * A + para_B * B_b + para_CF*(C_b - F_b))/para_DM)

green_ratio <- (p_g)^d / c_g
brown_ratio <- (p_b)^d / c_b

l_ratio <- lamd_seq_g / lamd_seq_b

ggplot() +
  geom_line(aes(x = l_ratio, y= green_ratio), color = "red") +
  geom_line(aes(x = l_ratio, y= brown_ratio), color = "blue")

for (i in 1:length(l_ratio)){
  cat('(', l_ratio[i], ",", brown_ratio[i], ')', sep="")
}

for (i in 1:length(l_ratio)){
  cat('(', l_ratio[i], ",", green_ratio[i], ')', sep="")
}

