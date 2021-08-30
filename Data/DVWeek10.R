# Presentation 10 Script - Revised Simulations
# Spellman Fellows
# Evan Perry

# Parameters

library(tidyverse)

beta <- .6
gamma <- .2
alpha <- .2
delta <- 1.25
lambda1g <- .11
lambda1b <- .1
lambda2b <- .1
mu <- 0.3
mu1 <- 1 - mu
N <- 4000
sigma_i <- 1.02
Psi <- (N^sigma_i)
K <- 20
ki <- 1500
Phi <- (beta^beta * gamma^gamma)^(1/alpha) - (beta^(1-gamma)*gamma^gamma)^(1/alpha) 
  - (beta^beta * gamma^(1-beta))^(1/alpha)
cg <- 12
cb <- 10
lbar <- 100000


C1 <- log((beta^(1-gamma) * gamma^gamma)^(1/alpha) * K)
C2 <- log(Phi* lambda1g^(1/alpha) * (K/ki))
C3 <- log(Phi* lambda2b^(1/alpha) * (K/ki))
C4 <- log((beta^beta * gamma^(1-beta) * lambda1g)^(1/alpha) * K)
C5 <- log((beta^beta * gamma^(1-beta) * lambda2b)^(1/alpha) * K)
C6 <- log((delta * cb)^(-1/(delta -1)) * lbar)

a1 <- lambda1b^(1/alpha)
a2 <- lambda2b^(1/alpha)
a3 <- (cb/cg)^(1/delta)*(lambda1g)^(1/gamma)*(lambda1b^((beta -1)/gamma)*lambda2b)^(1/alpha)

muC1 <- log(a1*mu + a2*mu1)
muC2 <- log(a3*(mu1/mu) + 1)

N <- log(N)
Psi <- log(Psi)
mu <- log(mu)

# Finding Equilibrium Values

Rg <- exp((Psi + C2*alpha - C6*beta - C6*gamma - Psi*beta - Psi*gamma + beta*muC2 
       + gamma*muC2 + C6*beta^2 + C6*gamma^2 - beta^2*muC2 - gamma^2*muC2 
       - C1*alpha*beta - 2*C2*alpha*beta + C3*alpha*beta + C4*alpha*beta + 
         C6*beta*delta - C2*alpha*gamma + 2*C6*beta*gamma + C6*delta*gamma + 
         N*alpha*beta + alpha*beta*mu - alpha*beta*muC1 - beta*delta*muC2 - 
         2*beta*gamma*muC2 - delta*gamma*muC2 - C6*beta^2*delta - C6*delta*gamma^2 + 
         beta^2*delta*muC2 + delta*gamma^2*muC2 - 2*C6*beta*delta*gamma + 
         2*beta*delta*gamma*muC2)/(alpha*beta - gamma - beta + beta*delta + 
         2*beta*gamma + delta*gamma - beta^2*delta - delta*gamma^2 + beta^2 
         + gamma^2 - 2*beta*delta*gamma))


Rb <- exp(-(C2*beta - C3*beta + C2*gamma - C3*gamma - Psi*gamma - 2*C2*beta^2 
            + C2*beta^3 + 2*C3*beta^2 - C3*beta^3 - C2*gamma^2 + C3*gamma^2 
            - C4*gamma^2 + C4*gamma^3 + C5*gamma^2 - C5*gamma^3 + C6*gamma^2 
            - C6*gamma^3 + Psi*gamma^2 - gamma^2*mu + gamma^2*mu1 + gamma^3*mu 
            - gamma^3*mu1 - gamma^2*muC2 + gamma^3*muC2 - C2*alpha*beta 
            + C3*alpha*beta - C2*beta*delta + C3*beta*delta - C2*alpha*gamma 
            - 3*C2*beta*gamma + 3*C3*beta*gamma - C4*beta*gamma + C5*beta*gamma 
            + C6*beta*gamma - C2*delta*gamma + C3*delta*gamma + Psi*beta*gamma 
            - beta*gamma*mu + beta*gamma*mu1 - beta*gamma*muC2 + C2*alpha*beta^2 
            - C3*alpha*beta^2 + 2*C2*beta^2*delta - C2*beta^3*delta - 2*C3*beta^2*delta 
            + C3*beta^3*delta + C2*alpha*gamma^2 + C2*beta*gamma^2 + 2*C2*beta^2*gamma 
            - C3*beta*gamma^2 - 2*C3*beta^2*gamma + 2*C4*beta*gamma^2 + C4*beta^2*gamma 
            - 2*C5*beta*gamma^2 - C5*beta^2*gamma - 2*C6*beta*gamma^2 - C6*beta^2*gamma 
            + C2*delta*gamma^2 - C3*delta*gamma^2 + C4*delta*gamma^2 - C4*delta*gamma^3 
            - C5*delta*gamma^2 + C5*delta*gamma^3 - C6*delta*gamma^2 + C6*delta*gamma^3 
            + 2*beta*gamma^2*mu + beta^2*gamma*mu - 2*beta*gamma^2*mu1 - beta^2*gamma*mu1 
            + 2*beta*gamma^2*muC2 + beta^2*gamma*muC2 + delta*gamma^2*mu 
            - delta*gamma^2*mu1 - delta*gamma^3*mu + delta*gamma^3*mu1 
            + delta*gamma^2*muC2 - delta*gamma^3*muC2 - C2*beta*delta*gamma^2 
            - 2*C2*beta^2*delta*gamma + C3*beta*delta*gamma^2 + 2*C3*beta^2*delta*gamma 
            - 2*C4*beta*delta*gamma^2 - C4*beta^2*delta*gamma + 2*C5*beta*delta*gamma^2 
            + C5*beta^2*delta*gamma + 2*C6*beta*delta*gamma^2 + C6*beta^2*delta*gamma 
            - 2*beta*delta*gamma^2*mu - beta^2*delta*gamma*mu + 2*beta*delta*gamma^2*mu1 
            + beta^2*delta*gamma*mu1 - 2*beta*delta*gamma^2*muC2 - beta^2*delta*gamma*muC2 
            + C1*alpha*beta*gamma + 2*C2*alpha*beta*gamma - C3*alpha*beta*gamma 
            - C5*alpha*beta*gamma + 3*C2*beta*delta*gamma - 3*C3*beta*delta*gamma 
            + C4*beta*delta*gamma - C5*beta*delta*gamma - C6*beta*delta*gamma 
            - N*alpha*beta*gamma - alpha*beta*gamma*mu1 + alpha*beta*gamma*muC1 
            + beta*delta*gamma*mu - beta*delta*gamma*mu1 + beta*delta*gamma*muC2)/
            (gamma*(alpha*beta - gamma - beta + beta*delta + 2*beta*gamma + delta*gamma 
            - beta^2*delta - delta*gamma^2 + beta^2 + gamma^2 - 2*beta*delta*gamma)))


M <- exp(-(Psi + C2*alpha - Psi*alpha - 2*Psi*beta - Psi*delta - 2*Psi*gamma - 
         C2*alpha^2 + Psi*beta^2 + Psi*gamma^2 - C1*alpha*beta - 2*C2*alpha*beta + 
         C3*alpha*beta - C2*alpha*delta - C2*alpha*gamma - C4*alpha*gamma + C6*alpha*gamma + 
         N*alpha*beta + Psi*alpha*beta + 2*Psi*beta*delta + Psi*alpha*gamma + 2*Psi*beta*gamma +
         2*Psi*delta*gamma - alpha*beta*muC1 - alpha*gamma*mu - alpha*gamma*muC2 + C1*alpha*beta^2 +
         C1*alpha^2*beta + C2*alpha*beta^2 + C2*alpha^2*beta - C3*alpha*beta^2 - 
         C3*alpha^2*beta + C2*alpha^2*gamma + C4*alpha*gamma^2 - C6*alpha*gamma^2 - 
         N*alpha*beta^2 - N*alpha^2*beta - Psi*beta^2*delta - Psi*delta*gamma^2 + 
         alpha*beta^2*muC1 + alpha^2*beta*muC1 + alpha*gamma^2*mu + alpha*gamma^2*muC2 - 
         C1*alpha*beta^2*delta - C2*alpha*beta^2*delta + C3*alpha*beta^2*delta - 
         C4*alpha*delta*gamma^2 + C6*alpha*delta*gamma^2 + N*alpha*beta^2*delta - 
         alpha*beta^2*delta*muC1 - alpha*delta*gamma^2*mu - alpha*delta*gamma^2*muC2 +
         C1*alpha*beta*delta + 2*C2*alpha*beta*delta - C3*alpha*beta*delta + 
         C1*alpha*beta*gamma + C2*alpha*beta*gamma - C3*alpha*beta*gamma + 
         C4*alpha*beta*gamma - C6*alpha*beta*gamma + C2*alpha*delta*gamma + 
         C4*alpha*delta*gamma - C6*alpha*delta*gamma - N*alpha*beta*delta - 
         N*alpha*beta*gamma - 2*Psi*beta*delta*gamma + alpha*beta*delta*muC1 + 
         alpha*beta*gamma*mu + alpha*beta*gamma*muC1 + alpha*beta*gamma*muC2 + 
         alpha*delta*gamma*mu + alpha*delta*gamma*muC2 - C1*alpha*beta*delta*gamma - 
         C2*alpha*beta*delta*gamma + C3*alpha*beta*delta*gamma - C4*alpha*beta*delta*gamma + 
         C6*alpha*beta*delta*gamma + N*alpha*beta*delta*gamma - alpha*beta*delta*gamma*mu - 
         alpha*beta*delta*gamma*muC1 - alpha*beta*delta*gamma*muC2)/(alpha*(alpha*beta 
         - gamma - beta + beta*delta + 2*beta*gamma + delta*gamma 
         - beta^2*delta - delta*gamma^2 + beta^2 + gamma^2 - 2*beta*delta*gamma)))

W <- exp((Psi*alpha - C2*alpha - Psi + Psi*beta + Psi*delta + Psi*gamma + C2*alpha^2 
          + C2*alpha*beta + C2*alpha*delta - C1*alpha*gamma + C3*alpha*gamma 
          + C4*alpha*gamma - C6*alpha*gamma + N*alpha*gamma - Psi*beta*delta 
          - Psi*delta*gamma + alpha*gamma*mu - alpha*gamma*muC1 + alpha*gamma*muC2 
          - C2*alpha*beta*delta + C1*alpha*delta*gamma - C3*alpha*delta*gamma 
          - C4*alpha*delta*gamma + C6*alpha*delta*gamma - N*alpha*delta*gamma 
          - alpha*delta*gamma*mu + alpha*delta*gamma*muC1 - alpha*delta*gamma*muC2)/
           (alpha*beta - gamma - beta + beta*delta + 2*beta*gamma + delta*gamma 
            - beta^2*delta - delta*gamma^2 + beta^2 + gamma^2 - 2*beta*delta*gamma))

pb <- exp(-(Psi*gamma - Psi*gamma^2 - C2*alpha*beta + C3*alpha*beta + C3*alpha*gamma 
            - Psi*beta*gamma - Psi*delta*gamma + C2*alpha*beta^2 + C2*alpha^2*beta 
            - C3*alpha*beta^2 - C3*alpha^2*beta - C3*alpha*gamma^2 + Psi*delta*gamma^2 
            - C2*alpha*beta^2*delta + C3*alpha*beta^2*delta + C3*alpha*delta*gamma^2 
            + C2*alpha*beta*delta - C3*alpha*beta*delta - C1*alpha*beta*gamma 
            - C3*alpha*beta*gamma + C4*alpha*beta*gamma - C6*alpha*beta*gamma 
            - C3*alpha*delta*gamma + N*alpha*beta*gamma + Psi*beta*delta*gamma 
            + alpha*beta*gamma*mu - alpha*beta*gamma*muC1 + alpha*beta*gamma*muC2 
            + C1*alpha*beta*delta*gamma + C3*alpha*beta*delta*gamma - C4*alpha*beta*delta*gamma 
            + C6*alpha*beta*delta*gamma - N*alpha*beta*delta*gamma - alpha*beta*delta*gamma*mu 
            + alpha*beta*delta*gamma*muC1 - alpha*beta*delta*gamma*muC2)/
            (gamma*(alpha*beta - gamma - beta + beta*delta + 2*beta*gamma 
                    + delta*gamma - beta^2*delta - delta*gamma^2 + beta^2 
                    + gamma^2 - 2*beta*delta*gamma)))

pg <- exp(-(Psi + C2*alpha - Psi*beta - Psi*delta - Psi*gamma - C1*alpha*beta 
            - 2*C2*alpha*beta + C3*alpha*beta + C4*alpha*beta - C6*alpha*beta 
            - C2*alpha*delta - C2*alpha*gamma + N*alpha*beta + Psi*beta*delta 
            + Psi*delta*gamma + alpha*beta*mu - alpha*beta*muC1 + alpha*beta*muC2 
            + C1*alpha*beta*delta + 2*C2*alpha*beta*delta - C3*alpha*beta*delta 
            - C4*alpha*beta*delta + C6*alpha*beta*delta + C2*alpha*delta*gamma 
            - N*alpha*beta*delta - alpha*beta*delta*mu + alpha*beta*delta*muC1 
            - alpha*beta*delta*muC2)/(alpha*beta - gamma - beta + beta*delta 
            + 2*beta*gamma + delta*gamma - beta^2*delta - delta*gamma^2 + beta^2 
            + gamma^2 - 2*beta*delta*gamma))


# Pi <- log(Phi) + (1/alpha)*(Psi + log(lambda1g) - gamma*pg - beta*W) + log(K) - log(ki)
# exp(Pi)
# Yay!





# Sim 1: How do endogenous variables change with N for given sector

Nseq <- seq(500, 10000, by = 500)
seqRg <- rep(0, length(Nseq))
seqRb <- rep(0, length(Nseq))
seqpg <- rep(0, length(Nseq))
seqpb <- rep(0, length(Nseq))
seqM <- rep(0, length(Nseq))
seqW <- rep(0, length(Nseq))


for (i in 1:length(Nseq)){
  
  beta <- .6
  gamma <- .2
  alpha <- .2
  delta <- 1.25
  lambda1g <- 1.01
  lambda1b <- 1
  lambda2b <- 1
  mu <- 0.3
  mu1 <- 1 - mu
  sigma_i <- 1.02
  Psi <- 1.5*Nseq[i]^sigma_i
  K <- 20
  ki <- 10000
  Phi <- (beta^beta * gamma^gamma)^(1/alpha) - (beta^(1-gamma)*gamma^gamma)^(1/alpha) - (beta^beta * gamma^(1-beta))^(1/alpha)
  cg <- 12
  cb <- 10
  lbar <- 100000
  
  C1 <- log((beta^(1-gamma) * gamma^gamma)^(1/alpha) * K)
  C2 <- log(Phi* lambda1g^(1/alpha) * (K/ki))
  C3 <- log(Phi* lambda2b^(1/alpha) * (K/ki))
  C4 <- log((beta^beta * gamma^(1-beta) * lambda1g)^(1/alpha) * K)
  C5 <- log((beta^beta * gamma^(1-beta) * lambda2b)^(1/alpha) * K)
  C6 <- log((delta * cb)^(-1/(delta -1)) * lbar)
  
  a1 <- lambda1b^(1/alpha)
  a2 <- lambda2b^(1/alpha)
  a3 <- (cb/cg)^(1/delta)*(lambda1g)^(1/gamma)*(lambda1b^((beta -1)/gamma)*lambda2b)^(1/alpha)
  
  muC1 <- log(a1*mu + a2*mu1)
  muC2 <- log(a3*(mu1/mu) + 1)
  
  Nseq[i] <- log(Nseq[i])
  Psi <- log(Psi)
  mu <- log(mu)
  
  seqRg[i] <- exp((Psi + C2*alpha - C6*beta - C6*gamma - Psi*beta - Psi*gamma + beta*muC2 
             + gamma*muC2 + C6*beta^2 + C6*gamma^2 - beta^2*muC2 - gamma^2*muC2 
             - C1*alpha*beta - 2*C2*alpha*beta + C3*alpha*beta + C4*alpha*beta + 
               C6*beta*delta - C2*alpha*gamma + 2*C6*beta*gamma + C6*delta*gamma + 
               Nseq[i]*alpha*beta + alpha*beta*mu - alpha*beta*muC1 - beta*delta*muC2 - 
               2*beta*gamma*muC2 - delta*gamma*muC2 - C6*beta^2*delta - C6*delta*gamma^2 + 
               beta^2*delta*muC2 + delta*gamma^2*muC2 - 2*C6*beta*delta*gamma + 
               2*beta*delta*gamma*muC2)/(alpha*beta - gamma - beta + beta*delta + 
                                           2*beta*gamma + delta*gamma - beta^2*delta - delta*gamma^2 + beta^2 
                                         + gamma^2 - 2*beta*delta*gamma))
  
  
  seqRb[i] <- exp(-(C2*beta - C3*beta + C2*gamma - C3*gamma - Psi*gamma - 2*C2*beta^2 
              + C2*beta^3 + 2*C3*beta^2 - C3*beta^3 - C2*gamma^2 + C3*gamma^2 
              - C4*gamma^2 + C4*gamma^3 + C5*gamma^2 - C5*gamma^3 + C6*gamma^2 
              - C6*gamma^3 + Psi*gamma^2 - gamma^2*mu + gamma^2*mu1 + gamma^3*mu 
              - gamma^3*mu1 - gamma^2*muC2 + gamma^3*muC2 - C2*alpha*beta 
              + C3*alpha*beta - C2*beta*delta + C3*beta*delta - C2*alpha*gamma 
              - 3*C2*beta*gamma + 3*C3*beta*gamma - C4*beta*gamma + C5*beta*gamma 
              + C6*beta*gamma - C2*delta*gamma + C3*delta*gamma + Psi*beta*gamma 
              - beta*gamma*mu + beta*gamma*mu1 - beta*gamma*muC2 + C2*alpha*beta^2 
              - C3*alpha*beta^2 + 2*C2*beta^2*delta - C2*beta^3*delta - 2*C3*beta^2*delta 
              + C3*beta^3*delta + C2*alpha*gamma^2 + C2*beta*gamma^2 + 2*C2*beta^2*gamma 
              - C3*beta*gamma^2 - 2*C3*beta^2*gamma + 2*C4*beta*gamma^2 + C4*beta^2*gamma 
              - 2*C5*beta*gamma^2 - C5*beta^2*gamma - 2*C6*beta*gamma^2 - C6*beta^2*gamma 
              + C2*delta*gamma^2 - C3*delta*gamma^2 + C4*delta*gamma^2 - C4*delta*gamma^3 
              - C5*delta*gamma^2 + C5*delta*gamma^3 - C6*delta*gamma^2 + C6*delta*gamma^3 
              + 2*beta*gamma^2*mu + beta^2*gamma*mu - 2*beta*gamma^2*mu1 - beta^2*gamma*mu1 
              + 2*beta*gamma^2*muC2 + beta^2*gamma*muC2 + delta*gamma^2*mu 
              - delta*gamma^2*mu1 - delta*gamma^3*mu + delta*gamma^3*mu1 
              + delta*gamma^2*muC2 - delta*gamma^3*muC2 - C2*beta*delta*gamma^2 
              - 2*C2*beta^2*delta*gamma + C3*beta*delta*gamma^2 + 2*C3*beta^2*delta*gamma 
              - 2*C4*beta*delta*gamma^2 - C4*beta^2*delta*gamma + 2*C5*beta*delta*gamma^2 
              + C5*beta^2*delta*gamma + 2*C6*beta*delta*gamma^2 + C6*beta^2*delta*gamma 
              - 2*beta*delta*gamma^2*mu - beta^2*delta*gamma*mu + 2*beta*delta*gamma^2*mu1 
              + beta^2*delta*gamma*mu1 - 2*beta*delta*gamma^2*muC2 - beta^2*delta*gamma*muC2 
              + C1*alpha*beta*gamma + 2*C2*alpha*beta*gamma - C3*alpha*beta*gamma 
              - C5*alpha*beta*gamma + 3*C2*beta*delta*gamma - 3*C3*beta*delta*gamma 
              + C4*beta*delta*gamma - C5*beta*delta*gamma - C6*beta*delta*gamma 
              - Nseq[i]*alpha*beta*gamma - alpha*beta*gamma*mu1 + alpha*beta*gamma*muC1 
              + beta*delta*gamma*mu - beta*delta*gamma*mu1 + beta*delta*gamma*muC2)/
              (gamma*(alpha*beta - gamma - beta + beta*delta + 2*beta*gamma + delta*gamma 
                      - beta^2*delta - delta*gamma^2 + beta^2 + gamma^2 - 2*beta*delta*gamma)))
  
  
  seqM[i] <- exp(-(Psi + C2*alpha - Psi*alpha - 2*Psi*beta - Psi*delta - 2*Psi*gamma - 
               C2*alpha^2 + Psi*beta^2 + Psi*gamma^2 - C1*alpha*beta - 2*C2*alpha*beta + 
               C3*alpha*beta - C2*alpha*delta - C2*alpha*gamma - C4*alpha*gamma + C6*alpha*gamma + 
               Nseq[i]*alpha*beta + Psi*alpha*beta + 2*Psi*beta*delta + Psi*alpha*gamma + 2*Psi*beta*gamma +
               2*Psi*delta*gamma - alpha*beta*muC1 - alpha*gamma*mu - alpha*gamma*muC2 + C1*alpha*beta^2 +
               C1*alpha^2*beta + C2*alpha*beta^2 + C2*alpha^2*beta - C3*alpha*beta^2 - 
               C3*alpha^2*beta + C2*alpha^2*gamma + C4*alpha*gamma^2 - C6*alpha*gamma^2 - 
               Nseq[i]*alpha*beta^2 - Nseq[i]*alpha^2*beta - Psi*beta^2*delta - Psi*delta*gamma^2 + 
               alpha*beta^2*muC1 + alpha^2*beta*muC1 + alpha*gamma^2*mu + alpha*gamma^2*muC2 - 
               C1*alpha*beta^2*delta - C2*alpha*beta^2*delta + C3*alpha*beta^2*delta - 
               C4*alpha*delta*gamma^2 + C6*alpha*delta*gamma^2 + Nseq[i]*alpha*beta^2*delta - 
               alpha*beta^2*delta*muC1 - alpha*delta*gamma^2*mu - alpha*delta*gamma^2*muC2 +
               C1*alpha*beta*delta + 2*C2*alpha*beta*delta - C3*alpha*beta*delta + 
               C1*alpha*beta*gamma + C2*alpha*beta*gamma - C3*alpha*beta*gamma + 
               C4*alpha*beta*gamma - C6*alpha*beta*gamma + C2*alpha*delta*gamma + 
               C4*alpha*delta*gamma - C6*alpha*delta*gamma - Nseq[i]*alpha*beta*delta - 
               Nseq[i]*alpha*beta*gamma - 2*Psi*beta*delta*gamma + alpha*beta*delta*muC1 + 
               alpha*beta*gamma*mu + alpha*beta*gamma*muC1 + alpha*beta*gamma*muC2 + 
               alpha*delta*gamma*mu + alpha*delta*gamma*muC2 - C1*alpha*beta*delta*gamma - 
               C2*alpha*beta*delta*gamma + C3*alpha*beta*delta*gamma - C4*alpha*beta*delta*gamma + 
               C6*alpha*beta*delta*gamma + Nseq[i]*alpha*beta*delta*gamma - alpha*beta*delta*gamma*mu - 
               alpha*beta*delta*gamma*muC1 - alpha*beta*delta*gamma*muC2)/(alpha*(alpha*beta - gamma - beta + beta*delta + 2*beta*gamma + delta*gamma - 
                                                                                    beta^2*delta - delta*gamma^2 + beta^2 + gamma^2 - 2*beta*delta*gamma)))
  
  seqW[i] <- exp((Psi*alpha - C2*alpha - Psi + Psi*beta + Psi*delta + Psi*gamma + C2*alpha^2 
            + C2*alpha*beta + C2*alpha*delta - C1*alpha*gamma + C3*alpha*gamma 
            + C4*alpha*gamma - C6*alpha*gamma + Nseq[i]*alpha*gamma - Psi*beta*delta 
            - Psi*delta*gamma + alpha*gamma*mu - alpha*gamma*muC1 + alpha*gamma*muC2 
            - C2*alpha*beta*delta + C1*alpha*delta*gamma - C3*alpha*delta*gamma 
            - C4*alpha*delta*gamma + C6*alpha*delta*gamma - Nseq[i]*alpha*delta*gamma 
            - alpha*delta*gamma*mu + alpha*delta*gamma*muC1 - alpha*delta*gamma*muC2)/
             (alpha*beta - gamma - beta + beta*delta + 2*beta*gamma + delta*gamma 
              - beta^2*delta - delta*gamma^2 + beta^2 + gamma^2 - 2*beta*delta*gamma))
  
  seqpb[i] <- exp(-(Psi*gamma - Psi*gamma^2 - C2*alpha*beta + C3*alpha*beta + C3*alpha*gamma 
              - Psi*beta*gamma - Psi*delta*gamma + C2*alpha*beta^2 + C2*alpha^2*beta 
              - C3*alpha*beta^2 - C3*alpha^2*beta - C3*alpha*gamma^2 + Psi*delta*gamma^2 
              - C2*alpha*beta^2*delta + C3*alpha*beta^2*delta + C3*alpha*delta*gamma^2 
              + C2*alpha*beta*delta - C3*alpha*beta*delta - C1*alpha*beta*gamma 
              - C3*alpha*beta*gamma + C4*alpha*beta*gamma - C6*alpha*beta*gamma 
              - C3*alpha*delta*gamma + Nseq[i]*alpha*beta*gamma + Psi*beta*delta*gamma 
              + alpha*beta*gamma*mu - alpha*beta*gamma*muC1 + alpha*beta*gamma*muC2 
              + C1*alpha*beta*delta*gamma + C3*alpha*beta*delta*gamma - C4*alpha*beta*delta*gamma 
              + C6*alpha*beta*delta*gamma - Nseq[i]*alpha*beta*delta*gamma - alpha*beta*delta*gamma*mu 
              + alpha*beta*delta*gamma*muC1 - alpha*beta*delta*gamma*muC2)/
              (gamma*(alpha*beta - gamma - beta + beta*delta + 2*beta*gamma 
                      + delta*gamma - beta^2*delta - delta*gamma^2 + beta^2 
                      + gamma^2 - 2*beta*delta*gamma)))
  
  seqpg[i] <- exp(-(Psi + C2*alpha - Psi*beta - Psi*delta - Psi*gamma - C1*alpha*beta 
              - 2*C2*alpha*beta + C3*alpha*beta + C4*alpha*beta - C6*alpha*beta 
              - C2*alpha*delta - C2*alpha*gamma + Nseq[i]*alpha*beta + Psi*beta*delta 
              + Psi*delta*gamma + alpha*beta*mu - alpha*beta*muC1 + alpha*beta*muC2 
              + C1*alpha*beta*delta + 2*C2*alpha*beta*delta - C3*alpha*beta*delta 
              - C4*alpha*beta*delta + C6*alpha*beta*delta + C2*alpha*delta*gamma 
              - Nseq[i]*alpha*beta*delta - alpha*beta*delta*mu + alpha*beta*delta*muC1 
              - alpha*beta*delta*muC2)/(alpha*beta - gamma - beta + beta*delta 
                                        + 2*beta*gamma + delta*gamma - beta^2*delta - delta*gamma^2 + beta^2 
                                        + gamma^2 - 2*beta*delta*gamma))
}

seqN <- exp(Nseq) *.001

df1 <- tibble(seqN, seqM, seqW, seqpb, seqpg, seqRb, seqRg)

ggplot(df1) +
  geom_line(aes(x = seqN, y= seqpb)) +
  geom_line(aes(x = seqN, y= seqpg))

ggplot(df1) +
  geom_line(aes(x = seqN, y= seqRg)) +
  geom_line(aes(x = seqN, y= seqRb))


for (i in 1:length(Nseq)){
  cat('(', seqN[i], ",", seqpb[i], ')', sep="")
}

for (i in 1:length(Nseq)){
  cat('(', seqN[i], ",", seqpg[i], ')', sep="")
}



for (i in 1:length(Nseq)){
  cat('(', seqN[i], ",", seqRg[i], ')', sep="")
}

for (i in 1:length(Nseq)){
  cat('(', seqN[i], ",", seqRb[i], ')', sep="")
}



for (i in 1:length(Nseq)){
  cat('(', seqN[i], ",", seqRg[i]/(seqRb[i] + seqRg[i]), ')', sep="")
}

for (i in 1:length(Nseq)){
  cat('(', seqN[i], ",", seqRb[i]/(seqRb[i] + seqRg[i]), ')', sep="")
}





Nseq <- seq(500, 10000, by = 500)
seqRg2 <- rep(0, length(Nseq))
seqRb2 <- rep(0, length(Nseq))
seqpg2 <- rep(0, length(Nseq))
seqpb2 <- rep(0, length(Nseq))
seqM2 <- rep(0, length(Nseq))
seqW2 <- rep(0, length(Nseq))


for (i in 1:length(Nseq)){
  
  beta <- .6
  gamma <- .2
  alpha <- .2
  delta <- 1.25
  lambda1g <- 1.01
  lambda1b <- 1
  lambda2b <- 1
  mu <- 0.6
  mu1 <- 1 - mu
  sigma_i <- 1.25
  Psi <- 2*Nseq[i]^sigma_i
  K <- 20
  ki <- 700000000
  Phi <- (beta^beta * gamma^gamma)^(1/alpha) - (beta^(1-gamma)*gamma^gamma)^(1/alpha) - (beta^beta * gamma^(1-beta))^(1/alpha)
  cg <- 12
  cb <- 10
  lbar <- 100000
  
  C1 <- log((beta^(1-gamma) * gamma^gamma)^(1/alpha) * K)
  C2 <- log(Phi* lambda1g^(1/alpha) * (K/ki))
  C3 <- log(Phi* lambda2b^(1/alpha) * (K/ki))
  C4 <- log((beta^beta * gamma^(1-beta) * lambda1g)^(1/alpha) * K)
  C5 <- log((beta^beta * gamma^(1-beta) * lambda2b)^(1/alpha) * K)
  C6 <- log((delta * cb)^(-1/(delta -1)) * lbar)
  
  a1 <- lambda1b^(1/alpha)
  a2 <- lambda2b^(1/alpha)
  a3 <- (cb/cg)^(1/delta)*(lambda1g)^(1/gamma)*(lambda1b^((beta -1)/gamma)*lambda2b)^(1/alpha)
  
  muC1 <- log(a1*mu + a2*mu1)
  muC2 <- log(a3*(mu1/mu) + 1)
  
  Nseq[i] <- log(Nseq[i])
  Psi <- log(Psi)
  mu <- log(mu)
  
  seqRg2[i] <- exp((Psi + C2*alpha - C6*beta - C6*gamma - Psi*beta - Psi*gamma + beta*muC2 
                   + gamma*muC2 + C6*beta^2 + C6*gamma^2 - beta^2*muC2 - gamma^2*muC2 
                   - C1*alpha*beta - 2*C2*alpha*beta + C3*alpha*beta + C4*alpha*beta + 
                     C6*beta*delta - C2*alpha*gamma + 2*C6*beta*gamma + C6*delta*gamma + 
                     Nseq[i]*alpha*beta + alpha*beta*mu - alpha*beta*muC1 - beta*delta*muC2 - 
                     2*beta*gamma*muC2 - delta*gamma*muC2 - C6*beta^2*delta - C6*delta*gamma^2 + 
                     beta^2*delta*muC2 + delta*gamma^2*muC2 - 2*C6*beta*delta*gamma + 
                     2*beta*delta*gamma*muC2)/(alpha*beta - gamma - beta + beta*delta + 
                                                 2*beta*gamma + delta*gamma - beta^2*delta - delta*gamma^2 + beta^2 
                                               + gamma^2 - 2*beta*delta*gamma))
  
  
  seqRb2[i] <- exp(-(C2*beta - C3*beta + C2*gamma - C3*gamma - Psi*gamma - 2*C2*beta^2 
                    + C2*beta^3 + 2*C3*beta^2 - C3*beta^3 - C2*gamma^2 + C3*gamma^2 
                    - C4*gamma^2 + C4*gamma^3 + C5*gamma^2 - C5*gamma^3 + C6*gamma^2 
                    - C6*gamma^3 + Psi*gamma^2 - gamma^2*mu + gamma^2*mu1 + gamma^3*mu 
                    - gamma^3*mu1 - gamma^2*muC2 + gamma^3*muC2 - C2*alpha*beta 
                    + C3*alpha*beta - C2*beta*delta + C3*beta*delta - C2*alpha*gamma 
                    - 3*C2*beta*gamma + 3*C3*beta*gamma - C4*beta*gamma + C5*beta*gamma 
                    + C6*beta*gamma - C2*delta*gamma + C3*delta*gamma + Psi*beta*gamma 
                    - beta*gamma*mu + beta*gamma*mu1 - beta*gamma*muC2 + C2*alpha*beta^2 
                    - C3*alpha*beta^2 + 2*C2*beta^2*delta - C2*beta^3*delta - 2*C3*beta^2*delta 
                    + C3*beta^3*delta + C2*alpha*gamma^2 + C2*beta*gamma^2 + 2*C2*beta^2*gamma 
                    - C3*beta*gamma^2 - 2*C3*beta^2*gamma + 2*C4*beta*gamma^2 + C4*beta^2*gamma 
                    - 2*C5*beta*gamma^2 - C5*beta^2*gamma - 2*C6*beta*gamma^2 - C6*beta^2*gamma 
                    + C2*delta*gamma^2 - C3*delta*gamma^2 + C4*delta*gamma^2 - C4*delta*gamma^3 
                    - C5*delta*gamma^2 + C5*delta*gamma^3 - C6*delta*gamma^2 + C6*delta*gamma^3 
                    + 2*beta*gamma^2*mu + beta^2*gamma*mu - 2*beta*gamma^2*mu1 - beta^2*gamma*mu1 
                    + 2*beta*gamma^2*muC2 + beta^2*gamma*muC2 + delta*gamma^2*mu 
                    - delta*gamma^2*mu1 - delta*gamma^3*mu + delta*gamma^3*mu1 
                    + delta*gamma^2*muC2 - delta*gamma^3*muC2 - C2*beta*delta*gamma^2 
                    - 2*C2*beta^2*delta*gamma + C3*beta*delta*gamma^2 + 2*C3*beta^2*delta*gamma 
                    - 2*C4*beta*delta*gamma^2 - C4*beta^2*delta*gamma + 2*C5*beta*delta*gamma^2 
                    + C5*beta^2*delta*gamma + 2*C6*beta*delta*gamma^2 + C6*beta^2*delta*gamma 
                    - 2*beta*delta*gamma^2*mu - beta^2*delta*gamma*mu + 2*beta*delta*gamma^2*mu1 
                    + beta^2*delta*gamma*mu1 - 2*beta*delta*gamma^2*muC2 - beta^2*delta*gamma*muC2 
                    + C1*alpha*beta*gamma + 2*C2*alpha*beta*gamma - C3*alpha*beta*gamma 
                    - C5*alpha*beta*gamma + 3*C2*beta*delta*gamma - 3*C3*beta*delta*gamma 
                    + C4*beta*delta*gamma - C5*beta*delta*gamma - C6*beta*delta*gamma 
                    - Nseq[i]*alpha*beta*gamma - alpha*beta*gamma*mu1 + alpha*beta*gamma*muC1 
                    + beta*delta*gamma*mu - beta*delta*gamma*mu1 + beta*delta*gamma*muC2)/
                    (gamma*(alpha*beta - gamma - beta + beta*delta + 2*beta*gamma + delta*gamma 
                            - beta^2*delta - delta*gamma^2 + beta^2 + gamma^2 - 2*beta*delta*gamma)))
  
  
  seqM2[i] <- exp(-(Psi + C2*alpha - Psi*alpha - 2*Psi*beta - Psi*delta - 2*Psi*gamma - 
                     C2*alpha^2 + Psi*beta^2 + Psi*gamma^2 - C1*alpha*beta - 2*C2*alpha*beta + 
                     C3*alpha*beta - C2*alpha*delta - C2*alpha*gamma - C4*alpha*gamma + C6*alpha*gamma + 
                     Nseq[i]*alpha*beta + Psi*alpha*beta + 2*Psi*beta*delta + Psi*alpha*gamma + 2*Psi*beta*gamma +
                     2*Psi*delta*gamma - alpha*beta*muC1 - alpha*gamma*mu - alpha*gamma*muC2 + C1*alpha*beta^2 +
                     C1*alpha^2*beta + C2*alpha*beta^2 + C2*alpha^2*beta - C3*alpha*beta^2 - 
                     C3*alpha^2*beta + C2*alpha^2*gamma + C4*alpha*gamma^2 - C6*alpha*gamma^2 - 
                     Nseq[i]*alpha*beta^2 - Nseq[i]*alpha^2*beta - Psi*beta^2*delta - Psi*delta*gamma^2 + 
                     alpha*beta^2*muC1 + alpha^2*beta*muC1 + alpha*gamma^2*mu + alpha*gamma^2*muC2 - 
                     C1*alpha*beta^2*delta - C2*alpha*beta^2*delta + C3*alpha*beta^2*delta - 
                     C4*alpha*delta*gamma^2 + C6*alpha*delta*gamma^2 + Nseq[i]*alpha*beta^2*delta - 
                     alpha*beta^2*delta*muC1 - alpha*delta*gamma^2*mu - alpha*delta*gamma^2*muC2 +
                     C1*alpha*beta*delta + 2*C2*alpha*beta*delta - C3*alpha*beta*delta + 
                     C1*alpha*beta*gamma + C2*alpha*beta*gamma - C3*alpha*beta*gamma + 
                     C4*alpha*beta*gamma - C6*alpha*beta*gamma + C2*alpha*delta*gamma + 
                     C4*alpha*delta*gamma - C6*alpha*delta*gamma - Nseq[i]*alpha*beta*delta - 
                     Nseq[i]*alpha*beta*gamma - 2*Psi*beta*delta*gamma + alpha*beta*delta*muC1 + 
                     alpha*beta*gamma*mu + alpha*beta*gamma*muC1 + alpha*beta*gamma*muC2 + 
                     alpha*delta*gamma*mu + alpha*delta*gamma*muC2 - C1*alpha*beta*delta*gamma - 
                     C2*alpha*beta*delta*gamma + C3*alpha*beta*delta*gamma - C4*alpha*beta*delta*gamma + 
                     C6*alpha*beta*delta*gamma + Nseq[i]*alpha*beta*delta*gamma - alpha*beta*delta*gamma*mu - 
                     alpha*beta*delta*gamma*muC1 - alpha*beta*delta*gamma*muC2)/(alpha*(alpha*beta - gamma - beta + beta*delta + 2*beta*gamma + delta*gamma - 
                                                                                          beta^2*delta - delta*gamma^2 + beta^2 + gamma^2 - 2*beta*delta*gamma)))
  
  seqW2[i] <- exp((Psi*alpha - C2*alpha - Psi + Psi*beta + Psi*delta + Psi*gamma + C2*alpha^2 
                  + C2*alpha*beta + C2*alpha*delta - C1*alpha*gamma + C3*alpha*gamma 
                  + C4*alpha*gamma - C6*alpha*gamma + Nseq[i]*alpha*gamma - Psi*beta*delta 
                  - Psi*delta*gamma + alpha*gamma*mu - alpha*gamma*muC1 + alpha*gamma*muC2 
                  - C2*alpha*beta*delta + C1*alpha*delta*gamma - C3*alpha*delta*gamma 
                  - C4*alpha*delta*gamma + C6*alpha*delta*gamma - Nseq[i]*alpha*delta*gamma 
                  - alpha*delta*gamma*mu + alpha*delta*gamma*muC1 - alpha*delta*gamma*muC2)/
                   (alpha*beta - gamma - beta + beta*delta + 2*beta*gamma + delta*gamma 
                    - beta^2*delta - delta*gamma^2 + beta^2 + gamma^2 - 2*beta*delta*gamma))
  
  seqpb2[i] <- exp(-(Psi*gamma - Psi*gamma^2 - C2*alpha*beta + C3*alpha*beta + C3*alpha*gamma 
                    - Psi*beta*gamma - Psi*delta*gamma + C2*alpha*beta^2 + C2*alpha^2*beta 
                    - C3*alpha*beta^2 - C3*alpha^2*beta - C3*alpha*gamma^2 + Psi*delta*gamma^2 
                    - C2*alpha*beta^2*delta + C3*alpha*beta^2*delta + C3*alpha*delta*gamma^2 
                    + C2*alpha*beta*delta - C3*alpha*beta*delta - C1*alpha*beta*gamma 
                    - C3*alpha*beta*gamma + C4*alpha*beta*gamma - C6*alpha*beta*gamma 
                    - C3*alpha*delta*gamma + Nseq[i]*alpha*beta*gamma + Psi*beta*delta*gamma 
                    + alpha*beta*gamma*mu - alpha*beta*gamma*muC1 + alpha*beta*gamma*muC2 
                    + C1*alpha*beta*delta*gamma + C3*alpha*beta*delta*gamma - C4*alpha*beta*delta*gamma 
                    + C6*alpha*beta*delta*gamma - Nseq[i]*alpha*beta*delta*gamma - alpha*beta*delta*gamma*mu 
                    + alpha*beta*delta*gamma*muC1 - alpha*beta*delta*gamma*muC2)/
                    (gamma*(alpha*beta - gamma - beta + beta*delta + 2*beta*gamma 
                            + delta*gamma - beta^2*delta - delta*gamma^2 + beta^2 
                            + gamma^2 - 2*beta*delta*gamma)))
  
  seqpg2[i] <- exp(-(Psi + C2*alpha - Psi*beta - Psi*delta - Psi*gamma - C1*alpha*beta 
                    - 2*C2*alpha*beta + C3*alpha*beta + C4*alpha*beta - C6*alpha*beta 
                    - C2*alpha*delta - C2*alpha*gamma + Nseq[i]*alpha*beta + Psi*beta*delta 
                    + Psi*delta*gamma + alpha*beta*mu - alpha*beta*muC1 + alpha*beta*muC2 
                    + C1*alpha*beta*delta + 2*C2*alpha*beta*delta - C3*alpha*beta*delta 
                    - C4*alpha*beta*delta + C6*alpha*beta*delta + C2*alpha*delta*gamma 
                    - Nseq[i]*alpha*beta*delta - alpha*beta*delta*mu + alpha*beta*delta*muC1 
                    - alpha*beta*delta*muC2)/(alpha*beta - gamma - beta + beta*delta 
                                              + 2*beta*gamma + delta*gamma - beta^2*delta - delta*gamma^2 + beta^2 
                                              + gamma^2 - 2*beta*delta*gamma))
}

seqN2 <- exp(Nseq)

df2 <- tibble(seqN2, seqM2, seqW2, seqpb2, seqpg2, seqRb2, seqRg2)

df3 <- cbind(df1, df2)

ggplot(df3) +
  geom_line(aes(x = seqN, y= seqpb)) +
  geom_line(aes(x = seqN, y= seqpb2), color = 'red') +
  geom_line(aes(x = seqN, y= seqpg)) +
  geom_line(aes(x = seqN, y= seqpg2), color = 'red')


for (i in 1:length(Nseq)){
  cat('(', seqN[i]*.001, ",", seqpb[i], ')', sep="")
}

for (i in 1:length(Nseq)){
  cat('(', seqN[i]*.001, ",", seqpb2[i], ')', sep="")
}


for (i in 1:length(Nseq)){
  cat('(', seqN[i], ",", seqRg2[i]/(seqRb2[i] + seqRg2[i]), ')', sep="")
}

for (i in 1:length(Nseq)){
  cat('(', seqN[i], ",", seqRb2[i]/(seqRb2[i] + seqRg2[i]), ')', sep="")
}


# 500 to 4500 -- Low
# 5000 to 10000 -- High




