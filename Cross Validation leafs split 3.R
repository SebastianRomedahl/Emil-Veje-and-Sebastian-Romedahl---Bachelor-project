#OLS cross validation under 1
testCV_leafs_OLS_3_U1 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U1)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = U1[group != i, ])
      muhat <- predict(modelcv, newdata = U1[group == i, ])
      tmp[group == i] <- (U1$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_3_U1 <- testCV_leafs_OLS_3_U1(OLSLeafs_3_U1, U1)

#OLS cross validation between 1 and 2
testCV_leafs_OLS_3_U2 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = U2[group != i, ])
      muhat <- predict(modelcv, newdata = U2[group == i, ])
      tmp[group == i] <- (U2$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_3_U2 <- testCV_leafs_OLS_3_U2(OLSLeafs_3_U2, U2)

#OLS cross validation between 2 and 3
testCV_leafs_OLS_3_U3 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = U3[group != i, ])
      muhat <- predict(modelcv, newdata = U3[group == i, ])
      tmp[group == i] <- (U3$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_3_U3 <- testCV_leafs_OLS_3_U3(OLSLeafs_3_U3, U3)

#OLS cross validation between 3 and 5
testCV_leafs_OLS_3_U5 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U5)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = U5[group != i, ])
      muhat <- predict(modelcv, newdata = U5[group == i, ])
      tmp[group == i] <- (U5$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_3_U5 <- testCV_leafs_OLS_3_U5(OLSLeafs_3_U5, U5)

#OLS cross validation between 5 and 10
testCV_leafs_OLS_3_U10 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U10)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = U10[group != i, ])
      muhat <- predict(modelcv, newdata = U10[group == i, ])
      tmp[group == i] <- (U10$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_3_U10 <- testCV_leafs_OLS_3_U10(OLSLeafs_3_U10, U10)

#OLS cross validation between 10 and 20
testCV_leafs_OLS_3_U20 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U20)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = U20[group != i, ])
      muhat <- predict(modelcv, newdata = U20[group == i, ])
      tmp[group == i] <- (U20$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_3_U20 <- testCV_leafs_OLS_3_U20(OLSLeafs_3_U20, U20)

#OLS cross validation between 20 and 30
testCV_leafs_OLS_3_U30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U30)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = U30[group != i, ])
      muhat <- predict(modelcv, newdata = U30[group == i, ])
      tmp[group == i] <- (U30$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_3_U30 <- testCV_leafs_OLS_3_U30(OLSLeafs_3_U30, U30)

#OLS cross validation over 30
testCV_leafs_OLS_3_O30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(O30)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = O30[group != i, ])
      muhat <- predict(modelcv, newdata = O30[group == i, ])
      tmp[group == i] <- (O30$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_3_O30 <- testCV_leafs_OLS_3_O30(OLSLeafs_3_O30, O30)

##RMSE for all OLS groups
sqrt((MSE_Leafs_OLS_3_O30*nrow(O30)+MSE_Leafs_OLS_3_U30*nrow(U30)+MSE_Leafs_OLS_3_U20*nrow(U20)+MSE_Leafs_OLS_3_U10*nrow(U10)+MSE_Leafs_OLS_3_U5*nrow(U5)+MSE_Leafs_OLS_3_U3*nrow(U3)+MSE_Leafs_OLS_3_U2*nrow(U2)+MSE_Leafs_OLS_3_U1*nrow(U1))/nrow(leafs))


#Log-Log OLS cross validation under 1
form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U1)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = U1[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*U1$Sc[group == i]^beta 
    tmp[group == i] <- (U1$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_3_U1 <- mean(unlist(PEcv))

#Log-Log OLS cross validation between 1 and 2
form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = U2[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*U2$Sc[group == i]^beta 
    tmp[group == i] <- (U2$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_3_U2 <- mean(unlist(PEcv))

#Log-Log OLS cross validation between 2 and 3
form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = U3[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*U3$Sc[group == i]^beta 
    tmp[group == i] <- (U3$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_3_U3 <- mean(unlist(PEcv))

#Log-Log OLS cross validation between 3 and 5
form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U5)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = U5[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*U5$Sc[group == i]^beta 
    tmp[group == i] <- (U5$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_3_U5 <- mean(unlist(PEcv))

#Log-Log OLS cross validation between 5 and 10
form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U10)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = U10[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*U10$Sc[group == i]^beta 
    tmp[group == i] <- (U10$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_3_U10 <- mean(unlist(PEcv))

#Log-Log OLS cross validation between 10 and 20
form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U20)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = U20[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*U20$Sc[group == i]^beta 
    tmp[group == i] <- (U20$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_3_U20 <- mean(unlist(PEcv))

#Log-Log OLS cross validation between 20 and 30
form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U30)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = U30[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*U30$Sc[group == i]^beta 
    tmp[group == i] <- (U30$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_3_U30 <- mean(unlist(PEcv))

#Log-Log OLS cross validation over 30
form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(O30)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = O30[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*O30$Sc[group == i]^beta 
    tmp[group == i] <- (O30$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_3_O30 <- mean(unlist(PEcv))

##RMSE for all Log-Log OLS groups
sqrt((MSE_LEAFS_LLOLS_3_O30*nrow(O30)+MSE_LEAFS_LLOLS_3_U30*nrow(U30)+MSE_LEAFS_LLOLS_3_U20*nrow(U20)+MSE_LEAFS_LLOLS_3_U10*nrow(U10)+MSE_LEAFS_LLOLS_3_U5*nrow(U5)+MSE_LEAFS_LLOLS_3_U3*nrow(U3)+MSE_LEAFS_LLOLS_3_U2*nrow(U2)+MSE_LEAFS_LLOLS_3_U1*nrow(U1))/nrow(leafs))


#Baskerville corrected cross validation under 1
set.seed(1)
Log_Log_form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U1)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = U1[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*U1$Sc[group == i]^beta 
    tmp[group == i] <- (U1$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_3_U1 <- mean(unlist(PEcv))

#Baskerville corrected cross validation between 1 and 2
set.seed(1)
Log_Log_form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = U2[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*U2$Sc[group == i]^beta 
    tmp[group == i] <- (U2$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_3_U2 <- mean(unlist(PEcv))

#Baskerville corrected cross validation between 2 and 3
set.seed(1)
Log_Log_form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = U3[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*U3$Sc[group == i]^beta 
    tmp[group == i] <- (U3$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_3_U3 <- mean(unlist(PEcv))

#Baskerville corrected cross validation between 3 and 5
set.seed(1)
Log_Log_form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U5)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = U5[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*U5$Sc[group == i]^beta 
    tmp[group == i] <- (U5$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_3_U5 <- mean(unlist(PEcv))

#Baskerville corrected cross validation between 5 and 10
set.seed(1)
Log_Log_form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U10)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = U10[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*U10$Sc[group == i]^beta 
    tmp[group == i] <- (U10$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_3_U10 <- mean(unlist(PEcv))

#Baskerville corrected cross validation between 10 and 20
set.seed(1)
Log_Log_form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U20)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = U20[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*U20$Sc[group == i]^beta 
    tmp[group == i] <- (U20$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_3_U20 <- mean(unlist(PEcv))

#Baskerville corrected cross validation between 20 and 30
set.seed(1)
Log_Log_form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(U30)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = U30[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*U30$Sc[group == i]^beta 
    tmp[group == i] <- (U30$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_3_U30 <- mean(unlist(PEcv))

#Baskerville corrected cross validation over 30
set.seed(1)
Log_Log_form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(O30)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = O30[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*O30$Sc[group == i]^beta 
    tmp[group == i] <- (O30$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_3_O30 <- mean(unlist(PEcv))

##RMSE for all Baskerville corrected groups
sqrt((MSE_LEAFS_Bask_3_O30*nrow(O30)+MSE_LEAFS_Bask_3_U30*nrow(U30)+MSE_LEAFS_Bask_3_U20*nrow(U20)+MSE_LEAFS_Bask_3_U10*nrow(U10)+MSE_LEAFS_Bask_3_U5*nrow(U5)+MSE_LEAFS_Bask_3_U3*nrow(U3)+MSE_LEAFS_Bask_3_U2*nrow(U2)+MSE_LEAFS_Bask_3_U1*nrow(U1))/nrow(leafs))



#NLR cross validation under 1
testCV_leafs_NLR_3_U1 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U1)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = U1[group != i, ], start = sv_L_3_U1)
      muhat <- predict(modelcv, newdata = U1[group == i, ])
      tmp[group == i] <- (U1$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_3_U1 <- testCV_leafs_NLR_3_U1(fit_L_3_U1, U1)

#NLR cross validation between 1 and 2
testCV_leafs_NLR_3_U2 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = U2[group != i, ], start = sv_L_3_U2)
      muhat <- predict(modelcv, newdata = U2[group == i, ])
      tmp[group == i] <- (U2$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_3_U2 <- testCV_leafs_NLR_3_U2(fit_L_3_U2, U2)

#NLR cross validation between 2 and 3
testCV_leafs_NLR_3_U3 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = U3[group != i, ], start = sv_L_3_U3)
      muhat <- predict(modelcv, newdata = U3[group == i, ])
      tmp[group == i] <- (U3$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_3_U3 <- testCV_leafs_NLR_3_U3(fit_L_3_U3, U3)

#NLR cross validation between 3 and 5
testCV_leafs_NLR_3_U5 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U5)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = U5[group != i, ], start = sv_L_3_U5)
      muhat <- predict(modelcv, newdata = U5[group == i, ])
      tmp[group == i] <- (U5$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_3_U5 <- testCV_leafs_NLR_3_U5(fit_L_3_U5, U5)

#NLR cross validation between 5 and 10
testCV_leafs_NLR_3_U10 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U10)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = U10[group != i, ], start = sv_L_3_U10)
      muhat <- predict(modelcv, newdata = U10[group == i, ])
      tmp[group == i] <- (U10$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_3_U10 <- testCV_leafs_NLR_3_U10(fit_L_3_U10, U10)

#NLR cross validation between 10 and 20
testCV_leafs_NLR_3_U20 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U20)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = U20[group != i, ], start = sv_L_3_U20)
      muhat <- predict(modelcv, newdata = U20[group == i, ])
      tmp[group == i] <- (U20$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_3_U20 <- testCV_leafs_NLR_3_U20(fit_L_3_U20, U20)

#NLR cross validation between 20 and 30
testCV_leafs_NLR_3_U30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(U30)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = U30[group != i, ], start = sv_L_3_U30)
      muhat <- predict(modelcv, newdata = U30[group == i, ])
      tmp[group == i] <- (U30$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_3_U30 <- testCV_leafs_NLR_3_U30(fit_L_3_U30, U30)

#NLR cross validation over 30
testCV_leafs_NLR_3_O30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(O30)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = O30[group != i, ], start = sv_L_3_O30)
      muhat <- predict(modelcv, newdata = O30[group == i, ])
      tmp[group == i] <- (O30$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_3_O30 <- testCV_leafs_NLR_3_O30(fit_L_3_O30, O30)

##RMSE for all NLR groups
sqrt((MSE_Leafs_NLR_3_O30*nrow(O30)+MSE_Leafs_NLR_3_U30*nrow(U30)+MSE_Leafs_NLR_3_U20*nrow(U20)+MSE_Leafs_NLR_3_U10*nrow(U10)+MSE_Leafs_NLR_3_U5*nrow(U5)+MSE_Leafs_NLR_3_U3*nrow(U3)+MSE_Leafs_NLR_3_U2*nrow(U2)+MSE_Leafs_NLR_3_U1*nrow(U1))/nrow(leafs))