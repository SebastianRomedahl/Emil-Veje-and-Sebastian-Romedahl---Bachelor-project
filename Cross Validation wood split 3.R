#OLS cross validation wood under 1
testCV_wood_3_OLS_U1 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU1_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU1_3[group != i, ])
      muhat <- predict(modelcv, newdata = WU1_3[group == i, ])
      tmp[group == i] <- (WU1_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_OLS_U1 <- testCV_wood_3_OLS_U1(OLSWood_3_U1, WU1_3)

#OLS cross validation wood between 1 and 2
testCV_wood_3_OLS_U2 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU2_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU2_3[group != i, ])
      muhat <- predict(modelcv, newdata = WU2_3[group == i, ])
      tmp[group == i] <- (WU2_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_OLS_U2 <- testCV_wood_3_OLS_U2(OLSWood_3_U2, WU2_3)

#OLS cross validation wood between 2 and 3
testCV_wood_3_OLS_U3 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU3_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU3_3[group != i, ])
      muhat <- predict(modelcv, newdata = WU3_3[group == i, ])
      tmp[group == i] <- (WU3_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_OLS_U3 <- testCV_wood_3_OLS_U3(OLSWood_3_U3, WU3_3)

#OLS cross validation wood between 3 and 5
testCV_wood_3_OLS_U5 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU5_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU5_3[group != i, ])
      muhat <- predict(modelcv, newdata = WU5_3[group == i, ])
      tmp[group == i] <- (WU5_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_OLS_U5 <- testCV_wood_3_OLS_U5(OLSWood_3_U5, WU5_3)

#OLS cross validation wood between 5 and 10
testCV_wood_3_OLS_U10 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU10_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU10_3[group != i, ])
      muhat <- predict(modelcv, newdata = WU10_3[group == i, ])
      tmp[group == i] <- (WU10_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_OLS_U10 <- testCV_wood_3_OLS_U10(OLSWood_3_U10, WU10_3)

#OLS cross validation wood between 10 and 20
testCV_wood_3_OLS_U20 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU20_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU20_3[group != i, ])
      muhat <- predict(modelcv, newdata = WU20_3[group == i, ])
      tmp[group == i] <- (WU20_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_OLS_U20 <- testCV_wood_3_OLS_U20(OLSWood_3_U20, WU20_3)

#OLS cross validation wood between 20 and 30
testCV_wood_3_OLS_U30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU30_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU30_3[group != i, ])
      muhat <- predict(modelcv, newdata = WU30_3[group == i, ])
      tmp[group == i] <- (WU30_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_OLS_U30 <- testCV_wood_3_OLS_U30(OLSWood_3_U30, WU30_3)

#OLS cross validation wood over 30
testCV_wood_3_OLS_O30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WO30_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WO30_3[group != i, ])
      muhat <- predict(modelcv, newdata = WO30_3[group == i, ])
      tmp[group == i] <- (WO30_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_OLS_O30 <- testCV_wood_3_OLS_O30(OLSWood_3_O30, WO30_3)

##RMSE for all OLS groups
sqrt((MSE_Wood_3_OLS_O30*nrow(WO30_3)+MSE_Wood_3_OLS_U30*nrow(WU30_3)+MSE_Wood_3_OLS_U20*nrow(WU20_3)+MSE_Wood_3_OLS_U10*nrow(WU10_3)+MSE_Wood_3_OLS_U5*nrow(WU5_3)+MSE_Wood_3_OLS_U3*nrow(WU3_3)+MSE_Wood_3_OLS_U2*nrow(WU2_3)+MSE_Wood_3_OLS_U1*nrow(WU1_3))/nrow(wood))



#Log-Log OLS cross validation wood under 1
form = log(mbt) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(WU1_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU1_3[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU1_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU1_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_LLOLS_U1 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 1 and 2
n <- nrow(WU2_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU2_3[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU2_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU2_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_LLOLS_U2 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 2 and 3
n <- nrow(WU3_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU3_3[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU3_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU3_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_LLOLS_U3 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 3 and 5
n <- nrow(WU5_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU5_3[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU5_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU5_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_LLOLS_U5 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 5 and 10
n <- nrow(WU10_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU10_3[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU10_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU10_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_LLOLS_U10 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 10 and 20
n <- nrow(WU20_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU20_3[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU20_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU20_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_LLOLS_U20 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 20 and 30
n <- nrow(WU30_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU30_3[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU30_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU30_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_LLOLS_U30 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood over 30
n <- nrow(WO30_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WO30_3[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WO30_3$Sc[group == i]^beta 
    tmp[group == i] <- (WO30_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_LLOLS_O30 <- mean(unlist(PEcv))

##RMSE for all Log-Log OLS groups
sqrt((MSE_wood_3_LLOLS_O30*nrow(WO30_3)+MSE_wood_3_LLOLS_U30*nrow(WU30_3)+MSE_wood_3_LLOLS_U20*nrow(WU20_3)+MSE_wood_3_LLOLS_U10*nrow(WU20_3)+MSE_wood_3_LLOLS_U5*nrow(WU5_3)+MSE_wood_3_LLOLS_U3*nrow(WU3_3)+MSE_wood_3_LLOLS_U2*nrow(WU2_3)+MSE_wood_3_LLOLS_U1*nrow(WU1_3))/nrow(wood))




#Baskerville corrected cross validation wood under 1
Log_Log_form = log(mbt) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(WU1_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU1_3[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU1_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU1_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_Bask_U1 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 1 and 2
n <- nrow(WU2_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU2_3[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU2_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU2_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_Bask_U2 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 2 and 3
n <- nrow(WU3_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU3_3[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU3_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU3_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_Bask_U3 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 3 and 5
n <- nrow(WU5_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU5_3[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU5_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU5_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_Bask_U5 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 5 and 10
n <- nrow(WU10_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU10_3[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU10_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU10_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_Bask_U10 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 10 and 20
n <- nrow(WU20_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU20_3[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU20_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU20_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_Bask_U20 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 20 and 30
n <- nrow(WU30_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU30_3[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU30_3$Sc[group == i]^beta 
    tmp[group == i] <- (WU30_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_Bask_U30 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood over 30
n <- nrow(WO30_3)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WO30_3[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WO30_3$Sc[group == i]^beta 
    tmp[group == i] <- (WO30_3$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_3_Bask_O30 <- mean(unlist(PEcv))

##RMSE for all Baskerville corrected groups
sqrt((MSE_wood_3_Bask_O30*nrow(WO30_3)+MSE_wood_3_Bask_U30*nrow(WU30_3)+MSE_wood_3_Bask_U20*nrow(WU20_3)+MSE_wood_3_Bask_U10*nrow(WU10_3)+MSE_wood_3_Bask_U5*nrow(WU5_3)+MSE_wood_3_Bask_U3*nrow(WU3_3)+MSE_wood_3_Bask_U2*nrow(WU2_3)+MSE_wood_3_Bask_U1*nrow(WU1_3))/nrow(wood))



#NLR cross validation wood under 1
testCV_wood_3_NLR_U1 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU1_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU1_3[group != i, ], start = sv_W_3_U1)
      muhat <- predict(modelcv, newdata = WU1_3[group == i, ])
      tmp[group == i] <- (WU1_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_NLR_U1 <- testCV_wood_3_NLR_U1(fit_W_3_U1, WU1_3)

#NLR cross validation wood between 1 and 2
testCV_wood_3_NLR_U2 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU2_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU2_3[group != i, ], start = sv_W_3_U2)
      muhat <- predict(modelcv, newdata = WU2_3[group == i, ])
      tmp[group == i] <- (WU2_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_NLR_U2 <- testCV_wood_3_NLR_U2(fit_W_3_U2, WU2_3)

#NLR cross validation wood between 2 and 3
testCV_wood_3_NLR_U3 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU3_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU3_3[group != i, ], start = sv_W_3_U3)
      muhat <- predict(modelcv, newdata = WU3_3[group == i, ])
      tmp[group == i] <- (WU3_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_NLR_U3 <- testCV_wood_3_NLR_U3(fit_W_3_U3, WU3_3)

#NLR cross validation wood between 3 and 5
testCV_wood_3_NLR_U5 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU5_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU5_3[group != i, ], start = sv_W_3_U5)
      muhat <- predict(modelcv, newdata = WU5_3[group == i, ])
      tmp[group == i] <- (WU5_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_NLR_U5 <- testCV_wood_3_NLR_U5(fit_W_3_U5, WU5_3)

#NLR cross validation wood between 5 and 10
testCV_wood_3_NLR_U10 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU10_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU10_3[group != i, ], start = sv_W_3_U10)
      muhat <- predict(modelcv, newdata = WU10_3[group == i, ])
      tmp[group == i] <- (WU10_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_NLR_U10 <- testCV_wood_3_NLR_U10(fit_W_3_U10, WU10_3)

#NLR cross validation wood between 10 and 20
testCV_wood_3_NLR_U20 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU20_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU20_3[group != i, ], start = sv_W_3_U20)
      muhat <- predict(modelcv, newdata = WU20_3[group == i, ])
      tmp[group == i] <- (WU20_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_NLR_U20 <- testCV_wood_3_NLR_U20(fit_W_3_U20, WU20_3)

#NLR cross validation wood between 20 and 30
testCV_wood_3_NLR_U30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU30_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU30_3[group != i, ], start = sv_W_3_U30)
      muhat <- predict(modelcv, newdata = WU30_3[group == i, ])
      tmp[group == i] <- (WU30_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_NLR_U30 <- testCV_wood_3_NLR_U30(fit_W_3_U30, WU30_3)

#NLR cross validation wood over 30
testCV_wood_3_NLR_O30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WO30_3)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WO30_3[group != i, ], start = sv_W_3_O30)
      muhat <- predict(modelcv, newdata = WO30_3[group == i, ])
      tmp[group == i] <- (WO30_3$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_3_NLR_O30 <- testCV_wood_3_NLR_O30(fit_W_3_O30, WO30_3)

##RMSE for all NLR groups
sqrt((MSE_Wood_3_NLR_O30*nrow(WO30_3)+MSE_Wood_3_NLR_U30*nrow(WU30_3)+MSE_Wood_3_NLR_U20*nrow(WU20_3)+MSE_Wood_3_NLR_U10*nrow(WU10_3)+MSE_Wood_3_NLR_U5*nrow(WU5_3)+MSE_Wood_3_NLR_U3*nrow(WU3_3)+MSE_Wood_3_NLR_U2*nrow(WU2_3)+MSE_Wood_3_NLR_U1*nrow(WU1_3))/nrow(wood))
