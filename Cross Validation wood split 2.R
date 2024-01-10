#OLS cross validation wood under 5
testCV_wood_2_OLS_U5 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU5_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU5_2[group != i, ])
      muhat <- predict(modelcv, newdata = WU5_2[group == i, ])
      tmp[group == i] <- (WU5_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_OLS_U5 <- testCV_wood_2_OLS_U5(OLSWood_2_U5, WU5_2)

#OLS cross validation wood between 5 and 10
testCV_wood_2_OLS_U10 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU10_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU10_2[group != i, ])
      muhat <- predict(modelcv, newdata = WU10_2[group == i, ])
      tmp[group == i] <- (WU10_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_OLS_U10 <- testCV_wood_2_OLS_U10(OLSWood_2_U10, WU10_2)

#OLS cross validation wood between 10 and 15
testCV_wood_2_OLS_U15 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU15_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU15_2[group != i, ])
      muhat <- predict(modelcv, newdata = WU15_2[group == i, ])
      tmp[group == i] <- (WU15_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_OLS_U15 <- testCV_wood_2_OLS_U15(OLSWood_2_U15, WU15_2)

#OLS cross validation wood between 15 and 20
testCV_wood_2_OLS_U20 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU20_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU20_2[group != i, ])
      muhat <- predict(modelcv, newdata = WU20_2[group == i, ])
      tmp[group == i] <- (WU20_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_OLS_U20 <- testCV_wood_2_OLS_U20(OLSWood_2_U20, WU20_2)

#OLS cross validation wood between 20 and 30
testCV_wood_2_OLS_U30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU30_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU30_2[group != i, ])
      muhat <- predict(modelcv, newdata = WU30_2[group == i, ])
      tmp[group == i] <- (WU30_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_OLS_U30 <- testCV_wood_2_OLS_U30(OLSWood_2_U30, WU30_2)

#OLS cross validation wood between 30 and 40
testCV_wood_2_OLS_U40 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU40_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU40_2[group != i, ])
      muhat <- predict(modelcv, newdata = WU40_2[group == i, ])
      tmp[group == i] <- (WU40_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_OLS_U40 <- testCV_wood_2_OLS_U40(OLSWood_2_U40, WU40_2)

#OLS cross validation wood between 40 and 50
testCV_wood_2_OLS_U50 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU50_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WU50_2[group != i, ])
      muhat <- predict(modelcv, newdata = WU50_2[group == i, ])
      tmp[group == i] <- (WU50_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_OLS_U50 <- testCV_wood_2_OLS_U50(OLSWood_2_U50, WU50_2)

#OLS cross validation wood over 50
testCV_wood_2_OLS_O50 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WO50_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = WO50_2[group != i, ])
      muhat <- predict(modelcv, newdata = WO50_2[group == i, ])
      tmp[group == i] <- (WO50_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_OLS_O50 <- testCV_wood_2_OLS_O50(OLSWood_2_O50, WO50_2)

##RMSE for all OLS groups
sqrt((MSE_Wood_2_OLS_O50*nrow(WO50_2)+MSE_Wood_2_OLS_U50*nrow(WU50_2)+MSE_Wood_2_OLS_U40*nrow(WU40_2)+MSE_Wood_2_OLS_U30*nrow(WU30_2)+MSE_Wood_2_OLS_U20*nrow(WU20_2)+MSE_Wood_2_OLS_U15*nrow(WU15_2)+MSE_Wood_2_OLS_U10*nrow(WU10_2)+MSE_Wood_2_OLS_U5*nrow(WU5_2))/nrow(wood))


#Log-Log OLS cross validation wood under 5
form = log(mbt) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(WU5_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU5_2[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU5_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU5_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_LLOLS_U5 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 5 and 10
n <- nrow(WU10_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU10_2[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU10_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU10_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_LLOLS_U10 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 10 and 15
n <- nrow(WU15_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU15_2[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU15_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU15_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_LLOLS_U15 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 15 and 20
n <- nrow(WU20_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU20_2[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU20_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU20_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_LLOLS_U20 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 20 and 30
n <- nrow(WU30_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU30_2[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU30_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU30_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_LLOLS_U30 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 30 and 40
n <- nrow(WU40_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU40_2[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU40_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU40_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_LLOLS_U40 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood between 40 and 50
n <- nrow(WU50_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WU50_2[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU50_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU50_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_LLOLS_U50 <- mean(unlist(PEcv))

#Log-Log OLS cross validation wood over 50
n <- nrow(WO50_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = WO50_2[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*WO50_2$Sc[group == i]^beta 
    tmp[group == i] <- (WO50_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_LLOLS_O50 <- mean(unlist(PEcv))

##RMSE for all Log-Log OLS groups
sqrt((MSE_wood_2_LLOLS_O50*nrow(WO50_2)+MSE_wood_2_LLOLS_U50*nrow(WU50_2)+MSE_wood_2_LLOLS_U40*nrow(WU40_2)+MSE_wood_2_LLOLS_U30*nrow(WU30_2)+MSE_wood_2_LLOLS_U20*nrow(WU20_2)+MSE_wood_2_LLOLS_U15*nrow(WU15_2)+MSE_wood_2_LLOLS_U10*nrow(WU10_2)+MSE_wood_2_LLOLS_U5*nrow(WU5_2))/nrow(wood))


#Baskerville corrected cross validation wood under 5
Log_Log_form = log(mbt) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(WU5_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU5_2[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU5_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU5_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_Bask_U5 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 5 and 10
n <- nrow(WU10_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU10_2[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU10_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU10_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_Bask_U10 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 10 and 15
n <- nrow(WU15_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU15_2[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU15_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU15_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_Bask_U15 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 15 and 20
n <- nrow(WU20_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU20_2[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU20_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU20_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_Bask_U20 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 20 and 30
n <- nrow(WU30_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU30_2[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU30_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU30_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_Bask_U30 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 30 and 40
n <- nrow(WU40_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU40_2[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU40_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU40_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_Bask_U40 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood between 40 and 50
n <- nrow(WU50_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WU50_2[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WU50_2$Sc[group == i]^beta 
    tmp[group == i] <- (WU50_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_Bask_U50 <- mean(unlist(PEcv))

#Baskerville corrected cross validation wood over 50
n <- nrow(WO50_2)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = WO50_2[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*WO50_2$Sc[group == i]^beta 
    tmp[group == i] <- (WO50_2$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_2_Bask_O50 <- mean(unlist(PEcv))

##RMSE for all Baskerville corrected groups
sqrt((MSE_wood_2_Bask_O50*nrow(WO50_2)+MSE_wood_2_Bask_U50*nrow(WU50_2)+MSE_wood_2_Bask_U40*nrow(WU40_2)+MSE_wood_2_Bask_U30*nrow(WU30_2)+MSE_wood_2_Bask_U20*nrow(WU20_2)+MSE_wood_2_Bask_U15*nrow(WU15_2)+MSE_wood_2_Bask_U10*nrow(WU10_2)+MSE_wood_2_Bask_U5*nrow(WU5_2))/nrow(wood))



#NLR cross validation wood under 5
testCV_wood_2_NLR_U5 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU5_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU5_2[group != i, ], start = sv_W_2_U5)
      muhat <- predict(modelcv, newdata = WU5_2[group == i, ])
      tmp[group == i] <- (WU5_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_NLR_U5 <- testCV_wood_2_NLR_U5(fit_W_2_U5, WU5_2)

#NLR cross validation wood between 5 and 10
testCV_wood_2_NLR_U10 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU10_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU10_2[group != i, ], start = sv_W_2_U10)
      muhat <- predict(modelcv, newdata = WU10_2[group == i, ])
      tmp[group == i] <- (WU10_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_NLR_U10 <- testCV_wood_2_NLR_U10(fit_W_2_U10, WU10_2)

#NLR cross validation wood between 10 and 15
testCV_wood_2_NLR_U15 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU15_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU15_2[group != i, ], start = sv_W_2_U15)
      muhat <- predict(modelcv, newdata = WU15_2[group == i, ])
      tmp[group == i] <- (WU15_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_NLR_U15 <- testCV_wood_2_NLR_U15(fit_W_2_U15, WU15_2)

#NLR cross validation wood between 15 and 20
nlc <- nls.control(maxiter = 1000, minFactor = 1/10000) 

testCV_wood_2_NLR_U20 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU20_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU20_2[group != i, ], control = nlc, start = sv_W_2_U20)
      muhat <- predict(modelcv, newdata = WU20_2[group == i, ])
      tmp[group == i] <- (WU20_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_NLR_U20 <- testCV_wood_2_NLR_U20(fit_W_2_U20, WU20_2)

#NLR cross validation wood between 20 and 30
testCV_wood_2_NLR_U30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU30_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU30_2[group != i, ], control = nlc, start = sv_W_2_U30)
      muhat <- predict(modelcv, newdata = WU30_2[group == i, ])
      tmp[group == i] <- (WU30_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_NLR_U30 <- testCV_wood_2_NLR_U30(fit_W_2_U30, WU30_2)

#NLR cross validation wood between 30 and 40
testCV_wood_2_NLR_U40 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU40_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU40_2[group != i, ], control = nlc, start = sv_W_2_U40)
      muhat <- predict(modelcv, newdata = WU40_2[group == i, ])
      tmp[group == i] <- (WU40_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_NLR_U40 <- testCV_wood_2_NLR_U40(fit_W_2_U40, WU40_2)

#NLR cross validation wood between 40 and 50
nlc1 <- nls.control(maxiter = 1000, minFactor = 1/10000000)
testCV_wood_2_NLR_U50 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WU50_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WU50_2[group != i, ], control = nlc1, start = sv_W_2_U50)
      muhat <- predict(modelcv, newdata = WU50_2[group == i, ])
      tmp[group == i] <- (WU50_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_NLR_U50 <- testCV_wood_2_NLR_U50(fit_W_2_U50, WU50_2)

#NLR cross validation wood over 50
testCV_wood_2_NLR_O50 <- function(form, data, B = 50, k = 10) {
  n <- nrow(WO50_2)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = WO50_2[group != i, ], control = nlc1, start = sv_W_2_O50)
      muhat <- predict(modelcv, newdata = WO50_2[group == i, ])
      tmp[group == i] <- (WO50_2$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_2_NLR_O50 <- testCV_wood_2_NLR_O50(fit_W_2_O50, WO50_2)

##RMSE for all NLR groups
sqrt((MSE_Wood_2_NLR_O50*nrow(WO50_2)+MSE_Wood_2_NLR_U50*nrow(WU50_2)+MSE_Wood_2_NLR_U40*nrow(WU40_2)+MSE_Wood_2_NLR_U30*nrow(WU30_2)+MSE_Wood_2_NLR_U20*nrow(WU20_2)+MSE_Wood_2_NLR_U15*nrow(WU15_2)+MSE_Wood_2_NLR_U10*nrow(WU10_2)+MSE_Wood_2_NLR_U5*nrow(WU5_2))/nrow(wood))