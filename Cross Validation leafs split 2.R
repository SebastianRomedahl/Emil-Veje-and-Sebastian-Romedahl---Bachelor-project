## OLS Under 5
testCV_leafs_OLS_U5 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und5)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = Und5[group != i, ])
      muhat <- predict(modelcv, newdata = Und5[group == i, ])
      tmp[group == i] <- (Und5$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_U5 <- testCV_leafs_OLS_U5(OLSLeafs_2_U5, Und5)

## OLS Between 5 and 10
testCV_leafs_OLS_U10 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und10)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = Und10[group != i, ])
      muhat <- predict(modelcv, newdata = Und10[group == i, ])
      tmp[group == i] <- (Und10$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_U10 <- testCV_leafs_OLS_U10(OLSLeafs_2_U10, Und10)

## OLS 10 to 15
testCV_leafs_OLS_U15 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und15)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = Und15[group != i, ])
      muhat <- predict(modelcv, newdata = Und15[group == i, ])
      tmp[group == i] <- (Und15$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_U15 <- testCV_leafs_OLS_U15(OLSLeafs_2_U15, Und15)

## OLS 15 to 20
testCV_leafs_OLS_U20 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und20)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = Und20[group != i, ])
      muhat <- predict(modelcv, newdata = Und20[group == i, ])
      tmp[group == i] <- (Und20$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_U20 <- testCV_leafs_OLS_U20(OLSLeafs_2_U20, Und20)

## OLS 20 to 30
testCV_leafs_OLS_U30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und30)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = Und30[group != i, ])
      muhat <- predict(modelcv, newdata = Und30[group == i, ])
      tmp[group == i] <- (Und30$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_U30 <- testCV_leafs_OLS_U30(OLSLeafs_2_U30, Und30)

## OLS 30 to 40
testCV_leafs_OLS_U40 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und40)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = Und40[group != i, ])
      muhat <- predict(modelcv, newdata = Und40[group == i, ])
      tmp[group == i] <- (Und40$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_U40 <- testCV_leafs_OLS_U40(OLSLeafs_2_U40, Und40)

## OLS 40 to 50
testCV_leafs_OLS_U50 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und50)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = Und50[group != i, ])
      muhat <- predict(modelcv, newdata = Und50[group == i, ])
      tmp[group == i] <- (Und50$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_U50 <- testCV_leafs_OLS_U50(OLSLeafs_2_U50, Und50

## OLS Over 50
testCV_leafs_OLS_O50 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Ove50)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = Ove50[group != i, ])
      muhat <- predict(modelcv, newdata = Ove50[group == i, ])
      tmp[group == i] <- (Ove50$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS_O50 <- testCV_leafs_OLS_O50(OLSLeafs_2_O50, Ove50)

### RMSE of all the groups
sqrt((MSE_Leafs_OLS_O50*nrow(Ove50)+MSE_Leafs_OLS_U50*nrow(Und50)+MSE_Leafs_OLS_U40*nrow(Und40)+MSE_Leafs_OLS_U30*nrow(Und30)+MSE_Leafs_OLS_U20*nrow(Und20)+MSE_Leafs_OLS_U15*nrow(Und15)+ MSE_Leafs_OLS_U10*nrow(Und10)+MSE_Leafs_OLS_U5*nrow(Und5))/nrow(leafs))


##Log-Log OLS under 5
form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(Und5)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = Und5[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und5$Sc[group == i]^beta 
    tmp[group == i] <- (Und5$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_U5 <- mean(unlist(PEcv))

MSE_LEAFS_LLOLS_U5

##Log-Log OLS between 5 and 10
n <- nrow(Und10)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = Und10[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und10$Sc[group == i]^beta 
    tmp[group == i] <- (Und10$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_U10 <- mean(unlist(PEcv))

MSE_LEAFS_LLOLS_U10

##Log-Log OLS between 10 and 15
n <- nrow(Und15)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = Und15[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und15$Sc[group == i]^beta 
    tmp[group == i] <- (Und15$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_U15 <- mean(unlist(PEcv))

MSE_LEAFS_LLOLS_U15
##Log-Log OLS between 15 and 20
n <- nrow(Und20)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = Und10[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und20$Sc[group == i]^beta 
    tmp[group == i] <- (Und20$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_U20 <- mean(unlist(PEcv))

MSE_LEAFS_LLOLS_U20

##Log-Log OLS between 20 and 30
n <- nrow(Und30)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = Und10[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und30$Sc[group == i]^beta 
    tmp[group == i] <- (Und30$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_U30 <- mean(unlist(PEcv))

MSE_LEAFS_LLOLS_U30

##Log-Log OLS between 30 and 40
n <- nrow(Und40)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = Und40[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und40$Sc[group == i]^beta 
    tmp[group == i] <- (Und40$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_U40 <- mean(unlist(PEcv))

MSE_LEAFS_LLOLS_U40

##Log-Log OLS between 40 and 50
n <- nrow(Und50)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = Und50[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und50$Sc[group == i]^beta 
    tmp[group == i] <- (Und50$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_U50 <- mean(unlist(PEcv))

MSE_LEAFS_LLOLS_U50

##Log-Log OLS over 50
n <- nrow(Ove50)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = Ove50[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*Ove50$Sc[group == i]^beta 
    tmp[group == i] <- (Ove50$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS_O50 <- mean(unlist(PEcv))

MSE_LEAFS_LLOLS_O50

### RMSE of all the groups
sqrt((MSE_LEAFS_LLOLS_O50*nrow(Ove50)+MSE_LEAFS_LLOLS_U50*nrow(Und50)+MSE_LEAFS_LLOLS_U40*nrow(Und40)+MSE_LEAFS_LLOLS_U30*nrow(Und30)+MSE_LEAFS_LLOLS_U20*nrow(Und20)+MSE_LEAFS_LLOLS_U15*nrow(Und15)+ MSE_LEAFS_LLOLS_U10*nrow(Und10)+MSE_LEAFS_LLOLS_U5*nrow(Und5))/nrow(leafs))



##Baskerville corrected under 5
set.seed(1)
Log_Log_form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(Und5)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = Und5[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und5$Sc[group == i]^beta 
    tmp[group == i] <- (Und5$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_U5 <- mean(unlist(PEcv))

MSE_LEAFS_Bask_U5

##Baskerville corrected between 5 and 10
n <- nrow(Und10)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = Und10[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und10$Sc[group == i]^beta 
    tmp[group == i] <- (Und10$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_U10 <- mean(unlist(PEcv))

MSE_LEAFS_Bask_U10

##Baskerville corrected between 10 and 15
n <- nrow(Und15)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = Und15[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und15$Sc[group == i]^beta 
    tmp[group == i] <- (Und15$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_U15 <- mean(unlist(PEcv))

MSE_LEAFS_Bask_U15

##Baskerville corrected between 15 and 20
n <- nrow(Und20)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = Und20[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und20$Sc[group == i]^beta 
    tmp[group == i] <- (Und20$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_U20 <- mean(unlist(PEcv))

MSE_LEAFS_Bask_U20

##Baskerville corrected between 20 and 30
n <- nrow(Und30)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = Und30[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und30$Sc[group == i]^beta 
    tmp[group == i] <- (Und30$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_U30 <- mean(unlist(PEcv))

MSE_LEAFS_Bask_U30

##Baskerville corrected between 30 and 40
n <- nrow(Und40)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = Und40[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und40$Sc[group == i]^beta 
    tmp[group == i] <- (Und40$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_U40 <- mean(unlist(PEcv))

MSE_LEAFS_Bask_U40

##Baskerville corrected between 40 and 50
n <- nrow(Und50)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = Und50[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*Und50$Sc[group == i]^beta 
    tmp[group == i] <- (Und50$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_U50 <- mean(unlist(PEcv))

MSE_LEAFS_Bask_U50

##Baskerville corrected over 50
n <- nrow(Ove50)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = Ove50[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*Ove50$Sc[group == i]^beta 
    tmp[group == i] <- (Ove50$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask_O50 <- mean(unlist(PEcv))

MSE_LEAFS_Bask_O50

### RMSE of all the groups
sqrt((MSE_LEAFS_Bask_O50*nrow(Ove50)+MSE_LEAFS_Bask_U50*nrow(Und50)+MSE_LEAFS_Bask_U40*nrow(Und40)+MSE_LEAFS_Bask_U30*nrow(Und30)+MSE_LEAFS_Bask_U20*nrow(Und20)+MSE_LEAFS_Bask_U15*nrow(Und15)+ MSE_LEAFS_Bask_U10*nrow(Und10)+MSE_LEAFS_Bask_U5*nrow(Und5))/nrow(leafs))


##NLR under 5
testCV_leafs_NLR_U5 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und5)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = Und5[group != i, ], start = sv_L_2_U5)
      muhat <- predict(modelcv, newdata = Und5[group == i, ])
      tmp[group == i] <- (Und5$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_U5 <- testCV_leafs_NLR_U5(fit_L_2_U5, Und5)

##NLR between 5 and 10
testCV_leafs_NLR_U10 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und10)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = Und10[group != i, ], start = sv_L_2_U10)
      muhat <- predict(modelcv, newdata = Und10[group == i, ])
      tmp[group == i] <- (Und10$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_U10 <- testCV_leafs_NLR_U10(fit_L_2_U10, Und10)

##NLR between 10 and 15
testCV_leafs_NLR_U15 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und15)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = Und15[group != i, ], start = sv_L_2_U15)
      muhat <- predict(modelcv, newdata = Und15[group == i, ])
      tmp[group == i] <- (Und15$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_U15 <- testCV_leafs_NLR_U15(fit_L_2_U15, Und15)

##NLR between 15 and 20
testCV_leafs_NLR_U20 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und20)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = Und20[group != i, ], start = sv_L_2_U20)
      muhat <- predict(modelcv, newdata = Und20[group == i, ])
      tmp[group == i] <- (Und20$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_U20 <- testCV_leafs_NLR_U20(fit_L_2_U20, Und20)

##NLR between 20 and 30
testCV_leafs_NLR_U30 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und30)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = Und30[group != i, ], start = sv_L_2_U30)
      muhat <- predict(modelcv, newdata = Und30[group == i, ])
      tmp[group == i] <- (Und30$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_U30 <- testCV_leafs_NLR_U30(fit_L_2_U30, Und30)

##NLR between 30 and 40
nlc <- nls.control(maxiter = 1000) 

testCV_leafs_NLR_U40 <- function(form, data, B = 100, k = 10) {
  n <- nrow(Und40)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = Und40[group != i, ], control = nlc, start = sv_L_2_U40)
      muhat <- predict(modelcv, newdata = Und40[group == i, ])
      tmp[group == i] <- (Und40$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_U40 <- testCV_leafs_NLR_U40(fit_L_2_U40, Und40)

##NLR between 40 and 50
testCV_leafs_NLR_U50 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Und50)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = Und50[group != i, ], control = nlc, start = sv_L_2_U50)
      muhat <- predict(modelcv, newdata = Und50[group == i, ])
      tmp[group == i] <- (Und50$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_U50 <- testCV_leafs_NLR_U50(fit_L_2_U50, Und50)

##NLR over 50
testCV_leafs_NLR_O50 <- function(form, data, B = 50, k = 10) {
  n <- nrow(Ove50)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = Ove50[group != i, ], control = nlc, start = sv_L_2_O50)
      muhat <- predict(modelcv, newdata = Ove50[group == i, ])
      tmp[group == i] <- (Ove50$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR_O50 <- testCV_leafs_NLR_O50(fit_L_2_O50, Ove50)

### RMSE of all the groups
sqrt((MSE_Leafs_NLR_O50*nrow(Ove50)+MSE_Leafs_NLR_U50*nrow(Und50)+MSE_Leafs_NLR_U40*nrow(Und40)+MSE_Leafs_NLR_U30*nrow(Und30)+MSE_Leafs_NLR_U20*nrow(Und20)+MSE_Leafs_NLR_U15*nrow(Und15)+ MSE_Leafs_NLR_U10*nrow(Und10)+MSE_Leafs_NLR_U5*nrow(Und5))/nrow(leafs))
