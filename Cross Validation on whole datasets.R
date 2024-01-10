#Leafs OLS cross validation and RMSE
testCV_leafs_OLS <- function(form, data, B = 50, k = 10) {
  n <- nrow(leafs)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = leafs[group != i, ])
      muhat <- predict(modelcv, newdata = leafs[group == i, ])
      tmp[group == i] <- (leafs$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_OLS <- testCV_leafs_OLS(OLSLeafs, leafs)
sqrt(MSE_Leafs_OLS)

#Leafs Log-Log OLS cross validation and RMSE
form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(leafs)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = leafs[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*leafs$Sc[group == i]^beta 
    tmp[group == i] <- (leafs$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_LLOLS <- mean(unlist(PEcv))

sqrt(MSE_LEAFS_LLOLS)


#Leafs Baskerville corrected cross validation and RMSE
set.seed(1)
Log_Log_form = log(Bfkg) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(leafs)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = leafs[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*leafs$Sc[group == i]^beta 
    tmp[group == i] <- (leafs$Bfkg[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_LEAFS_Bask <- mean(unlist(PEcv))

sqrt(MSE_LEAFS_Bask)

#Leafs NLR cross validation and RMSE
testCV_leafs_NLR <- function(form, data, B = 50, k = 10) {
  n <- nrow(leafs)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = leafs[group != i, ], start = sv_L)
      muhat <- predict(modelcv, newdata = leafs[group == i, ])
      tmp[group == i] <- (leafs$Bfkg[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Leafs_NLR <- testCV_leafs_NLR(fit_L, leafs)
sqrt(MSE_Leafs_NLR)

#Wood OLS cross validation and RMSE
testCV_wood_OLS <- function(form, data, B = 50, k = 10) {
  n <- nrow(wood)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- lm(form, data = wood[group != i, ])
      muhat <- predict(modelcv, newdata = wood[group == i, ])
      tmp[group == i] <- (wood$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_OLS <- testCV_wood_OLS(OLSWood, wood)
sqrt(MSE_Wood_OLS)


#Wood Log-Log OLS cross validation and RMSE
set.seed(2024)
form = log(mbt) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(wood)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form, data = wood[group != i,])
    alpha <- exp(coef(modelcv)[1])
    beta <- coef(modelcv)[2]
    muhat <- alpha*wood$Sc[group == i]^beta 
    tmp[group == i] <- (wood$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_LLOLS <- mean(unlist(PEcv))

sqrt(MSE_wood_LLOLS)

#Wood Baskerville corrected cross validation and RMSE
set.seed(1)
Log_Log_form = log(mbt) ~ log(Sc)
B <- 50
k <- 10
n <- nrow(wood)
PEcv <- vector("list", B)
tmp <- numeric(n)
for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(Log_Log_form, data = wood[group != i,])
    alpha <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta <- coef(modelcv)[2]
    muhat <- alpha*wood$Sc[group == i]^beta 
    tmp[group == i] <- (wood$mbt[group == i] - muhat) ^ 2
  }
  PEcv[[b]] <- tmp
}
MSE_wood_Bask <- mean(unlist(PEcv))

sqrt(MSE_wood_Bask)

#Wood NLR cross validation and RMSE
testCV_wood_NLR <- function(form, data, B = 50, k = 10) {
  n <- nrow(wood)
  PEcv <- vector("list", B)
  tmp <- numeric(n)
  for(b in 1:B) {
    ## Generating the random division into groups
    group <- sample(rep(1:k, length.out = n))
    for(i in 1:k) {
      modelcv <- nls(form, data = wood[group != i, ], control = nlc, start = sv_W)
      muhat <- predict(modelcv, newdata = wood[group == i, ])
      tmp[group == i] <- (wood$mbt[group == i] - muhat) ^ 2
    }
    PEcv[[b]] <- tmp
  }
  mean(unlist(PEcv))
}

MSE_Wood_NLR <- testCV_wood_NLR(fit_W, wood)
sqrt(MSE_Wood_NLR)