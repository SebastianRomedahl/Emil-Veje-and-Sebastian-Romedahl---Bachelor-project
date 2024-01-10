#Bootstrap leafs Log-log OLS split 3
##Under 1 Sc
form_LogLog_leafs <- log(Bfkg) ~ log(Sc)
B <- 1000
k <- 10
n <- nrow(U1)
LogB_Boot_UB_U1 <- vector("list", B)
LogB_Boot_LB_U1 <- vector("list", B)
Upper_leafs_U1 <- numeric(n)
Lower_leafs_U1 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_U1 <- lm(form_LogLog_leafs, data = sample(U1[group != i,], replace = TRUE))
      alpha_Bas_U1_leafs <- exp(coef(lmsub_U1)[1]+((sigma(lmsub_U1))^2)/2)
      beta_Bas_U1_leafs <- coef(lmsub_U1)[2]
      Yhat_U1_leafs <- alpha_Bas_U1_leafs*U1$Sc[group == i]^beta_Bas_U1_leafs
      eps_U1 <- Yhat_U1_leafs - U1$Bfkg[group == i]
      eps_U1 <- sample(eps_U1, size=nrow(U1[group == i,]), replace = TRUE)
      Upper_leafs_U1[group == i] <- Yhat_U1_leafs + quantile(eps_U1,probs=c(0.975))
      Lower_leafs_U1[group == i] <- Yhat_U1_leafs + quantile(eps_U1,probs=c(0.025))
    }
  LogB_Boot_UB_U1[[b]] <- Upper_leafs_U1
  LogB_Boot_LB_U1[[b]] <- Lower_leafs_U1
}
Upper_Limit_U1 <- unlist(LogB_Boot_UB_U1)
Lower_Limit_U1 <- unlist(LogB_Boot_LB_U1)
#Create the plot
plot_U1 <- cbind(Upper_Limit_U1, Lower_Limit_U1, U1)
#Compute the global coverage
plot_U1 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_U1 & Bfkg <= Upper_Limit_U1, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_U1, aes(x = Sc, y = Bfkg))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_U1), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_U1), col = "coral2", linetype = "dashed")


##Between 1 and 2 Sc
n <- nrow(U2)
LogB_Boot_UB_U2 <- vector("list", B)
LogB_Boot_LB_U2 <- vector("list", B)
Upper_leafs_U2 <- numeric(n)
Lower_leafs_U2 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_U2 <- lm(form_LogLog_leafs, data = sample(U2[group != i,], replace = TRUE))
      alpha_Bas_U2_leafs <- exp(coef(lmsub_U2)[1]+((sigma(lmsub_U2))^2)/2)
      beta_Bas_U2_leafs <- coef(lmsub_U2)[2]
      Yhat_U2_leafs <- alpha_Bas_U2_leafs*U2$Sc[group == i]^beta_Bas_U2_leafs
      eps_U2 <- Yhat_U2_leafs - U2$Bfkg[group == i]
      eps_U2 <- sample(eps_U2, size=nrow(U2[group == i,]), replace = TRUE)
      Upper_leafs_U2[group == i] <- Yhat_U2_leafs + quantile(eps_U2,probs=c(0.975))
      Lower_leafs_U2[group == i] <- Yhat_U2_leafs + quantile(eps_U2,probs=c(0.025))
    }
  LogB_Boot_UB_U2[[b]] <- Upper_leafs_U2
  LogB_Boot_LB_U2[[b]] <- Lower_leafs_U2
}
Upper_Limit_U2 <- unlist(LogB_Boot_UB_U2)
Lower_Limit_U2 <- unlist(LogB_Boot_LB_U2)
#Create the plot
plot_U2 <- cbind(Upper_Limit_U2, Lower_Limit_U2, U2)
#Compute the global coverage
plot_U2 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_U2 & Bfkg <= Upper_Limit_U2, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_U2, aes(x = Sc, y = Bfkg))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_U2), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_U2), col = "coral2", linetype = "dashed")


##Between 2 and 3 Sc
n <- nrow(U3)
LogB_Boot_UB_U3 <- vector("list", B)
LogB_Boot_LB_U3 <- vector("list", B)
Upper_leafs_U3 <- numeric(n)
Lower_leafs_U3 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_U3 <- lm(form_LogLog_leafs, data = sample(U3[group != i,], replace = TRUE))
      alpha_Bas_U3_leafs <- exp(coef(lmsub_U3)[1]+((sigma(lmsub_U3))^2)/2)
      beta_Bas_U3_leafs <- coef(lmsub_U3)[2]
      Yhat_U3_leafs <- alpha_Bas_U3_leafs*U3$Sc[group == i]^beta_Bas_U3_leafs
      eps_U3 <- Yhat_U3_leafs - U3$Bfkg[group == i]
      eps_U3 <- sample(eps_U3, size=nrow(U3[group == i,]), replace = TRUE)
      Upper_leafs_U3[group == i] <- Yhat_U3_leafs + quantile(eps_U3,probs=c(0.975))
      Lower_leafs_U3[group == i] <- Yhat_U3_leafs + quantile(eps_U3,probs=c(0.025))
    }
  LogB_Boot_UB_U3[[b]] <- Upper_leafs_U3
  LogB_Boot_LB_U3[[b]] <- Lower_leafs_U3
}
Upper_Limit_U3 <- unlist(LogB_Boot_UB_U3)
Lower_Limit_U3 <- unlist(LogB_Boot_LB_U3)
#Create the plot
plot_U3 <- cbind(Upper_Limit_U3, Lower_Limit_U3, U3)
#Compute the global coverage
plot_U3 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_U3 & Bfkg <= Upper_Limit_U3, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_U3, aes(x = Sc, y = Bfkg))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_U3), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_U3), col = "coral2", linetype = "dashed")


##Between 3 and 5 Sc
n <- nrow(U5)
LogB_Boot_UB_U5 <- vector("list", B)
LogB_Boot_LB_U5 <- vector("list", B)
Upper_leafs_U5 <- numeric(n)
Lower_leafs_U5 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_U5 <- lm(form_LogLog_leafs, data = sample(U5[group != i,], replace = TRUE))
      alpha_Bas_U5_leafs <- exp(coef(lmsub_U5)[1]+((sigma(lmsub_U5))^2)/2)
      beta_Bas_U5_leafs <- coef(lmsub_U5)[2]
      Yhat_U5_leafs <- alpha_Bas_U5_leafs*U5$Sc[group == i]^beta_Bas_U5_leafs
      eps_U5 <- Yhat_U5_leafs - U5$Bfkg[group == i]
      eps_U5 <- sample(eps_U5, size=nrow(U5[group == i,]), replace = TRUE)
      Upper_leafs_U5[group == i] <- Yhat_U5_leafs + quantile(eps_U5,probs=c(0.975))
      Lower_leafs_U5[group == i] <- Yhat_U5_leafs + quantile(eps_U5,probs=c(0.025))
    }
  LogB_Boot_UB_U5[[b]] <- Upper_leafs_U5
  LogB_Boot_LB_U5[[b]] <- Lower_leafs_U5
}
Upper_Limit_U5 <- unlist(LogB_Boot_UB_U5)
Lower_Limit_U5 <- unlist(LogB_Boot_LB_U5)
#Create the plot
plot_U5 <- cbind(Upper_Limit_U5, Lower_Limit_U5, U5)
#Compute the global coverage
plot_U5 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_U5 & Bfkg <= Upper_Limit_U5, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_U5, aes(x = Sc, y = Bfkg))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_U5), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_U5), col = "coral2", linetype = "dashed")


##Between 5 and 10 Sc
n <- nrow(U10)
LogB_Boot_UB_U10 <- vector("list", B)
LogB_Boot_LB_U10 <- vector("list", B)
Upper_leafs_U10 <- numeric(n)
Lower_leafs_U10 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_U10 <- lm(form_LogLog_leafs, data = sample(U10[group != i,], replace = TRUE))
      alpha_Bas_U10_leafs <- exp(coef(lmsub_U10)[1]+((sigma(lmsub_U10))^2)/2)
      beta_Bas_U10_leafs <- coef(lmsub_U10)[2]
      Yhat_U10_leafs <- alpha_Bas_U10_leafs*U10$Sc[group == i]^beta_Bas_U10_leafs
      eps_U10 <- Yhat_U10_leafs - U10$Bfkg[group == i]
      eps_U10 <- sample(eps_U10, size=nrow(U10[group == i,]), replace = TRUE)
      Upper_leafs_U10[group == i] <- Yhat_U10_leafs + quantile(eps_U10,probs=c(0.975))
      Lower_leafs_U10[group == i] <- Yhat_U10_leafs + quantile(eps_U10,probs=c(0.025))
    }
  LogB_Boot_UB_U10[[b]] <- Upper_leafs_U10
  LogB_Boot_LB_U10[[b]] <- Lower_leafs_U10
}
Upper_Limit_U10 <- unlist(LogB_Boot_UB_U10)
Lower_Limit_U10 <- unlist(LogB_Boot_LB_U10)
#Create the plot
plot_U10 <- cbind(Upper_Limit_U10, Lower_Limit_U10, U10)
#Compute the global coverage
plot_U10 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_U10 & Bfkg <= Upper_Limit_U10, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_U10, aes(x = Sc, y = Bfkg))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_U10), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_U10), col = "coral2", linetype = "dashed")


##Between 10 and 20 Sc
n <- nrow(U20)
LogB_Boot_UB_U20 <- vector("list", B)
LogB_Boot_LB_U20 <- vector("list", B)
Upper_leafs_U20 <- numeric(n)
Lower_leafs_U20 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_U20 <- lm(form_LogLog_leafs, data = sample(U20[group != i,], replace = TRUE))
      alpha_Bas_U20_leafs <- exp(coef(lmsub_U20)[1]+((sigma(lmsub_U20))^2)/2)
      beta_Bas_U20_leafs <- coef(lmsub_U20)[2]
      Yhat_U20_leafs <- alpha_Bas_U20_leafs*U20$Sc[group == i]^beta_Bas_U20_leafs
      eps_U20 <- Yhat_U20_leafs - U20$Bfkg[group == i]
      eps_U20 <- sample(eps_U20, size=nrow(U20[group == i,]), replace = TRUE)
      Upper_leafs_U20[group == i] <- Yhat_U20_leafs + quantile(eps_U20,probs=c(0.975))
      Lower_leafs_U20[group == i] <- Yhat_U20_leafs + quantile(eps_U20,probs=c(0.025))
    }
  LogB_Boot_UB_U20[[b]] <- Upper_leafs_U20
  LogB_Boot_LB_U20[[b]] <- Lower_leafs_U20
}
Upper_Limit_U20 <- unlist(LogB_Boot_UB_U20)
Lower_Limit_U20 <- unlist(LogB_Boot_LB_U20)
#Create the plot
plot_U20 <- cbind(Upper_Limit_U20, Lower_Limit_U20, U20)
#Compute the global coverage
plot_U20 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_U20 & Bfkg <= Upper_Limit_U20, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_U20, aes(x = Sc, y = Bfkg))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_U20), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_U20), col = "coral2", linetype = "dashed")


##Between 20 and 30 Sc
n <- nrow(U30)
LogB_Boot_UB_U30 <- vector("list", B)
LogB_Boot_LB_U30 <- vector("list", B)
Upper_leafs_U30 <- numeric(n)
Lower_leafs_U30 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_U30 <- lm(form_LogLog_leafs, data = sample(U30[group != i,], replace = TRUE))
      alpha_Bas_U30_leafs <- exp(coef(lmsub_U30)[1]+((sigma(lmsub_U30))^2)/2)
      beta_Bas_U30_leafs <- coef(lmsub_U30)[2]
      Yhat_U30_leafs <- alpha_Bas_U30_leafs*U30$Sc[group == i]^beta_Bas_U30_leafs
      eps_U30 <- Yhat_U30_leafs - U30$Bfkg[group == i]
      eps_U30 <- sample(eps_U30, size=nrow(U30[group == i,]), replace = TRUE)
      Upper_leafs_U30[group == i] <- Yhat_U30_leafs + quantile(eps_U30,probs=c(0.975))
      Lower_leafs_U30[group == i] <- Yhat_U30_leafs + quantile(eps_U30,probs=c(0.025))
    }
  LogB_Boot_UB_U30[[b]] <- Upper_leafs_U30
  LogB_Boot_LB_U30[[b]] <- Lower_leafs_U30
}
Upper_Limit_U30 <- unlist(LogB_Boot_UB_U30)
Lower_Limit_U30 <- unlist(LogB_Boot_LB_U30)
#Create the plot
plot_U30 <- cbind(Upper_Limit_U30, Lower_Limit_U30, U30)
#Compute the global coverage
plot_U30 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_U30 & Bfkg <= Upper_Limit_U30, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_U30, aes(x = Sc, y = Bfkg))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_U30), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_U30), col = "coral2", linetype = "dashed")


##Over 30 Sc
n <- nrow(O30)
LogB_Boot_UB_O30 <- vector("list", B)
LogB_Boot_LB_O30 <- vector("list", B)
Upper_leafs_O30 <- numeric(n)
Lower_leafs_O30 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_O30 <- lm(form_LogLog_leafs, data = sample(O30[group != i,], replace = TRUE))
      alpha_Bas_O30_leafs <- exp(coef(lmsub_O30)[1]+((sigma(lmsub_O30))^2)/2)
      beta_Bas_O30_leafs <- coef(lmsub_O30)[2]
      Yhat_O30_leafs <- alpha_Bas_O30_leafs*O30$Sc[group == i]^beta_Bas_O30_leafs
      eps_O30 <- Yhat_O30_leafs - O30$Bfkg[group == i]
      eps_O30 <- sample(eps_O30, size=nrow(O30[group == i,]), replace = TRUE)
      Upper_leafs_O30[group == i] <- Yhat_O30_leafs + quantile(eps_O30,probs=c(0.975))
      Lower_leafs_O30[group == i] <- Yhat_O30_leafs + quantile(eps_O30,probs=c(0.025))
    }
  LogB_Boot_UB_O30[[b]] <- Upper_leafs_O30
  LogB_Boot_LB_O30[[b]] <- Lower_leafs_O30
}
Upper_Limit_O30 <- unlist(LogB_Boot_UB_O30)
Lower_Limit_O30 <- unlist(LogB_Boot_LB_O30)
#Create the plot
plot_O30 <- cbind(Upper_Limit_O30, Lower_Limit_O30, O30)
#Compute the global coverage
plot_O30 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_O30 & Bfkg <= Upper_Limit_O30, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_O30, aes(x = Sc, y = Bfkg))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_O30), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_O30), col = "coral2", linetype = "dashed")