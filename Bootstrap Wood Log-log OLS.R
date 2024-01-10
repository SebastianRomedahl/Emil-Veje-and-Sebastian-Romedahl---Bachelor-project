#Bootstrap wood Log-log OLS split 3
##Under 1 Sc
form_LogLog_wood <- log(mbt) ~ log(Sc)
B <- 1000
n <- nrow(WU1_3)
LogB_Boot_UB_WU1 <- vector("list", B)
LogB_Boot_LB_WU1 <- vector("list", B)
Upper_wood_WU1 <- numeric(n)
Lower_wood_WU1 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_WU1 <- lm(form_LogLog_wood, data = sample(WU1_3[group != i,], replace = TRUE))
      alpha_Bas_WU1_wood <- exp(coef(lmsub_WU1)[1]+((sigma(lmsub_WU1))^2)/2)
      beta_Bas_WU1_wood <- coef(lmsub_WU1)[2]
      Yhat_WU1_wood <- alpha_Bas_WU1_wood*WU1_3$Sc[group == i]^beta_Bas_WU1_wood
      eps_WU1 <- Yhat_WU1_wood - WU1_3$mbt[group == i]
      eps_WU1 <- sample(eps_WU1, size=nrow(WU1_3[group == i,]), replace = TRUE)
      Upper_wood_WU1[group == i] <- Yhat_WU1_wood + quantile(eps_WU1,probs=c(0.975))
      Lower_wood_WU1[group == i] <- Yhat_WU1_wood + quantile(eps_WU1,probs=c(0.025))
    }
  LogB_Boot_UB_WU1[[b]] <- Upper_wood_WU1
  LogB_Boot_LB_WU1[[b]] <- Lower_wood_WU1
}
Upper_Limit_WU1 <- unlist(LogB_Boot_UB_WU1)
Lower_Limit_WU1 <- unlist(LogB_Boot_LB_WU1)
#Create the plot
plot_WU1 <- cbind(Upper_Limit_WU1, Lower_Limit_WU1, WU1_3)
#Compute the global coverage
plot_WU1 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_WU1 & mbt <= Upper_Limit_WU1, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_WU1, aes(x = Sc, y = mbt))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_WU1), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_WU1), col = "coral2", linetype = "dashed")


##Between 1 and 2 Sc
n <- nrow(WU2_3)
LogB_Boot_UB_WU2 <- vector("list", B)
LogB_Boot_LB_WU2 <- vector("list", B)
Upper_wood_WU2 <- numeric(n)
Lower_wood_WU2 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_WU2 <- lm(form_LogLog_wood, data = sample(WU2_3[group != i,], replace = TRUE))
      alpha_Bas_WU2_wood <- exp(coef(lmsub_WU2)[1]+((sigma(lmsub_WU2))^2)/2)
      beta_Bas_WU2_wood <- coef(lmsub_WU2)[2]
      Yhat_WU2_wood <- alpha_Bas_WU2_wood*WU2_3$Sc[group == i]^beta_Bas_WU2_wood
      eps_WU2 <- Yhat_WU2_wood - WU2_3$mbt[group == i]
      eps_WU2 <- sample(eps_WU2, size=nrow(WU2_3[group == i,]), replace = TRUE)
      Upper_wood_WU2[group == i] <- Yhat_WU2_wood + quantile(eps_WU2,probs=c(0.975))
      Lower_wood_WU2[group == i] <- Yhat_WU2_wood + quantile(eps_WU2,probs=c(0.025))
    }
  LogB_Boot_UB_WU2[[b]] <- Upper_wood_WU2
  LogB_Boot_LB_WU2[[b]] <- Lower_wood_WU2
}
Upper_Limit_WU2 <- unlist(LogB_Boot_UB_WU2)
Lower_Limit_WU2 <- unlist(LogB_Boot_LB_WU2)
#Create the plot
plot_WU2 <- cbind(Upper_Limit_WU2, Lower_Limit_WU2, WU2_3)
#Compute the global coverage
plot_WU2 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_WU2 & mbt <= Upper_Limit_WU2, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_WU2, aes(x = Sc, y = mbt))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_WU2), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_WU2), col = "coral2", linetype = "dashed")


##Between 2 and 3 Sc
n <- nrow(WU3_3)
LogB_Boot_UB_WU3 <- vector("list", B)
LogB_Boot_LB_WU3 <- vector("list", B)
Upper_wood_WU3 <- numeric(n)
Lower_wood_WU3 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_WU3 <- lm(form_LogLog_wood, data = sample(WU3_3[group != i,], replace = TRUE))
      alpha_Bas_WU3_wood <- exp(coef(lmsub_WU3)[1]+((sigma(lmsub_WU3))^2)/2)
      beta_Bas_WU3_wood <- coef(lmsub_WU3)[2]
      Yhat_WU3_wood <- alpha_Bas_WU3_wood*WU3_3$Sc[group == i]^beta_Bas_WU3_wood
      eps_WU3 <- Yhat_WU3_wood - WU3_3$mbt[group == i]
      eps_WU3 <- sample(eps_WU3, size=nrow(WU3_3[group == i,]), replace = TRUE)
      Upper_wood_WU3[group == i] <- Yhat_WU3_wood + quantile(eps_WU3,probs=c(0.975))
      Lower_wood_WU3[group == i] <- Yhat_WU3_wood + quantile(eps_WU3,probs=c(0.025))
    }
  LogB_Boot_UB_WU3[[b]] <- Upper_wood_WU3
  LogB_Boot_LB_WU3[[b]] <- Lower_wood_WU3
}
Upper_Limit_WU3 <- unlist(LogB_Boot_UB_WU3)
Lower_Limit_WU3 <- unlist(LogB_Boot_LB_WU3)
#Create the plot
plot_WU3 <- cbind(Upper_Limit_WU3, Lower_Limit_WU3, WU3_3)
#Compute the global coverage
plot_WU3 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_WU3 & mbt <= Upper_Limit_WU3, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_WU3, aes(x = Sc, y = mbt))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_WU3), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_WU3), col = "coral2", linetype = "dashed")


##Between 3 and 5 Sc
n <- nrow(WU5_3)
LogB_Boot_UB_WU5 <- vector("list", B)
LogB_Boot_LB_WU5 <- vector("list", B)
Upper_wood_WU5 <- numeric(n)
Lower_wood_WU5 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_WU5 <- lm(form_LogLog_wood, data = sample(WU5_3[group != i,], replace = TRUE))
      alpha_Bas_WU5_wood <- exp(coef(lmsub_WU5)[1]+((sigma(lmsub_WU5))^2)/2)
      beta_Bas_WU5_wood <- coef(lmsub_WU5)[2]
      Yhat_WU5_wood <- alpha_Bas_WU5_wood*WU5_3$Sc[group == i]^beta_Bas_WU5_wood
      eps_WU5 <- Yhat_WU5_wood - WU5_3$mbt[group == i]
      eps_WU5 <- sample(eps_WU5, size=nrow(WU5_3[group == i,]), replace = TRUE)
      Upper_wood_WU5[group == i] <- Yhat_WU5_wood + quantile(eps_WU5,probs=c(0.975))
      Lower_wood_WU5[group == i] <- Yhat_WU5_wood + quantile(eps_WU5,probs=c(0.025))
    }
  LogB_Boot_UB_WU5[[b]] <- Upper_wood_WU5
  LogB_Boot_LB_WU5[[b]] <- Lower_wood_WU5
}
Upper_Limit_WU5 <- unlist(LogB_Boot_UB_WU5)
Lower_Limit_WU5 <- unlist(LogB_Boot_LB_WU5)
#Create the plot
plot_WU5 <- cbind(Upper_Limit_WU5, Lower_Limit_WU5, WU5_3)
#Compute the global coverage
plot_WU5 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_WU5 & mbt <= Upper_Limit_WU5, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_WU5, aes(x = Sc, y = mbt))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_WU5), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_WU5), col = "coral2", linetype = "dashed")


##Between 5 and 10 Sc
n <- nrow(WU10_3)
LogB_Boot_UB_WU10 <- vector("list", B)
LogB_Boot_LB_WU10 <- vector("list", B)
Upper_wood_WU10 <- numeric(n)
Lower_wood_WU10 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_WU10 <- lm(form_LogLog_wood, data = sample(WU10_3[group != i,], replace = TRUE))
      alpha_Bas_WU10_wood <- exp(coef(lmsub_WU10)[1]+((sigma(lmsub_WU10))^2)/2)
      beta_Bas_WU10_wood <- coef(lmsub_WU10)[2]
      Yhat_WU10_wood <- alpha_Bas_WU10_wood*WU10_3$Sc[group == i]^beta_Bas_WU10_wood
      eps_WU10 <- Yhat_WU10_wood - WU10_3$mbt[group == i]
      eps_WU10 <- sample(eps_WU10, size=nrow(WU10_3[group == i,]), replace = TRUE)
      Upper_wood_WU10[group == i] <- Yhat_WU10_wood + quantile(eps_WU10,probs=c(0.975))
      Lower_wood_WU10[group == i] <- Yhat_WU10_wood + quantile(eps_WU10,probs=c(0.025))
    }
  LogB_Boot_UB_WU10[[b]] <- Upper_wood_WU10
  LogB_Boot_LB_WU10[[b]] <- Lower_wood_WU10
}
Upper_Limit_WU10 <- unlist(LogB_Boot_UB_WU10)
Lower_Limit_WU10 <- unlist(LogB_Boot_LB_WU10)
#Create the plot
plot_WU10 <- cbind(Upper_Limit_WU10, Lower_Limit_WU10, WU10_3)
#Compute the global coverage
plot_WU10%>%
  mutate(covered = ifelse(mbt >= Lower_Limit_WU10 & mbt <= Upper_Limit_WU10, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_WU10, aes(x = Sc, y = mbt))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_WU10), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_WU10), col = "coral2", linetype = "dashed")


## Between 10 and 20 Sc
n <- nrow(WU20_3)
LogB_Boot_UB_WU20 <- vector("list", B)
LogB_Boot_LB_WU20 <- vector("list", B)
Upper_wood_WU20 <- numeric(n)
Lower_wood_WU20 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_WU20 <- lm(form_LogLog_wood, data = sample(WU20_3[group != i,], replace = TRUE))
      alpha_Bas_WU20_wood <- exp(coef(lmsub_WU20)[1]+((sigma(lmsub_WU20))^2)/2)
      beta_Bas_WU20_wood <- coef(lmsub_WU20)[2]
      Yhat_WU20_wood <- alpha_Bas_WU20_wood*WU20_3$Sc[group == i]^beta_Bas_WU20_wood
      eps_WU20 <- Yhat_WU20_wood - WU20_3$mbt[group == i]
      eps_WU20 <- sample(eps_WU20, size=nrow(WU20_3[group == i,]), replace = TRUE)
      Upper_wood_WU20[group == i] <- Yhat_WU20_wood + quantile(eps_WU20,probs=c(0.975))
      Lower_wood_WU20[group == i] <- Yhat_WU20_wood + quantile(eps_WU20,probs=c(0.025))
    }
  LogB_Boot_UB_WU20[[b]] <- Upper_wood_WU20
  LogB_Boot_LB_WU20[[b]] <- Lower_wood_WU20
}
Upper_Limit_WU20 <- unlist(LogB_Boot_UB_WU20)
Lower_Limit_WU20 <- unlist(LogB_Boot_LB_WU20)
#Create the plot
plot_WU20 <- cbind(Upper_Limit_WU20, Lower_Limit_WU20, WU20_3)
#Compute the global coverage
plot_WU20%>%
  mutate(covered = ifelse(mbt >= Lower_Limit_WU20 & mbt <= Upper_Limit_WU20, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_WU20, aes(x = Sc, y = mbt))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_WU20), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_WU20), col = "coral2", linetype = "dashed")


##Between 20 and 30 Sc
n <- nrow(WU30_3)
LogB_Boot_UB_WU30 <- vector("list", B)
LogB_Boot_LB_WU30 <- vector("list", B)
Upper_wood_WU30 <- numeric(n)
Lower_wood_WU30 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_WU30 <- lm(form_LogLog_wood, data = sample(WU30_3[group != i,], replace = TRUE))
      alpha_Bas_WU30_wood <- exp(coef(lmsub_WU30)[1]+((sigma(lmsub_WU30))^2)/2)
      beta_Bas_WU30_wood <- coef(lmsub_WU30)[2]
      Yhat_WU30_wood <- alpha_Bas_WU30_wood*WU30_3$Sc[group == i]^beta_Bas_WU30_wood
      eps_WU30 <- Yhat_WU30_wood - WU30_3$mbt[group == i]
      eps_WU30 <- sample(eps_WU30, size=nrow(WU30_3[group == i,]), replace = TRUE)
      Upper_wood_WU30[group == i] <- Yhat_WU30_wood + quantile(eps_WU30,probs=c(0.975))
      Lower_wood_WU30[group == i] <- Yhat_WU30_wood + quantile(eps_WU30,probs=c(0.025))
    }
  LogB_Boot_UB_WU30[[b]] <- Upper_wood_WU30
  LogB_Boot_LB_WU30[[b]] <- Lower_wood_WU30
}
Upper_Limit_WU30 <- unlist(LogB_Boot_UB_WU30)
Lower_Limit_WU30 <- unlist(LogB_Boot_LB_WU30)
#Create the plot
plot_WU30 <- cbind(Upper_Limit_WU30, Lower_Limit_WU30, WU30_3)
#Compute the global coverage
plot_WU30%>%
  mutate(covered = ifelse(mbt >= Lower_Limit_WU30 & mbt <= Upper_Limit_WU30, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_WU30, aes(x = Sc, y = mbt))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_WU30), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_WU30), col = "coral2", linetype = "dashed")


##Over 30 Sc
n <- nrow(WO30_3)
LogB_Boot_UB_WO30 <- vector("list", B)
LogB_Boot_LB_WO30 <- vector("list", B)
Upper_wood_WO30 <- numeric(n)
Lower_wood_WO30 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub_WO30 <- lm(form_LogLog_wood, data = sample(WO30_3[group != i,], replace = TRUE))
      alpha_Bas_WO30_wood <- exp(coef(lmsub_WO30)[1]+((sigma(lmsub_WO30))^2)/2)
      beta_Bas_WO30_wood <- coef(lmsub_WO30)[2]
      Yhat_WO30_wood <- alpha_Bas_WO30_wood*WO30_3$Sc[group == i]^beta_Bas_WO30_wood
      eps_WO30 <- Yhat_WO30_wood - WO30_3$mbt[group == i]
      eps_WO30 <- sample(eps_WO30, size=nrow(WO30_3[group == i,]), replace = TRUE)
      Upper_wood_WO30[group == i] <- Yhat_WO30_wood + quantile(eps_WO30,probs=c(0.975))
      Lower_wood_WO30[group == i] <- Yhat_WO30_wood + quantile(eps_WO30,probs=c(0.025))
    }
  LogB_Boot_UB_WO30[[b]] <- Upper_wood_WO30
  LogB_Boot_LB_WO30[[b]] <- Lower_wood_WO30
}
Upper_Limit_WO30 <- unlist(LogB_Boot_UB_WO30)
Lower_Limit_WO30 <- unlist(LogB_Boot_LB_WO30)
#Create the plot
plot_WO30 <- cbind(Upper_Limit_WO30, Lower_Limit_WO30, WO30_3)
#Compute the global coverage
plot_WO30%>%
  mutate(covered = ifelse(mbt >= Lower_Limit_WO30 & mbt <= Upper_Limit_WO30, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)
#Plotting the prediction intervals
ggplot(plot_WO30, aes(x = Sc, y = mbt))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_WO30), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_WO30), col = "coral2", linetype = "dashed")