#t-distribution prediction intervals for log-log model with Baskerville corrections for split 3 of leafs
##Under 1 - leafs
B <- 1
k <- 10
n <- nrow(U1)
LLB_Leafs_UB_U1 <- vector("list", B)
LLB_Leafs_LB_U1 <- vector("list", B)
LLB_Leafs_Upper_U1 <- numeric(n)
LLB_Leafs_Lower_U1 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U1[group != i,])
    yhat <- predict(modelcv, data = U1$Sc[group == i])
    logxhat <- log(U1$Sc[group == i])
    df_leafs_U1 <- nrow(U1[group != i,])-1
    t_value_leafs_U1 <- qt(p = 0.95, df = df_leafs_U1)
    sigmahat <- sigma(modelcv)
    LLB_Leafs_Upper_U1[group == i] <- exp((yhat+t_value_leafs_U1*sigmahat)+(sigmahat^2)/2)
    LLB_Leafs_Lower_U1[group == i] <- exp((yhat-t_value_leafs_U1*sigmahat)+(sigmahat^2)/2)
  }
  LLB_Leafs_UB_U1[[b]] <- LLB_Leafs_Upper_U1
  LLB_Leafs_LB_U1[[b]] <- LLB_Leafs_Lower_U1
}
Upper_Limit_leafs_U1 <- unlist(LLB_Leafs_UB_U1)
Lower_Limit_leafs_U1 <- unlist(LLB_Leafs_LB_U1)




Plot_leafs_U1 <- cbind(U1, Upper_Limit_leafs_U1, Lower_Limit_leafs_U1)

Plot_leafs_U1 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_leafs_U1 & Bfkg <= Upper_Limit_leafs_U1, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_leafs_split_U1 <- ggplot(Plot_leafs_U1, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_leafs_U1), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_leafs_U1), col = "coral2", linetype = "dashed")+
  ggtitle("Under 1")


##Between 1 and 2 - leafs
n <- nrow(U2)
LLB_Leafs_UB_U2 <- vector("list", B)
LLB_Leafs_LB_U2 <- vector("list", B)
LLB_Leafs_Upper_U2 <- numeric(n)
LLB_Leafs_Lower_U2 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U2[group != i,])
    yhat <- predict(modelcv, data = U2$Sc[group == i])
    logxhat <- log(U2$Sc[group == i])
    df_leafs_U2 <- nrow(U2[group != i,])-1
    t_value_leafs_U2 <- qt(p = 0.95, df = df_leafs_U2)
    sigmahat <- sigma(modelcv)
    LLB_Leafs_Upper_U2[group == i] <- exp((yhat+t_value_leafs_U2*sigmahat)+(sigmahat^2)/2)
    LLB_Leafs_Lower_U2[group == i] <- exp((yhat-t_value_leafs_U2*sigmahat)+(sigmahat^2)/2)
  }
  LLB_Leafs_UB_U2[[b]] <- LLB_Leafs_Upper_U2
  LLB_Leafs_LB_U2[[b]] <- LLB_Leafs_Lower_U2
}
Upper_Limit_leafs_U2 <- unlist(LLB_Leafs_UB_U2)
Lower_Limit_leafs_U2 <- unlist(LLB_Leafs_LB_U2)



Plot_leafs_U2 <- cbind(U2, Upper_Limit_leafs_U2, Lower_Limit_leafs_U2)

Plot_leafs_U2 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_leafs_U2 & Bfkg <= Upper_Limit_leafs_U2, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_leafs_split_U2 <- ggplot(Plot_leafs_U2, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_leafs_U2), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_leafs_U2), col = "coral2", linetype = "dashed")+
  ggtitle("Between 1 and 2")


##Between 2 and 3 - leafs
n <- nrow(U3)
LLB_Leafs_UB_U3 <- vector("list", B)
LLB_Leafs_LB_U3 <- vector("list", B)
LLB_Leafs_Upper_U3 <- numeric(n)
LLB_Leafs_Lower_U3 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U3[group != i,])
    yhat <- predict(modelcv, data = U3$Sc[group == i])
    logxhat <- log(U3$Sc[group == i])
    df_leafs_U3 <- nrow(U3[group != i,])-1
    t_value_leafs_U3 <- qt(p = 0.95, df = df_leafs_U3)
    sigmahat <- sigma(modelcv)
    LLB_Leafs_Upper_U3[group == i] <- exp((yhat+t_value_leafs_U3*sigmahat)+(sigmahat^2)/2)
    LLB_Leafs_Lower_U3[group == i] <- exp((yhat-t_value_leafs_U3*sigmahat)+(sigmahat^2)/2)
  }
  LLB_Leafs_UB_U3[[b]] <- LLB_Leafs_Upper_U3
  LLB_Leafs_LB_U3[[b]] <- LLB_Leafs_Lower_U3
}
Upper_Limit_leafs_U3 <- unlist(LLB_Leafs_UB_U3)
Lower_Limit_leafs_U3 <- unlist(LLB_Leafs_LB_U3)



Plot_leafs_U3 <- cbind(U3, Upper_Limit_leafs_U3, Lower_Limit_leafs_U3)

Plot_leafs_U3 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_leafs_U3 & Bfkg <= Upper_Limit_leafs_U3, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_leafs_split_U3 <- ggplot(Plot_leafs_U3, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_leafs_U3), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_leafs_U3), col = "coral2", linetype = "dashed")+
  ggtitle("Between 2 and 3")


##Between 3 and 5 - leafs
n <- nrow(U5)
LLB_Leafs_UB_U5 <- vector("list", B)
LLB_Leafs_LB_U5 <- vector("list", B)
LLB_Leafs_Upper_U5 <- numeric(n)
LLB_Leafs_Lower_U5 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U5[group != i,])
    yhat <- predict(modelcv, data = U5$Sc[group == i])
    logxhat <- log(U5$Sc[group == i])
    df_leafs_U5 <- nrow(U5[group != i,])-1
    t_value_leafs_U5 <- qt(p = 0.95, df = df_leafs_U5)
    sigmahat <- sigma(modelcv)
    LLB_Leafs_Upper_U5[group == i] <- exp((yhat+t_value_leafs_U5*sigmahat)+(sigmahat^2)/2)
    LLB_Leafs_Lower_U5[group == i] <- exp((yhat-t_value_leafs_U5*sigmahat)+(sigmahat^2)/2)
  }
  LLB_Leafs_UB_U5[[b]] <- LLB_Leafs_Upper_U5
  LLB_Leafs_LB_U5[[b]] <- LLB_Leafs_Lower_U5
}
Upper_Limit_leafs_U5 <- unlist(LLB_Leafs_UB_U5)
Lower_Limit_leafs_U5 <- unlist(LLB_Leafs_LB_U5)



Plot_leafs_U5 <- cbind(U5, Upper_Limit_leafs_U5, Lower_Limit_leafs_U5)

Plot_leafs_U5 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_leafs_U5 & Bfkg <= Upper_Limit_leafs_U5, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_leafs_split_U5 <- ggplot(Plot_leafs_U5, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_leafs_U5), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_leafs_U5), col = "coral2", linetype = "dashed")+
  ggtitle("Between 3 and 5")


##Between 5 and 10 - leafs
n <- nrow(U10)
LLB_Leafs_UB_U10 <- vector("list", B)
LLB_Leafs_LB_U10 <- vector("list", B)
LLB_Leafs_Upper_U10 <- numeric(n)
LLB_Leafs_Lower_U10 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U10[group != i,])
    yhat <- predict(modelcv, data = U10$Sc[group == i])
    logxhat <- log(U10$Sc[group == i])
    df_leafs_U10 <- nrow(U10[group != i,])-1
    t_value_leafs_U10 <- qt(p = 0.95, df = df_leafs_U10)
    sigmahat <- sigma(modelcv)
    LLB_Leafs_Upper_U10[group == i] <- exp((yhat+t_value_leafs_U10*sigmahat)+(sigmahat^2)/2)
    LLB_Leafs_Lower_U10[group == i] <- exp((yhat-t_value_leafs_U10*sigmahat)+(sigmahat^2)/2)
  }
  LLB_Leafs_UB_U10[[b]] <- LLB_Leafs_Upper_U10
  LLB_Leafs_LB_U10[[b]] <- LLB_Leafs_Lower_U10
}
Upper_Limit_leafs_U10 <- unlist(LLB_Leafs_UB_U10)
Lower_Limit_leafs_U10 <- unlist(LLB_Leafs_LB_U10)



Plot_leafs_U10 <- cbind(U10, Upper_Limit_leafs_U10, Lower_Limit_leafs_U10)

Plot_leafs_U10 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_leafs_U10 & Bfkg <= Upper_Limit_leafs_U10, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_leafs_split_U10 <- ggplot(Plot_leafs_U10, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_leafs_U10), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_leafs_U10), col = "coral2", linetype = "dashed")+
  ggtitle("Between 5 and 10")


##Between 10 and 20 - leafs
n <- nrow(U20)
LLB_Leafs_UB_U20 <- vector("list", B)
LLB_Leafs_LB_U20 <- vector("list", B)
LLB_Leafs_Upper_U20 <- numeric(n)
LLB_Leafs_Lower_U20 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U20[group != i,])
    yhat <- predict(modelcv, data = U20$Sc[group == i])
    logxhat <- log(U20$Sc[group == i])
    df_leafs_U20 <- nrow(U20[group != i,])-1
    t_value_leafs_U20 <- qt(p = 0.95, df = df_leafs_U20)
    sigmahat <- sigma(modelcv)
    LLB_Leafs_Upper_U20[group == i] <- exp((yhat+t_value_leafs_U20*sigmahat)+(sigmahat^2)/2)
    LLB_Leafs_Lower_U20[group == i] <- exp((yhat-t_value_leafs_U20*sigmahat)+(sigmahat^2)/2)
  }
  LLB_Leafs_UB_U20[[b]] <- LLB_Leafs_Upper_U20
  LLB_Leafs_LB_U20[[b]] <- LLB_Leafs_Lower_U20
}
Upper_Limit_leafs_U20 <- unlist(LLB_Leafs_UB_U20)
Lower_Limit_leafs_U20 <- unlist(LLB_Leafs_LB_U20)



Plot_leafs_U20 <- cbind(U20, Upper_Limit_leafs_U20, Lower_Limit_leafs_U20)

Plot_leafs_U20 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_leafs_U20 & Bfkg <= Upper_Limit_leafs_U20, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_leafs_split_U20 <- ggplot(Plot_leafs_U20, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_leafs_U20), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_leafs_U20), col = "coral2", linetype = "dashed")+
  ggtitle("Between 10 and 20")


##Between 20 and 30 - leafs
n <- nrow(U30)
LLB_Leafs_UB_U30 <- vector("list", B)
LLB_Leafs_LB_U30 <- vector("list", B)
LLB_Leafs_Upper_U30 <- numeric(n)
LLB_Leafs_Lower_U30 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U30[group != i,])
    yhat <- predict(modelcv, data = U30$Sc[group == i])
    logxhat <- log(U30$Sc[group == i])
    df_leafs_U30 <- nrow(U30[group != i,])-1
    t_value_leafs_U30 <- qt(p = 0.95, df = df_leafs_U30)
    sigmahat <- sigma(modelcv)
    LLB_Leafs_Upper_U30[group == i] <- exp((yhat+t_value_leafs_U30*sigmahat)+(sigmahat^2)/2)
    LLB_Leafs_Lower_U30[group == i] <- exp((yhat-t_value_leafs_U30*sigmahat)+(sigmahat^2)/2)
  }
  LLB_Leafs_UB_U30[[b]] <- LLB_Leafs_Upper_U30
  LLB_Leafs_LB_U30[[b]] <- LLB_Leafs_Lower_U30
}
Upper_Limit_leafs_U30 <- unlist(LLB_Leafs_UB_U30)
Lower_Limit_leafs_U30 <- unlist(LLB_Leafs_LB_U30)



Plot_leafs_U30 <- cbind(U30, Upper_Limit_leafs_U30, Lower_Limit_leafs_U30)

Plot_leafs_U30 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_leafs_U30 & Bfkg <= Upper_Limit_leafs_U30, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_leafs_split_U30 <- ggplot(Plot_leafs_U30, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_leafs_U30), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_leafs_U30), col = "coral2", linetype = "dashed")+
  ggtitle("Between 20 and 30")


##Over 30 - leafs
n <- nrow(O30)
LLB_Leafs_UB_O30 <- vector("list", B)
LLB_Leafs_LB_O30 <- vector("list", B)
LLB_Leafs_Upper_O30 <- numeric(n)
LLB_Leafs_Lower_O30 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = O30[group != i,])
    yhat <- predict(modelcv, data = O30$Sc[group == i])
    logxhat <- log(O30$Sc[group == i])
    df_leafs_O30 <- nrow(O30[group != i,])-1
    t_value_leafs_O30 <- qt(p = 0.95, df = df_leafs_O30)
    sigmahat <- sigma(modelcv)
    LLB_Leafs_Upper_O30[group == i] <- exp((yhat+t_value_leafs_O30*sigmahat)+(sigmahat^2)/2)
    LLB_Leafs_Lower_O30[group == i] <- exp((yhat-t_value_leafs_O30*sigmahat)+(sigmahat^2)/2)
  }
  LLB_Leafs_UB_O30[[b]] <- LLB_Leafs_Upper_O30
  LLB_Leafs_LB_O30[[b]] <- LLB_Leafs_Lower_O30
}
Upper_Limit_leafs_O30 <- unlist(LLB_Leafs_UB_O30)
Lower_Limit_leafs_O30 <- unlist(LLB_Leafs_LB_O30)



Plot_leafs_O30 <- cbind(O30, Upper_Limit_leafs_O30, Lower_Limit_leafs_O30)

Plot_leafs_O30 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_leafs_O30 & Bfkg <= Upper_Limit_leafs_O30, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_leafs_split_O30 <- ggplot(Plot_leafs_O30, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_leafs_O30), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_leafs_O30), col = "coral2", linetype = "dashed")+
  ggtitle("Over 30")

##Making a combined plot
t_dist_split_plot <- grid.arrange(t_dist_leafs_split_U1, t_dist_leafs_split_U2, t_dist_leafs_split_U3, t_dist_leafs_split_U5, t_dist_leafs_split_U10, t_dist_leafs_split_U20, t_dist_leafs_split_U30, t_dist_leafs_split_O30, ncol = 2)

ggsave(file = "Leafs Splits t-distribution Prediction Intervals.pdf", plot = t_dist_split_plot, height = 10, width = 10)


#t-distribution prediction intervals for log-log model with Baskerville corrections for split 3 of wood
##Under 1 - wood
woodform_loglog <- log(mbt)~log(Sc)
B <- 1
k <- 10
n <- nrow(WU1_3)
LLB_Wood_UB_U1 <- vector("list", B)
LLB_Wood_LB_U1 <- vector("list", B)
LLB_Wood_Upper_U1 <- numeric(n)
LLB_Wood_Lower_U1 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(woodform_loglog, data = WU1_3[group != i,])
    yhat <- predict(modelcv, data = WU1_3$Sc[group == i])
    df_wood_U1 <- nrow(WU1_3[group != i,])-1
    t_value_wood_U1 <- qt(p = 0.95, df = df_wood_U1)
    sigmahat_wood <- sigma(modelcv)
    LLB_Wood_Upper_U1[group == i] <- exp((yhat+t_value_wood_U1*sigmahat_wood)+(sigmahat_wood^2)/2)
    LLB_Wood_Lower_U1[group == i] <- exp((yhat-t_value_wood_U1*sigmahat_wood)+(sigmahat_wood^2)/2)
  }
  LLB_Wood_UB_U1[[b]] <- LLB_Wood_Upper_U1
  LLB_Wood_LB_U1[[b]] <- LLB_Wood_Lower_U1
}
Upper_Limit_wood_U1 <- unlist(LLB_Wood_UB_U1)
Lower_Limit_wood_U1 <- unlist(LLB_Wood_LB_U1)



Plot_wood_U1 <- cbind(WU1_3, Upper_Limit_wood_U1, Lower_Limit_wood_U1)

Plot_wood_U1 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_wood_U1 & mbt <= Upper_Limit_wood_U1, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_wood_split_U1 <- ggplot(Plot_wood_U1, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_wood_U1), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_wood_U1), col = "coral2", linetype = "dashed")+
  ggtitle("Under 1")

##Between 1 and 2 - wood
n <- nrow(WU2_3)
LLB_Wood_UB_U2 <- vector("list", B)
LLB_Wood_LB_U2 <- vector("list", B)
LLB_Wood_Upper_U2 <- numeric(n)
LLB_Wood_Lower_U2 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(woodform_loglog, data = WU2_3[group != i,])
    yhat <- predict(modelcv, data = WU2_3$Sc[group == i])
    df_wood_U2 <- nrow(WU2_3[group != i,])-1
    t_value_wood_U2 <- qt(p = 0.95, df = df_wood_U2)
    sigmahat_wood <- sigma(modelcv)
    LLB_Wood_Upper_U2[group == i] <- exp((yhat+t_value_wood_U2*sigmahat_wood)+(sigmahat_wood^2)/2)
    LLB_Wood_Lower_U2[group == i] <- exp((yhat-t_value_wood_U2*sigmahat_wood)+(sigmahat_wood^2)/2)
  }
  LLB_Wood_UB_U2[[b]] <- LLB_Wood_Upper_U2
  LLB_Wood_LB_U2[[b]] <- LLB_Wood_Lower_U2
}
Upper_Limit_wood_U2 <- unlist(LLB_Wood_UB_U2)
Lower_Limit_wood_U2 <- unlist(LLB_Wood_LB_U2)



Plot_wood_U2 <- cbind(WU2_3, Upper_Limit_wood_U2, Lower_Limit_wood_U2)

Plot_wood_U2 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_wood_U2 & mbt <= Upper_Limit_wood_U2, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_wood_split_U2 <- ggplot(Plot_wood_U2, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_wood_U2), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_wood_U2), col = "coral2", linetype = "dashed")+
  ggtitle("Between 1 and 2")



##Between 2 and 3 - wood
n <- nrow(WU3_3)
LLB_Wood_UB_U3 <- vector("list", B)
LLB_Wood_LB_U3 <- vector("list", B)
LLB_Wood_Upper_U3 <- numeric(n)
LLB_Wood_Lower_U3 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(woodform_loglog, data = WU3_3[group != i,])
    yhat <- predict(modelcv, data = WU3_3$Sc[group == i])
    df_wood_U3 <- nrow(WU3_3[group != i,])-1
    t_value_wood_U3 <- qt(p = 0.95, df = df_wood_U3)
    sigmahat_wood <- sigma(modelcv)
    LLB_Wood_Upper_U3[group == i] <- exp((yhat+t_value_wood_U3*sigmahat_wood)+(sigmahat_wood^2)/2)
    LLB_Wood_Lower_U3[group == i] <- exp((yhat-t_value_wood_U3*sigmahat_wood)+(sigmahat_wood^2)/2)
  }
  LLB_Wood_UB_U3[[b]] <- LLB_Wood_Upper_U3
  LLB_Wood_LB_U3[[b]] <- LLB_Wood_Lower_U3
}
Upper_Limit_wood_U3 <- unlist(LLB_Wood_UB_U3)
Lower_Limit_wood_U3 <- unlist(LLB_Wood_LB_U3)



Plot_wood_U3 <- cbind(WU3_3, Upper_Limit_wood_U3, Lower_Limit_wood_U3)

Plot_wood_U3 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_wood_U3 & mbt <= Upper_Limit_wood_U3, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_wood_split_U3 <- ggplot(Plot_wood_U3, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_wood_U3), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_wood_U3), col = "coral2", linetype = "dashed")+
  ggtitle("Between 2 and 3")



##Between 3 and 5 - wood
n <- nrow(WU5_3)
LLB_Wood_UB_U5 <- vector("list", B)
LLB_Wood_LB_U5 <- vector("list", B)
LLB_Wood_Upper_U5 <- numeric(n)
LLB_Wood_Lower_U5 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(woodform_loglog, data = WU5_3[group != i,])
    yhat <- predict(modelcv, data = WU5_3$Sc[group == i])
    df_wood_U5 <- nrow(WU5_3[group != i,])-1
    t_value_wood_U5 <- qt(p = 0.95, df = df_wood_U5)
    sigmahat_wood <- sigma(modelcv)
    LLB_Wood_Upper_U5[group == i] <- exp((yhat+t_value_wood_U5*sigmahat_wood)+(sigmahat_wood^2)/2)
    LLB_Wood_Lower_U5[group == i] <- exp((yhat-t_value_wood_U5*sigmahat_wood)+(sigmahat_wood^2)/2)
  }
  LLB_Wood_UB_U5[[b]] <- LLB_Wood_Upper_U5
  LLB_Wood_LB_U5[[b]] <- LLB_Wood_Lower_U5
}
Upper_Limit_wood_U5 <- unlist(LLB_Wood_UB_U5)
Lower_Limit_wood_U5 <- unlist(LLB_Wood_LB_U5)



Plot_wood_U5 <- cbind(WU5_3, Upper_Limit_wood_U5, Lower_Limit_wood_U5)

Plot_wood_U5 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_wood_U5 & mbt <= Upper_Limit_wood_U5, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_wood_split_U5 <- ggplot(Plot_wood_U5, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_wood_U5), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_wood_U5), col = "coral2", linetype = "dashed")+
  ggtitle("Between 3 and 5")



##Between 5 and 10 - wood
n <- nrow(WU10_3)
LLB_Wood_UB_U10 <- vector("list", B)
LLB_Wood_LB_U10 <- vector("list", B)
LLB_Wood_Upper_U10 <- numeric(n)
LLB_Wood_Lower_U10 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(woodform_loglog, data = WU10_3[group != i,])
    yhat <- predict(modelcv, data = WU10_3$Sc[group == i])
    df_wood_U10 <- nrow(WU10_3[group != i,])-1
    t_value_wood_U10 <- qt(p = 0.95, df = df_wood_U10)
    sigmahat_wood <- sigma(modelcv)
    LLB_Wood_Upper_U10[group == i] <- exp((yhat+t_value_wood_U10*sigmahat_wood)+(sigmahat_wood^2)/2)
    LLB_Wood_Lower_U10[group == i] <- exp((yhat-t_value_wood_U10*sigmahat_wood)+(sigmahat_wood^2)/2)
  }
  LLB_Wood_UB_U10[[b]] <- LLB_Wood_Upper_U10
  LLB_Wood_LB_U10[[b]] <- LLB_Wood_Lower_U10
}
Upper_Limit_wood_U10 <- unlist(LLB_Wood_UB_U10)
Lower_Limit_wood_U10 <- unlist(LLB_Wood_LB_U10)



Plot_wood_U10 <- cbind(WU10_3, Upper_Limit_wood_U10, Lower_Limit_wood_U10)

Plot_wood_U10 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_wood_U10 & mbt <= Upper_Limit_wood_U10, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_wood_split_U10 <- ggplot(Plot_wood_U10, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_wood_U10), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_wood_U10), col = "coral2", linetype = "dashed")+
  ggtitle("Between 5 and 10")



##Between 10 and 20 - wood
n <- nrow(WU20_3)
LLB_Wood_UB_U20 <- vector("list", B)
LLB_Wood_LB_U20 <- vector("list", B)
LLB_Wood_Upper_U20 <- numeric(n)
LLB_Wood_Lower_U20 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(woodform_loglog, data = WU20_3[group != i,])
    yhat <- predict(modelcv, data = WU20_3$Sc[group == i])
    df_wood_U20 <- nrow(WU20_3[group != i,])-1
    t_value_wood_U20 <- qt(p = 0.95, df = df_wood_U20)
    sigmahat_wood <- sigma(modelcv)
    LLB_Wood_Upper_U20[group == i] <- exp((yhat+t_value_wood_U20*sigmahat_wood)+(sigmahat_wood^2)/2)
    LLB_Wood_Lower_U20[group == i] <- exp((yhat-t_value_wood_U20*sigmahat_wood)+(sigmahat_wood^2)/2)
  }
  LLB_Wood_UB_U20[[b]] <- LLB_Wood_Upper_U20
  LLB_Wood_LB_U20[[b]] <- LLB_Wood_Lower_U20
}
Upper_Limit_wood_U20 <- unlist(LLB_Wood_UB_U20)
Lower_Limit_wood_U20 <- unlist(LLB_Wood_LB_U20)



Plot_wood_U20 <- cbind(WU20_3, Upper_Limit_wood_U20, Lower_Limit_wood_U20)

Plot_wood_U20 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_wood_U20 & mbt <= Upper_Limit_wood_U20, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_wood_split_U20 <- ggplot(Plot_wood_U20, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_wood_U20), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_wood_U20), col = "coral2", linetype = "dashed")+
  ggtitle("Between 10 and 20")


##Between 20 and 30 - wood
n <- nrow(WU30_3)
LLB_Wood_UB_U30 <- vector("list", B)
LLB_Wood_LB_U30 <- vector("list", B)
LLB_Wood_Upper_U30 <- numeric(n)
LLB_Wood_Lower_U30 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(woodform_loglog, data = WU30_3[group != i,])
    yhat <- predict(modelcv, data = WU30_3$Sc[group == i])
    df_wood_U30 <- nrow(WU30_3[group != i,])-1
    t_value_wood_U30 <- qt(p = 0.95, df = df_wood_U30)
    sigmahat_wood <- sigma(modelcv)
    LLB_Wood_Upper_U30[group == i] <- exp((yhat+t_value_wood_U30*sigmahat_wood)+(sigmahat_wood^2)/2)
    LLB_Wood_Lower_U30[group == i] <- exp((yhat-t_value_wood_U30*sigmahat_wood)+(sigmahat_wood^2)/2)
  }
  LLB_Wood_UB_U30[[b]] <- LLB_Wood_Upper_U30
  LLB_Wood_LB_U30[[b]] <- LLB_Wood_Lower_U30
}
Upper_Limit_wood_U30 <- unlist(LLB_Wood_UB_U30)
Lower_Limit_wood_U30 <- unlist(LLB_Wood_LB_U30)



Plot_wood_U30 <- cbind(WU30_3, Upper_Limit_wood_U30, Lower_Limit_wood_U30)

Plot_wood_U30 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_wood_U30 & mbt <= Upper_Limit_wood_U30, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_wood_split_U30 <- ggplot(Plot_wood_U30, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_wood_U30), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_wood_U30), col = "coral2", linetype = "dashed")+
  ggtitle("Between 20 and 30")


##Over 30 - wood
n <- nrow(WO30_3)
LLB_Wood_UB_O30 <- vector("list", B)
LLB_Wood_LB_O30 <- vector("list", B)
LLB_Wood_Upper_O30 <- numeric(n)
LLB_Wood_Lower_O30 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(woodform_loglog, data = WO30_3[group != i,])
    yhat <- predict(modelcv, data = WO30_3$Sc[group == i])
    df_wood_O30 <- nrow(WO30_3[group != i,])-1
    t_value_wood_O30 <- qt(p = 0.95, df = df_wood_O30)
    sigmahat_wood <- sigma(modelcv)
    LLB_Wood_Upper_O30[group == i] <- exp((yhat+t_value_wood_O30*sigmahat_wood)+(sigmahat_wood^2)/2)
    LLB_Wood_Lower_O30[group == i] <- exp((yhat-t_value_wood_O30*sigmahat_wood)+(sigmahat_wood^2)/2)
  }
  LLB_Wood_UB_O30[[b]] <- LLB_Wood_Upper_O30
  LLB_Wood_LB_O30[[b]] <- LLB_Wood_Lower_O30
}
Upper_Limit_wood_O30 <- unlist(LLB_Wood_UB_O30)
Lower_Limit_wood_O30 <- unlist(LLB_Wood_LB_O30)



Plot_wood_O30 <- cbind(WO30_3, Upper_Limit_wood_O30, Lower_Limit_wood_O30)

Plot_wood_O30 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_wood_O30 & mbt <= Upper_Limit_wood_O30, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

t_dist_wood_split_O30 <- ggplot(Plot_wood_O30, aes(x = Sc, y = mbt))+
  geom_point(color= "blue")+
  geom_smooth(aes(y = Upper_Limit_wood_O30), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_wood_O30), col = "coral2", linetype = "dashed")+
  ggtitle("Over 30")


#Combining the plots
t_dist_split_plot_wood <- grid.arrange(t_dist_wood_split_U1, t_dist_wood_split_U2, t_dist_wood_split_U3, t_dist_wood_split_U5, t_dist_wood_split_U10, t_dist_wood_split_U20, t_dist_wood_split_U30, t_dist_wood_split_O30, ncol =2)

ggsave(file = "Wood Splits t-distribution Prediction Intervals.pdf", plot = t_dist_split_plot_wood, height = 10, width = 10)