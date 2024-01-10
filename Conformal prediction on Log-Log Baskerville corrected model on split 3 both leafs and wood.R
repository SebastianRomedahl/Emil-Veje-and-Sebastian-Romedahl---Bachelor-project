#Conformal prediction on leafs splits
##Under 1 - leafs
form_LogLog <- log(Bfkg) ~ log(Sc)
B <- 10
k <- 10
n <- nrow(U1)
Conformal_Bas <- vector("list", 2)
Conf_Pred_Bas <- vector("list", 2)
muhat_U1 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U1[group != i,])
    alph_Bas_U1 <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U1 <- coef(modelcv)[2]
    Yhat_U1 <- alph_Bas_U1*U1$Sc[group == i]^beta_Bas_U1
    muhat_U1 [group == i] <- sqrt((Yhat_U1-U1$Bfkg[group == i])^2)
    
  }
  Conf_Pred_Bas[[b]] <- muhat_U1
}
Top_U1 <- quantile(unlist(Conf_Pred_Bas),0.975)
Bund_U1 <- quantile(unlist(Conf_Pred_Bas),0.025)


Plot_U1 <- cbind(U1, Top_U1, Bund_U1)

Plot_U1 %>%
  mutate(covered = ifelse(Bfkg >= Bund_U1 & Bfkg <= Top_U1, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Leafs_split_Conf_U1 <- ggplot(Plot_U1, aes(x = Sc, y = Bfkg))+
  geom_point(color= "blue")+
  geom_smooth(aes(y = Top_U1), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U1), col = "coral2", linetype = "dashed")+
  ggtitle("Under 1")

##Between 1 and 2 - leafs
n <- nrow(U2)
Conformal_Bas_U2 <- vector("list", 2)
Conf_Pred_Bas_U2 <- vector("list", 2)
muhat_U2 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U2[group != i,])
    alph_Bas_U2 <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U2 <- coef(modelcv)[2]
    Yhat_U2 <- alph_Bas_U2*U2$Sc[group == i]^beta_Bas_U2
    muhat_U2 [group == i] <- sqrt((Yhat_U2-U2$Bfkg[group == i])^2)
    
  }
  Conf_Pred_Bas_U2[[b]] <- muhat_U2
}
Top_U2 <- quantile(unlist(Conf_Pred_Bas_U2),0.975)
Bund_U2 <- quantile(unlist(Conf_Pred_Bas_U2),0.025)


Plot_U2 <- cbind(U2, Top_U2, Bund_U2)

Plot_U2 %>%
  mutate(covered = ifelse(Bfkg >= Bund_U2 & Bfkg <= Top_U2, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Leafs_split_Conf_U2 <- ggplot(Plot_U2, aes(x = Sc, y = Bfkg))+
  geom_point(color= "blue")+
  geom_smooth(aes(y = Top_U2), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U2), col = "coral2", linetype = "dashed")+
  ggtitle("Between 1 and 2")


##Between 2 and 3 - leafs
n <- nrow(U3)
Conformal_Bas_U3 <- vector("list", 2)
Conf_Pred_Bas_U3 <- vector("list", 2)
muhat_U3 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U3[group != i,])
    alph_Bas_U3 <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U3 <- coef(modelcv)[2]
    Yhat_U3 <- alph_Bas_U3*U3$Sc[group == i]^beta_Bas_U3
    muhat_U3 [group == i] <- sqrt((Yhat_U3-U3$Bfkg[group == i])^2)
    
  }
  Conf_Pred_Bas_U3[[b]] <- muhat_U3
}
Top_U3 <- quantile(unlist(Conf_Pred_Bas_U3),0.975)
Bund_U3 <- quantile(unlist(Conf_Pred_Bas_U3),0.025)


Plot_U3 <- cbind(U3, Top_U3, Bund_U3)

Plot_U3 %>%
  mutate(covered = ifelse(Bfkg >= Bund_U3 & Bfkg <= Top_U3, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Leafs_split_Conf_U3 <- ggplot(Plot_U3, aes(x = Sc, y = Bfkg))+
  geom_point(color= "blue")+
  geom_smooth(aes(y = Top_U3), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U3), col = "coral2", linetype = "dashed")+
  ggtitle("Between 2 and 3")


##Between 3 and 5 - leafs
n <- nrow(U5)
Conformal_Bas_U5 <- vector("list", 2)
Conf_Pred_Bas_U5 <- vector("list", 2)
muhat_U5 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U5[group != i,])
    alph_Bas_U5 <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U5 <- coef(modelcv)[2]
    Yhat_U5 <- alph_Bas_U5*U5$Sc[group == i]^beta_Bas_U5
    muhat_U5 [group == i] <- sqrt((Yhat_U5-U5$Bfkg[group == i])^2)
    
  }
  Conf_Pred_Bas_U5[[b]] <- muhat_U5
}
Top_U5 <- quantile(unlist(Conf_Pred_Bas_U5),0.975)
Bund_U5 <- quantile(unlist(Conf_Pred_Bas_U5),0.025)


Plot_U5 <- cbind(U5, Top_U5, Bund_U5)

Plot_U5 %>%
  mutate(covered = ifelse(Bfkg >= Bund_U5 & Bfkg <= Top_U5, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Leafs_split_Conf_U5 <- ggplot(Plot_U5, aes(x = Sc, y = Bfkg))+
  geom_point(color= "blue")+
  geom_smooth(aes(y = Top_U5), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U5), col = "coral2", linetype = "dashed")+
  ggtitle("Between 3 and 5")


##Between 5 and 10 - leafs
n <- nrow(U10)
Conformal_Bas_U10 <- vector("list", 2)
Conf_Pred_Bas_U10 <- vector("list", 2)
muhat_U10 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U10[group != i,])
    alph_Bas_U10 <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U10 <- coef(modelcv)[2]
    Yhat_U10 <- alph_Bas_U10*U10$Sc[group == i]^beta_Bas_U10
    muhat_U10 [group == i] <- sqrt((Yhat_U10-U10$Bfkg[group == i])^2)
    
  }
  Conf_Pred_Bas_U10[[b]] <- muhat_U10
}
Top_U10 <- quantile(unlist(Conf_Pred_Bas_U10),0.975)
Bund_U10 <- quantile(unlist(Conf_Pred_Bas_U10),0.025)


Plot_U10 <- cbind(U10, Top_U10, Bund_U10)

Plot_U10 %>%
  mutate(covered = ifelse(Bfkg >= Bund_U10 & Bfkg <= Top_U10, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Leafs_split_Conf_U10 <- ggplot(Plot_U10, aes(x = Sc, y = Bfkg))+
  geom_point(color= "blue")+
  geom_smooth(aes(y = Top_U10), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U10), col = "coral2", linetype = "dashed")+
  ggtitle("Between 5 and 10")


##Between 10 and 20 - leafs
n <- nrow(U20)
Conformal_Bas_U20 <- vector("list", 2)
Conf_Pred_Bas_U20 <- vector("list", 2)
muhat_U20 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U20[group != i,])
    alph_Bas_U20 <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U20 <- coef(modelcv)[2]
    Yhat_U20 <- alph_Bas_U20*U20$Sc[group == i]^beta_Bas_U20
    muhat_U20 [group == i] <- sqrt((Yhat_U20-U20$Bfkg[group == i])^2)
    
  }
  Conf_Pred_Bas_U20[[b]] <- muhat_U20
}
Top_U20 <- quantile(unlist(Conf_Pred_Bas_U20),0.975)
Bund_U20 <- quantile(unlist(Conf_Pred_Bas_U20),0.025)


Plot_U20 <- cbind(U20, Top_U20, Bund_U20)

Plot_U20 %>%
  mutate(covered = ifelse(Bfkg >= Bund_U20 & Bfkg <= Top_U20, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Leafs_split_Conf_U20 <- ggplot(Plot_U20, aes(x = Sc, y = Bfkg))+
  geom_point(color= "blue")+
  geom_smooth(aes(y = Top_U20), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U20), col = "coral2", linetype = "dashed")+
  ggtitle("Between 10 and 20")



##Between 20 and 30 - leafs
n <- nrow(U30)
Conformal_Bas_U30 <- vector("list", 2)
Conf_Pred_Bas_U30 <- vector("list", 2)
muhat_U30 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = U30[group != i,])
    alph_Bas_U30 <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U30 <- coef(modelcv)[2]
    Yhat_U30 <- alph_Bas_U30*U30$Sc[group == i]^beta_Bas_U30
    muhat_U30 [group == i] <- sqrt((Yhat_U30-U30$Bfkg[group == i])^2)
    
  }
  Conf_Pred_Bas_U30[[b]] <- muhat_U30
}
Top_U30 <- quantile(unlist(Conf_Pred_Bas_U30),0.975)
Bund_U30 <- quantile(unlist(Conf_Pred_Bas_U30),0.025)


Plot_U30 <- cbind(U30, Top_U30, Bund_U30)

Plot_U30 %>%
  mutate(covered = ifelse(Bfkg >= Bund_U30 & Bfkg <= Top_U30, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Leafs_split_Conf_U30 <- ggplot(Plot_U30, aes(x = Sc, y = Bfkg))+
  geom_point(color= "blue")+
  geom_smooth(aes(y = Top_U30), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U30), col = "coral2", linetype = "dashed")+
  ggtitle("Between 20 and 30")


##Over 30 - leafs
n <- nrow(O30)
Conformal_Bas_O30 <- vector("list", 2)
Conf_Pred_Bas_O30 <- vector("list", 2)
muhat_O30 <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog, data = O30[group != i,])
    alph_Bas_O30 <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_O30 <- coef(modelcv)[2]
    Yhat_O30 <- alph_Bas_O30*O30$Sc[group == i]^beta_Bas_O30
    muhat_O30 [group == i] <- sqrt((Yhat_O30-O30$Bfkg[group == i])^2)
    
  }
  Conf_Pred_Bas_O30[[b]] <- muhat_O30
}
Top_O30 <- quantile(unlist(Conf_Pred_Bas_O30),0.975)
Bund_O30 <- quantile(unlist(Conf_Pred_Bas_O30),0.025)


Plot_O30 <- cbind(O30, Top_O30, Bund_O30)

Plot_O30 %>%
  mutate(covered = ifelse(Bfkg >= Bund_O30 & Bfkg <= Top_O30, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Leafs_split_Conf_O30 <- ggplot(Plot_O30, aes(x = Sc, y = Bfkg))+
  geom_point(color= "blue")+
  geom_smooth(aes(y = Top_O30), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_O30), col = "coral2", linetype = "dashed")+
  ggtitle("Over 30")

Leafs_Split_Conf <- grid.arrange(Leafs_split_Conf_U1, Leafs_split_Conf_U2, Leafs_split_Conf_U3, Leafs_split_Conf_U5, Leafs_split_Conf_U10, Leafs_split_Conf_U20, Leafs_split_Conf_U30, Leafs_split_Conf_O30, ncol = 2)

ggsave(file = "Leafs Splits Conformal Prediction Intervals.pdf", plot = Leafs_Split_Conf, height = 10, width = 10)


##Combining the plots into one on the whole leafs dataset
Op_U1 <- as.vector(Plot_U1$Top_U1)
Ned_U1 <- as.vector(Plot_U1$Bund_U1)
Op_U2 <- as.vector(Plot_U2$Top_U2)
Ned_U2 <- as.vector(Plot_U2$Bund_U2)
Op_U3 <- as.vector(Plot_U3$Top_U3)
Ned_U3 <- as.vector(Plot_U3$Bund_U3)
Op_U5 <- as.vector(Plot_U5$Top_U5)
Ned_U5 <- as.vector(Plot_U5$Bund_U5)
Op_U10 <- as.vector(Plot_U10$Top_U10)
Ned_U10 <- as.vector(Plot_U10$Bund_U10)
Op_U20 <- as.vector(Plot_U20$Top_U20)
Ned_U20 <- as.vector(Plot_U20$Bund_U20)
Op_U30 <- as.vector(Plot_U30$Top_U30)
Ned_U30 <- as.vector(Plot_U30$Bund_U30)
Op_O30 <- as.vector(Plot_O30$Top_O30)
Ned_O30 <- as.vector(Plot_O30$Bund_O30)

U1_Sc <- as.vector(U1$Sc)
U1_Bfkg <- as.vector(U1$Bfkg)
U2_Sc <- as.vector(U2$Sc)
U2_Bfkg <- as.vector(U2$Bfkg)
U3_Sc <- as.vector(U3$Sc)
U3_Bfkg <- as.vector(U3$Bfkg)
U5_Sc <- as.vector(U5$Sc)
U5_Bfkg <- as.vector(U5$Bfkg)
U10_Sc <- as.vector(U10$Sc)
U10_Bfkg <- as.vector(U10$Bfkg)
U20_Sc <- as.vector(U20$Sc)
U20_Bfkg <- as.vector(U20$Bfkg)
U30_Sc <- as.vector(U30$Sc)
U30_Bfkg <- as.vector(U30$Bfkg)
O30_Sc <- as.vector(O30$Sc)
O30_Bfkg <- as.vector(O30$Bfkg)

ny_Sc <- c(U1_Sc, U2_Sc, U3_Sc, U5_Sc, U10_Sc, U20_Sc, U30_Sc, O30_Sc)
ny_Bfkg <- c(U1_Bfkg, U2_Bfkg, U3_Bfkg, U5_Bfkg, U10_Bfkg, U20_Bfkg, U30_Bfkg, O30_Bfkg)
Conf_Ovre <- c(Op_U1, Op_U2, Op_U3, Op_U5, Op_U10, Op_U20, Op_U30, Op_O30)
Conf_Nedre <- c(Ned_U1, Ned_U2, Ned_U3, Ned_U5, Ned_U10, Ned_U20, Ned_U30, Ned_O30)
Comb <- data.frame(cbind(ny_Sc, ny_Bfkg, Conf_Ovre, Conf_Nedre))


samlet <- ggplot(Comb, aes(x = ny_Sc, y = ny_Bfkg))+
  geom_point(color = "blue")+
  geom_line(aes(y = Conf_Ovre), col = "coral2", linetype = "dashed")+
  geom_line(aes(y = Conf_Nedre), col = "coral2", linetype = "dashed")+
  ggtitle("Conformal prediction of groups")+
  xlab("Crown Size")+ylab("Biomass in kg")+
  theme(plot.title = element_text(size = 15, face = "bold"))

ggsave(file = "Samlet conf pred.pdf", plot = samlet, height = 7, width = 9)




#Conformal prediction on wood splits
##Under 1 wood
form_LogLog_wood <- log(mbt) ~ log(Sc)
B <- 10
k <- 10
n <- nrow(WU1_3)
Conformal_Bas_wood <- vector("list", 2)
Conf_Pred_Bas_wood <- vector("list", 2)
muhat_U1_wood <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog_wood, data = WU1_3[group != i,])
    alph_Bas_U1_wood <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U1_wood <- coef(modelcv)[2]
    Yhat_U1_wood <- alph_Bas_U1_wood*WU1_3$Sc[group == i]^beta_Bas_U1_wood
    muhat_U1_wood [group == i] <- sqrt((Yhat_U1_wood-WU1_3$mbt[group == i])^2)
    
  }
  Conf_Pred_Bas_wood[[b]] <- muhat_U1_wood
}
Top_U1_wood <- quantile(unlist(Conf_Pred_Bas_wood),0.975)
Bund_U1_wood <- quantile(unlist(Conf_Pred_Bas_wood),0.025)


Plot_U1_wood <- cbind(WU1_3, Top_U1_wood, Bund_U1_wood)

Plot_U1_wood %>%
  mutate(covered = ifelse(mbt >= Bund_U1_wood & mbt <= Top_U1_wood, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Wood_split_Conf_U1 <- ggplot(Plot_U1_wood, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Top_U1_wood), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U1_wood), col = "coral2", linetype = "dashed")+
  ggtitle("Under 1")

##Between 1 and 2 wood
n <- nrow(WU2_3)
Conformal_Bas_U2_wood <- vector("list", 2)
Conf_Pred_Bas_U2_wood <- vector("list", 2)
muhat_U2_wood <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog_wood, data = WU2_3[group != i,])
    alph_Bas_U2_wood <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U2_wood <- coef(modelcv)[2]
    Yhat_U2_wood <- alph_Bas_U2_wood*WU2_3$Sc[group == i]^beta_Bas_U2_wood
    muhat_U2_wood [group == i] <- sqrt((Yhat_U2_wood-WU2_3$mbt[group == i])^2)
    
  }
  Conf_Pred_Bas_U2_wood[[b]] <- muhat_U2_wood
}
Top_U2_wood <- quantile(unlist(Conf_Pred_Bas_U2_wood),0.975)
Bund_U2_wood <- quantile(unlist(Conf_Pred_Bas_U2_wood),0.025)


Plot_U2_wood <- cbind(WU2_3, Top_U2_wood, Bund_U2_wood)

Plot_U2_wood %>%
  mutate(covered = ifelse(mbt >= Bund_U2_wood & mbt <= Top_U2_wood, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Wood_split_Conf_U2 <- ggplot(Plot_U2_wood, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Top_U2_wood), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U2_wood), col = "coral2", linetype = "dashed")+
  ggtitle("Between 1 and 2")


##Between 2 and 3 wood
n <- nrow(WU3_3)
Conformal_Bas_U3_wood <- vector("list", 2)
Conf_Pred_Bas_U3_wood <- vector("list", 2)
muhat_U3_wood <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog_wood, data = WU3_3[group != i,])
    alph_Bas_U3_wood <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U3_wood <- coef(modelcv)[2]
    Yhat_U3_wood <- alph_Bas_U3_wood*WU3_3$Sc[group == i]^beta_Bas_U3_wood
    muhat_U3_wood [group == i] <- sqrt((Yhat_U3_wood-WU3_3$mbt[group == i])^2)
    
  }
  Conf_Pred_Bas_U3_wood[[b]] <- muhat_U3_wood
}
Top_U3_wood <- quantile(unlist(Conf_Pred_Bas_U3_wood),0.975)
Bund_U3_wood <- quantile(unlist(Conf_Pred_Bas_U3_wood),0.025)


Plot_U3_wood <- cbind(WU3_3, Top_U3_wood, Bund_U3_wood)

Plot_U3_wood %>%
  mutate(covered = ifelse(mbt >= Bund_U3_wood & mbt <= Top_U3_wood, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Wood_split_Conf_U3 <- ggplot(Plot_U3_wood, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Top_U3_wood), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U3_wood), col = "coral2", linetype = "dashed")+
  ggtitle("Between 2 and 3")


##Between 3 and 5 wood

n <- nrow(WU5_3)
Conformal_Bas_U5_wood <- vector("list", 2)
Conf_Pred_Bas_U5_wood <- vector("list", 2)
muhat_U5_wood <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog_wood, data = WU5_3[group != i,])
    alph_Bas_U5_wood <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U5_wood <- coef(modelcv)[2]
    Yhat_U5_wood <- alph_Bas_U5_wood*WU5_3$Sc[group == i]^beta_Bas_U5_wood
    muhat_U5_wood [group == i] <- sqrt((Yhat_U5_wood-WU5_3$mbt[group == i])^2)
    
  }
  Conf_Pred_Bas_U5_wood[[b]] <- muhat_U5_wood
}
Top_U5_wood <- quantile(unlist(Conf_Pred_Bas_U5_wood),0.975)
Bund_U5_wood <- quantile(unlist(Conf_Pred_Bas_U5_wood),0.025)


Plot_U5_wood <- cbind(WU5_3, Top_U5_wood, Bund_U5_wood)

Plot_U5_wood %>%
  mutate(covered = ifelse(mbt >= Bund_U5_wood & mbt <= Top_U5_wood, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Wood_split_Conf_U5 <- ggplot(Plot_U5_wood, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Top_U5_wood), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U5_wood), col = "coral2", linetype = "dashed")+
  ggtitle("Between 3 and 5")


##Between 5 and 10 wood
n <- nrow(WU10_3)
Conformal_Bas_U10_wood <- vector("list", 2)
Conf_Pred_Bas_U10_wood <- vector("list", 2)
muhat_U10_wood <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog_wood, data = WU10_3[group != i,])
    alph_Bas_U10_wood <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U10_wood <- coef(modelcv)[2]
    Yhat_U10_wood <- alph_Bas_U10_wood*WU10_3$Sc[group == i]^beta_Bas_U10_wood
    muhat_U10_wood [group == i] <- sqrt((Yhat_U10_wood-WU10_3$mbt[group == i])^2)
    
  }
  Conf_Pred_Bas_U10_wood[[b]] <- muhat_U10_wood
}
Top_U10_wood <- quantile(unlist(Conf_Pred_Bas_U10_wood),0.975)
Bund_U10_wood <- quantile(unlist(Conf_Pred_Bas_U10_wood),0.025)


Plot_U10_wood <- cbind(WU10_3, Top_U10_wood, Bund_U10_wood)

Plot_U10_wood %>%
  mutate(covered = ifelse(mbt >= Bund_U10_wood & mbt <= Top_U10_wood, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Wood_split_Conf_U10 <- ggplot(Plot_U10_wood, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Top_U10_wood), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U10_wood), col = "coral2", linetype = "dashed")+
  ggtitle("Between 5 and 10")


##Between 10 and 20 wood
n <- nrow(WU20_3)
Conformal_Bas_U20_wood <- vector("list", 2)
Conf_Pred_Bas_U20_wood <- vector("list", 2)
muhat_U20_wood <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog_wood, data = WU20_3[group != i,])
    alph_Bas_U20_wood <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U20_wood <- coef(modelcv)[2]
    Yhat_U20_wood <- alph_Bas_U20_wood*WU20_3$Sc[group == i]^beta_Bas_U20_wood
    muhat_U20_wood [group == i] <- sqrt((Yhat_U20_wood-WU20_3$mbt[group == i])^2)
    
  }
  Conf_Pred_Bas_U20_wood[[b]] <- muhat_U20_wood
}
Top_U20_wood <- quantile(unlist(Conf_Pred_Bas_U20_wood),0.975)
Bund_U20_wood <- quantile(unlist(Conf_Pred_Bas_U20_wood),0.025)


Plot_U20_wood <- cbind(WU20_3, Top_U20_wood, Bund_U20_wood)
Plot_U20_wood %>%
  mutate(covered = ifelse(mbt >= Bund_U20_wood & mbt <= Top_U20_wood, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Wood_split_Conf_U20 <- ggplot(Plot_U20_wood, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Top_U20_wood), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U20_wood), col = "coral2", linetype = "dashed")+
  ggtitle("Between 10 and 20")


##Between 20 and 30 wood
n <- nrow(WU30_3)
Conformal_Bas_U30_wood <- vector("list", 2)
Conf_Pred_Bas_U30_wood <- vector("list", 2)
muhat_U30_wood <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog_wood, data = WU30_3[group != i,])
    alph_Bas_U30_wood <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_U30_wood <- coef(modelcv)[2]
    Yhat_U30_wood <- alph_Bas_U30_wood*WU30_3$Sc[group == i]^beta_Bas_U30_wood
    muhat_U30_wood [group == i] <- sqrt((Yhat_U30_wood-WU30_3$mbt[group == i])^2)
    
  }
  Conf_Pred_Bas_U30_wood[[b]] <- muhat_U30_wood
}
Top_U30_wood <- quantile(unlist(Conf_Pred_Bas_U30_wood),0.975)
Bund_U30_wood <- quantile(unlist(Conf_Pred_Bas_U30_wood),0.025)


Plot_U30_wood <- cbind(WU30_3, Top_U30_wood, Bund_U30_wood)

Plot_U30_wood %>%
  mutate(covered = ifelse(mbt >= Bund_U30_wood & mbt <= Top_U30_wood, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Wood_split_Conf_U30 <- ggplot(Plot_U30_wood, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Top_U30_wood), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_U30_wood), col = "coral2", linetype = "dashed")+
  ggtitle("Between 20 and 30")


##Over 30 wood
n <- nrow(WO30_3)
Conformal_Bas_O30_wood <- vector("list", 2)
Conf_Pred_Bas_O30_wood <- vector("list", 2)
muhat_O30_wood <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- lm(form_LogLog_wood, data = WO30_3[group != i,])
    alph_Bas_O30_wood <- exp(coef(modelcv)[1]+((sigma(modelcv))^2)/2)
    beta_Bas_O30_wood <- coef(modelcv)[2]
    Yhat_O30_wood <- alph_Bas_O30_wood*WO30_3$Sc[group == i]^beta_Bas_O30_wood
    muhat_O30_wood [group == i] <- sqrt((Yhat_O30_wood-WO30_3$mbt[group == i])^2)
    
  }
  Conf_Pred_Bas_O30_wood[[b]] <- muhat_O30_wood
}
Top_O30_wood <- quantile(unlist(Conf_Pred_Bas_O30_wood),0.975)
Bund_O30_wood <- quantile(unlist(Conf_Pred_Bas_O30_wood),0.025)


Plot_O30_wood <- cbind(WO30_3, Top_O30_wood, Bund_O30_wood)

Plot_O30_wood %>%
  mutate(covered = ifelse(mbt >= Bund_O30_wood & mbt <= Top_O30_wood, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

Wood_split_Conf_O30 <- ggplot(Plot_O30_wood, aes(x = Sc, y = mbt))+
  geom_point(color="blue")+
  geom_smooth(aes(y = Top_O30_wood), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund_O30_wood), col = "coral2", linetype = "dashed")+
  ggtitle("Over 30")

Wood_Split_Conf <- grid.arrange(Wood_split_Conf_U1, Wood_split_Conf_U2, Wood_split_Conf_U3, Wood_split_Conf_U5, Wood_split_Conf_U10, Wood_split_Conf_U20, Wood_split_Conf_U30, Wood_split_Conf_O30, ncol = 2)

ggsave(file = "Wood Splits Conformal Prediction Intervals.pdf", plot = Wood_Split_Conf, height = 10, width = 10)


##Combining the plots into one on the whole wood dataset
Op_U1_wood <- as.vector(Plot_U1_wood$Top_U1_wood)
Ned_U1_wood <- as.vector(Plot_U1_wood$Bund_U1_wood)
Op_U2_wood <- as.vector(Plot_U2_wood$Top_U2_wood)
Ned_U2_wood <- as.vector(Plot_U2_wood$Bund_U2_wood)
Op_U3_wood <- as.vector(Plot_U3_wood$Top_U3_wood)
Ned_U3_wood <- as.vector(Plot_U3_wood$Bund_U3_wood)
Op_U5_wood <- as.vector(Plot_U5_wood$Top_U5_wood)
Ned_U5_wood <- as.vector(Plot_U5_wood$Bund_U5_wood)
Op_U10_wood <- as.vector(Plot_U10_wood$Top_U10_wood)
Ned_U10_wood <- as.vector(Plot_U10_wood$Bund_U10_wood)
Op_U20_wood <- as.vector(Plot_U20_wood$Top_U20_wood)
Ned_U20_wood <- as.vector(Plot_U20_wood$Bund_U20_wood)
Op_U30_wood <- as.vector(Plot_U30_wood$Top_U30_wood)
Ned_U30_wood <- as.vector(Plot_U30_wood$Bund_U30_wood)
Op_O30_wood <- as.vector(Plot_O30_wood$Top_O30_wood)
Ned_O30_wood <- as.vector(Plot_O30_wood$Bund_O30_wood)

U1_Sc_wood <- as.vector(WU1_3$Sc)
U1_mbt_wood <- as.vector(WU1_3$mbt)
U2_Sc_wood <- as.vector(WU2_3$Sc)
U2_mbt_wood <- as.vector(WU2_3$mbt)
U3_Sc_wood <- as.vector(WU3_3$Sc)
U3_mbt_wood <- as.vector(WU3_3$mbt)
U5_Sc_wood <- as.vector(WU5_3$Sc)
U5_mbt_wood <- as.vector(WU5_3$mbt)
U10_Sc_wood <- as.vector(WU10_3$Sc)
U10_mbt_wood <- as.vector(WU10_3$mbt)
U20_Sc_wood <- as.vector(WU20_3$Sc)
U20_mbt_wood <- as.vector(WU20_3$mbt)
U30_Sc_wood <- as.vector(WU30_3$Sc)
U30_mbt_wood <- as.vector(WU30_3$mbt)
O30_Sc_wood <- as.vector(WO30_3$Sc)
O30_mbt_wood <- as.vector(WO30_3$mbt)

ny_Sc_wood <- c(U1_Sc_wood, U2_Sc_wood, U3_Sc_wood, U5_Sc_wood, U10_Sc_wood, U20_Sc_wood, U30_Sc_wood, O30_Sc_wood)
ny_mbt <- c(U1_mbt_wood, U2_mbt_wood, U3_mbt_wood, U5_mbt_wood, U10_mbt_wood, U20_mbt_wood, U30_mbt_wood, O30_mbt_wood)
Conf_Ovre_wood <- c(Op_U1_wood, Op_U2_wood, Op_U3_wood, Op_U5_wood, Op_U10_wood, Op_U20_wood, Op_U30_wood, Op_O30_wood)
Conf_Nedre_wood <- c(Ned_U1_wood, Ned_U2_wood, Ned_U3_wood, Ned_U5_wood, Ned_U10_wood, Ned_U20_wood, Ned_U30_wood, Ned_O30_wood)
Comb_wood <- data.frame(cbind(ny_Sc_wood, ny_mbt, Conf_Ovre_wood, Conf_Nedre_wood))

samlet_wood <- ggplot(Comb_wood, aes(x = ny_Sc_wood, y = ny_mbt))+
  geom_point(color = "blue")+
  geom_line(aes(y = Conf_Ovre_wood), col = "coral2", linetype = "dashed")+
  geom_line(aes(y = Conf_Nedre_wood), col = "coral2", linetype = "dashed")+
  ggtitle("Conformal prediction of groups")+
  xlab("Crown Size")+ylab("Biomass in kg")+
  theme(plot.title = element_text(size = 15, face = "bold"))

ggsave(file = "Samlet conf pred wood.pdf", plot = samlet_wood, height = 7, width = 9)