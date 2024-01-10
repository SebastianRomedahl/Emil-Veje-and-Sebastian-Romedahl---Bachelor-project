#Quantile regression prediction intervals on split 3 of leafs
##Under 1 - leafs
k <- 10
n <- nrow(U1)
UB_OOB_leafs_U1 <- vector("list", B)
LB_OOB_leafs_U1 <- vector("list", B)
UB_QR_leafs_U1 <- vector("list", B)
LB_QR_leafs_U1 <- vector("list", B)
Upper_OOB_leafs_U1 <- numeric(n)
Lower_OOB_leafs_U1 <- numeric(n)
Upper_QR_leafs_U1 <- numeric(n)
Lower_QR_leafs_U1 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_leafs_U1 <- data.frame(U1$Sc[group != i])
  y_leafs_U1 <- U1$Bfkg[group != i]
  qrf_leafs_U1_OOB <- quantregForest(x_leafs_U1, y_leafs_U1, keep.inbag = TRUE)
  qrf_leafs_U1 <- quantregForest(x_leafs_U1, y_leafs_U1, keep.inbag = FALSE)
  Upper_OOB_leafs_U1[group == i] <- predict(qrf_leafs_U1_OOB, newdata = matrix(U1$Sc[group == i]), what = c(0.975))
  Lower_OOB_leafs_U1[group == i] <- predict(qrf_leafs_U1_OOB, newdata = matrix(U1$Sc[group == i]), what = c(0.025))
  Upper_QR_leafs_U1[group == i]  <- predict(qrf_leafs_U1, newdata = matrix(U1$Sc[group == i]), what = c(0.975))
  Lower_QR_leafs_U1[group == i]  <- predict(qrf_leafs_U1, newdata = matrix(U1$Sc[group == i]), what = c(0.025))
}
UB_QR_leafs_U1[[b]] <- Upper_QR_leafs_U1
LB_QR_leafs_U1[[b]] <- Lower_QR_leafs_U1
UB_OOB_leafs_U1[[b]] <- Upper_OOB_leafs_U1
LB_OOB_leafs_U1[[b]] <- Lower_OOB_leafs_U1

Upper_Limit_QR_leafs_U1 <- unlist(UB_QR_leafs_U1)
Lower_Limit_QR_leafs_U1 <- unlist(LB_QR_leafs_U1)
Upper_Limit_OOB_leafs_U1 <- unlist(UB_OOB_leafs_U1)
Lower_Limit_OOB_leafs_U1 <- unlist(LB_OOB_leafs_U1)

Plot_RF_leafs_U1 <- cbind(U1, Upper_Limit_QR_leafs_U1, Lower_Limit_QR_leafs_U1, Upper_Limit_OOB_leafs_U1, Lower_Limit_OOB_leafs_U1)

Plot_RF_leafs_U1 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_QR_leafs_U1 & Bfkg <= Upper_Limit_QR_leafs_U1, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_RF_leafs_U1 %>%
  mutate(covered_OOB = ifelse(Bfkg >= Lower_Limit_OOB_leafs_U1 & Bfkg <= Upper_Limit_OOB_leafs_U1, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

leafs_RF_split_U1 <- ggplot(Plot_RF_leafs_U1, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_leafs_U1), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_leafs_U1), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_leafs_U1), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_leafs_U1), col = "red", linetype = "dashed")+
  ggtitle("Under 1")


##Between 1 and 2 - leafs
n <- nrow(U2)
UB_OOB_leafs_U2 <- vector("list", B)
LB_OOB_leafs_U2 <- vector("list", B)
UB_QR_leafs_U2 <- vector("list", B)
LB_QR_leafs_U2 <- vector("list", B)
Upper_OOB_leafs_U2 <- numeric(n)
Lower_OOB_leafs_U2 <- numeric(n)
Upper_QR_leafs_U2 <- numeric(n)
Lower_QR_leafs_U2 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_leafs_U2 <- data.frame(U2$Sc[group != i])
  y_leafs_U2 <- U2$Bfkg[group != i]
  qrf_leafs_U2_OOB <- quantregForest(x_leafs_U2, y_leafs_U2, keep.inbag = TRUE)
  qrf_leafs_U2 <- quantregForest(x_leafs_U2, y_leafs_U2, keep.inbag = FALSE)
  Upper_OOB_leafs_U2[group == i] <- predict(qrf_leafs_U2_OOB, newdata = matrix(U2$Sc[group == i]), what = c(0.975))
  Lower_OOB_leafs_U2[group == i] <- predict(qrf_leafs_U2_OOB, newdata = matrix(U2$Sc[group == i]), what = c(0.025))
  Upper_QR_leafs_U2[group == i]  <- predict(qrf_leafs_U2, newdata = matrix(U2$Sc[group == i]), what = c(0.975))
  Lower_QR_leafs_U2[group == i]  <- predict(qrf_leafs_U2, newdata = matrix(U2$Sc[group == i]), what = c(0.025))
}
UB_QR_leafs_U2[[b]] <- Upper_QR_leafs_U2
LB_QR_leafs_U2[[b]] <- Lower_QR_leafs_U2
UB_OOB_leafs_U2[[b]] <- Upper_OOB_leafs_U2
LB_OOB_leafs_U2[[b]] <- Lower_OOB_leafs_U2

Upper_Limit_QR_leafs_U2 <- unlist(UB_QR_leafs_U2)
Lower_Limit_QR_leafs_U2 <- unlist(LB_QR_leafs_U2)
Upper_Limit_OOB_leafs_U2 <- unlist(UB_OOB_leafs_U2)
Lower_Limit_OOB_leafs_U2 <- unlist(LB_OOB_leafs_U2)

Plot_RF_leafs_U2 <- cbind(U2, Upper_Limit_QR_leafs_U2, Lower_Limit_QR_leafs_U2, Upper_Limit_OOB_leafs_U2, Lower_Limit_OOB_leafs_U2)

Plot_RF_leafs_U2 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_QR_leafs_U2 & Bfkg <= Upper_Limit_QR_leafs_U2, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_RF_leafs_U2 %>%
  mutate(covered_OOB = ifelse(Bfkg >= Lower_Limit_OOB_leafs_U2 & Bfkg <= Upper_Limit_OOB_leafs_U2, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

leafs_RF_split_U2 <- ggplot(Plot_RF_leafs_U2, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_leafs_U2), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_leafs_U2), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_leafs_U2), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_leafs_U2), col = "red", linetype = "dashed")+
  ggtitle("Between 1 and 2")


##Between 2 and 3 - leafs
n <- nrow(U3)
UB_OOB_leafs_U3 <- vector("list", B)
LB_OOB_leafs_U3 <- vector("list", B)
UB_QR_leafs_U3 <- vector("list", B)
LB_QR_leafs_U3 <- vector("list", B)
Upper_OOB_leafs_U3 <- numeric(n)
Lower_OOB_leafs_U3 <- numeric(n)
Upper_QR_leafs_U3 <- numeric(n)
Lower_QR_leafs_U3 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_leafs_U3 <- data.frame(U3$Sc[group != i])
  y_leafs_U3 <- U3$Bfkg[group != i]
  qrf_leafs_U3_OOB <- quantregForest(x_leafs_U3, y_leafs_U3, keep.inbag = TRUE)
  qrf_leafs_U3 <- quantregForest(x_leafs_U3, y_leafs_U3, keep.inbag = FALSE)
  Upper_OOB_leafs_U3[group == i] <- predict(qrf_leafs_U3_OOB, newdata = matrix(U3$Sc[group == i]), what = c(0.975))
  Lower_OOB_leafs_U3[group == i] <- predict(qrf_leafs_U3_OOB, newdata = matrix(U3$Sc[group == i]), what = c(0.025))
  Upper_QR_leafs_U3[group == i]  <- predict(qrf_leafs_U3, newdata = matrix(U3$Sc[group == i]), what = c(0.975))
  Lower_QR_leafs_U3[group == i]  <- predict(qrf_leafs_U3, newdata = matrix(U3$Sc[group == i]), what = c(0.025))
}
UB_QR_leafs_U3[[b]] <- Upper_QR_leafs_U3
LB_QR_leafs_U3[[b]] <- Lower_QR_leafs_U3
UB_OOB_leafs_U3[[b]] <- Upper_OOB_leafs_U3
LB_OOB_leafs_U3[[b]] <- Lower_OOB_leafs_U3

Upper_Limit_QR_leafs_U3 <- unlist(UB_QR_leafs_U3)
Lower_Limit_QR_leafs_U3 <- unlist(LB_QR_leafs_U3)
Upper_Limit_OOB_leafs_U3 <- unlist(UB_OOB_leafs_U3)
Lower_Limit_OOB_leafs_U3 <- unlist(LB_OOB_leafs_U3)


Plot_RF_leafs_U3 <- cbind(U3, Upper_Limit_QR_leafs_U3, Lower_Limit_QR_leafs_U3, Upper_Limit_OOB_leafs_U3, Lower_Limit_OOB_leafs_U3)

Plot_RF_leafs_U3 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_QR_leafs_U3 & Bfkg <= Upper_Limit_QR_leafs_U3, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_RF_leafs_U3 %>%
  mutate(covered_OOB = ifelse(Bfkg >= Lower_Limit_OOB_leafs_U3 & Bfkg <= Upper_Limit_OOB_leafs_U3, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

leafs_RF_split_U3 <- ggplot(Plot_RF_leafs_U3, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_leafs_U3), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_leafs_U3), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_leafs_U3), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_leafs_U3), col = "red", linetype = "dashed")+
  ggtitle("Between 2 and 3")


##Between 3 and 5 - leafs
n <- nrow(U5)
UB_OOB_leafs_U5 <- vector("list", B)
LB_OOB_leafs_U5 <- vector("list", B)
UB_QR_leafs_U5 <- vector("list", B)
LB_QR_leafs_U5 <- vector("list", B)
Upper_OOB_leafs_U5 <- numeric(n)
Lower_OOB_leafs_U5 <- numeric(n)
Upper_QR_leafs_U5 <- numeric(n)
Lower_QR_leafs_U5 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_leafs_U5 <- data.frame(U5$Sc[group != i])
  y_leafs_U5 <- U5$Bfkg[group != i]
  qrf_leafs_U5_OOB <- quantregForest(x_leafs_U5, y_leafs_U5, keep.inbag = TRUE)
  qrf_leafs_U5 <- quantregForest(x_leafs_U5, y_leafs_U5, keep.inbag = FALSE)
  Upper_OOB_leafs_U5[group == i] <- predict(qrf_leafs_U5_OOB, newdata = matrix(U5$Sc[group == i]), what = c(0.975))
  Lower_OOB_leafs_U5[group == i] <- predict(qrf_leafs_U5_OOB, newdata = matrix(U5$Sc[group == i]), what = c(0.025))
  Upper_QR_leafs_U5[group == i]  <- predict(qrf_leafs_U5, newdata = matrix(U5$Sc[group == i]), what = c(0.975))
  Lower_QR_leafs_U5[group == i]  <- predict(qrf_leafs_U5, newdata = matrix(U5$Sc[group == i]), what = c(0.025))
}
UB_QR_leafs_U5[[b]] <- Upper_QR_leafs_U5
LB_QR_leafs_U5[[b]] <- Lower_QR_leafs_U5
UB_OOB_leafs_U5[[b]] <- Upper_OOB_leafs_U5
LB_OOB_leafs_U5[[b]] <- Lower_OOB_leafs_U5

Upper_Limit_QR_leafs_U5 <- unlist(UB_QR_leafs_U5)
Lower_Limit_QR_leafs_U5 <- unlist(LB_QR_leafs_U5)
Upper_Limit_OOB_leafs_U5 <- unlist(UB_OOB_leafs_U5)
Lower_Limit_OOB_leafs_U5 <- unlist(LB_OOB_leafs_U5)

Plot_RF_leafs_U5 <- cbind(U5, Upper_Limit_QR_leafs_U5, Lower_Limit_QR_leafs_U5, Upper_Limit_OOB_leafs_U5, Lower_Limit_OOB_leafs_U5)

Plot_RF_leafs_U5 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_QR_leafs_U5 & Bfkg <= Upper_Limit_QR_leafs_U5, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_RF_leafs_U5 %>%
  mutate(covered_OOB = ifelse(Bfkg >= Lower_Limit_OOB_leafs_U5 & Bfkg <= Upper_Limit_OOB_leafs_U5, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

leafs_RF_split_U5 <- ggplot(Plot_RF_leafs_U5, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_leafs_U5), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_leafs_U5), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_leafs_U5), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_leafs_U5), col = "red", linetype = "dashed")+
  ggtitle("Between 3 and 5")



##Between 5 and 10 - leafs
n <- nrow(U10)
UB_OOB_leafs_U10 <- vector("list", B)
LB_OOB_leafs_U10 <- vector("list", B)
UB_QR_leafs_U10 <- vector("list", B)
LB_QR_leafs_U10 <- vector("list", B)
Upper_OOB_leafs_U10 <- numeric(n)
Lower_OOB_leafs_U10 <- numeric(n)
Upper_QR_leafs_U10 <- numeric(n)
Lower_QR_leafs_U10 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_leafs_U10 <- data.frame(U10$Sc[group != i])
  y_leafs_U10 <- U10$Bfkg[group != i]
  qrf_leafs_U10_OOB <- quantregForest(x_leafs_U10, y_leafs_U10, keep.inbag = TRUE)
  qrf_leafs_U10 <- quantregForest(x_leafs_U10, y_leafs_U10, keep.inbag = FALSE)
  Upper_OOB_leafs_U10[group == i] <- predict(qrf_leafs_U10_OOB, newdata = matrix(U10$Sc[group == i]), what = c(0.975))
  Lower_OOB_leafs_U10[group == i] <- predict(qrf_leafs_U10_OOB, newdata = matrix(U10$Sc[group == i]), what = c(0.025))
  Upper_QR_leafs_U10[group == i]  <- predict(qrf_leafs_U10, newdata = matrix(U10$Sc[group == i]), what = c(0.975))
  Lower_QR_leafs_U10[group == i]  <- predict(qrf_leafs_U10, newdata = matrix(U10$Sc[group == i]), what = c(0.025))
}
UB_QR_leafs_U10[[b]] <- Upper_QR_leafs_U10
LB_QR_leafs_U10[[b]] <- Lower_QR_leafs_U10
UB_OOB_leafs_U10[[b]] <- Upper_OOB_leafs_U10
LB_OOB_leafs_U10[[b]] <- Lower_OOB_leafs_U10

Upper_Limit_QR_leafs_U10 <- unlist(UB_QR_leafs_U10)
Lower_Limit_QR_leafs_U10 <- unlist(LB_QR_leafs_U10)
Upper_Limit_OOB_leafs_U10 <- unlist(UB_OOB_leafs_U10)
Lower_Limit_OOB_leafs_U10 <- unlist(LB_OOB_leafs_U10)

Plot_RF_leafs_U10 <- cbind(U10, Upper_Limit_QR_leafs_U10, Lower_Limit_QR_leafs_U10, Upper_Limit_OOB_leafs_U10, Lower_Limit_OOB_leafs_U10)

Plot_RF_leafs_U10 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_QR_leafs_U10 & Bfkg <= Upper_Limit_QR_leafs_U10, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_RF_leafs_U10 %>%
  mutate(covered_OOB = ifelse(Bfkg >= Lower_Limit_OOB_leafs_U10 & Bfkg <= Upper_Limit_OOB_leafs_U10, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

leafs_RF_split_U10 <- ggplot(Plot_RF_leafs_U10, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_leafs_U10), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_leafs_U10), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_leafs_U10), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_leafs_U10), col = "red", linetype = "dashed")+
  ggtitle("Between 5 and 10")



##Between 10 and 20 - leafs
n <- nrow(U20)
UB_OOB_leafs_U20 <- vector("list", B)
LB_OOB_leafs_U20 <- vector("list", B)
UB_QR_leafs_U20 <- vector("list", B)
LB_QR_leafs_U20 <- vector("list", B)
Upper_OOB_leafs_U20 <- numeric(n)
Lower_OOB_leafs_U20 <- numeric(n)
Upper_QR_leafs_U20 <- numeric(n)
Lower_QR_leafs_U20 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_leafs_U20 <- data.frame(U20$Sc[group != i])
  y_leafs_U20 <- U20$Bfkg[group != i]
  qrf_leafs_U20_OOB <- quantregForest(x_leafs_U20, y_leafs_U20, keep.inbag = TRUE)
  qrf_leafs_U20 <- quantregForest(x_leafs_U20, y_leafs_U20, keep.inbag = FALSE)
  Upper_OOB_leafs_U20[group == i] <- predict(qrf_leafs_U20_OOB, newdata = matrix(U20$Sc[group == i]), what = c(0.975))
  Lower_OOB_leafs_U20[group == i] <- predict(qrf_leafs_U20_OOB, newdata = matrix(U20$Sc[group == i]), what = c(0.025))
  Upper_QR_leafs_U20[group == i]  <- predict(qrf_leafs_U20, newdata = matrix(U20$Sc[group == i]), what = c(0.975))
  Lower_QR_leafs_U20[group == i]  <- predict(qrf_leafs_U20, newdata = matrix(U20$Sc[group == i]), what = c(0.025))
}
UB_QR_leafs_U20[[b]] <- Upper_QR_leafs_U20
LB_QR_leafs_U20[[b]] <- Lower_QR_leafs_U20
UB_OOB_leafs_U20[[b]] <- Upper_OOB_leafs_U20
LB_OOB_leafs_U20[[b]] <- Lower_OOB_leafs_U20

Upper_Limit_QR_leafs_U20 <- unlist(UB_QR_leafs_U20)
Lower_Limit_QR_leafs_U20 <- unlist(LB_QR_leafs_U20)
Upper_Limit_OOB_leafs_U20 <- unlist(UB_OOB_leafs_U20)
Lower_Limit_OOB_leafs_U20 <- unlist(LB_OOB_leafs_U20)

Plot_RF_leafs_U20 <- cbind(U20, Upper_Limit_QR_leafs_U20, Lower_Limit_QR_leafs_U20, Upper_Limit_OOB_leafs_U20, Lower_Limit_OOB_leafs_U20)

Plot_RF_leafs_U20 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_QR_leafs_U20 & Bfkg <= Upper_Limit_QR_leafs_U20, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_RF_leafs_U20 %>%
  mutate(covered_OOB = ifelse(Bfkg >= Lower_Limit_OOB_leafs_U20 & Bfkg <= Upper_Limit_OOB_leafs_U20, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

leafs_RF_split_U20 <- ggplot(Plot_RF_leafs_U20, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_leafs_U20), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_leafs_U20), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_leafs_U20), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_leafs_U20), col = "red", linetype = "dashed")+
  ggtitle("Between 10 and 20")



##Between 20 and 30 - leafs
n <- nrow(U30)
UB_OOB_leafs_U30 <- vector("list", B)
LB_OOB_leafs_U30 <- vector("list", B)
UB_QR_leafs_U30 <- vector("list", B)
LB_QR_leafs_U30 <- vector("list", B)
Upper_OOB_leafs_U30 <- numeric(n)
Lower_OOB_leafs_U30 <- numeric(n)
Upper_QR_leafs_U30 <- numeric(n)
Lower_QR_leafs_U30 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_leafs_U30 <- data.frame(U30$Sc[group != i])
  y_leafs_U30 <- U30$Bfkg[group != i]
  qrf_leafs_U30_OOB <- quantregForest(x_leafs_U30, y_leafs_U30, keep.inbag = TRUE)
  qrf_leafs_U30 <- quantregForest(x_leafs_U30, y_leafs_U30, keep.inbag = FALSE)
  Upper_OOB_leafs_U30[group == i] <- predict(qrf_leafs_U30_OOB, newdata = matrix(U30$Sc[group == i]), what = c(0.975))
  Lower_OOB_leafs_U30[group == i] <- predict(qrf_leafs_U30_OOB, newdata = matrix(U30$Sc[group == i]), what = c(0.025))
  Upper_QR_leafs_U30[group == i]  <- predict(qrf_leafs_U30, newdata = matrix(U30$Sc[group == i]), what = c(0.975))
  Lower_QR_leafs_U30[group == i]  <- predict(qrf_leafs_U30, newdata = matrix(U30$Sc[group == i]), what = c(0.025))
}
UB_QR_leafs_U30[[b]] <- Upper_QR_leafs_U30
LB_QR_leafs_U30[[b]] <- Lower_QR_leafs_U30
UB_OOB_leafs_U30[[b]] <- Upper_OOB_leafs_U30
LB_OOB_leafs_U30[[b]] <- Lower_OOB_leafs_U30

Upper_Limit_QR_leafs_U30 <- unlist(UB_QR_leafs_U30)
Lower_Limit_QR_leafs_U30 <- unlist(LB_QR_leafs_U30)
Upper_Limit_OOB_leafs_U30 <- unlist(UB_OOB_leafs_U30)
Lower_Limit_OOB_leafs_U30 <- unlist(LB_OOB_leafs_U30)

Plot_RF_leafs_U30 <- cbind(U30, Upper_Limit_QR_leafs_U30, Lower_Limit_QR_leafs_U30, Upper_Limit_OOB_leafs_U30, Lower_Limit_OOB_leafs_U30)

Plot_RF_leafs_U30 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_QR_leafs_U30 & Bfkg <= Upper_Limit_QR_leafs_U30, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_RF_leafs_U30 %>%
  mutate(covered_OOB = ifelse(Bfkg >= Lower_Limit_OOB_leafs_U30 & Bfkg <= Upper_Limit_OOB_leafs_U30, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

leafs_RF_split_U30 <- ggplot(Plot_RF_leafs_U30, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_leafs_U30), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_leafs_U30), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_leafs_U30), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_leafs_U30), col = "red", linetype = "dashed")+
  ggtitle("Between 20 and 30")



##Over 30 - leafs
n <- nrow(O30)
UB_OOB_leafs_O30 <- vector("list", B)
LB_OOB_leafs_O30 <- vector("list", B)
UB_QR_leafs_O30 <- vector("list", B)
LB_QR_leafs_O30 <- vector("list", B)
Upper_OOB_leafs_O30 <- numeric(n)
Lower_OOB_leafs_O30 <- numeric(n)
Upper_QR_leafs_O30 <- numeric(n)
Lower_QR_leafs_O30 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_leafs_O30 <- data.frame(O30$Sc[group != i])
  y_leafs_O30 <- O30$Bfkg[group != i]
  qrf_leafs_O30_OOB <- quantregForest(x_leafs_O30, y_leafs_O30, keep.inbag = TRUE)
  qrf_leafs_O30 <- quantregForest(x_leafs_O30, y_leafs_O30, keep.inbag = FALSE)
  Upper_OOB_leafs_O30[group == i] <- predict(qrf_leafs_O30_OOB, newdata = matrix(O30$Sc[group == i]), what = c(0.975))
  Lower_OOB_leafs_O30[group == i] <- predict(qrf_leafs_O30_OOB, newdata = matrix(O30$Sc[group == i]), what = c(0.025))
  Upper_QR_leafs_O30[group == i]  <- predict(qrf_leafs_O30, newdata = matrix(O30$Sc[group == i]), what = c(0.975))
  Lower_QR_leafs_O30[group == i]  <- predict(qrf_leafs_O30, newdata = matrix(O30$Sc[group == i]), what = c(0.025))
}
UB_QR_leafs_O30[[b]] <- Upper_QR_leafs_O30
LB_QR_leafs_O30[[b]] <- Lower_QR_leafs_O30
UB_OOB_leafs_O30[[b]] <- Upper_OOB_leafs_O30
LB_OOB_leafs_O30[[b]] <- Lower_OOB_leafs_O30

Upper_Limit_QR_leafs_O30 <- unlist(UB_QR_leafs_O30)
Lower_Limit_QR_leafs_O30 <- unlist(LB_QR_leafs_O30)
Upper_Limit_OOB_leafs_O30 <- unlist(UB_OOB_leafs_O30)
Lower_Limit_OOB_leafs_O30 <- unlist(LB_OOB_leafs_O30)

Plot_RF_leafs_O30 <- cbind(O30, Upper_Limit_QR_leafs_O30, Lower_Limit_QR_leafs_O30, Upper_Limit_OOB_leafs_O30, Lower_Limit_OOB_leafs_O30)

Plot_RF_leafs_O30 %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_QR_leafs_O30 & Bfkg <= Upper_Limit_QR_leafs_O30, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_RF_leafs_O30 %>%
  mutate(covered_OOB = ifelse(Bfkg >= Lower_Limit_OOB_leafs_O30 & Bfkg <= Upper_Limit_OOB_leafs_O30, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

leafs_RF_split_O30 <- ggplot(Plot_RF_leafs_O30, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_leafs_O30), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_leafs_O30), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_leafs_O30), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_leafs_O30), col = "red", linetype = "dashed")+
  ggtitle("Over 30")

###Combining the plots into 1
Random_Forest_split_Leafs <- grid.arrange(leafs_RF_split_U1, leafs_RF_split_U2, leafs_RF_split_U3, leafs_RF_split_U5, leafs_RF_split_U10, leafs_RF_split_U20, leafs_RF_split_U30, leafs_RF_split_O30, ncol =2)

ggsave(file = "Leafs Splits Quantile Regression Prediction Intervals.pdf", plot = Random_Forest_split_Leafs, height = 10, width = 10)




#Quantile regression prediction intervals on split 3 of wood
##Under 1 - wood
k <- 10
n <- nrow(WU1_3)
UB_OOB_wood_U1 <- vector("list", B)
LB_OOB_wood_U1 <- vector("list", B)
UB_QR_wood_U1 <- vector("list", B)
LB_QR_wood_U1 <- vector("list", B)
Upper_OOB_wood_U1 <- numeric(n)
Lower_OOB_wood_U1 <- numeric(n)
Upper_QR_wood_U1 <- numeric(n)
Lower_QR_wood_U1 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_wood_U1 <- data.frame(WU1_3$Sc[group != i])
  y_wood_U1 <- WU1_3$mbt[group != i]
  qrf_wood_U1_OOB <- quantregForest(x_wood_U1, y_wood_U1, keep.inbag = TRUE)
  qrf_wood_U1 <- quantregForest(x_wood_U1, y_wood_U1, keep.inbag = FALSE)
  Upper_OOB_wood_U1[group == i] <- predict(qrf_wood_U1_OOB, newdata = matrix(WU1_3$Sc[group == i]), what = c(0.975))
  Lower_OOB_wood_U1[group == i] <- predict(qrf_wood_U1_OOB, newdata = matrix(WU1_3$Sc[group == i]), what = c(0.025))
  Upper_QR_wood_U1[group == i]  <- predict(qrf_wood_U1, newdata = matrix(WU1_3$Sc[group == i]), what = c(0.975))
  Lower_QR_wood_U1[group == i]  <- predict(qrf_wood_U1, newdata = matrix(WU1_3$Sc[group == i]), what = c(0.025))
}
UB_QR_wood_U1[[b]] <- Upper_QR_wood_U1
LB_QR_wood_U1[[b]] <- Lower_QR_wood_U1
UB_OOB_wood_U1[[b]] <- Upper_OOB_wood_U1
LB_OOB_wood_U1[[b]] <- Lower_OOB_wood_U1

Upper_Limit_QR_wood_U1 <- unlist(UB_QR_wood_U1)
Lower_Limit_QR_wood_U1 <- unlist(LB_QR_wood_U1)
Upper_Limit_OOB_wood_U1 <- unlist(UB_OOB_wood_U1)
Lower_Limit_OOB_wood_U1 <- unlist(LB_OOB_wood_U1)


Plot_wood_U1 <- cbind(WU1_3, Upper_Limit_QR_wood_U1, Lower_Limit_QR_wood_U1, Upper_Limit_OOB_wood_U1, Lower_Limit_OOB_wood_U1)

Plot_wood_U1 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_QR_wood_U1 & mbt <= Upper_Limit_QR_wood_U1, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_wood_U1 %>%
  mutate(covered_OOB = ifelse(mbt >= Lower_Limit_OOB_wood_U1 & mbt <= Upper_Limit_OOB_wood_U1, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

wood_RF_split_U1 <- ggplot(Plot_wood_U1, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_wood_U1), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_wood_U1), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_wood_U1), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_wood_U1), col = "red", linetype = "dashed")+
  ggtitle("Under 1")


##Between 1 and 2 - wood
n <- nrow(WU2_3)
UB_OOB_wood_U2 <- vector("list", B)
LB_OOB_wood_U2 <- vector("list", B)
UB_QR_wood_U2 <- vector("list", B)
LB_QR_wood_U2 <- vector("list", B)
Upper_OOB_wood_U2 <- numeric(n)
Lower_OOB_wood_U2 <- numeric(n)
Upper_QR_wood_U2 <- numeric(n)
Lower_QR_wood_U2 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_wood_U2 <- data.frame(WU2_3$Sc[group != i])
  y_wood_U2 <- WU2_3$mbt[group != i]
  qrf_wood_U2_OOB <- quantregForest(x_wood_U2, y_wood_U2, keep.inbag = TRUE)
  qrf_wood_U2 <- quantregForest(x_wood_U2, y_wood_U2, keep.inbag = FALSE)
  Upper_OOB_wood_U2[group == i] <- predict(qrf_wood_U2_OOB, newdata = matrix(WU2_3$Sc[group == i]), what = c(0.975))
  Lower_OOB_wood_U2[group == i] <- predict(qrf_wood_U2_OOB, newdata = matrix(WU2_3$Sc[group == i]), what = c(0.025))
  Upper_QR_wood_U2[group == i]  <- predict(qrf_wood_U2, newdata = matrix(WU2_3$Sc[group == i]), what = c(0.975))
  Lower_QR_wood_U2[group == i]  <- predict(qrf_wood_U2, newdata = matrix(WU2_3$Sc[group == i]), what = c(0.025))
}
UB_QR_wood_U2[[b]] <- Upper_QR_wood_U2
LB_QR_wood_U2[[b]] <- Lower_QR_wood_U2
UB_OOB_wood_U2[[b]] <- Upper_OOB_wood_U2
LB_OOB_wood_U2[[b]] <- Lower_OOB_wood_U2

Upper_Limit_QR_wood_U2 <- unlist(UB_QR_wood_U2)
Lower_Limit_QR_wood_U2 <- unlist(LB_QR_wood_U2)
Upper_Limit_OOB_wood_U2 <- unlist(UB_OOB_wood_U2)
Lower_Limit_OOB_wood_U2 <- unlist(LB_OOB_wood_U2)


Plot_wood_U2 <- cbind(WU2_3, Upper_Limit_QR_wood_U2, Lower_Limit_QR_wood_U2, Upper_Limit_OOB_wood_U2, Lower_Limit_OOB_wood_U2)

Plot_wood_U2 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_QR_wood_U2 & mbt <= Upper_Limit_QR_wood_U2, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_wood_U2 %>%
  mutate(covered_OOB = ifelse(mbt >= Lower_Limit_OOB_wood_U2 & mbt <= Upper_Limit_OOB_wood_U2, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

wood_RF_split_U2 <- ggplot(Plot_wood_U2, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_wood_U2), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_wood_U2), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_wood_U2), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_wood_U2), col = "red", linetype = "dashed")+
  ggtitle("Between 1 and 2")



##Between 2 and 3 - wood
n <- nrow(WU3_3)
UB_OOB_wood_U3 <- vector("list", B)
LB_OOB_wood_U3 <- vector("list", B)
UB_QR_wood_U3 <- vector("list", B)
LB_QR_wood_U3 <- vector("list", B)
Upper_OOB_wood_U3 <- numeric(n)
Lower_OOB_wood_U3 <- numeric(n)
Upper_QR_wood_U3 <- numeric(n)
Lower_QR_wood_U3 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_wood_U3 <- data.frame(WU3_3$Sc[group != i])
  y_wood_U3 <- WU3_3$mbt[group != i]
  qrf_wood_U3_OOB <- quantregForest(x_wood_U3, y_wood_U3, keep.inbag = TRUE)
  qrf_wood_U3 <- quantregForest(x_wood_U3, y_wood_U3, keep.inbag = FALSE)
  Upper_OOB_wood_U3[group == i] <- predict(qrf_wood_U3_OOB, newdata = matrix(WU3_3$Sc[group == i]), what = c(0.975))
  Lower_OOB_wood_U3[group == i] <- predict(qrf_wood_U3_OOB, newdata = matrix(WU3_3$Sc[group == i]), what = c(0.025))
  Upper_QR_wood_U3[group == i]  <- predict(qrf_wood_U3, newdata = matrix(WU3_3$Sc[group == i]), what = c(0.975))
  Lower_QR_wood_U3[group == i]  <- predict(qrf_wood_U3, newdata = matrix(WU3_3$Sc[group == i]), what = c(0.025))
}
UB_QR_wood_U3[[b]] <- Upper_QR_wood_U3
LB_QR_wood_U3[[b]] <- Lower_QR_wood_U3
UB_OOB_wood_U3[[b]] <- Upper_OOB_wood_U3
LB_OOB_wood_U3[[b]] <- Lower_OOB_wood_U3

Upper_Limit_QR_wood_U3 <- unlist(UB_QR_wood_U3)
Lower_Limit_QR_wood_U3 <- unlist(LB_QR_wood_U3)
Upper_Limit_OOB_wood_U3 <- unlist(UB_OOB_wood_U3)
Lower_Limit_OOB_wood_U3 <- unlist(LB_OOB_wood_U3)


Plot_wood_U3 <- cbind(WU3_3, Upper_Limit_QR_wood_U3, Lower_Limit_QR_wood_U3, Upper_Limit_OOB_wood_U3, Lower_Limit_OOB_wood_U3)

Plot_wood_U3 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_QR_wood_U3 & mbt <= Upper_Limit_QR_wood_U3, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_wood_U3 %>%
  mutate(covered_OOB = ifelse(mbt >= Lower_Limit_OOB_wood_U3 & mbt <= Upper_Limit_OOB_wood_U3, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

wood_RF_split_U3 <- ggplot(Plot_wood_U3, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_wood_U3), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_wood_U3), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_wood_U3), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_wood_U3), col = "red", linetype = "dashed")+
  ggtitle("Between 2 and 3")



##Between 3 and 5 - wood
n <- nrow(WU5_3)
UB_OOB_wood_U5 <- vector("list", B)
LB_OOB_wood_U5 <- vector("list", B)
UB_QR_wood_U5 <- vector("list", B)
LB_QR_wood_U5 <- vector("list", B)
Upper_OOB_wood_U5 <- numeric(n)
Lower_OOB_wood_U5 <- numeric(n)
Upper_QR_wood_U5 <- numeric(n)
Lower_QR_wood_U5 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_wood_U5 <- data.frame(WU5_3$Sc[group != i])
  y_wood_U5 <- WU5_3$mbt[group != i]
  qrf_wood_U5_OOB <- quantregForest(x_wood_U5, y_wood_U5, keep.inbag = TRUE)
  qrf_wood_U5 <- quantregForest(x_wood_U5, y_wood_U5, keep.inbag = FALSE)
  Upper_OOB_wood_U5[group == i] <- predict(qrf_wood_U5_OOB, newdata = matrix(WU5_3$Sc[group == i]), what = c(0.975))
  Lower_OOB_wood_U5[group == i] <- predict(qrf_wood_U5_OOB, newdata = matrix(WU5_3$Sc[group == i]), what = c(0.025))
  Upper_QR_wood_U5[group == i]  <- predict(qrf_wood_U5, newdata = matrix(WU5_3$Sc[group == i]), what = c(0.975))
  Lower_QR_wood_U5[group == i]  <- predict(qrf_wood_U5, newdata = matrix(WU5_3$Sc[group == i]), what = c(0.025))
}
UB_QR_wood_U5[[b]] <- Upper_QR_wood_U5
LB_QR_wood_U5[[b]] <- Lower_QR_wood_U5
UB_OOB_wood_U5[[b]] <- Upper_OOB_wood_U5
LB_OOB_wood_U5[[b]] <- Lower_OOB_wood_U5

Upper_Limit_QR_wood_U5 <- unlist(UB_QR_wood_U5)
Lower_Limit_QR_wood_U5 <- unlist(LB_QR_wood_U5)
Upper_Limit_OOB_wood_U5 <- unlist(UB_OOB_wood_U5)
Lower_Limit_OOB_wood_U5 <- unlist(LB_OOB_wood_U5)


Plot_wood_U5 <- cbind(WU5_3, Upper_Limit_QR_wood_U5, Lower_Limit_QR_wood_U5, Upper_Limit_OOB_wood_U5, Lower_Limit_OOB_wood_U5)

Plot_wood_U5 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_QR_wood_U5 & mbt <= Upper_Limit_QR_wood_U5, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_wood_U5 %>%
  mutate(covered_OOB = ifelse(mbt >= Lower_Limit_OOB_wood_U5 & mbt <= Upper_Limit_OOB_wood_U5, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

wood_RF_split_U5 <- ggplot(Plot_wood_U5, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_wood_U5), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_wood_U5), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_wood_U5), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_wood_U5), col = "red", linetype = "dashed")+
  ggtitle("Between 3 and 5")



##Between 5 and 10 - wood
n <- nrow(WU10_3)
UB_OOB_wood_U10 <- vector("list", B)
LB_OOB_wood_U10 <- vector("list", B)
UB_QR_wood_U10 <- vector("list", B)
LB_QR_wood_U10 <- vector("list", B)
Upper_OOB_wood_U10 <- numeric(n)
Lower_OOB_wood_U10 <- numeric(n)
Upper_QR_wood_U10 <- numeric(n)
Lower_QR_wood_U10 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_wood_U10 <- data.frame(WU10_3$Sc[group != i])
  y_wood_U10 <- WU10_3$mbt[group != i]
  qrf_wood_U10_OOB <- quantregForest(x_wood_U10, y_wood_U10, keep.inbag = TRUE)
  qrf_wood_U10 <- quantregForest(x_wood_U10, y_wood_U10, keep.inbag = FALSE)
  Upper_OOB_wood_U10[group == i] <- predict(qrf_wood_U10_OOB, newdata = matrix(WU10_3$Sc[group == i]), what = c(0.975))
  Lower_OOB_wood_U10[group == i] <- predict(qrf_wood_U10_OOB, newdata = matrix(WU10_3$Sc[group == i]), what = c(0.025))
  Upper_QR_wood_U10[group == i]  <- predict(qrf_wood_U10, newdata = matrix(WU10_3$Sc[group == i]), what = c(0.975))
  Lower_QR_wood_U10[group == i]  <- predict(qrf_wood_U10, newdata = matrix(WU10_3$Sc[group == i]), what = c(0.025))
}
UB_QR_wood_U10[[b]] <- Upper_QR_wood_U10
LB_QR_wood_U10[[b]] <- Lower_QR_wood_U10
UB_OOB_wood_U10[[b]] <- Upper_OOB_wood_U10
LB_OOB_wood_U10[[b]] <- Lower_OOB_wood_U10

Upper_Limit_QR_wood_U10 <- unlist(UB_QR_wood_U10)
Lower_Limit_QR_wood_U10 <- unlist(LB_QR_wood_U10)
Upper_Limit_OOB_wood_U10 <- unlist(UB_OOB_wood_U10)
Lower_Limit_OOB_wood_U10 <- unlist(LB_OOB_wood_U10)


Plot_wood_U10 <- cbind(WU10_3, Upper_Limit_QR_wood_U10, Lower_Limit_QR_wood_U10, Upper_Limit_OOB_wood_U10, Lower_Limit_OOB_wood_U10)

Plot_wood_U10 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_QR_wood_U10 & mbt <= Upper_Limit_QR_wood_U10, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_wood_U10 %>%
  mutate(covered_OOB = ifelse(mbt >= Lower_Limit_OOB_wood_U10 & mbt <= Upper_Limit_OOB_wood_U10, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

wood_RF_split_U10 <- ggplot(Plot_wood_U10, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_wood_U10), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_wood_U10), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_wood_U10), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_wood_U10), col = "red", linetype = "dashed")+
  ggtitle("Between 5 and 10")



##Between 10 and 20 - wood
n <- nrow(WU20_3)
UB_OOB_wood_U20 <- vector("list", B)
LB_OOB_wood_U20 <- vector("list", B)
UB_QR_wood_U20 <- vector("list", B)
LB_QR_wood_U20 <- vector("list", B)
Upper_OOB_wood_U20 <- numeric(n)
Lower_OOB_wood_U20 <- numeric(n)
Upper_QR_wood_U20 <- numeric(n)
Lower_QR_wood_U20 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_wood_U20 <- data.frame(WU20_3$Sc[group != i])
  y_wood_U20 <- WU20_3$mbt[group != i]
  qrf_wood_U20_OOB <- quantregForest(x_wood_U20, y_wood_U20, keep.inbag = TRUE)
  qrf_wood_U20 <- quantregForest(x_wood_U20, y_wood_U20, keep.inbag = FALSE)
  Upper_OOB_wood_U20[group == i] <- predict(qrf_wood_U20_OOB, newdata = matrix(WU20_3$Sc[group == i]), what = c(0.975))
  Lower_OOB_wood_U20[group == i] <- predict(qrf_wood_U20_OOB, newdata = matrix(WU20_3$Sc[group == i]), what = c(0.025))
  Upper_QR_wood_U20[group == i]  <- predict(qrf_wood_U20, newdata = matrix(WU20_3$Sc[group == i]), what = c(0.975))
  Lower_QR_wood_U20[group == i]  <- predict(qrf_wood_U20, newdata = matrix(WU20_3$Sc[group == i]), what = c(0.025))
}
UB_QR_wood_U20[[b]] <- Upper_QR_wood_U20
LB_QR_wood_U20[[b]] <- Lower_QR_wood_U20
UB_OOB_wood_U20[[b]] <- Upper_OOB_wood_U20
LB_OOB_wood_U20[[b]] <- Lower_OOB_wood_U20

Upper_Limit_QR_wood_U20 <- unlist(UB_QR_wood_U20)
Lower_Limit_QR_wood_U20 <- unlist(LB_QR_wood_U20)
Upper_Limit_OOB_wood_U20 <- unlist(UB_OOB_wood_U20)
Lower_Limit_OOB_wood_U20 <- unlist(LB_OOB_wood_U20)


Plot_wood_U20 <- cbind(WU20_3, Upper_Limit_QR_wood_U20, Lower_Limit_QR_wood_U20, Upper_Limit_OOB_wood_U20, Lower_Limit_OOB_wood_U20)

Plot_wood_U20 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_QR_wood_U20 & mbt <= Upper_Limit_QR_wood_U20, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_wood_U20 %>%
  mutate(covered_OOB = ifelse(mbt >= Lower_Limit_OOB_wood_U20 & mbt <= Upper_Limit_OOB_wood_U20, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

wood_RF_split_U20 <- ggplot(Plot_wood_U20, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_wood_U20), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_wood_U20), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_wood_U20), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_wood_U20), col = "red", linetype = "dashed")+
  ggtitle("Between 10 and 20")



##Between 20 and 30 - wood
n <- nrow(WU30_3)
UB_OOB_wood_U30 <- vector("list", B)
LB_OOB_wood_U30 <- vector("list", B)
UB_QR_wood_U30 <- vector("list", B)
LB_QR_wood_U30 <- vector("list", B)
Upper_OOB_wood_U30 <- numeric(n)
Lower_OOB_wood_U30 <- numeric(n)
Upper_QR_wood_U30 <- numeric(n)
Lower_QR_wood_U30 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_wood_U30 <- data.frame(WU30_3$Sc[group != i])
  y_wood_U30 <- WU30_3$mbt[group != i]
  qrf_wood_U30_OOB <- quantregForest(x_wood_U30, y_wood_U30, keep.inbag = TRUE)
  qrf_wood_U30 <- quantregForest(x_wood_U30, y_wood_U30, keep.inbag = FALSE)
  Upper_OOB_wood_U30[group == i] <- predict(qrf_wood_U30_OOB, newdata = matrix(WU30_3$Sc[group == i]), what = c(0.975))
  Lower_OOB_wood_U30[group == i] <- predict(qrf_wood_U30_OOB, newdata = matrix(WU30_3$Sc[group == i]), what = c(0.025))
  Upper_QR_wood_U30[group == i]  <- predict(qrf_wood_U30, newdata = matrix(WU30_3$Sc[group == i]), what = c(0.975))
  Lower_QR_wood_U30[group == i]  <- predict(qrf_wood_U30, newdata = matrix(WU30_3$Sc[group == i]), what = c(0.025))
}
UB_QR_wood_U30[[b]] <- Upper_QR_wood_U30
LB_QR_wood_U30[[b]] <- Lower_QR_wood_U30
UB_OOB_wood_U30[[b]] <- Upper_OOB_wood_U30
LB_OOB_wood_U30[[b]] <- Lower_OOB_wood_U30

Upper_Limit_QR_wood_U30 <- unlist(UB_QR_wood_U30)
Lower_Limit_QR_wood_U30 <- unlist(LB_QR_wood_U30)
Upper_Limit_OOB_wood_U30 <- unlist(UB_OOB_wood_U30)
Lower_Limit_OOB_wood_U30 <- unlist(LB_OOB_wood_U30)


Plot_wood_U30 <- cbind(WU30_3, Upper_Limit_QR_wood_U30, Lower_Limit_QR_wood_U30, Upper_Limit_OOB_wood_U30, Lower_Limit_OOB_wood_U30)

Plot_wood_U30 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_QR_wood_U30 & mbt <= Upper_Limit_QR_wood_U30, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_wood_U30 %>%
  mutate(covered_OOB = ifelse(mbt >= Lower_Limit_OOB_wood_U30 & mbt <= Upper_Limit_OOB_wood_U30, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

wood_RF_split_U30 <- ggplot(Plot_wood_U30, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_wood_U30), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_wood_U30), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_wood_U30), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_wood_U30), col = "red", linetype = "dashed")+
  ggtitle("Between 20 and 30")



##Over 30 - wood
n <- nrow(WO30_3)
UB_OOB_wood_O30 <- vector("list", B)
LB_OOB_wood_O30 <- vector("list", B)
UB_QR_wood_O30 <- vector("list", B)
LB_QR_wood_O30 <- vector("list", B)
Upper_OOB_wood_O30 <- numeric(n)
Lower_OOB_wood_O30 <- numeric(n)
Upper_QR_wood_O30 <- numeric(n)
Lower_QR_wood_O30 <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x_wood_O30 <- data.frame(WO30_3$Sc[group != i])
  y_wood_O30 <- WO30_3$mbt[group != i]
  qrf_wood_O30_OOB <- quantregForest(x_wood_O30, y_wood_O30, keep.inbag = TRUE)
  qrf_wood_O30 <- quantregForest(x_wood_O30, y_wood_O30, keep.inbag = FALSE)
  Upper_OOB_wood_O30[group == i] <- predict(qrf_wood_O30_OOB, newdata = matrix(WO30_3$Sc[group == i]), what = c(0.975))
  Lower_OOB_wood_O30[group == i] <- predict(qrf_wood_O30_OOB, newdata = matrix(WO30_3$Sc[group == i]), what = c(0.025))
  Upper_QR_wood_O30[group == i]  <- predict(qrf_wood_O30, newdata = matrix(WO30_3$Sc[group == i]), what = c(0.975))
  Lower_QR_wood_O30[group == i]  <- predict(qrf_wood_O30, newdata = matrix(WO30_3$Sc[group == i]), what = c(0.025))
}
UB_QR_wood_O30[[b]] <- Upper_QR_wood_O30
LB_QR_wood_O30[[b]] <- Lower_QR_wood_O30
UB_OOB_wood_O30[[b]] <- Upper_OOB_wood_O30
LB_OOB_wood_O30[[b]] <- Lower_OOB_wood_O30

Upper_Limit_QR_wood_O30 <- unlist(UB_QR_wood_O30)
Lower_Limit_QR_wood_O30 <- unlist(LB_QR_wood_O30)
Upper_Limit_OOB_wood_O30 <- unlist(UB_OOB_wood_O30)
Lower_Limit_OOB_wood_O30 <- unlist(LB_OOB_wood_O30)


Plot_wood_O30 <- cbind(WO30_3, Upper_Limit_QR_wood_O30, Lower_Limit_QR_wood_O30, Upper_Limit_OOB_wood_O30, Lower_Limit_OOB_wood_O30)

Plot_wood_O30 %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_QR_wood_O30 & mbt <= Upper_Limit_QR_wood_O30, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_wood_O30 %>%
  mutate(covered_OOB = ifelse(mbt >= Lower_Limit_OOB_wood_O30 & mbt <= Upper_Limit_OOB_wood_O30, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

wood_RF_split_O30 <- ggplot(Plot_wood_O30, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR_wood_O30), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR_wood_O30), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB_wood_O30), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB_wood_O30), col = "red", linetype = "dashed")+
  ggtitle("Over 30")


##Combining the plots into 1
Random_Forest_split_Wood <- grid.arrange(wood_RF_split_U1, wood_RF_split_U2, wood_RF_split_U3, wood_RF_split_U5, wood_RF_split_U10, wood_RF_split_U20, wood_RF_split_U30, wood_RF_split_O30, ncol =2)

ggsave(file = "Wood Splits Quantile Regression Prediction Intervals.pdf", plot = Random_Forest_split_Wood, height = 10, width = 10)