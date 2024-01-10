#Quantile regression prediction intervals on leafs dataset
k <- 10
n <- nrow(leafs)
UB_OOB <- vector("list", B)
LB_OOB <- vector("list", B)
UB_QR <- vector("list", B)
LB_QR <- vector("list", B)
Upper_OOB <- numeric(n)
Lower_OOB <- numeric(n)
Upper_QR <- numeric(n)
Lower_QR <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x <- data.frame(leafs$Sc[group != i])
  y <- leafs$Bfkg[group != i]
  qrf_OOB <- quantregForest(x, y, keep.inbag = TRUE)
  qrf<- quantregForest(x, y, keep.inbag = FALSE)
  Upper_OOB[group == i] <- predict(qrf_OOB, newdata = matrix(leafs$Sc[group == i]), what = c(0.975))
  Lower_OOB[group == i] <- predict(qrf_OOB, newdata = matrix(leafs$Sc[group == i]), what = c(0.025))
  Upper_QR[group == i]  <- predict(qrf, newdata = matrix(leafs$Sc[group == i]), what = c(0.975))
  Lower_QR[group == i]  <- predict(qrf, newdata = matrix(leafs$Sc[group == i]), what = c(0.025))
}
UB_QR[[b]] <- Upper_QR
LB_QR[[b]] <- Lower_QR
UB_OOB[[b]] <- Upper_OOB
LB_OOB[[b]] <- Lower_OOB

##Finding upper and lower bound of the interval
Upper_Limit_QR <- unlist(UB_QR)
Lower_Limit_QR <- unlist(LB_QR)
Upper_Limit_OOB <- unlist(UB_OOB)
Lower_Limit_OOB <- unlist(LB_OOB)


Plot_RF <- cbind(leafs, Upper_Limit_QR, Lower_Limit_QR, Upper_Limit_OOB, Lower_Limit_OOB)

##Computing global coverage
Plot_RF %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit_QR & Bfkg <= Upper_Limit_QR, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_RF %>%
  mutate(covered_OOB = ifelse(Bfkg >= Lower_Limit_OOB & Bfkg <= Upper_Limit_OOB, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

##The plot
leafs_Quantile_Reg_plot <- ggplot(Plot_RF, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR), col = "green", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB), col = "red", linetype = "dashed")+
  ggtitle("Quantile Regression Prediction")+
  theme(plot.title = element_text(size = 25, face = "bold"))

ggsave(file = "Leafs Quantile Regression plot.pdf", plot = leafs_Quantile_Reg_plot, height = 7, width = 9)


#Quantile regression prediction intervals on wood dataset
k <- 10
n <- nrow(wood)
UB_OOB <- vector("list", B)
LB_OOB <- vector("list", B)
UB_QR <- vector("list", B)
LB_QR <- vector("list", B)
Upper_OOB <- numeric(n)
Lower_OOB <- numeric(n)
Upper_QR <- numeric(n)
Lower_QR <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  x <- data.frame(wood$Sc[group != i])
  y <- wood$mbt[group != i]
  qrf_OOB <- quantregForest(x, y, keep.inbag = TRUE)
  qrf <- quantregForest(x, y, keep.inbag = FALSE)
  Upper_OOB[group == i] <- predict(qrf_OOB, newdata = matrix(wood$Sc[group == i]), what = c(0.975))
  Lower_OOB[group == i] <- predict(qrf_OOB, newdata = matrix(wood$Sc[group == i]), what = c(0.025))
  Upper_QR[group == i]  <- predict(qrf, newdata = matrix(wood$Sc[group == i]), what = c(0.975))
  Lower_QR[group == i]  <- predict(qrf, newdata = matrix(wood$Sc[group == i]), what = c(0.025))
}
UB_QR[[b]] <- Upper_QR
LB_QR[[b]] <- Lower_QR
UB_OOB[[b]] <- Upper_OOB
LB_OOB[[b]] <- Lower_OOB

##Finding upper and lower bound of the interval
Upper_Limit_QR <- unlist(UB_QR)
Lower_Limit_QR <- unlist(LB_QR)
Upper_Limit_OOB <- unlist(UB_OOB)
Lower_Limit_OOB <- unlist(LB_OOB)


Plot_RF_wood <- cbind(wood, Upper_Limit_QR, Lower_Limit_QR, Upper_Limit_OOB, Lower_Limit_OOB)

##Computing global coverage
Plot_RF_wood %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_QR & mbt <= Upper_Limit_QR, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)
Plot_RF_wood %>%
  mutate(covered_OOB = ifelse(mbt >= Lower_Limit_OOB & mbt <= Upper_Limit_OOB, 1, 0)) %>%
  summarise(number_OOB = n(),
            n_covered_OOB = sum(covered_OOB),
            coverage_prop_OOB = 100*n_covered_OOB / number_OOB)

##The plot
wood_Quantile_Reg_plot <- ggplot(Plot_RF_wood, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Upper_Limit_QR), col = "blue", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_QR), col = "blue", linetype = "dashed")+
  geom_smooth(aes(y = Upper_Limit_OOB), col = "red", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_OOB), col = "red", linetype = "dashed")+
  ggtitle("Quantile Regression Prediction")+
  theme(plot.title = element_text(size = 25, face = "bold"))

ggsave(file = "Wood Quantile Regression plot.pdf", plot = wood_Quantile_Reg_plot, height = 7, width = 9)