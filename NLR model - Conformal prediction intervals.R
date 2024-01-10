#Leafs Conformal prediction interval
form <- Bfkg ~ a*I(Sc^b)
start_values <- c(a=-1.3, b=0.9)
B <- 10
k <- 10
n <- nrow(leafs)
Conformal <- vector("list", 2)
Conf_Pred <- vector("list", 2)
muhat <- numeric(n)

for(b in 1:B) {
  ## Generating the random division into groups
  group <- sample(rep(1:k, length.out = n))
  for(i in 1:k) {
    modelcv <- nls(form, start = start_values, algorithm = "port", control = nls.control(maxiter = 1000), data = leafs[group != i,])
    Yhat <- coef(modelcv)[1]*leafs$Sc[group == i]^coef(modelcv)[2]
    muhat [group == i] <- sqrt((Yhat-leafs$Bfkg[group == i])^2)
  }
  Conf_Pred[[b]] <- muhat
}
Top <- quantile(unlist(Conf_Pred),0.975)
Bund <- quantile(unlist(Conf_Pred),0.025)


Plot <- cbind(leafs, Top, Bund)

##Computing global coverage
Plot %>%
  mutate(covered = ifelse(Bfkg >= Bund & Bfkg <= Top, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

##Making the plot
leafs_NLR_conformal_plot <- ggplot(Plot, aes(x = Sc, y = Bfkg))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Top), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund), col = "coral2", linetype = "dashed")+
  ggtitle("NLR Conformal Prediction")+
  theme(plot.title = element_text(size = 25, face = "bold"))+
  xlab("Crownsize")+ylab("Biomass")

ggsave(file = "Leafs NLR Conformal plot.pdf", plot = leafs_NLR_conformal_plot, height = 7, width = 9)


#Wood Conformal prediction interval
form <- mbt ~ a*I(Sc^b)
start_values_W <- c(a=5.678889, b=1.105895)
k <- 10
n <- nrow(wood)
Conformal <- vector("list", 2)
Conf_Pred <- vector("list", 2)
muhat <- numeric(n)

## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  modelcv <- nls(form, start = start_values_W, algorithm = "port", control = nls.control(maxiter = 1000), data = wood[group != i,])
  Yhat <- coef(modelcv)[1]*wood$Sc[group == i]^coef(modelcv)[2]
  muhat [group == i] <- sqrt((Yhat-wood$mbt[group == i])^2)
}

Conf_Pred[[b]] <- muhat

Top <- quantile(unlist(Conf_Pred),0.975)+mean(wood$Sc)
Bund <- quantile(unlist(Conf_Pred),0.025)


Plot_W <- cbind(wood, Top, Bund)

##Computing global coverage
Plot_W %>%
  mutate(covered = ifelse(mbt >= Bund & mbt <= Top, 1, 0)) %>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered / number)

##Making the plot
wood_NLR_conformal_plot <- ggplot(Plot_W, aes(x = Sc, y = mbt))+
  geom_point(color = "blue")+
  geom_smooth(aes(y = Top), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Bund), col = "coral2", linetype = "dashed")+
  ggtitle("NLR Conformal Prediction")+
  theme(plot.title = element_text(size = 25, face = "bold"))

ggsave(file = "Wood NLR Conformal plot.pdf", plot = wood_NLR_conformal_plot, height = 7, width = 9)