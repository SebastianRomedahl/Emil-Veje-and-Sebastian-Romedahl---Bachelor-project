#Bootstrap Leafs
##NLR
start_values <- c(alphahat=-1.3, betahat=0.9)
form <- Bfkg ~ alphahat*Sc^betahat
B <- 1000
k <- 10
n <- nrow(leafs)
ytilde <- vector("list", B)
NLS_Boot_UB <- vector("list", B)
NLS_Boot_LB <- vector("list", B)
NLS_Boot_Upper <- vector("list", B)
NLS_Boot_Lower <- vector("list", B)
Upper_leafs <- numeric(n)
Lower_leafs <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred <- 
    for(b in 1:B){
      lmsub <- nls(form, start = start_values, algorithm = "port", control = nls.control(maxiter = 10000), data = sample(leafs[group != i,], replace = TRUE))
      yhat <- predict(lmsub,newdata=leafs[group == i,])
      eps <- sample(resid(lmsub), size=nrow(leafs[group == i,]), replace = TRUE)
      Upper_leafs[group == i] <- yhat + quantile(eps,probs=c(0.975))
      Lower_leafs[group == i] <- yhat + quantile(eps,probs=c(0.025))
    }
  NLS_Boot_UB[[b]] <- Upper_leafs
  NLS_Boot_LB[[b]] <- Lower_leafs
}
Upper_Limit <- unlist(NLS_Boot_UB)
Lower_Limit <- unlist(NLS_Boot_LB)

#Create the plot
plot <- cbind(Upper_Limit, Lower_Limit, leafs)

#Compute the global coverage
plot %>%
  mutate(covered = ifelse(Bfkg >= Lower_Limit & Bfkg <= Upper_Limit, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)

#Plotting the prediction intervals
ggplot(plot, aes(x = Sc, y = Bfkg))+
  geom_point()+
  ggtitle("NLR Bootstrap")+
  geom_smooth(aes(y = Upper_Limit), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit), col = "coral2", linetype = "dashed")


#Bootstrap Wood
##NLR
start_values1 <- c(alphahat_wood=3.944818, betahat_wood=1.106841)
form1 <- mbt ~ alphahat_wood*Sc^betahat_wood
B <- 100
k <- 10
n <- nrow(wood)
NLS_Boot_UB_wood <- vector("list", B)
NLS_Boot_LB_wood <- vector("list", B)
NLS_Boot_Upper_wood <- vector("list", B)
NLS_Boot_Lower_wood <- vector("list", B)
Upper_wood <- numeric(n)
Lower_wood <- numeric(n)
## Generating the random division into groups
group <- sample(rep(1:k, length.out = n))
for(i in 1:k) {
  boot_pred_wood <- 
    for(b in 1:B){
      lmsub_wood <- nls(form1, start = start_values1, algorithm = "port", control = nls.control(maxiter = 10000), data = sample(wood[group != i,], replace = TRUE))
      yhat_wood <- predict(lmsub_wood,newdata=wood[group == i,])
      eps_wood <- sample(resid(lmsub_wood), size=nrow(wood[group == i,]), replace = TRUE)
      Upper_wood[group == i] <- yhat_wood + quantile(eps_wood,probs=c(0.975))
      Lower_wood[group == i] <- yhat_wood + quantile(eps_wood,probs=c(0.025))
    }
  NLS_Boot_UB_wood[[b]] <- Upper_wood
  NLS_Boot_LB_wood[[b]] <- Lower_wood
}
Upper_Limit_wood <- unlist(NLS_Boot_UB_wood)
Lower_Limit_wood <- unlist(NLS_Boot_LB_wood)


#Creating the plot
plot_wood <- cbind(Upper_Limit_wood, Lower_Limit_wood, wood)


#Compute the global coverage
plot_wood %>%
  mutate(covered = ifelse(mbt >= Lower_Limit_wood & mbt <= Upper_Limit_wood, 1, 0 ))%>%
  summarise(number = n(),
            n_covered = sum(covered),
            coverage_prop = 100*n_covered/number)


#Plotting the prediction intervals
ggplot(plot_wood, aes(x = Sc, y = mbt))+
  geom_point()+
  geom_smooth(aes(y = Upper_Limit_wood), col = "coral2", linetype = "dashed")+
  geom_smooth(aes(y = Lower_Limit_wood), col = "coral2", linetype = "dashed")