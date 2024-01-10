#OLS methods and NLR on leafs:
OLSLeafs <- lm(Bfkg~Sc, data=leafs)
LLOLSleafs <- lm(log(Bfkg)~log(Sc), data=leafs)
Alpha_Leafs <- exp(coef(LLOLSleafs)[["(Intercept)"]])
Beta_Leafs <- coef(LLOLSleafs)[["log(Sc)"]]
equ_Leafs <- function(x){Alpha_Leafs*x^Beta_Leafs}
Alpha_Leafs_Bas <- exp(coef(LLOLSleafs)[["(Intercept)"]]+(Sum_Leafs$sigma)^2/2)
Equ_Leafs_Bas <- function(x){Alpha_Leafs_Bas*x^Beta_Leafs}
sv_L <- c(a=Alpha_Leafs, b=Beta_Leafs)
fit_L <- nls(Bfkg ~ a*Sc^b,
             start = sv_L,
             algorithm = "port",
             control = nls.control(maxiter = 1000), data = leafs)

#Plot of regressions:
LLOLSLeafs_graf <- ggplot(leafs, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line(aes(Sc, predict(OLSLeafs), color="OLS"))+
  stat_function(fun=equ_Leafs, aes(color="Log-Log OLS")) +
  stat_function(fun=Equ_Leafs_Bas,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L), color="NLR")) +
  ggtitle("Leafs")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.position = c(.85,.2), legend.text = element_text(size=10),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

ggsave(file="LogLogOLSLeafsGraf.pdf", plot = LLOLSLeafs_graf)

#OLS on wood:
OLSWood <- lm(mbt~Sc, data=wood)
LLOLS_wood <- lm(log(mbt)~log(Sc), data=wood)
Alpha_wood <- exp(coef(LLOLS_wood)[["(Intercept)"]])
Beta_wood <- coef(LLOLS_wood)[["log(Sc)"]]
equ_wood <- function(x){Alpha_wood*x^Beta_wood}
Alpha_wood_Bas <- exp(coef(LLOLS_wood)[["(Intercept)"]]+(Sum_wood$sigma)^2/2)
Equ_wood_Bas <- function(x){Alpha_wood_Bas*x^Beta_wood}
sv_W <- c(a=Alpha_wood, b=Beta_wood)
fit_W <- nls(mbt ~ a*Sc^b,
             start = sv_W,
             algorithm = "port",
             control = nls.control(maxiter = 1000), data = wood)

#Plot of regressions:
GrafWood <- ggplot(wood, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line(aes(Sc, predict(OLSWood), color="OLS"))+
  stat_function(fun=equ_wood, aes(color="Log-Log OLS")) +
  stat_function(fun=Equ_wood_Bas, aes(color="Baskerville"))+
  geom_line(aes(Sc, predict(fit_W), color="NLR"))+
  ggtitle("Wood")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.position = c(.85,.2), legend.text = element_text(size=10), 
        legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

#Plotting them together:
Bask_Graf <- grid.arrange(LLOLSLeafs_graf, GrafWood, ncol=2)

ggsave(file="OLS og Baskerville Graf.pdf", plot = Bask_Graf, width = 15, height = 7)