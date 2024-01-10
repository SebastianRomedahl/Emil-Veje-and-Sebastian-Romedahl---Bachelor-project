#Split 1:
##OLS:
OLSLeafs_1_U5 <- lm(Bfkg~Sc, data=Under5)
OLSLeafs_1_U10 <- lm(Bfkg~Sc, data=Under10)
OLSLeafs_1_U15 <- lm(Bfkg~Sc, data=Under15)
OLSLeafs_1_U20 <- lm(Bfkg~Sc, data=Under20)
OLSLeafs_1_U25 <- lm(Bfkg~Sc, data=Under25)
OLSLeafs_1_U30 <- lm(Bfkg~Sc, data=Under30)
OLSLeafs_1_U35 <- lm(Bfkg~Sc, data=Under35)
OLSLeafs_1_U40 <- lm(Bfkg~Sc, data=Under40)
OLSLeafs_1_U45 <- lm(Bfkg~Sc, data=Under45)
OLSLeafs_1_U50 <- lm(Bfkg~Sc, data=Under50)
OLSLeafs_1_U55 <- lm(Bfkg~Sc, data=Under55)
OLSLeafs_1_O55 <- lm(Bfkg~Sc, data=Over55)

##Log-Log OLS:
Und5_LLOLS <- lm(log(Bfkg)~log(Sc), data=Under5)
Und10_LLOLS <- lm(log(Bfkg)~log(Sc), data=Under10)
Und15_LLOLS <- lm(log(Bfkg)~log(Sc), data=Under15)
Und20_LLOLS <- lm(log(Bfkg)~log(Sc), data=Under20)
Und25_LLOLS <- lm(log(Bfkg)~log(Sc), data=Under25)
Und30_LLOLS <- lm(log(Bfkg)~log(Sc), data=Under30)
Und35_LLOLS <- lm(log(Bfkg)~log(Sc), data=Under35)
Und40_LLOLS <- lm(log(Bfkg)~log(Sc), data=Under40)
Und45_LLOLS <- lm(log(Bfkg)~log(Sc), data=Under45)
Und50_LLOLS <- lm(log(Bfkg)~log(Sc), data=Under50)
Und55_LLOLS <- lm(log(Bfkg)~log(Sc), data=Under55)
Over55_LLOLS <- lm(log(Bfkg)~log(Sc), data=Over55)

Sum5_1 <- summary(Und5_LLOLS)
Sum10_1 <- summary(Und10_LLOLS)
Sum15_1 <- summary(Und15_LLOLS)
Sum20_1 <- summary(Und20_LLOLS)
Sum25_1 <- summary(Und25_LLOLS)
Sum30_1 <- summary(Und30_LLOLS)
Sum35_1 <- summary(Und35_LLOLS)
Sum40_1 <- summary(Und40_LLOLS)
Sum45_1 <- summary(Und45_LLOLS)
Sum50_1 <- summary(Und50_LLOLS)
Sum55_1 <- summary(Und55_LLOLS)
SumO55_1 <- summary(Over55_LLOLS)

###Coefficients and functions for the plot
LOGA5 <- exp(coef(Und5_LLOLS)[["(Intercept)"]])
BETA5 <- coef(Und5_LLOLS)[["log(Sc)"]]
equ5 <- function(x){LOGA5*x^BETA5}

LOGA10 <- exp(coef(Und10_LLOLS)[["(Intercept)"]])
BETA10 <- coef(Und10_LLOLS)[["log(Sc)"]]
equ10 <- function(x){LOGA10*x^BETA10}

LOGA15 <- exp(coef(Und15_LLOLS)[["(Intercept)"]])
BETA15 <- coef(Und15_LLOLS)[["log(Sc)"]]
equ15 <- function(x){LOGA15*x^BETA15}

LOGA20 <- exp(coef(Und20_LLOLS)[["(Intercept)"]])
BETA20 <- coef(Und20_LLOLS)[["log(Sc)"]]
equ20 <- function(x){LOGA20*x^BETA20}

LOGA25 <- exp(coef(Und25_LLOLS)[["(Intercept)"]])
BETA25 <- coef(Und25_LLOLS)[["log(Sc)"]]
equ25 <- function(x){LOGA25*x^BETA25}

LOGA30 <- exp(coef(Und30_LLOLS)[["(Intercept)"]])
BETA30 <- coef(Und30_LLOLS)[["log(Sc)"]]
equ30 <- function(x){LOGA30*x^BETA30}

LOGA35 <- exp(coef(Und35_LLOLS)[["(Intercept)"]])
BETA35 <- coef(Und35_LLOLS)[["log(Sc)"]]
equ35 <- function(x){LOGA35*x^BETA35}

LOGA40 <- exp(coef(Und40_LLOLS)[["(Intercept)"]])
BETA40 <- coef(Und40_LLOLS)[["log(Sc)"]]
equ40 <- function(x){LOGA40*x^BETA40}

LOGA45 <- exp(coef(Und45_LLOLS)[["(Intercept)"]])
BETA45 <- coef(Und45_LLOLS)[["log(Sc)"]]
equ45 <- function(x){LOGA45*x^BETA45}

LOGA50 <- exp(coef(Und50_LLOLS)[["(Intercept)"]])
BETA50 <- coef(Und50_LLOLS)[["log(Sc)"]]
equ50 <- function(x){LOGA50*x^BETA50}

LOGA55 <- exp(coef(Und55_LLOLS)[["(Intercept)"]])
BETA55 <- coef(Und55_LLOLS)[["log(Sc)"]]
equ55 <- function(x){LOGA55*x^BETA55}

LOGAO55 <- exp(coef(Over55_LLOLS)[["(Intercept)"]])
BETAO55 <- coef(Over55_LLOLS)[["log(Sc)"]]
equOver55 <- function(x){LOGAO55*x^BETAO55}

#Baskerville corrected
LOGBA5 <- exp(coef(Und5_LLOLS)[["(Intercept)"]]+(Sum5_1$sigma)^2/2)
equB5 <- function(x){LOGBA5*x^BETA5}

LOGBA10 <- exp(coef(Und10_LLOLS)[["(Intercept)"]]+(Sum10_1$sigma)^2/2)
equB10 <- function(x){LOGBA10*x^BETA10}

LOGBA15 <- exp(coef(Und15_LLOLS)[["(Intercept)"]]+(Sum15_1$sigma)^2/2)
equB15 <- function(x){LOGBA15*x^BETA15}

LOGBA20 <- exp(coef(Und20_LLOLS)[["(Intercept)"]]+(Sum20_1$sigma)^2/2)
equB20 <- function(x){LOGBA20*x^BETA20}

LOGBA25 <- exp(coef(Und25_LLOLS)[["(Intercept)"]]+(Sum25_1$sigma)^2/2)
equB25 <- function(x){LOGBA25*x^BETA25}

LOGBA30 <- exp(coef(Und30_LLOLS)[["(Intercept)"]]+(Sum30_1$sigma)^2/2)
equB30 <- function(x){LOGBA30*x^BETA30}

LOGBA35 <- exp(coef(Und35_LLOLS)[["(Intercept)"]]+(Sum35_1$sigma)^2/2)
equB35 <- function(x){LOGBA35*x^BETA35}

LOGBA40 <- exp(coef(Und40_LLOLS)[["(Intercept)"]]+(Sum40_1$sigma)^2/2)
equB40 <- function(x){LOGBA40*x^BETA40}

LOGBA45 <- exp(coef(Und45_LLOLS)[["(Intercept)"]]+(Sum45_1$sigma)^2/2)
equB45 <- function(x){LOGBA45*x^BETA45}

LOGBA50 <- exp(coef(Und50_LLOLS)[["(Intercept)"]]+(Sum50_1$sigma)^2/2)
equB50 <- function(x){LOGBA50*x^BETA50}

LOGBA55 <- exp(coef(Und55_LLOLS)[["(Intercept)"]]+(Sum55_1$sigma)^2/2)
equB55 <- function(x){LOGBA55*x^BETA55}

LOGBAO55 <- exp(coef(Over55_LLOLS)[["(Intercept)"]]+(SumO55_1$sigma)^2/2)
equBO55 <- function(x){LOGBAO55*x^BETAO55}

##NLR:
sv_L_1_U5 <- c(a=LOGA5, b=BETA5)
fit_L_1_U5 <- nls(Bfkg ~ a*Sc^b,
                  start = sv_L_1_U5,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = Under5)

sv_L_1_U10 <- c(a=LOGA10, b=BETA10)
fit_L_1_U10 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_1_U10,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Under10)

sv_L_1_U15 <- c(a=LOGA15, b=BETA15)
fit_L_1_U15 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_1_U15,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Under15)

sv_L_1_U20 <- c(a=LOGA20, b=BETA20)
fit_L_1_U20 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_1_U20,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Under20)

sv_L_1_U25 <- c(a=LOGA25, b=BETA25)
fit_L_1_U25 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_1_U25,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Under25)

sv_L_1_U30 <- c(a=LOGA30, b=BETA30)
fit_L_1_U30 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_1_U30,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Under30)

sv_L_1_U35 <- c(a=LOGA35, b=BETA35)
fit_L_1_U35 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_1_U35,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Under35)

sv_L_1_U40 <- c(a=LOGA40, b=BETA40)
fit_L_1_U40 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_1_U40,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Under40)

sv_L_1_U45 <- c(a=LOGA45, b=BETA45)
fit_L_1_U45 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_1_U45,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Under45)

sv_L_1_U50 <- c(a=LOGA50, b=BETA50)
fit_L_1_U50 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_1_U50,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Under50)

sv_L_1_U55 <- c(a=LOGA55, b=BETA55)
fit_L_1_U55 <- nls(Bfkg ~ a*Sc^b,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Under55)

sv_L_1_O55 <- c(a=LOGAO55, b=BETAO55)
fit_L_1_O55 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_1_O55,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Over55)

#Split 2:
##OLS:
OLSLeafs_2_U5 <- lm(Bfkg~Sc, data=Und5)
OLSLeafs_2_U10 <- lm(Bfkg~Sc, data=Und10)
OLSLeafs_2_U15 <- lm(Bfkg~Sc, data=Und15)
OLSLeafs_2_U20 <- lm(Bfkg~Sc, data=Und20)
OLSLeafs_2_U30 <- lm(Bfkg~Sc, data=Und30)
OLSLeafs_2_U40 <- lm(Bfkg~Sc, data=Und40)
OLSLeafs_2_U50 <- lm(Bfkg~Sc, data=Und50)
OLSLeafs_2_O50 <- lm(Bfkg~Sc, data=Ove50)

##Log-Log OLS:
Und5_LLOLS_2 <- lm(log(Bfkg)~log(Sc), data=Und5)
Und10_LLOLS_2 <- lm(log(Bfkg)~log(Sc), data=Und10)
Und15_LLOLS_2 <- lm(log(Bfkg)~log(Sc), data=Und15)
Und20_LLOLS_2 <- lm(log(Bfkg)~log(Sc), data=Und20)
Und30_LLOLS_2 <- lm(log(Bfkg)~log(Sc), data=Und30)
Und40_LLOLS_2 <- lm(log(Bfkg)~log(Sc), data=Und40)
Und50_LLOLS_2 <- lm(log(Bfkg)~log(Sc), data=Und50)
Over50_LLOLS_2 <- lm(log(Bfkg)~log(Sc), data=Ove50)

Sum5_2 <- summary(Und5_LLOLS_2)
Sum10_2 <- summary(Und10_LLOLS_2)
Sum15_2 <- summary(Und15_LLOLS_2)
Sum20_2 <- summary(Und20_LLOLS_2)
Sum30_2 <- summary(Und30_LLOLS_2)
Sum40_2 <- summary(Und40_LLOLS_2)
Sum50_2 <- summary(Und50_LLOLS_2)
SumO50_2 <- summary(Over50_LLOLS_2)

###Coefficients and functions for the plot
ALF5 <- exp(coef(Und5_LLOLS_2)[["(Intercept)"]])
BET5 <- coef(Und5_LLOLS_2)[["log(Sc)"]]
Lin5 <- function(x){ALF5*x^BET5}

ALF10 <- exp(coef(Und10_LLOLS_2)[["(Intercept)"]])
BET10 <- coef(Und10_LLOLS_2)[["log(Sc)"]]
Lin10 <- function(x){ALF10*x^BET10}

ALF15 <- exp(coef(Und15_LLOLS_2)[["(Intercept)"]])
BET15 <- coef(Und15_LLOLS_2)[["log(Sc)"]]
Lin15 <- function(x){ALF15*x^BET15}

ALF20 <- exp(coef(Und20_LLOLS_2)[["(Intercept)"]])
BET20 <- coef(Und20_LLOLS_2)[["log(Sc)"]]
Lin20 <- function(x){ALF20*x^BET20}

ALF30 <- exp(coef(Und30_LLOLS_2)[["(Intercept)"]])
BET30 <- coef(Und30_LLOLS_2)[["log(Sc)"]]
Lin30 <- function(x){ALF30*x^BET30}

ALF40 <- exp(coef(Und40_LLOLS_2)[["(Intercept)"]])
BET40 <- coef(Und40_LLOLS_2)[["log(Sc)"]]
Lin40 <- function(x){ALF40*x^BET40}

ALF50 <- exp(coef(Und50_LLOLS_2)[["(Intercept)"]])
BET50 <- coef(Und50_LLOLS_2)[["log(Sc)"]]
Lin50 <- function(x){ALF50*x^BET50}

ALFO50 <- exp(coef(Over50_LLOLS_2)[["(Intercept)"]])
BETO50 <- coef(Over50_LLOLS_2)[["log(Sc)"]]
LinOver50 <- function(x){ALFO50*x^BETO50}

##Baskerville corrected:
LOGBA5_2 <- exp(coef(Und5_LLOLS_2)[["(Intercept)"]]+(Sum5_2$sigma)^2/2)
equB5_2 <- function(x){LOGBA5_2*x^BET5}

LOGBA10_2 <- exp(coef(Und10_LLOLS_2)[["(Intercept)"]]+(Sum10_2$sigma)^2/2)
equB10_2 <- function(x){LOGBA10_2*x^BET10}

LOGBA15_2 <- exp(coef(Und15_LLOLS_2)[["(Intercept)"]]+(Sum15_2$sigma)^2/2)
equB15_2 <- function(x){LOGBA15_2*x^BET15}

LOGBA20_2 <- exp(coef(Und20_LLOLS_2)[["(Intercept)"]]+(Sum20_2$sigma)^2/2)
equB20_2 <- function(x){LOGBA20_2*x^BET20}

LOGBA30_2 <- exp(coef(Und30_LLOLS_2)[["(Intercept)"]]+(Sum30_2$sigma)^2/2)
equB30_2 <- function(x){LOGBA30_2*x^BET30}

LOGBA40_2 <- exp(coef(Und40_LLOLS_2)[["(Intercept)"]]+(Sum40_2$sigma)^2/2)
equB40_2 <- function(x){LOGBA40_2*x^BET40}

LOGBA50_2 <- exp(coef(Und50_LLOLS_2)[["(Intercept)"]]+(Sum50_2$sigma)^2/2)
equB50_2 <- function(x){LOGBA50_2*x^BET50}

LOGBAO50_2 <- exp(coef(Over50_LLOLS_2)[["(Intercept)"]]+(SumO50_2$sigma)^2/2)
equBO50_2 <- function(x){LOGBAO50_2*x^BETO50}

##NLR:
sv_L_2_U5 <- c(a=ALF5, b=BET5)
fit_L_2_U5 <- nls(Bfkg ~ a*Sc^b,
                  start = sv_L_2_U5,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = Und5)

sv_L_2_U10 <- c(a=ALF10, b=BET10)
fit_L_2_U10 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_2_U10,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Und10)

sv_L_2_U15 <- c(a=ALF15, b=BET15)
fit_L_2_U15 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_2_U15,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Und15)

sv_L_2_U20 <- c(a=ALF20, b=BET20)
fit_L_2_U20 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_2_U20,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Und20)

sv_L_2_U30 <- c(a=ALF30, b=BET30)
fit_L_2_U30 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_2_U30,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Und30)

sv_L_2_U40 <- c(a=ALF40, b=BET40)
fit_L_2_U40 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_2_U40,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Und40)

sv_L_2_U50 <- c(a=ALF50, b=BET50)
fit_L_2_U50 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_2_U50,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Und50)

sv_L_2_O50 <- c(a=ALFO50, b=BETO50)
fit_L_2_O50 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_2_O50,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = Ove50)

#Split 3:
##OLS:
OLSLeafs_3_U1 <- lm(Bfkg~Sc, data=U1)
OLSLeafs_3_U2 <- lm(Bfkg~Sc, data=U2)
OLSLeafs_3_U3 <- lm(Bfkg~Sc, data=U3)
OLSLeafs_3_U5 <- lm(Bfkg~Sc, data=U5)
OLSLeafs_3_U10 <- lm(Bfkg~Sc, data=U10)
OLSLeafs_3_U20 <- lm(Bfkg~Sc, data=U20)
OLSLeafs_3_U30 <- lm(Bfkg~Sc, data=U30)
OLSLeafs_3_O30 <- lm(Bfkg~Sc, data=O30)

##Log-Log OLS:
U1_LLOLS <- lm(log(Bfkg)~log(Sc), data=U1)
U2_LLOLS <- lm(log(Bfkg)~log(Sc), data=U2)
U3_LLOLS <- lm(log(Bfkg)~log(Sc), data=U3)
U5_LLOLS <- lm(log(Bfkg)~log(Sc), data=U5)
U10_LLOLS <- lm(log(Bfkg)~log(Sc), data=U10)
U20_LLOLS <- lm(log(Bfkg)~log(Sc), data=U20)
U30_LLOLS <- lm(log(Bfkg)~log(Sc), data=U30)
O30_LLOLS <- lm(log(Bfkg)~log(Sc), data=O30)

Sum1_3 <- summary(U1_LLOLS)
Sum2_3 <- summary(U2_LLOLS)
Sum3_3 <- summary(U3_LLOLS)
Sum5_3 <- summary(U5_LLOLS)
Sum10_3 <- summary(U10_LLOLS)
Sum20_3 <- summary(U20_LLOLS)
Sum30_3 <- summary(U30_LLOLS)
SumO30_3 <- summary(O30_LLOLS)

###Coefficients and functions for the plot:
A1 <- exp(coef(U1_LLOLS)[["(Intercept)"]])
B1 <- coef(U1_LLOLS)[["log(Sc)"]]
F1 <- function(x){A1*x^B1}

A2 <- exp(coef(U2_LLOLS)[["(Intercept)"]])
B2 <- coef(U2_LLOLS)[["log(Sc)"]]
F2 <- function(x){A2*x^B2}

A3 <- exp(coef(U3_LLOLS)[["(Intercept)"]])
B3 <- coef(U3_LLOLS)[["log(Sc)"]]
F3 <- function(x){A3*x^B3}

A5 <- exp(coef(U5_LLOLS)[["(Intercept)"]])
B5 <- coef(U5_LLOLS)[["log(Sc)"]]
F5 <- function(x){A5*x^B5}

A10 <- exp(coef(U10_LLOLS)[["(Intercept)"]])
B10 <- coef(U10_LLOLS)[["log(Sc)"]]
F10 <- function(x){A10*x^B10}

A20 <- exp(coef(U20_LLOLS)[["(Intercept)"]])
B20 <- coef(U20_LLOLS)[["log(Sc)"]]
F20 <- function(x){A20*x^B20}

A30 <- exp(coef(U30_LLOLS)[["(Intercept)"]])
B30 <- coef(U30_LLOLS)[["log(Sc)"]]
F30 <- function(x){A30*x^B30}

AO30 <- exp(coef(O30_LLOLS)[["(Intercept)"]])
BO30 <- coef(O30_LLOLS)[["log(Sc)"]]
FO30 <- function(x){AO30*x^BO30}

##Baskerville corrected:
AB1_3 <- exp(coef(U1_LLOLS)[["(Intercept)"]]+(Sum1_3$sigma)^2/2)
F1_3 <- function(x){AB1_3*x^B1}

AB2_3 <- exp(coef(U2_LLOLS)[["(Intercept)"]]+(Sum2_3$sigma)^2/2)
F2_3 <- function(x){AB2_3*x^B2}

AB3_3 <- exp(coef(U3_LLOLS)[["(Intercept)"]]+(Sum3_3$sigma)^2/2)
F3_3 <- function(x){AB3_3*x^B3}

AB5_3 <- exp(coef(U5_LLOLS)[["(Intercept)"]]+(Sum5_3$sigma)^2/2)
F5_3 <- function(x){AB5_3*x^B5}

AB10_3 <- exp(coef(U10_LLOLS)[["(Intercept)"]]+(Sum10_3$sigma)^2/2)
F10_3 <- function(x){AB10_3*x^B10}

AB20_3 <- exp(coef(U20_LLOLS)[["(Intercept)"]]+(Sum20_3$sigma)^2/2)
F20_3 <- function(x){AB20_3*x^B20}

AB30_3 <- exp(coef(U30_LLOLS)[["(Intercept)"]]+(Sum30_3$sigma)^2/2)
F30_3 <- function(x){AB30_3*x^B30}

ABO30_3 <- exp(coef(O30_LLOLS)[["(Intercept)"]]+(SumO30_3$sigma)^2/2)
FO30_3 <- function(x){ABO30_3*x^BO30}

##NLR:
sv_L_3_U1 <- c(a=A1, b=B1)
fit_L_3_U1 <- nls(Bfkg ~ a*Sc^b,
                  start = sv_L_3_U1,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = U1)

sv_L_3_U2 <- c(a=A2, b=B2)
fit_L_3_U2 <- nls(Bfkg ~ a*Sc^b,
                  start = sv_L_3_U2,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = U2)

sv_L_3_U3 <- c(a=A3, b=B3)
fit_L_3_U3 <- nls(Bfkg ~ a*Sc^b,
                  start = sv_L_3_U3,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = U3)

sv_L_3_U5 <- c(a=A5, b=B5)
fit_L_3_U5 <- nls(Bfkg ~ a*Sc^b,
                  start = sv_L_3_U5,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = U5)

sv_L_3_U10 <- c(a=A10, b=B10)
fit_L_3_U10 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_3_U10,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = U10)


sv_L_3_U20 <- c(a=A20, b=B20)
fit_L_3_U20 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_3_U20,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = U20)


sv_L_3_U30 <- c(a=A30, b=B30)
fit_L_3_U30 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_3_U30,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = U30)


sv_L_3_O30 <- c(a=AO30, b=BO30)
fit_L_3_O30 <- nls(Bfkg ~ a*Sc^b,
                   start = sv_L_3_O30,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = O30)