#Split 1:
##OLS:
OLSWood_1_U5 <- lm(mbt~Sc, data=WU5_1)
OLSWood_1_U10 <- lm(mbt~Sc, data=WU10_1)
OLSWood_1_U15 <- lm(mbt~Sc, data=WU15_1)
OLSWood_1_U20 <- lm(mbt~Sc, data=WU20_1)
OLSWood_1_U25 <- lm(mbt~Sc, data=WU25_1)
OLSWood_1_U30 <- lm(mbt~Sc, data=WU30_1)
OLSWood_1_U35 <- lm(mbt~Sc, data=WU35_1)
OLSWood_1_U40 <- lm(mbt~Sc, data=WU40_1)
OLSWood_1_U45 <- lm(mbt~Sc, data=WU45_1)
OLSWood_1_U50 <- lm(mbt~Sc, data=WU50_1)
OLSWood_1_U55 <- lm(mbt~Sc, data=WU55_1)
OLSWood_1_O55 <- lm(mbt~Sc, data=WO55_1)

##Log-Log OLS:
WU5_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WU5_1)
WU10_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WU10_1)
WU15_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WU15_1)
WU20_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WU20_1)
WU25_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WU25_1)
WU30_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WU30_1)
WU35_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WU35_1)
WU40_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WU40_1)
WU45_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WU45_1)
WU50_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WU50_1)
WU55_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WU55_1)
WO55_LLOLS_1 <- lm(log(mbt)~log(Sc), data=WO55_1)

Sum_W_5_1 <- summary(WU5_LLOLS_1)
Sum_W_10_1 <- summary(WU10_LLOLS_1)
Sum_W_15_1 <- summary(WU15_LLOLS_1)
Sum_W_20_1 <- summary(WU20_LLOLS_1)
Sum_W_25_1 <- summary(WU25_LLOLS_1)
Sum_W_30_1 <- summary(WU30_LLOLS_1)
Sum_W_35_1 <- summary(WU35_LLOLS_1)
Sum_W_40_1 <- summary(WU40_LLOLS_1)
Sum_W_45_1 <- summary(WU45_LLOLS_1)
Sum_W_50_1 <- summary(WU50_LLOLS_1)
Sum_W_55_1 <- summary(WU55_LLOLS_1)
Sum_W_O55_1 <- summary(WO55_LLOLS_1)

###Coefficients and functions for plot:
WA5_1 <- exp(coef(WU5_LLOLS_1)[["(Intercept)"]])
WB5_1 <- coef(WU5_LLOLS_1)[["log(Sc)"]]
WF5_1 <- function(x){WA5_1*x^WB5_1}

WA10_1 <- exp(coef(WU10_LLOLS_1)[["(Intercept)"]])
WB10_1 <- coef(WU10_LLOLS_1)[["log(Sc)"]]
WF10_1 <- function(x){WA10_1*x^WB10_1}

WA15_1 <- exp(coef(WU15_LLOLS_1)[["(Intercept)"]])
WB15_1 <- coef(WU15_LLOLS_1)[["log(Sc)"]]
WF15_1 <- function(x){WA15_1*x^WB15_1}

WA20_1 <- exp(coef(WU20_LLOLS_1)[["(Intercept)"]])
WB20_1 <- coef(WU20_LLOLS_1)[["log(Sc)"]]
WF20_1 <- function(x){WA20_1*x^WB20_1}

WA25_1 <- exp(coef(WU25_LLOLS_1)[["(Intercept)"]])
WB25_1 <- coef(WU25_LLOLS_1)[["log(Sc)"]]
WF25_1 <- function(x){WA25_1*x^WB25_1}

WA30_1 <- exp(coef(WU30_LLOLS_1)[["(Intercept)"]])
WB30_1 <- coef(WU30_LLOLS_1)[["log(Sc)"]]
WF30_1 <- function(x){WA30_1*x^WB30_1}

WA35_1 <- exp(coef(WU35_LLOLS_1)[["(Intercept)"]])
WB35_1 <- coef(WU35_LLOLS_1)[["log(Sc)"]]
WF35_1 <- function(x){WA35_1*x^WB35_1}

WA40_1 <- exp(coef(WU40_LLOLS_1)[["(Intercept)"]])
WB40_1 <- coef(WU40_LLOLS_1)[["log(Sc)"]]
WF40_1 <- function(x){WA40_1*x^WB40_1}

WA45_1 <- exp(coef(WU45_LLOLS_1)[["(Intercept)"]])
WB45_1 <- coef(WU45_LLOLS_1)[["log(Sc)"]]
WF45_1 <- function(x){WA45_1*x^WB45_1}

WA50_1 <- exp(coef(WU50_LLOLS_1)[["(Intercept)"]])
WB50_1 <- coef(WU50_LLOLS_1)[["log(Sc)"]]
WF50_1 <- function(x){WA50_1*x^WB50_1}

WA55_1 <- exp(coef(WU55_LLOLS_1)[["(Intercept)"]])
WB55_1 <- coef(WU55_LLOLS_1)[["log(Sc)"]]
WF55_1 <- function(x){WA55_1*x^WB55_1}

WAO55_1 <- exp(coef(WO55_LLOLS_1)[["(Intercept)"]])
WBO55_1 <- coef(WO55_LLOLS_1)[["log(Sc)"]]
WFO55_1 <- function(x){WAO55_1*x^WBO55_1}

##Baskerville corrected:
WAB5_1 <- exp(coef(WU5_LLOLS_1)[["(Intercept)"]]+(Sum_W_5_1$sigma)^2/2)
WBF5_1 <- function(x){WAB5_1*x^WB5_1}

WAB10_1 <- exp(coef(WU10_LLOLS_1)[["(Intercept)"]]+(Sum_W_10_1$sigma)^2/2)
WBF10_1 <- function(x){WAB10_1*x^WB10_1}

WAB15_1 <- exp(coef(WU15_LLOLS_1)[["(Intercept)"]]+(Sum_W_15_1$sigma)^2/2)
WBF15_1 <- function(x){WAB15_1*x^WB15_1}

WAB20_1 <- exp(coef(WU20_LLOLS_1)[["(Intercept)"]]+(Sum_W_20_1$sigma)^2/2)
WBF20_1 <- function(x){WAB20_1*x^WB20_1}

WAB25_1 <- exp(coef(WU25_LLOLS_1)[["(Intercept)"]]+(Sum_W_25_1$sigma)^2/2)
WBF25_1 <- function(x){WAB25_1*x^WB25_1}

WAB30_1 <- exp(coef(WU30_LLOLS_1)[["(Intercept)"]]+(Sum_W_30_1$sigma)^2/2)
WBF30_1 <- function(x){WAB30_1*x^WB30_1}

WAB35_1 <- exp(coef(WU35_LLOLS_1)[["(Intercept)"]]+(Sum_W_35_1$sigma)^2/2)
WBF35_1 <- function(x){WAB35_1*x^WB35_1}

WAB40_1 <- exp(coef(WU40_LLOLS_1)[["(Intercept)"]]+(Sum_W_40_1$sigma)^2/2)
WBF40_1 <- function(x){WAB40_1*x^WB40_1}

WAB45_1 <- exp(coef(WU45_LLOLS_1)[["(Intercept)"]]+(Sum_W_45_1$sigma)^2/2)
WBF45_1 <- function(x){WAB45_1*x^WB45_1}

WAB50_1 <- exp(coef(WU50_LLOLS_1)[["(Intercept)"]]+(Sum_W_50_1$sigma)^2/2)
WBF50_1 <- function(x){WAB50_1*x^WB50_1}

WAB55_1 <- exp(coef(WU55_LLOLS_1)[["(Intercept)"]]+(Sum_W_55_1$sigma)^2/2)
WBF55_1 <- function(x){WAB55_1*x^WB55_1}

WABO55_1 <- exp(coef(WO55_LLOLS_1)[["(Intercept)"]]+(Sum_W_O55_1$sigma)^2/2)
WBFO55_1 <- function(x){WABO55_1*x^WBO55_1}

##NLR:
sv_W_1_U5 <- c(a=WA5_1, b=WB5_1)
fit_W_1_U5 <- nls(mbt ~ a*Sc^b,
                  start = sv_W_1_U5,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = WU5_1)

sv_W_1_U10 <- c(a=WA10_1, b=WB10_1)
fit_W_1_U10 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_1_U10,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU10_1)

sv_W_1_U15 <- c(a=WA15_1, b=WB15_1)
fit_W_1_U15 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_1_U15,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU15_1)

sv_W_1_U20 <- c(a=WA20_1, b=WB20_1)
fit_W_1_U20 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_1_U20,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU20_1)

sv_W_1_U25 <- c(a=WA25_1, b=WB25_1)
fit_W_1_U25 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_1_U25,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU25_1)

sv_W_1_U30 <- c(a=WA30_1, b=WB30_1)
fit_W_1_U30 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_1_U30,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU30_1)

sv_W_1_U35 <- c(a=WA35_1, b=WB35_1)
fit_W_1_U35 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_1_U35,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU35_1)

sv_W_1_U45 <- c(a=WA45_1, b=WB45_1)
fit_W_1_U45 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_1_U45,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU45_1)

sv_W_1_U50 <- c(a=WA50_1, b=WB50_1)
fit_W_1_U50 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_1_U50,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU50_1)

sv_W_1_O55 <- c(a=WAO55_1, b=WBO55_1)
fit_W_1_O55 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_1_O55,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WO55_1)



#Split 2:
##OLS:
OLSWood_2_U5 <- lm(mbt~Sc, data=WU5_2)
OLSWood_2_U10 <- lm(mbt~Sc, data=WU10_2)
OLSWood_2_U15 <- lm(mbt~Sc, data=WU15_2)
OLSWood_2_U20 <- lm(mbt~Sc, data=WU20_2)
OLSWood_2_U30 <- lm(mbt~Sc, data=WU30_2)
OLSWood_2_U40 <- lm(mbt~Sc, data=WU40_2)
OLSWood_2_U50 <- lm(mbt~Sc, data=WU50_2)
OLSWood_2_O50 <- lm(mbt~Sc, data=WO50_2)

##Log-Log OLS:
WU5_LLOLS_2 <- lm(log(mbt)~log(Sc), data=WU5_2)
WU10_LLOLS_2 <- lm(log(mbt)~log(Sc), data=WU10_2)
WU15_LLOLS_2 <- lm(log(mbt)~log(Sc), data=WU15_2)
WU20_LLOLS_2 <- lm(log(mbt)~log(Sc), data=WU20_2)
WU30_LLOLS_2 <- lm(log(mbt)~log(Sc), data=WU30_2)
WU40_LLOLS_2 <- lm(log(mbt)~log(Sc), data=WU40_2)
WU50_LLOLS_2 <- lm(log(mbt)~log(Sc), data=WU50_2)
WO50_LLOLS_2 <- lm(log(mbt)~log(Sc), data=WO50_2)

Sum_W_5_2 <- summary(WU5_LLOLS_2)
Sum_W_10_2 <- summary(WU10_LLOLS_2)
Sum_W_15_2 <- summary(WU15_LLOLS_2)
Sum_W_20_2 <- summary(WU20_LLOLS_2)
Sum_W_30_2 <- summary(WU30_LLOLS_2)
Sum_W_40_2 <- summary(WU40_LLOLS_2)
Sum_W_50_2 <- summary(WU50_LLOLS_2)
Sum_W_O50_2 <- summary(WO50_LLOLS_2)

###Coefficients and functions for plot:
WA5_2 <- exp(coef(WU5_LLOLS_2)[["(Intercept)"]])
WB5_2 <- coef(WU5_LLOLS_2)[["log(Sc)"]]
WF5_2 <- function(x){WA5_2*x^WB5_2}

WA10_2 <- exp(coef(WU10_LLOLS_2)[["(Intercept)"]])
WB10_2 <- coef(WU10_LLOLS_2)[["log(Sc)"]]
WF10_2 <- function(x){WA10_2*x^WB10_2}

WA15_2 <- exp(coef(WU15_LLOLS_2)[["(Intercept)"]])
WB15_2 <- coef(WU15_LLOLS_2)[["log(Sc)"]]
WF15_2 <- function(x){WA15_2*x^WB15_2}

WA20_2 <- exp(coef(WU20_LLOLS_2)[["(Intercept)"]])
WB20_2 <- coef(WU20_LLOLS_2)[["log(Sc)"]]
WF20_2 <- function(x){WA20_2*x^WB20_2}

WA30_2 <- exp(coef(WU30_LLOLS_2)[["(Intercept)"]])
WB30_2 <- coef(WU30_LLOLS_2)[["log(Sc)"]]
WF30_2 <- function(x){WA30_2*x^WB30_2}

WA40_2 <- exp(coef(WU40_LLOLS_2)[["(Intercept)"]])
WB40_2 <- coef(WU40_LLOLS_2)[["log(Sc)"]]
WF40_2 <- function(x){WA40_2*x^WB40_2}

WA50_2 <- exp(coef(WU50_LLOLS_2)[["(Intercept)"]])
WB50_2 <- coef(WU50_LLOLS_2)[["log(Sc)"]]
WF50_2 <- function(x){WA50_2*x^WB50_2}

WAO50_2 <- exp(coef(WO50_LLOLS_2)[["(Intercept)"]])
WBO50_2 <- coef(WO50_LLOLS_2)[["log(Sc)"]]
WFO50_2 <- function(x){WAO50_2*x^WBO50_2}

##Baskerville corrected
WAB5_2 <- exp(coef(WU5_LLOLS_2)[["(Intercept)"]]+(Sum_W_5_2$sigma)^2/2)
WBF5_2 <- function(x){WAB5_2*x^WB5_2}

WAB10_2 <- exp(coef(WU10_LLOLS_2)[["(Intercept)"]]+(Sum_W_10_2$sigma)^2/2)
WBF10_2 <- function(x){WAB10_2*x^WB10_2}

WAB15_2 <- exp(coef(WU15_LLOLS_2)[["(Intercept)"]]+(Sum_W_15_2$sigma)^2/2)
WBF15_2 <- function(x){WAB15_2*x^WB15_2}

WAB20_2 <- exp(coef(WU20_LLOLS_2)[["(Intercept)"]]+(Sum_W_20_2$sigma)^2/2)
WBF20_2 <- function(x){WAB20_2*x^WB20_2}

WAB30_2 <- exp(coef(WU30_LLOLS_2)[["(Intercept)"]]+(Sum_W_30_2$sigma)^2/2)
WBF30_2 <- function(x){WAB30_2*x^WB30_2}

WAB40_2 <- exp(coef(WU40_LLOLS_2)[["(Intercept)"]]+(Sum_W_40_2$sigma)^2/2)
WBF40_2 <- function(x){WAB40_2*x^WB40_2}

WAB50_2 <- exp(coef(WU50_LLOLS_2)[["(Intercept)"]]+(Sum_W_50_2$sigma)^2/2)
WBF50_2 <- function(x){WAB50_2*x^WB50_2}

WABO50_2 <- exp(coef(WO50_LLOLS_2)[["(Intercept)"]]+(Sum_W_O50_2$sigma)^2/2)
WBFO50_2 <- function(x){WABO50_2*x^WBO50_2}

##NLR:
sv_W_2_U5 <- c(a=WA5_2, b=WB5_2)
fit_W_2_U5 <- nls(mbt ~ a*Sc^b,
                  start = sv_W_2_U5,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = WU5_2)

sv_W_2_U10 <- c(a=WA10_2, b=WB10_2)
fit_W_2_U10 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_2_U10,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU10_2)

sv_W_2_U15 <- c(a=WA15_2, b=WB15_2)
fit_W_2_U15 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_2_U15,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU15_2)

sv_W_2_U20 <- c(a=WA20_2, b=WB20_2)
fit_W_2_U20 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_2_U20,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU20_2)

sv_W_2_U30 <- c(a=WA30_2, b=WB30_2)
fit_W_2_U30 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_2_U30,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU30_2)

sv_W_2_U40 <- c(a=WA40_2, b=WB40_2)
fit_W_2_U40 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_2_U40,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU40_2)

sv_W_2_U50 <- c(a=WA50_2, b=WB50_2)
fit_W_2_U50 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_2_U50,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU50_2)

sv_W_2_O50 <- c(a=WAO50_2, b=WBO50_2)
fit_W_2_O50 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_2_O50,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WO50_2)




#Split 3:
##OLS:
OLSWood_3_U1 <- lm(mbt~Sc, data=WU1_3)
OLSWood_3_U2 <- lm(mbt~Sc, data=WU2_3)
OLSWood_3_U3 <- lm(mbt~Sc, data=WU3_3)
OLSWood_3_U5 <- lm(mbt~Sc, data=WU5_3)
OLSWood_3_U10 <- lm(mbt~Sc, data=WU10_3)
OLSWood_3_U20 <- lm(mbt~Sc, data=WU20_3)
OLSWood_3_U30 <- lm(mbt~Sc, data=WU30_3)
OLSWood_3_O30 <- lm(mbt~Sc, data=WO30_3)

##Log-Log OLS:
WU1_LLOLS_3 <- lm(log(mbt)~log(Sc), data=WU1_3)
WU2_LLOLS_3 <- lm(log(mbt)~log(Sc), data=WU2_3)
WU3_LLOLS_3 <- lm(log(mbt)~log(Sc), data=WU3_3)
WU5_LLOLS_3 <- lm(log(mbt)~log(Sc), data=WU5_3)
WU10_LLOLS_3 <- lm(log(mbt)~log(Sc), data=WU10_3)
WU20_LLOLS_3 <- lm(log(mbt)~log(Sc), data=WU20_3)
WU30_LLOLS_3 <- lm(log(mbt)~log(Sc), data=WU30_3)
WO30_LLOLS_3 <- lm(log(mbt)~log(Sc), data=WO30_3)


Sum_W_1_3 <- summary(WU1_LLOLS_3)
Sum_W_2_3 <- summary(WU2_LLOLS_3)
Sum_W_3_3 <- summary(WU3_LLOLS_3)
Sum_W_5_3 <- summary(WU5_LLOLS_3)
Sum_W_10_3 <- summary(WU10_LLOLS_3)
Sum_W_20_3 <- summary(WU20_LLOLS_3)
Sum_W_30_3 <- summary(WU30_LLOLS_3)
Sum_W_O30_3 <- summary(WO30_LLOLS_3)

###Coefficients and functions for plot
WA1_3 <- exp(coef(WU1_LLOLS_3)[["(Intercept)"]])
WB1_3 <- coef(WU1_LLOLS_3)[["log(Sc)"]]
WF1_3 <- function(x){WA1_3*x^WB1_3}

WA2_3 <- exp(coef(WU2_LLOLS_3)[["(Intercept)"]])
WB2_3 <- coef(WU2_LLOLS_3)[["log(Sc)"]]
WF2_3 <- function(x){WA2_3*x^WB2_3}

WA3_3 <- exp(coef(WU3_LLOLS_3)[["(Intercept)"]])
WB3_3 <- coef(WU3_LLOLS_3)[["log(Sc)"]]
WF3_3 <- function(x){WA3_3*x^WB3_3}

WA5_3 <- exp(coef(WU5_LLOLS_3)[["(Intercept)"]])
WB5_3 <- coef(WU5_LLOLS_3)[["log(Sc)"]]
WF5_3 <- function(x){WA5_3*x^WB5_3}

WA10_3 <- exp(coef(WU10_LLOLS_3)[["(Intercept)"]])
WB10_3 <- coef(WU10_LLOLS_3)[["log(Sc)"]]
WF10_3 <- function(x){WA10_3*x^WB10_3}

WA20_3 <- exp(coef(WU20_LLOLS_3)[["(Intercept)"]])
WB20_3 <- coef(WU20_LLOLS_3)[["log(Sc)"]]
WF20_3 <- function(x){WA20_3*x^WB20_3}

WA30_3 <- exp(coef(WU30_LLOLS_3)[["(Intercept)"]])
WB30_3 <- coef(WU30_LLOLS_3)[["log(Sc)"]]
WF30_3 <- function(x){WA30_3*x^WB30_3}

WAO30_3 <- exp(coef(WO30_LLOLS_3)[["(Intercept)"]])
WBO30_3 <- coef(WO30_LLOLS_3)[["log(Sc)"]]
WFO30_3 <- function(x){WAO30_3*x^WBO30_3}

##Baskerville corrected
WAB1_3 <- exp(coef(WU1_LLOLS_3)[["(Intercept)"]]+(Sum_W_1_3$sigma)^2/2)
WBF1_3 <- function(x){WAB1_3*x^WB1_3}

WAB2_3 <- exp(coef(WU2_LLOLS_3)[["(Intercept)"]]+(Sum_W_2_3$sigma)^2/2)
WBF2_3 <- function(x){WAB2_3*x^WB2_3}

WAB3_3 <- exp(coef(WU3_LLOLS_3)[["(Intercept)"]]+(Sum_W_3_3$sigma)^2/2)
WBF3_3 <- function(x){WAB3_3*x^WB3_3}

WAB5_3 <- exp(coef(WU5_LLOLS_3)[["(Intercept)"]]+(Sum_W_5_3$sigma)^2/2)
WBF5_3 <- function(x){WAB5_3*x^WB5_3}

WAB10_3 <- exp(coef(WU10_LLOLS_3)[["(Intercept)"]]+(Sum_W_10_3$sigma)^2/2)
WBF10_3 <- function(x){WAB10_3*x^WB10_3}

WAB20_3 <- exp(coef(WU20_LLOLS_3)[["(Intercept)"]]+(Sum_W_20_3$sigma)^2/2)
WBF20_3 <- function(x){WAB20_3*x^WB20_3}

WAB30_3 <- exp(coef(WU30_LLOLS_3)[["(Intercept)"]]+(Sum_W_30_3$sigma)^2/2)
WBF30_3 <- function(x){WAB30_3*x^WB30_3}

WABO30_3 <- exp(coef(WO30_LLOLS_3)[["(Intercept)"]]+(Sum_W_O30_3$sigma)^2/2)
WBFO30_3 <- function(x){WABO30_3*x^WBO30_3}

##NLR:
sv_W_3_U1 <- c(a=WA1_3, b=WB1_3)
fit_W_3_U1 <- nls(mbt ~ a*Sc^b,
                  start = sv_W_3_U1,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = WU1_3)

sv_W_3_U2 <- c(a=WA2_3, b=WB2_3)
fit_W_3_U2 <- nls(mbt ~ a*Sc^b,
                  start = sv_W_3_U2,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = WU2_3)

sv_W_3_U3 <- c(a=WA3_3, b=WB3_3)
fit_W_3_U3 <- nls(mbt ~ a*Sc^b,
                  start = sv_W_3_U3,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = WU3_3)

sv_W_3_U5 <- c(a=WA5_3, b=WB5_3)
fit_W_3_U5 <- nls(mbt ~ a*Sc^b,
                  start = sv_W_3_U5,
                  algorithm = "port",
                  control = nls.control(maxiter = 1000), data = WU5_3)

sv_W_3_U10 <- c(a=WA10_3, b=WB10_3)
fit_W_3_U10 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_3_U10,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU10_3)

sv_W_3_U20 <- c(a=WA20_3, b=WB20_3)
fit_W_3_U20 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_3_U20,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU20_3)

sv_W_3_U30 <- c(a=WA30_3, b=WB30_3)
fit_W_3_U30 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_3_U30,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WU30_3)

sv_W_3_O30 <- c(a=WAO30_3, b=WBO30_3)
fit_W_3_O30 <- nls(mbt ~ a*Sc^b,
                   start = sv_W_3_O30,
                   algorithm = "port",
                   control = nls.control(maxiter = 1000), data = WO30_3)