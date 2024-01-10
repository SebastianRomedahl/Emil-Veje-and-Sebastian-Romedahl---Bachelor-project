#Split 1:
Wood_graf_1_U5 <- ggplot(WU5_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_U5), color="OLS")))+
  stat_function(fun=WF5_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF5_1,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_1_U5), color="NLR")) +
  ggtitle("Under 5")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_1_U10 <- ggplot(WU10_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_U10), color="OLS")))+
  stat_function(fun=WF10_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF10_1,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_1_U10), color="NLR")) +
  ggtitle("Between 5 and 10")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_1_U15 <- ggplot(WU15_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_U15), color="OLS")))+
  stat_function(fun=WF15_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF15_1,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_1_U15), color="NLR")) +
  ggtitle("Between 10 and 15")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_1_U20 <- ggplot(WU20_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_U20), color="OLS")))+
  stat_function(fun=WF20_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF20_1,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_1_U20), color="NLR")) +
  ggtitle("Between 15 and 20")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_1_U25 <- ggplot(WU25_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_U25), color="OLS")))+
  stat_function(fun=WF25_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF25_1,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_1_U25), color="NLR")) +
  ggtitle("Between 20 and 25")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_1_U30 <- ggplot(WU30_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_U30), color="OLS")))+
  stat_function(fun=WF30_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF30_1,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_1_U30), color="NLR")) +
  ggtitle("Between 25 and 30")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_1_U35 <- ggplot(WU35_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_U35), color="OLS")))+
  stat_function(fun=WF35_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF35_1,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_1_U35), color="NLR")) +
  ggtitle("Between 30 and 35")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_1_U40 <- ggplot(WU40_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_U40), color="OLS")))+
  stat_function(fun=WF40_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF40_1,aes(color="Baskerville")) +
  ggtitle("Between 35 and 40")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville'),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green'))

Wood_graf_1_U45 <- ggplot(WU45_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_U45), color="OLS")))+
  stat_function(fun=WF45_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF45_1,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_1_U45), color="NLR")) +
  ggtitle("Between 40 and 45")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_1_U50 <- ggplot(WU50_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_U50), color="OLS")))+
  stat_function(fun=WF50_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF50_1,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_1_U50), color="NLR")) +
  ggtitle("Between 45 and 50")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_1_U55 <- ggplot(WU55_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_U55), color="OLS")))+
  stat_function(fun=WF55_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF55_1,aes(color="Baskerville")) +
  ggtitle("Between 50 and 55")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville'),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green'))

Wood_graf_1_O55 <- ggplot(WO55_1, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_1_O55), color="OLS")))+
  stat_function(fun=WFO55_1, aes(color="Log-Log OLS")) +
  stat_function(fun=WBFO55_1,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_1_O55), color="NLR")) +
  ggtitle("Over 55")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Split_1_wood <- grid.arrange(Wood_graf_1_U5, Wood_graf_1_U10, Wood_graf_1_U15, Wood_graf_1_U20, Wood_graf_1_U25, Wood_graf_1_U30, Wood_graf_1_U35, Wood_graf_1_U40, Wood_graf_1_U45, Wood_graf_1_U50, Wood_graf_1_U55, Wood_graf_1_O55, ncol =3)


ggsave(file="OLS grafer af første split wood.pdf", plot = Split_1_wood, width = 10, height = 10)


#Split 2:
Wood_graf_2_U5 <- ggplot(WU5_2, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_2_U5), color="OLS")))+
  stat_function(fun=WF5_2, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF5_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_2_U5), color="NLR")) +
  ggtitle("Under 5")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_2_U10 <- ggplot(WU10_2, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_2_U10), color="OLS")))+
  stat_function(fun=WF10_2, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF10_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_2_U10), color="NLR")) +
  ggtitle("Between 5 and 10")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_2_U15 <- ggplot(WU15_2, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_2_U15), color="OLS")))+
  stat_function(fun=WF15_2, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF15_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_2_U15), color="NLR")) +
  ggtitle("Between 10 and 15")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_2_U20 <- ggplot(WU20_2, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_2_U20), color="OLS")))+
  stat_function(fun=WF20_2, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF20_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_2_U20), color="NLR")) +
  ggtitle("Between 15 and 20")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_2_U30 <- ggplot(WU30_2, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_2_U30), color="OLS")))+
  stat_function(fun=WF30_2, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF30_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_2_U30), color="NLR")) +
  ggtitle("Between 20 and 30")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_2_U40 <- ggplot(WU40_2, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_2_U40), color="OLS")))+
  stat_function(fun=WF40_2, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF40_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_2_U40), color="NLR")) +
  ggtitle("Between 30 and 40")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_2_U50 <- ggplot(WU50_2, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_2_U50), color="OLS")))+
  stat_function(fun=WF50_2, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF50_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_2_U50), color="NLR")) +
  ggtitle("Between 40 and 50")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_2_O50 <- ggplot(WO50_2, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_2_O50), color="OLS")))+
  stat_function(fun=WFO50_2, aes(color="Log-Log OLS")) +
  stat_function(fun=WBFO50_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_2_O50), color="NLR")) +
  ggtitle("Over 50")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Split_2_wood <- grid.arrange(Wood_graf_2_U5, Wood_graf_2_U10, Wood_graf_2_U15, Wood_graf_2_U20, Wood_graf_2_U30, Wood_graf_2_U40, Wood_graf_2_U50, Wood_graf_2_O50, ncol =2)


ggsave(file="OLS grafer af andet split wood.pdf", plot = Split_2_wood, width = 10, height = 10)

#Split 3:
Wood_graf_3_U1 <- ggplot(WU1_3, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_3_U1), color="OLS")))+
  stat_function(fun=WF1_3, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF1_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_3_U1), color="NLR")) +
  ggtitle("Under 1")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_3_U2 <- ggplot(WU2_3, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_3_U2), color="OLS")))+
  stat_function(fun=WF2_3, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF2_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_3_U2), color="NLR")) +
  ggtitle("Between 1 and 2")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_3_U3 <- ggplot(WU3_3, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_3_U3), color="OLS")))+
  stat_function(fun=WF3_3, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF3_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_3_U3), color="NLR")) +
  ggtitle("Between 2 and 3")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_3_U5 <- ggplot(WU5_3, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_3_U5), color="OLS")))+
  stat_function(fun=WF5_3, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF5_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_3_U5), color="NLR")) +
  ggtitle("Between 3 and 5")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_3_U10 <- ggplot(WU10_3, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_3_U10), color="OLS")))+
  stat_function(fun=WF10_3, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF10_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_3_U10), color="NLR")) +
  ggtitle("Between 5 and 10")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_3_U20 <- ggplot(WU20_3, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_3_U20), color="OLS")))+
  stat_function(fun=WF20_3, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF20_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_3_U20), color="NLR")) +
  ggtitle("Between 10 and 20")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_3_U30 <- ggplot(WU30_3, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_3_U30), color="OLS")))+
  stat_function(fun=WF30_3, aes(color="Log-Log OLS")) +
  stat_function(fun=WBF30_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_3_U30), color="NLR")) +
  ggtitle("Between 20 and 30")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

Wood_graf_3_O30 <- ggplot(WO30_3, aes(Sc, mbt)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSWood_3_O30), color="OLS")))+
  stat_function(fun=WFO30_3, aes(color="Log-Log OLS")) +
  stat_function(fun=WBFO30_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_W_3_O30), color="NLR")) +
  ggtitle("Over 30")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


Split_3_wood <- grid.arrange(Wood_graf_3_U1 , Wood_graf_3_U2, Wood_graf_3_U3, Wood_graf_3_U5, Wood_graf_3_U10, Wood_graf_3_U20, Wood_graf_3_U30, Wood_graf_3_O30, ncol =2)


ggsave(file="OLS grafer af tredje split wood.pdf", plot = Split_3_wood, width = 10, height = 10)