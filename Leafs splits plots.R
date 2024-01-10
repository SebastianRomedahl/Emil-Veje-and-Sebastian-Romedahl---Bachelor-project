#Split 1:
LLOLSLeafs_graf_5 <- ggplot(Under5, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_U5), color="OLS")))+
  stat_function(fun=equ5, aes(color="Log-Log OLS")) +
  stat_function(fun=equB5,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_1_U5), color="NLR")) +
  ggtitle("Under 5")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_10 <- ggplot(Under10, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_U10), color="OLS")))+
  stat_function(fun=equ10, aes(color="Log-Log OLS")) +
  stat_function(fun=equB10,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_1_U10), color="NLR")) +
  ggtitle("Between 5 and 10")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_15 <- ggplot(Under15, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_U15), color="OLS")))+
  stat_function(fun=equ15, aes(color="Log-Log OLS")) +
  stat_function(fun=equB15,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_1_U15), color="NLR")) +
  ggtitle("Between 10 and 15")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_20 <- ggplot(Under20, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_U20), color="OLS")))+
  stat_function(fun=equ20, aes(color="Log-Log OLS")) +
  stat_function(fun=equB20,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_1_U20), color="NLR")) +
  ggtitle("Between 15 and 20")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_25 <- ggplot(Under25, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_U25), color="OLS")))+
  stat_function(fun=equ25, aes(color="Log-Log OLS")) +
  stat_function(fun=equB25,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_1_U25), color="NLR")) +
  ggtitle("Between 20 and 25")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_30 <- ggplot(Under30, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_U30), color="OLS")))+
  stat_function(fun=equ30, aes(color="Log-Log OLS")) +
  stat_function(fun=equB30,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_1_U30), color="NLR")) +
  ggtitle("Between 25 and 30")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_35 <- ggplot(Under35, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_U35), color="OLS")))+
  stat_function(fun=equ35, aes(color="Log-Log OLS")) +
  stat_function(fun=equB35,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_1_U35), color="NLR")) +
  ggtitle("Between 30 and 35")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_40 <- ggplot(Under40, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_U40), color="OLS")))+
  stat_function(fun=equ40, aes(color="Log-Log OLS")) +
  stat_function(fun=equB40,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_1_U40), color="NLR")) +
  ggtitle("Between 35 and 40")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_45 <- ggplot(Under45, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_U45), color="OLS")))+
  stat_function(fun=equ45, aes(color="Log-Log OLS")) +
  stat_function(fun=equB45,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_1_U45), color="NLR")) +
  ggtitle("Between 40 and 45")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_50 <- ggplot(Under50, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_U50), color="OLS")))+
  stat_function(fun=equ50, aes(color="Log-Log OLS")) +
  stat_function(fun=equB50,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_1_U50), color="NLR")) +
  ggtitle("Between 45 and 50")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_55 <- ggplot(Under55, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_U55), color="OLS")))+
  stat_function(fun=equ55, aes(color="Log-Log OLS")) +
  stat_function(fun=equB55,aes(color="Baskerville")) +
  ggtitle("Between 50 and 55")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville'),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green'))



LLOLSLeafs_graf_O55 <- ggplot(Over55, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_1_O55), color="OLS")))+
  stat_function(fun=equOver55, aes(color="Log-Log OLS")) +
  stat_function(fun=equBO55,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_1_O55), color="NLR")) +
  ggtitle("Over 55")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


Split_1_leafs <- grid.arrange(LLOLSLeafs_graf_5, LLOLSLeafs_graf_10, LLOLSLeafs_graf_15, LLOLSLeafs_graf_20, LLOLSLeafs_graf_25, LLOLSLeafs_graf_30, LLOLSLeafs_graf_35, LLOLSLeafs_graf_40, LLOLSLeafs_graf_45, LLOLSLeafs_graf_50, LLOLSLeafs_graf_55, LLOLSLeafs_graf_O55,  ncol=3)


ggsave(file="OLS grafer af første split leafs.pdf", plot = Split_1_leafs, width = 10, height = 10)


#Split 2:
LLOLSLeafs_graf_5_2 <- ggplot(Und5, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_2_U5), color="OLS")))+
  stat_function(fun=Lin5, aes(color="Log-Log OLS")) +
  stat_function(fun=equB5_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_2_U5), color="NLR")) +
  ggtitle("Under 5")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

LLOLSLeafs_graf_10_2 <- ggplot(Und10, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_2_U10), color="OLS")))+
  stat_function(fun=Lin10, aes(color="Log-Log OLS")) +
  stat_function(fun=equB10_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_2_U10), color="NLR")) +
  ggtitle("Between 5 and 10")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_15_2 <- ggplot(Und15, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_2_U15), color="OLS")))+
  stat_function(fun=Lin15, aes(color="Log-Log OLS")) +
  stat_function(fun=equB15_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_2_U15), color="NLR")) +
  ggtitle("Between 10 and 15")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_20_2 <- ggplot(Und20, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_2_U20), color="OLS")))+
  stat_function(fun=Lin20, aes(color="Log-Log OLS")) +
  stat_function(fun=equB20_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_2_U20), color="NLR")) +
  ggtitle("Between 15 and 20")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))



LLOLSLeafs_graf_30_2 <- ggplot(Und30, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_2_U30), color="OLS")))+
  stat_function(fun=Lin30, aes(color="Log-Log OLS")) +
  stat_function(fun=equB30_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_2_U30), color="NLR")) +
  ggtitle("Between 20 and 30")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))



LLOLSLeafs_graf_40_2 <- ggplot(Und40, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_2_U40), color="OLS")))+
  stat_function(fun=Lin40, aes(color="Log-Log OLS")) +
  stat_function(fun=equB40_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_2_U40), color="NLR")) +
  ggtitle("Between 30 and 40")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))



LLOLSLeafs_graf_50_2 <- ggplot(Und50, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_2_U50), color="OLS")))+
  stat_function(fun=Lin50, aes(color="Log-Log OLS")) +
  stat_function(fun=equB50_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_2_U50), color="NLR")) +
  ggtitle("Between 40 and 50")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


LLOLSLeafs_graf_O50_2 <- ggplot(Ove50, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_2_O50), color="OLS")))+
  stat_function(fun=LinOver50, aes(color="Log-Log OLS")) +
  stat_function(fun=equBO50_2,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_2_O50), color="NLR")) +
  ggtitle("Over 50")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=6),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


Split_2_leafs <- grid.arrange(LLOLSLeafs_graf_5_2, LLOLSLeafs_graf_10_2, LLOLSLeafs_graf_15_2, LLOLSLeafs_graf_20_2, LLOLSLeafs_graf_30_2, LLOLSLeafs_graf_40_2, LLOLSLeafs_graf_50_2, LLOLSLeafs_graf_O50_2,  ncol=2)


ggsave(file="OLS grafer af anden split leafs.pdf", plot = Split_2_leafs, width = 10, height = 10)


#Split 3:
LLOLSLeafs_graf_1_3 <- ggplot(U1, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_3_U1), color="OLS")))+
  stat_function(fun=F1, aes(color="Log-Log OLS")) +
  stat_function(fun=F1_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_3_U1), color="NLR")) +
  ggtitle("Under 1")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=8),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

LLOLSLeafs_graf_2_3 <- ggplot(U2, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_3_U2), color="OLS")))+
  stat_function(fun=F2, aes(color="Log-Log OLS")) +
  stat_function(fun=F2_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_3_U2), color="NLR")) +
  ggtitle("Between 1 and 2")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=8),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

LLOLSLeafs_graf_3_3 <- ggplot(U3, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_3_U3), color="OLS")))+
  stat_function(fun=F3, aes(color="Log-Log OLS")) +
  stat_function(fun=F3_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_3_U3), color="NLR")) +
  ggtitle("Between 2 and 3")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=8),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

LLOLSLeafs_graf_5_3 <- ggplot(U5, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_3_U5), color="OLS")))+
  stat_function(fun=F5, aes(color="Log-Log OLS")) +
  stat_function(fun=F5_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_3_U5), color="NLR")) +
  ggtitle("Between 3 and 5")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=8),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

LLOLSLeafs_graf_10_3 <- ggplot(U10, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_3_U10), color="OLS")))+
  stat_function(fun=F10, aes(color="Log-Log OLS")) +
  stat_function(fun=F10_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_3_U10), color="NLR")) +
  ggtitle("Between 5 and 10")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=8),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

LLOLSLeafs_graf_20_3 <- ggplot(U20, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_3_U20), color="OLS")))+
  stat_function(fun=F20, aes(color="Log-Log OLS")) +
  stat_function(fun=F20_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_3_U20), color="NLR")) +
  ggtitle("Between 10 and 20")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=8),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

LLOLSLeafs_graf_30_3 <- ggplot(U30, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_3_U30), color="OLS")))+
  stat_function(fun=F30, aes(color="Log-Log OLS")) +
  stat_function(fun=F30_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_3_U30), color="NLR")) +
  ggtitle("Between 20 and 30")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=8),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))

LLOLSLeafs_graf_O30_3 <- ggplot(O30, aes(Sc, Bfkg)) + 
  geom_point(color="blue") +
  geom_line((aes(Sc, predict(OLSLeafs_3_O30), color="OLS")))+
  stat_function(fun=FO30, aes(color="Log-Log OLS")) +
  stat_function(fun=FO30_3,aes(color="Baskerville")) +
  geom_line(aes(Sc, predict(fit_L_3_O30), color="NLR")) +
  ggtitle("Over 30")+
  xlab("Crown Size")+ylab("Biomass in kg") +
  theme(legend.text = element_text(size=8),  legend.background=element_blank())+
  scale_color_manual(name=" ",
                     breaks=c("OLS", 'Log-Log OLS', 'Baskerville', "NLR"),
                     values=c("OLS"="purple", 'Log-Log OLS'='red', 'Baskerville'='green', "NLR"="black"))


Split_3_leafs <- grid.arrange(LLOLSLeafs_graf_1_3, LLOLSLeafs_graf_2_3, LLOLSLeafs_graf_3_3, LLOLSLeafs_graf_5_3, LLOLSLeafs_graf_10_3, LLOLSLeafs_graf_20_3, LLOLSLeafs_graf_30_3, LLOLSLeafs_graf_O30_3,  ncol=2)


ggsave(file="OLS grafer af trejde split leafs.pdf", plot = Split_3_leafs, width = 10, height = 10)