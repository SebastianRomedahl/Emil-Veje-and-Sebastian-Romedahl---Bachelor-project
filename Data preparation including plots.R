#Library
library(randomForest)
library(ggplot2)
library(tidyverse)
library(caret)
library(quantregForest)
library(rfinterval)
library(dplyr)
library(moderndive)
library(ranger)
library(lava)
library(gridExtra)
library(plotrix)
library(minpack.lm)
theme_update(plot.title = element_text(hjust = 0.5))

#Loading data:
leafs <- read.csv("leafs.csv", header=TRUE, sep=";")
roots <- read.csv("roots2.csv", header=TRUE, sep=";")
wood <- read.csv("wood.csv", header=TRUE, sep=";")
roots$Sc...Crown.Area <- as.numeric(roots$Sc...Crown.Area)
roots$mract <- as.numeric(roots$mract)

#Plots of all datasets:
Leafs_plot <- ggplot(data = leafs, mapping = aes(x = Sc, y = Bfkg))+
  geom_point(color="blue")+
  ggtitle("Leafs") +
  xlab("Crown Size")+ylab("Biomass")

Wood_plot <- ggplot(data = wood, mapping = aes(x = Sc, y = mbt))+
  geom_point(color="blue")+
  ggtitle("Wood") +
  xlab("Crown Size")+ylab("Biomass")

Roots_plot <- ggplot(data = roots, mapping = aes(x = Sc...Crown.Area , y = mract))+
  geom_point(color="blue")+
  ggtitle("Roots")+
  xlab("Crown Size")+ylab("Biomass")

Scatter <- grid.arrange(Leafs_plot, Wood_plot, Roots_plot, ncol=3)

ggsave(file="Scatterplots af data.pdf", plot = Scatter)

#First datasplit of leafs
leafs_1 <- leafs %>%
  mutate(Crown_intervals = case_when(
    Sc < 5 ~ "<5",
    Sc < 10 ~ "[5,10)",
    Sc < 15 ~ "[10,15)",
    Sc < 20 ~ "[15,20)",
    Sc < 25 ~ "[20,25)",
    Sc < 30 ~ "[25,30)",
    Sc < 35 ~ "[30,35)",
    Sc < 40 ~ "[35,40)",
    Sc < 45 ~ "[40,45)",
    Sc < 50 ~ "[45,50)",
    Sc < 55 ~ "[50,55)",
    TRUE ~ ">55"
  ))
leafs_1%>%
  group_by(Crown_intervals)%>%
  summarise(c=n(),
            Sm=mean(Bfkg),
            Min=min(Bfkg),
            Max=max(Bfkg))

ggplot(leafs_1, aes(Sc, Bfkg)) +
  geom_point(aes(color=Crown_intervals)) +
  geom_smooth(method=lm, se=FALSE)

Under5 <- filter(leafs_1, Crown_intervals=="<5")
Under10 <- filter(leafs_1, Crown_intervals=="[5,10)")
Under15 <- filter(leafs_1, Crown_intervals=="[10,15)")
Under20 <- filter(leafs_1, Crown_intervals=="[15,20)")
Under25 <- filter(leafs_1, Crown_intervals=="[20,25)")
Under30 <- filter(leafs_1, Crown_intervals=="[25,30)")
Under35 <- filter(leafs_1, Crown_intervals=="[30,35)")
Under40 <- filter(leafs_1, Crown_intervals=="[35,40)")
Under45 <- filter(leafs_1, Crown_intervals=="[40,45)")
Under50 <- filter(leafs_1, Crown_intervals=="[45,50)")
Under55 <- filter(leafs_1, Crown_intervals=="[50,55)")
Over55 <- filter(leafs_1, Crown_intervals==">55")

#Second datasplit of leafs
leafs_2 <- leafs %>%
  mutate(Crown_intervals = case_when(
    Sc < 5 ~ "<5",
    Sc < 10 ~ "[5,10)",
    Sc < 15 ~ "[10,15)",
    Sc < 20 ~ "[15,20)",
    Sc < 30 ~ "[20,30)",
    Sc < 40 ~ "[30,40)",
    Sc < 50 ~ "[40,50)",
    TRUE ~ ">50"
  ))

leafs_2%>%
  group_by(Crown_intervals)%>%
  summarise(c=n(),
            Sm=mean(Bfkg),
            Min=min(Bfkg),
            Max=max(Bfkg))

ggplot(leafs_2, aes(Sc, Bfkg)) +
  geom_point(aes(color=Crown_intervals)) +
  geom_smooth(method=lm, se=FALSE)

Und5 <- filter(leafs_2, Crown_intervals=="<5")
Und10 <- filter(leafs_2, Crown_intervals=="[5,10)")
Und15 <- filter(leafs_2, Crown_intervals=="[10,15)")
Und20 <- filter(leafs_2, Crown_intervals=="[15,20)")
Und30 <- filter(leafs_2, Crown_intervals=="[20,30)")
Und40 <- filter(leafs_2, Crown_intervals=="[30,40)")
Und50 <- filter(leafs_2, Crown_intervals=="[40,50)")
Ove50 <- filter(leafs_2, Crown_intervals==">50")

#Third datasplit of leafs
leafs_3 <- leafs %>%
  mutate(Crown_intervals = case_when(
    Sc < 1 ~ "<1",
    Sc < 2 ~ "[1,2)",
    Sc < 3 ~ "[2,3)",
    Sc < 5 ~ "[3,5)",
    Sc < 10 ~ "[5,10)",
    Sc < 20 ~ "[10,20)",
    Sc < 30 ~ "[20,30)",
    TRUE ~ ">30"
  ))

leafs_3%>%
  group_by(Crown_intervals)%>%
  summarise(c=n(),
            Sm=mean(Bfkg),
            Min=min(Bfkg),
            Max=max(Bfkg))

ggplot(leafs_3, aes(Sc, Bfkg)) +
  geom_point(aes(color=Crown_intervals)) +
  geom_smooth(method=lm, se=FALSE)

U1 <- filter(leafs_3, Crown_intervals=="<1")
U2 <- filter(leafs_3, Crown_intervals=="[1,2)")
U3 <- filter(leafs_3, Crown_intervals=="[2,3)")
U5 <- filter(leafs_3, Crown_intervals=="[3,5)")
U10 <- filter(leafs_3, Crown_intervals=="[5,10)")
U20 <- filter(leafs_3, Crown_intervals=="[10,20)")
U30 <- filter(leafs_3, Crown_intervals=="[20,30)")
O30 <- filter(leafs_3, Crown_intervals==">30")

#First datasplit of wood
wood_1 <- wood %>%
  mutate(Crown_intervals = case_when(
    Sc < 5 ~ "<5",
    Sc < 10 ~ "[5,10)",
    Sc < 15 ~ "[10,15)",
    Sc < 20 ~ "[15,20)",
    Sc < 25 ~ "[20,25)",
    Sc < 30 ~ "[25,30)",
    Sc < 35 ~ "[30,35)",
    Sc < 40 ~ "[35,40)",
    Sc < 45 ~ "[40,45)",
    Sc < 50 ~ "[45,50)",
    Sc < 55 ~ "[50,55)",
    TRUE ~ ">55"
  ))
wood_1%>%
  group_by(Crown_intervals)%>%
  summarise(c=n(),
            Sm=mean(mbt),
            Min=min(mbt),
            Max=max(mbt))

ggplot(wood_1, aes(Sc, mbt)) +
  geom_point(aes(color=Crown_intervals)) +
  geom_smooth(method=lm, se=FALSE)

WU5_1 <- filter(wood_1, Crown_intervals=="<5")
WU10_1 <- filter(wood_1, Crown_intervals=="[5,10)")
WU15_1 <- filter(wood_1, Crown_intervals=="[10,15)")
WU20_1 <- filter(wood_1, Crown_intervals=="[15,20)")
WU25_1 <- filter(wood_1, Crown_intervals=="[20,25)")
WU30_1 <- filter(wood_1, Crown_intervals=="[25,30)")
WU35_1 <- filter(wood_1, Crown_intervals=="[30,35)")
WU40_1 <- filter(wood_1, Crown_intervals=="[35,40)")
WU45_1 <- filter(wood_1, Crown_intervals=="[40,45)")
WU50_1 <- filter(wood_1, Crown_intervals=="[45,50)")
WU55_1 <- filter(wood_1, Crown_intervals=="[50,55)")
WO55_1 <- filter(wood_1, Crown_intervals==">55")

#Second datasplit of wood
wood_2 <- wood %>%
  mutate(Crown_intervals = case_when(
    Sc < 5 ~ "<5",
    Sc < 10 ~ "[5,10)",
    Sc < 15 ~ "[10,15)",
    Sc < 20 ~ "[15,20)",
    Sc < 30 ~ "[20,30)",
    Sc < 40 ~ "[30,40)",
    Sc < 50 ~ "[40,50)",
    TRUE ~ ">50"
  ))
wood_2%>%
  group_by(Crown_intervals)%>%
  summarise(c=n(),
            Sm=mean(mbt),
            Min=min(mbt),
            Max=max(mbt))

ggplot(wood_2, aes(Sc, mbt)) +
  geom_point(aes(color=Crown_intervals)) +
  geom_smooth(method=lm, se=FALSE)

WU5_2 <- filter(wood_2, Crown_intervals=="<5")
WU10_2 <- filter(wood_2, Crown_intervals=="[5,10)")
WU15_2 <- filter(wood_2, Crown_intervals=="[10,15)")
WU20_2 <- filter(wood_2, Crown_intervals=="[15,20)")
WU30_2 <- filter(wood_2, Crown_intervals=="[20,30)")
WU40_2 <- filter(wood_2, Crown_intervals=="[30,40)")
WU50_2 <- filter(wood_2, Crown_intervals=="[40,50)")
WO50_2 <- filter(wood_2, Crown_intervals==">50")

#Third datasplit of wood
wood_3 <- wood %>%
  mutate(Crown_intervals = case_when(
    Sc < 1 ~ "<1",
    Sc < 2 ~ "[1,2)",
    Sc < 3 ~ "[2,3)",
    Sc < 5 ~ "[3,5)",
    Sc < 10 ~ "[5,10)",
    Sc < 20 ~ "[10,20)",
    Sc < 30 ~ "[20,30)",
    TRUE ~ ">30"
  ))

wood_3%>%
  group_by(Crown_intervals)%>%
  summarise(c=n(),
            Sm=mean(mbt),
            Min=min(mbt),
            Max=max(mbt))

ggplot(wood_3, aes(Sc, mbt)) +
  geom_point(aes(color=Crown_intervals)) +
  geom_smooth(method=lm, se=FALSE)

WU1_3 <- filter(wood_3, Crown_intervals=="<1")
WU2_3 <- filter(wood_3, Crown_intervals=="[1,2)")
WU3_3 <- filter(wood_3, Crown_intervals=="[2,3)")
WU5_3 <- filter(wood_3, Crown_intervals=="[3,5)")
WU10_3 <- filter(wood_3, Crown_intervals=="[5,10)")
WU20_3 <- filter(wood_3, Crown_intervals=="[10,20)")
WU30_3 <- filter(wood_3, Crown_intervals=="[20,30)")
WO30_3 <- filter(wood_3, Crown_intervals==">30")

#Plot of the different splits on leafs
leafs_1$Crown_intervals <- factor(leafs_1$Crown_intervals, levels = c("<5","[5,10)","[10,15)","[15,20)", "[20,25)","[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", "[50,55)", "[55,60)", ">60"))
Leafsplit_1 <- ggplot(leafs_1, aes(Sc, Bfkg))+
  geom_point(aes(color=Crown_intervals))+
  ggtitle("Leafs split 1") +
  xlab("Crown Size")+ylab("Biomass")


leafs_2$Crown_intervals <- factor(leafs_2$Crown_intervals, levels = c("<5","[5,10)","[10,15)","[15,20)", "[20,30)", "[30,40)", "[40,50)", ">50"))
Leafsplit_2 <- ggplot(leafs_2, aes(Sc, Bfkg))+
  geom_point(aes(color=Crown_intervals))+
  ggtitle("Leafs split 2") +
  xlab("Crown Size")+ylab("Biomass")



leafs_3$Crown_intervals <- factor(leafs_3$Crown_intervals, levels = c("<1","[1,2)","[2,3)","[3,5)", "[5,10)", "[10,20)", "[20,30)", ">30"))
Leafsplit_3 <- ggplot(leafs_3, aes(Sc, Bfkg))+
  geom_point(aes(color=Crown_intervals))+
  ggtitle("Leafs split 3") +
  xlab("Crown Size")+ylab("Biomass")



Leafs_Split <- grid.arrange(Leafsplit_1, Leafsplit_2, Leafsplit_3, ncol=3)

ggsave(file="Split af Leafs data.pdf", plot = Leafs_Split, width = 15, height = 5)

#Plots of the different splits on wood
wood_1$Crown_intervals <- factor(wood_1$Crown_intervals, levels = c("<5","[5,10)","[10,15)","[15,20)", "[20,25)","[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", "[50,55)", ">55"))
Woodsplit_1 <- ggplot(wood_1, aes(Sc, mbt))+
  geom_point(aes(color=Crown_intervals))+
  ggtitle("Wood split 1") +
  xlab("Crown Size")+ylab("Biomass")


wood_2$Crown_intervals <- factor(wood_2$Crown_intervals, levels = c("<5","[5,10)","[10,15)","[15,20)", "[20,30)", "[30,40)", "[40,50)", ">50"))
Woodsplit_2 <- ggplot(wood_2, aes(Sc, mbt))+
  geom_point(aes(color=Crown_intervals))+
  ggtitle("Wood split 2") +
  xlab("Crown Size")+ylab("Biomass")



wood_3$Crown_intervals <- factor(wood_3$Crown_intervals, levels = c("<1","[1,2)","[2,3)","[3,5)", "[5,10)", "[10,20)", "[20,30)", ">30"))
Woodsplit_3 <- ggplot(wood_3, aes(Sc, mbt))+
  geom_point(aes(color=Crown_intervals))+
  ggtitle("Wood split 3") +
  xlab("Crown Size")+ylab("Biomass")



Wood_Split <- grid.arrange(Woodsplit_1, Woodsplit_2, Woodsplit_3, ncol=3)

ggsave(file="Split af Wood data.pdf", plot = Wood_Split, width = 15, height = 5)