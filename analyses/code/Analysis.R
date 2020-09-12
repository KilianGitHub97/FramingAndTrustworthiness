# Libraries ---------------------------------------------------------------

library(MASS)
library(effsize)
library(stats)
library(broom)
library(afex)
library(rstatix)
library(egg)
library(DescTools)

# Import ------------------------------------------------------------------

#setwd
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Libraries
source("Packages.R")

#Load data
data <- readRDS("..//..//data//processed//FramingAndTrust_clean.rds")


# Exploration -------------------------------------------------------------

#Duration
summary(data$Duration)
hist(data$Duration, breaks = 50)

#Date
range(data$Date)
table(
  as.Date(
    as.character(data$Date)
  )
)
hist(data$Date, breaks = 50)

#Gender
table(factor(data$Gender))
prop.table(table(factor(data$Gender)))

#Age
summary(data$Age)
hist(data$Age, breaks = 50)

#Condition
table(data$Condition)
prop.table(table(data$Condition))

#Understanding of experiment
rbind(
  UnderDesc = summary(data$UnderDesc),
  UnderRole = summary(data$UnderRole)
)
table(
  UnderDesc = data$UnderDesc,
  UnderRole = data$UnderRole
)
cor(
  data$UnderDesc,
  data$UnderRole,
  method = "spearman"
)


# Balance check -----------------------------------------------------------

#Age per Condition
data %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(Age),
    Median = median(Age),
    Range = range(Age)
  )
wilcox.test(
  x = data[Condition == "PositiveFrame"]$Age,
  y = data[Condition == "NegativeFrame"]$Age,
  paired = FALSE)

#Gender per Condition
chisq.test(
  table(data$Condition, factor(data$Gender))
)

#TrustInStranger per Condition
data %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(TrustInStranger),
    Median = median(TrustInStranger),
    Range = range(TrustInStranger)
  )
wilcox.test(
  data[Condition == "PositiveFrame"]$TrustInStranger,
  data[Condition == "NegativeFrame"]$TrustInStranger,
  paired = FALSE)


# Analysis ----------------------------------------------------------------

#Correlation Table
infos <- c("PastGame", "Promise", "PersTrait")
measures <- c("Trust", "Conf", "Transfer")
conditions <- c("_PosFrame", "_NegFrame")
cor_table <- list()
l = 1

for (info in 1:3) {
  for (mes in 1:3) {
    for (cond in 1:2) {
      
      #set up variables
      var_one <- paste0(
        "data$",
        infos[info],
        "1",
        conditions[cond],
        measures[mes]
      )
      var_two <- paste0(
        "data$",
        infos[info],
        "2",
        conditions[cond],
        measures[mes]
      )
      
      #Correlation & Results
      cor <- cor.test(
        eval(parse(text = var_one)),
        eval(parse(text = var_two)),
        method = "spearman"
        )
      cor_table[[l]] <- data.table(
        name = paste(
          infos[info],
          conditions[cond],
          measures[mes],
          sep = "_"),
        correlation = round(
          cor$estimate,
          4),
        p.value = round(
          cor$p.value,
          4)
        )
      
      l = l + 1
    }
  }
}

cor_table <- rbindlist(cor_table)
#Goal: Try without parse

#analysis cor_table
range(
  cor_table[grep("Pos", name)]$correlation
)
plot(
  cor_table[grep("Pos", name)]$correlation
)
range(
  cor_table[grep("Neg", name)]$correlation
)
plot(
  cor_table[grep("Neg", name)]$correlation
)

#Item combination
data <- data %>%
  mutate(
    PastGame_PosFrame_Trust = 
      round(
        (PastGame1_PosFrameTrust +
        PastGame2_PosFrameTrust) /
          2,
        0)) %>%
  mutate(
    PastGame_PosFrame_Conf = 
      round(
        (PastGame1_PosFrameConf +
        PastGame2_PosFrameConf) /
          2,
        0)) %>% 
  mutate(
    PastGame_PosFrame_Transfer = 
      round(
        (PastGame1_PosFrameTransfer +
        PastGame2_PosFrameTransfer) /
          2,
        0)) %>%
  mutate(
    PersTrait_PosFrame_Trust = 
      round(
        (PersTrait1_PosFrameTrust +
        PersTrait2_PosFrameTrust) /
          2,
        0)) %>%
  mutate(
    PersTrait_PosFrame_Conf = 
      round(
        (PersTrait1_PosFrameConf +
        PersTrait2_PosFrameConf) /
          2,
        0)) %>%
  mutate(
    PersTrait_PosFrame_Transfer = 
      round(
        (PersTrait1_PosFrameTransfer +
        PersTrait2_PosFrameTransfer) /
          2,
        0)) %>%
  mutate(
    Promise_PosFrame_Trust = 
      round(
        (Promise1_PosFrameTrust +
        Promise2_PosFrameTrust) /
          2,
        0)) %>%
  mutate(
    Promise_PosFrame_Conf = 
      round(
        (Promise1_PosFrameConf +
        Promise2_PosFrameConf) /
          2,
        0)) %>%
  mutate(
    Promise_PosFrame_Transfer = 
      round(
        (Promise1_PosFrameTransfer +
        Promise2_PosFrameTransfer) /
          2,
        0)) %>%
  mutate(
    PastGame_NegFrame_Trust = 
      round(
        (PastGame1_NegFrameTrust +
        PastGame2_NegFrameTrust) /
          2,
        0)) %>%
  mutate(
    PastGame_NegFrame_Conf = 
      round(
        (PastGame1_NegFrameConf +
        PastGame2_NegFrameConf) /
          2,
        0)) %>%
  mutate(
    PastGame_NegFrame_Transfer = 
      round(
        (PastGame1_NegFrameTransfer +
        PastGame2_NegFrameTransfer) /
          2,
        0)) %>%
  mutate(
    PersTrait_NegFrame_Trust = 
      round(
        (PersTrait1_NegFrameTrust +
        PersTrait2_NegFrameTrust) /
          2,
        0)) %>%
  mutate(
    PersTrait_NegFrame_Conf = 
      round(
        (PersTrait1_NegFrameConf +
        PersTrait2_NegFrameConf) /
          2,
        0)) %>%
  mutate(
    PersTrait_NegFrame_Transfer = 
      round(
        (PersTrait1_NegFrameTransfer +
        PersTrait2_NegFrameTransfer) /
          2,
        0)) %>%
  mutate(
    Promise_NegFrame_Trust = 
      round(
        (Promise1_NegFrameTrust +
        Promise2_NegFrameTrust) /
          2,
        0)) %>%
  mutate(
    Promise_NegFrame_Conf = 
      round(
        (Promise1_NegFrameConf +
        Promise2_NegFrameConf) /
          2,
        0)) %>%
  mutate(
    Promise_NegFrame_Transfer = 
      round(
        (Promise1_NegFrameTransfer +
        Promise2_NegFrameTransfer) /
          2,
        0)) %>%
  select(-contains("1_")) %>%
  select(-contains("2_"))
# Goal: shorten code


# Graphs ------------------------------------------------------------------

#Preparation: Long format
data_long <- melt(
  data = data,
  measure.vars = grep("Frame", colnames(data)),
  na.rm = TRUE
)

#Boxplot with all Items
BoxAll <- ggplot(
  data = data_long,
  mapping = aes(
    x = variable,
    y = value
  )) +
  geom_boxplot(fill = "lightblue") +
  ylab("likert") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8)
  )

# Boxplot for Trust Items
BoxTrust <- ggplot(
    data = data_long[grep("Trust", variable)],
    mapping = aes(
      x = variable,
      y = value
    )) +
    geom_boxplot(fill = "lightblue") +
    ylab("likert") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, size = 8)
      )

#Boxplot for Confidence Items
BoxConf <- ggplot(
  data = data_long[grep("Conf", variable)],
  mapping = aes(
    x = variable,
    y = value
  )) +
  geom_boxplot(fill = "lightblue") +
  ylab("likert") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8)
  )

#Boxplot for Transfer Items
BoxTransfer <- ggplot(
  data = data_long[grep("Transfer", variable)],
  mapping = aes(
    x = variable,
    y = value
  )) +
  geom_boxplot(fill = "lightblue") +
  ylab("likert") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8)
  )

#correlation network: positive frame
range(data[, grep("Pos", colnames(data))])
CorMatPos <- cor(
  data[, 13:21],
  method = "spearman",
  use = "complete.obs"
)
QgraphPos <- qgraph(
  CorMatPos,
  graph = "pcor",
  layout = "spring",
  label.prop = 2
)

#correlation network: positive frame
range(data[, grep("Neg", colnames(data))])
CorMatNeg <- cor(
  data[, 22:30],
  method = "spearman",
  use = "complete.obs"
)
QgraphNeg <- qgraph(
  CorMatNeg,
  graph = "pcor",
  layout = "spring",
  label.prop = 2
)

# Hypothesis --------------------------------------------------------------

# 1. Items from the positive frame rank significantly better (higher likert
# score) than their direct correspondent from the negative frame. 

#Note: infos, conditions and measures are defined on lines 105-107

FramingTest <- list()
l = 1

for(i in 1:3){
  for (j in 1:3) {
    var_one <- paste0(
      "data$",
      infos[i],
      conditions[1],
      "_",
      measures[j]
    )
    var_two <- paste0(
      "data$",
      infos[i],
      conditions[2],
      "_",
      measures[j]
    )
    WilcoxTest <- wilcox.test(
      na.omit(eval(parse(text = var_one))),
      na.omit(eval(parse(text = var_two))),
      paired = FALSE
    )
    Effsize <- cohen.d(
      eval(parse(text = var_one)),
      eval(parse(text = var_two))
    )
    FramingTest[[l]] <- data.table(
      var1 = var_one,
      var2 = var_two,
      w.stat = round(WilcoxTest$statistic, 4),
      p.value = round(WilcoxTest$p.value, 4),
      cohen.d = Effsize$estimate
      )
    l = l + 1
  }
}
FramingTest <- rbindlist(FramingTest)
FramingTest$p.adjust <- p.adjust(FramingTest$p.value)




#Summarise Mean and SD
PastGameTrust.summary <- AddedValuesFull %>%
  summarise( #PastGame
    MeanPastGamePos = mean(PastGame1_PosFrameTrust, na.rm = TRUE),
    MeanPastGameNeg = mean(PastGame1_NegFrameTrust, na.rm = TRUE),
    SDPastGamePos = sd(PastGame1_PosFrameTrust, na.rm = TRUE),
    SDPastGameNeg = sd(PastGame1_NegFrameTrust, na.rm = TRUE),
    MinPastGamePos = min(PastGame1_PosFrameTrust, na.rm = TRUE),
    MinPastGameNeg = min(PastGame1_NegFrameTrust, na.rm = TRUE),
    MaxPastGamePos = max(PastGame1_PosFrameTrust, na.rm = TRUE),
    MaxPastGameNeg = max(PastGame1_NegFrameTrust, na.rm = TRUE)
  )
PersTraitTrust.summary <- AddedValuesFull %>%
  summarise(
    MeanPersTraitPos = mean(PersTrait1_PosFrameTrust, na.rm = TRUE),
    MeanPersTraitNeg = mean(PersTrait1_NegFrameTrust, na.rm = TRUE),
    SDPersTraitPos = sd(PersTrait1_PosFrameTrust, na.rm = TRUE),
    SDPersTraitNeg = sd(PersTrait1_NegFrameTrust, na.rm = TRUE),
    MinPersTraitPos = min(PersTrait1_PosFrameTrust, na.rm = TRUE),
    MinPersTraitNeg = min(PersTrait1_NegFrameTrust, na.rm = TRUE),
    MaxPersTraitPos = max(PersTrait1_PosFrameTrust, na.rm = TRUE),
    MaxPersTraitNeg = max(PersTrait1_NegFrameTrust, na.rm = TRUE)
  )
PromiseTrust.summary <- AddedValuesFull %>%  
  summarise(
    MeanPromisePos = mean(Promise1_PosFrameTrust, na.rm = TRUE),
    MeanPromiseNeg = mean(Promise1_NegFrameTrust, na.rm = TRUE),
    SDPromisePos = sd(Promise1_PosFrameTrust, na.rm = TRUE),
    SDPromiseNeg = sd(Promise1_NegFrameTrust, na.rm = TRUE),
    MinPromisePos = min(Promise1_PosFrameTrust, na.rm = TRUE),
    MinPromiseNeg = min(Promise1_NegFrameTrust, na.rm = TRUE),
    MaxPromisePos = max(Promise1_PosFrameTrust, na.rm = TRUE),
    MaxPromiseNeg = max(Promise1_NegFrameTrust, na.rm = TRUE)
  )

H1.summary <- data.frame(cbind(c(PastGameTrust.summary, PersTraitTrust.summary, PromiseTrust.summary))) #Dataframe with all the summary info

#Effectsize
PastGameTrust.d <- cohen.d(AddedValuesFull$PastGame1_PosFrameTrust, AddedValuesFull$PastGame1_NegFrameTrust, na.rm = TRUE) #d estimate: 0.2172247 (small)
PersTraitTrust.d <- cohen.d(AddedValuesFull$PersTrait1_PosFrameTrust, AddedValuesFull$PersTrait1_NegFrameTrust, na.rm = TRUE) # d estimate: 1.55833 (large)  
PromiseTrust.d <-  cohen.d(AddedValuesFull$Promise1_PosFrameTrust, AddedValuesFull$Promise1_NegFrameTrust, na.rm = TRUE) #d estimate: -0.1069272 (negligible)




#Hypothesis 2================================================================

#overall: checking wether the tree groups differenciate 
Hyp2data.wide <- data%>%
  transmute(id, ResponseId, UsefulPastBehav, UsefulPersTrait, UsefulPersMess)
 
Hyp2data.long <- Hyp2data.wide %>%
  gather(key = "Usefulness", value = "Likert", -id, -ResponseId)%>%
  na.omit()

#Boxplot that shows the differences of the Usefulness between the information types
Hyp2Box <- ggplot(data = Hyp2data.long, #Boxplot for the usefulness of the information
                  aes(
                    x= Usefulness,
                    y = Likert
                  )) +
  geom_barplot() +
  facet_wrap(~id)


#test wether there are significant differences between the 3 groups: why friedman.test(): bc. the usefulness was collected by all participants and therefore is a within subject design (with paired = TRUE). therefore we have to do an anova with paired = True. But the scale is ordinal so--> friedman.test
friedman.test(y = Hyp2data.long$Likert, groups = Hyp2data.long$Usefulness, blocks = Hyp2data.long$ResponseId) #Friedman Test (bc: non-normal distributed input and within subject)

#PostHoc test for friedman:  
wilcox_test(data = Hyp2data.long,formula = Likert ~ Usefulness, p.adjust.method = "bonferroni", paired = TRUE) #package rstatix


# for Positive and Negative Frame
#function that caluculates the friedmantest and post hoc test
H2.Test <- function(x){
  Hyp2data.wide <- data%>%
    transmute(id, ResponseId, UsefulPastBehav, UsefulPersTrait, UsefulPersMess) %>%
    filter(id == x)
  
  Hyp2data.long <- Hyp2data.wide %>%
    gather(key = "Usefulness", value = "Likert", -id, -ResponseId)%>%
    na.omit()
  
  #test wether there are significant differences between the 3 groups
  Res1 <- friedman.test(y = Hyp2data.long$Likert, groups = Hyp2data.long$Usefulness, blocks = Hyp2data.long$ResponseId)
  
  #PostHoc test: Tukey HSD
  Res2 <- wilcox_test(data = Hyp2data.long, formula = Likert ~ Usefulness, p.adjust.method = "bonferroni", paired = TRUE) #package rstatix

  print(Res1)
  print(Res2)
}

#ANOVA and POSTHOC for Positive Frame
H2.Test(x = "Positive Frame")
#ANOVA and POSTHOC for Negative Frame
H2.Test(x = "Negative Frame")



#Hypothesis 3============================================
#Table for positive Frame
Tab.Pos.Frame <- AddedValuesFull %>%
  filter(id == "Positive Frame")

#Table for negative Frame
Tab.Neg.Frame <- AddedValuesFull %>%
  filter(id == "Negative Frame")

#cor.test for positive frame
PosPastGameCor <- tidy(cor.test(Tab.Pos.Frame$PastGame1_PosFrameConf, Tab.Pos.Frame$UsefulPastBehav))
PosPersTraitCor <- tidy(cor.test(x =Tab.Pos.Frame$PersTrait1_PosFrameConf, y =Tab.Pos.Frame$UsefulPersTrait))
PosPromiseCor <- tidy(cor.test(x =Tab.Pos.Frame$Promise1_PosFrameConf, y =Tab.Pos.Frame$UsefulPersMess))

#cor.test for negative frame
NegPastGameCor <- tidy(cor.test(x =Tab.Neg.Frame$PastGame1_NegFrameConf, y =Tab.Neg.Frame$UsefulPastBehav))
NegPersTraitCor <- tidy(cor.test(x =Tab.Neg.Frame$PersTrait1_NegFrameConf, y =Tab.Neg.Frame$UsefulPersTrait))
NegPromiseCor <- tidy(cor.test(x =Tab.Neg.Frame$Promise1_NegFrameConf, y =Tab.Neg.Frame$UsefulPersMess))

#put them into a data.frame
corTestInfo <- data.frame(rbind(PosPastGameCor,PosPersTraitCor,PosPromiseCor, NegPastGameCor, NegPersTraitCor, NegPromiseCor), 
                          cbind(information_type = c("Past behavior in trustgame", #add a new column
                                                     "Information on personality trait",
                                                     "Promise",
                                                     "Past behavior in trustgame", 
                                                     "Information on personality trait",
                                                     "Promise"
                          )),
                          cbind(condition = c("Positive", #add a new column
                                              "Positive", 
                                              "Positive", 
                                              "Negative", 
                                              "Negative", 
                                              "Negative")))%>%
  rename(correlation = "estimate")%>%
  rename(t.value = "statistic")%>%
  select(-method)%>%
  select(-alternative)%>%
  select(-parameter)

#reorder columns by names
col_order <- c("information_type",
               "condition", 
               "correlation",
               "p.value", 
               "t.value", 
               "conf.low", 
               "conf.high")
corTestInfo <- corTestInfo[, col_order] 

formattable(corTestInfo)

#Visualised Correlations
VisCorelOutput <- function(data, x, y){
  ggplot(data = data,
         mapping = aes(
           x = x,
           y = y
         )) +
    geom_point()+
    geom_smooth(method = lm)
}

#Correlation Graphs for Positive Frame
VisCorelOutput.1 <- VisCorelOutput(data = Tab.Pos.Frame, x = Tab.Pos.Frame$PastGame1_PosFrameConf, y =Tab.Pos.Frame$UsefulPastBehav)
VisCorelOutput.2 <- VisCorelOutput(data = Tab.Pos.Frame, x = Tab.Pos.Frame$PersTrait1_PosFrameConf, y = Tab.Pos.Frame$UsefulPersTrait)
VisCorelOutput.3 <- VisCorelOutput(data = Tab.Pos.Frame, x = Tab.Pos.Frame$Promise1_PosFrameConf, y = Tab.Pos.Frame$UsefulPersMess)

#Correlation Graphs for Positive Frame
VisCorelOutput.4 <- VisCorelOutput(data = Tab.Neg.Frame, x = Tab.Neg.Frame$PastGame1_NegFrameConf, y =Tab.Neg.Frame$UsefulPastBehav)
VisCorelOutput.5 <- VisCorelOutput(data = Tab.Neg.Frame, x = Tab.Neg.Frame$PersTrait1_NegFrameConf, y = Tab.Neg.Frame$UsefulPersTrait)
VisCorelOutput.6 <- VisCorelOutput(data = Tab.Neg.Frame, x = Tab.Neg.Frame$Promise1_NegFrameConf, y = Tab.Neg.Frame$UsefulPersMess)

#Put all the graphs together
CorelGraphic <- ggarrange( VisCorelOutput.1, 
                           VisCorelOutput.2,
                           VisCorelOutput.3,
                           VisCorelOutput.4,
                           VisCorelOutput.5,
                           VisCorelOutput.6,
                           labels = c("A", "B", "C", "D", "E", "F"),
                           ncol = 3, 
                           nrow = 2 )
