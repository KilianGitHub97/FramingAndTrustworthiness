# Libraries ---------------------------------------------------------------

library(formattable)
library(MASS)
library(effsize)
library(stats)
library(broom)
library(afex)
library(rstatix)
library(egg)
library(DescTools)

# Import ------------------------------------------------------------------

#Libraries
source("Packages.R")

#setwd
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

#Understanding of Experiment===============================================

#Test wether people understand the experiment
data%>%
  summarise(
    mean_understanding = mean(UnderDesc),
    SD_understanding = sd(UnderDesc),
    max_understanding = max(UnderDesc),
    min_understanding = min(UnderDesc),
  )  #Understanding of whats to do was good, considering that the possible inputs were between 1-7: mean = 5.56, sd = 1.32, max = 7, min = 2

data%>%
  summarise(
    mean_role = mean(UnderRole),
    SD_role = sd(UnderRole),
    max_role = max(UnderRole),
    min_role = min(UnderRole),
  ) #Understanding of whats to do was good, considering that the possible inputs were between 1-7: mean = 5.62, sd = 1.29, max = 7, min = 2

Correlation_EvaluativeTasks<- cor(data$UnderDesc, data$UnderRole, method = "pearson") #cor = 0.8 suggests that people did not understand the difference in the two items.

#Parallel coordinates plot
ColorParcoord <- ifelse(data$UnderRole <= data$UnderDesc,"green","red") #Vector color
parcoord(data[,c("UnderRole","UnderDesc")], var.label = TRUE, col = ColorParcoord) #parallel coordinates Diagramm suggests, that most of the people recorded the same value

#see how many people gave the same input to UnderRole and UnderDesc
data%>% 
  filter(UnderDesc == UnderRole)%>%
  count() #71 of 102 responses did not differ between UnderDesc and UnderRole

#conclusion: The overall understanding of the Experiment was ok, but people did not understand, that the two items did not ask the same thing =D






# correlation tables for positive and negative correlation ----------------

select <- dplyr::select #bc Mass uses select too. So we have to say, that we want select to be from dplyr

DrawCorTable <- function(data, x, y){
  #Select all the columns that contain "x"
  corvar1 <- data %>% 
    select(contains(x)) %>%
    na.omit()
  
  #Select all the columns that contain "y"
  corvar2 <- data%>% 
    select(contains(y)) %>%
    na.omit()
  
  #draw a correlation matrix
  PosWideAllCorrelation <- as.data.frame(cor(x = corvar1, y = corvar2, method = "pearson"))%>% 
    rownames_to_column() #since a matrix contains separate rownames, we have to put them inside a column
  
  #the correlation df is in wide format, what makes it difficult to understand. We have to make it "long"
  PosLongAllCorellation <- gather(data = PosWideAllCorrelation, Second_Item, Correlation, -rowname) 
 
  #now we have a dataframe, that contains the correlation between every single variable from corvar 1 & corvar 2, which is not useful. Therefore we filter out the unnecessary ones  
  PoscountRowame <- nchar(PosLongAllCorellation$rowname, type = "chars") #the idea is, that the corresponding variables contain the same amount of characters. Therefore we can count the characters and compare them afterwards
  PoscountSecondCond <- nchar(PosLongAllCorellation$Second_Item, type = "chars")
  
  CorTable <- PosLongAllCorellation %>%
    filter(PoscountRowame == PoscountSecondCond)%>% #compare the number of characters of the two corresponding variables.
    filter(grepl("Trust", rowname) == grepl("Trust", Second_Item))%>%#Some variables don't match the system, so we have to adjust
    rename(First_Item = "rowname") #Rename for style
}

#making a correlation table with all corresponding items of the positive frame
CorTablePos <- DrawCorTable(data = data,x = "1_PosFrame", y = "2_PosFrame") 
PosRangeOfCorrelation <- range(CorTablePos$Correlation) #Range
formattable(CorTablePos)

#making a correlation table with all corresponding items of the negative frame
CorTableNeg <- DrawCorTable(data = data, x = "1_NegFrame", y = "2_NegFrame") 
NegRangeOfCorrelation <- range(CorTableNeg$Correlation) #Range
formattable(CorTableNeg)





# Item combined dataframe -------------------------------------------------

#function, that adds columns together
AddValues <- function(data, x, y){ 
  TabX <- data%>%
    select(Duration,
           UsefulPastBehav,
           UsefulPersTrait,
           UsefulPersMess,
           UnderDesc,
           UnderRole,
           Gender,
           id,
           contains(x))%>%
    na.omit()
    
  TabY <- data%>%
    select(Duration,
           UsefulPastBehav,
           UsefulPersTrait,
           UsefulPersMess,
           UnderDesc,
           UnderRole,
           Gender,
           id,
           contains(y))%>%
    na.omit()
    
  names(TabX) <- names(TabY)
  
  CombinedData <- rbind(TabX, TabY)
}

#inserting the pattern from columns of the positive condition, that have to be combined
AddedValuesPosFrame <- AddValues(data = data, x = "2_Pos", y = "1_Pos")

#inserting the pattern from columns of the negative condition, that have to be combined
AddedValuesNegFrame <- AddValues(data = data, x = "2_Neg", y = "1_Neg")

#put the two frames together.
AddedValuesFull <- full_join(AddedValuesPosFrame, AddedValuesNegFrame, by = c("Duration", 
                                                                              "UsefulPastBehav",
                                                                              "UsefulPersTrait",
                                                                              "UsefulPersMess",
                                                                              "UnderDesc",
                                                                              "UnderRole",
                                                                              "Gender",
                                                                              "id"))%>%
  arrange(desc(id))

view(AddedValuesFull) #dataframe with all the combinded items


# transform from wide to long ---------------------------------------------
select <- dplyr::select()

AddedValuesFull.long <- gather(data = AddedValuesFull, key = "Condition", value = "Likert",  -Duration, 
                                                                                             -UsefulPastBehav,
                                                                                             -UsefulPersTrait,
                                                                                             -UsefulPersMess,
                                                                                             -UnderDesc,
                                                                                             -UnderRole,
                                                                                             -Gender,
                                                                                             -id)

#select only transfer items
AddedValuesFull.long.Transfer <- AddedValuesFull.long %>% 
  filter(grepl("Transfer", AddedValuesFull.long$Condition)) %>%
  na.omit()

#Barplot that shows the means of all variables with "Transfer
BarMeanTransfer <- ggplot(data = AddedValuesFull.long.Transfer,  
       mapping = aes(
         x = Condition,
         y = Likert,
         fill = id
       )) +
  geom_bar(stat = "summary", fun.y = "mean")

#Boxplot that shows the means of all variables with "Transfer
BoxMeanTransfer <- ggplot(data = AddedValuesFull.long.Transfer,  
                          mapping = aes(
                            x = Condition,
                            y = Likert,
                            fill = id
                          )) +
  geom_boxplot()




# testing Hypothesis ------------------------------------------------------

# Hypothesis 1 ============================================================

#filtering all the rows of our gathered dataframe that include "Trust"
AddedValuesFull.long.Trust <- AddedValuesFull.long %>%
  filter(grepl("Trust", AddedValuesFull.long$Condition)) %>%
  na.omit()

#calculate Range
AddedValuesFull.long.Trust.Range <- range(AddedValuesFull.long.Trust$Likert)

#visually check the distribution of the trustcondition -> not normally distributed
DistCheckTrust.Bar <- ggplot(data = AddedValuesFull.long.Trust, 
       aes(
         x = Likert,
         fill = id
       )) +
  geom_bar()+
  facet_wrap(~Condition)

#visually check the differences between the Frames using a Boxplot
DistCheckTrust.Box <- ggplot(data = AddedValuesFull.long.Trust, 
                         aes(
                           x = Condition,
                           y = Likert,
                           fill = id
                            )) +
  geom_boxplot()

#Mann Whitney U Test: Checking the significant differences between the Positive and the Negative Frames of "Trust"
PastGameTrust.p <- wilcox.test(AddedValuesFull$PastGame1_PosFrameTrust, AddedValuesFull$PastGame1_NegFrameTrust, paired = FALSE) #p-value = 0.2496 
PersTraitTrust.p <- wilcox.test(AddedValuesFull$PersTrait1_PosFrameTrust, AddedValuesFull$PersTrait1_NegFrameTrust, paired = FALSE) #p-value < 2.2e-16
PromiseTrust.p <- wilcox.test(AddedValuesFull$Promise1_PosFrameTrust, AddedValuesFull$Promise1_NegFrameTrust, paired = FALSE) #p-value = 0.4403

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
