install.packages("")
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(formattable)
library(MASS)
library(effsize)
library(stats)
library(broom)
library(afex)
library(rstatix)
library(egg)
library(DescTools)

# read --------------------------------------------------------------------
rm(list=ls())

setwd("D:\\Bibliotheken\\Dokumente\\R\\Projektseminar")

data <- read_excel("D:\\Bibliotheken\\Dokumente\\R\\Projektseminar\\Projsem.data_Excel_cleaned.xlsx") #Excel übernimmt auch ü, ä, usw.

head(data)

# converting variables ----------------------------------------------------

str(data) #check the data structure
glimpse(data) #dplyr aproach to check data structure

data$Gender <- as.factor(data$Gender)
data$Condition <- as.factor(data$Condition)
data$id <- as.numeric(data$Condition)


# cleaning invalid responses ----------------------------------------------

data <- data %>%
  rename(Duration = `Duration (in seconds)`) %>%
  filter(Duration < 3000) %>%
  filter(Duration > 120) %>%
  filter(Progress == 100) %>%
  mutate(ConsentConfirm = recode(
    ConsentConfirm,
    "Ich habe die aufgeführten Bedingungen gelesen und verstanden. Mit der Auswahl dieser Antwort bestätige ich mein Einverständnis zur Teilnahme an dieser Studie." = 2,
    "Nein, ich bin mit den Bedingungen nicht einverstanden oder habe mich aus anderen Gründen entschieden, nicht an der Studie teilzunehmen." = 1
  )) %>%
  mutate(id = recode(
    id,
    "2" = "Positive Frame",
    "1" = "Negative Frame"
  )) %>%
  filter(ConsentConfirm == 2)

view(data) 


# first look at the data --------------------------------------------------

occurrence_gender <- count(data, Gender) 
print(occurrence_gender) #23 male, 2 don't want to say, 77 female

occurrence_condition <- count(data, id) 
print(occurrence_condition) # posFrame: 54, negFrame: 49

summary(data$Age) # Min. 19 1st Qu.: 21  Median: 22    Mean 3rd Qu.: 22.72    Max.:47 
barplot_age <- ggplot(data = data,
                      mapping = aes(
                           x = Age
                         )) +
                  geom_bar(aes(color = Gender)) + #adds the Genders in color to the barplot
                  labs( #adds lables to the data
                    title = "Age by Gender",
                    subtitle = "From dataframe 'data'",
                    caption = "This is a graphic that ilustrates the Age by Gender" #metadescription
                    )         
print (barplot_age) #Age distribution in a histogram
#ggsave("AgeANDGender.jpeg") #save distribution to the setwd


duplicated_rows <- data %>% duplicated()%>%
                            table()
print(duplicated_rows) #no duplicated rows

data%>%
  summarise(
    Mean_age = mean(Age),
    SD_age = sd(Age),
    MAX_age = max(Age),
    MIN_age = min(Age)
  )

#Means of all the Items
ItemMeans <- tidy(
  data %>% 
    select(contains("Frame"))%>%
    colMeans(na.rm = TRUE, dims = 1)
  )

#Modes of all the Items
CondVars <- data%>%
  select(contains("Frame"))
sapply(CondVars, Mode, na.rm = TRUE)




# Check Demographics ------------------------------------------------------

## dataframe for positive and negative ===================================

negFrame_Data <- data%>%
  filter(id == "Positive Frame")

posFrame_Data <- data%>%
  filter(id == "Negative Frame")

#age======================================================================

#summary
data%>%
  group_by(id)%>%
  summarise(
    Mean_age = mean(Age),
    SD_age = sd(Age),
    MAX_age = max(Age),
    MIN_age = min(Age)
  )

#graphical
Agedistribution <- ggplot(data = data,
                          aes(
                            x = data$Age
                          ))+
  geom_bar(aes(color = data$Gender)) +
  facet_wrap(~id)

#test for significant differences in the two conditions: Since there is no normal distribution shown, we have to use the Mann Whitney U Test that is calculated with the wilcox function
wilcox.test(posFrame_Data$Age, negFrame_Data$Age, paired = FALSE) #there are no significant differences

#check for outliers
Outlier_box <- ggplot(data = data,
                      aes(
                        y = data$Age
                      )) +
  geom_boxplot() +
  facet_wrap(~data$id)

#Gender==================================================================

#draw up a table with the frequency of the Gender by Condition
tab_for_chisq<- table(data$id, data$Gender)

#test for significant differences between the two Conditions
chisq.test(tab_for_chisq) #no significant difference

#graphical
Gender_plot <- ggplot(data = data,
                      aes(
                        x = data$Gender
                      )) +
  geom_bar() +
  facet_wrap(~data$id)

#Trust in Stranger=======================================================

#summary
data%>%
  group_by(id)%>%
  summarise(
    Mean_TIS = mean(TrustInStranger),
    SD_TIS = sd(TrustInStranger),
    MAX_TIS = max(TrustInStranger),
    MIN_TIS = min(TrustInStranger)
  )

#graphical
Agedistribution <- ggplot(data = data,
                          aes(
                            x = data$TrustInStranger
                          ))+
  geom_bar(aes(color = data$Gender)) +
  facet_wrap(~id)

#are these normally distributed?
shapiro.test(posFrame_Data$TrustInStranger) #p-value = 0.006689 -> significant -> not normally distributed
shapiro.test(negFrame_Data$TrustInStranger) #p-value = 0.003557 -> significant -> not normally distributed

#therefore we use the Mann Whitney U Test
wilcox.test(negFrame_Data$TrustInStranger, posFrame_Data$TrustInStranger, paired = FALSE) # there are no significant differences

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
