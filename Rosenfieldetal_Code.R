# Load Experimental Trial data
# compare females’ look-time at masculine vs. feminine stimuli
# calculate z-score
# calculate effect size

Rhesus.Trials <- as.data.frame(read.csv("Experimental_Trial_Data.csv"))

Rhesus.Trials$Masc_time <- Rhesus.Trials$Masc_time / 29

Rhesus.Trials$Fem_time <- Rhesus.Trials$Fem_time / 29

Prediction1.1 <- wilcox.test(Rhesus.Trials$Masc_time, Rhesus.Trials$Fem_time, paired = TRUE); Prediction1.1

Zstat1.1<-qnorm(Prediction1.1$p.value/2); Zstat1.1
Effect_Size1.1 <- abs(Zstat1.1)/sqrt(20); Effect_Size1.1

# test for sig diff in # trials in which subjects looked longer at masculine vs. feminine stimulus

Prediction1.2 <- binom.test(64, 105, p = 0.5,
           alternative = c("two.sided"),
           conf.level = 0.95); Prediction1.2

Prediction1.3 <- glm(Longer_M1_F0.1 ~ Masc_diff,family=binomial(link='logit'),data=Rhesus.Trials)

summary(Prediction1.3)
  
Prediction1.3b <- glm(Longer_M1_F0.1 ~ Masc_diff + Masc_score,family=binomial(link='logit'),data=Rhesus.Trials)

summary(Prediction1.3b)

# Test  prediction 2.1 that the proportion of time spent looking at the more masculine face will be positively related to the difference in masculinity between the two faces presented  

fit <- plot(Masculine_look_Percent ~ Masc_diff, data = Rhesus.Trials)
abline(a=0.42602, b=0.29027, col = "blue")

Prediction2.1a <- lm(Masculine_look_Percent ~ Masc_diff, data = Rhesus.Trials); summary(Prediction2.1a)

Prediction2.1b <- lm(Masculine_look_Percent ~ Masc_diff + Masc_score, data = Rhesus.Trials); summary(Prediction2.1b)

# Subset dataset into trials with high and low masculinity differences between paired stimuli

High_Diff <- subset(Rhesus.Trials, MD_High_1_or_Low_0 >0)
Low_Diff <- subset(Rhesus.Trials, MD_High_1_or_Low_0 <1)

# Repeat test of prediction 1 in high differences and low differences groups seperately
# calculate z-scores
# calculate effect sizes

Prediction2.2a <- wilcox.test(High_Diff$Masc_time, High_Diff$Fem_time, paired = TRUE); Prediction2.2a

Zstat2.2a<-qnorm(Prediction2.2a$p.value/2); Zstat2.2a
Effect_Size2.2a <- abs(Zstat2.2a)/sqrt(20); Effect_Size2.2a

Prediction2.2b <- wilcox.test(Low_Diff$Masc_time, Low_Diff$Fem_time, paired = TRUE); Prediction2.2b

Zstat2.2b<-qnorm(Prediction2.2b$p.value/2); Zstat2.2b
Effect_Size2.2b <- abs(Zstat2.2b)/sqrt(20); Effect_Size2.2b

# Subset into six dataframes: trials in which subjects are groupmates with masculine stimulus, NOT masc stim, fem stim, NOT fem stim, both stim, or neither stim
#Check for confound of facial masculinity on look-time: co-groupmembership

No_Groupmate <- subset(Rhesus.Trials, Groupmate_Both_3_Masc_2_Fem_1_None_0 == 0, select = "Masculine_look_Percent")
Fem_Groupmate <- subset(subset(Rhesus.Trials, Groupmate_Both_3_Masc_2_Fem_1_None_0 != 0), Groupmate_Both_3_Masc_2_Fem_1_None_0 != 2, select = "Masculine_look_Percent")
Masc_Groupmate <- subset(Rhesus.Trials, Groupmate_Both_3_Masc_2_Fem_1_None_0 > 1, select = "Masculine_look_Percent")
NoFem_Groupmate <- subset(subset(Rhesus.Trials, Groupmate_Both_3_Masc_2_Fem_1_None_0 != 1), Groupmate_Both_3_Masc_2_Fem_1_None_0 != 3, select = "Masculine_look_Percent")
Masc_Groupmate <- subset(Rhesus.Trials, Groupmate_Both_3_Masc_2_Fem_1_None_0 > 1, select = "Masculine_look_Percent")
NoMasc_Groupmate <- subset(subset(Rhesus.Trials, Groupmate_Both_3_Masc_2_Fem_1_None_0 != 2), Groupmate_Both_3_Masc_2_Fem_1_None_0 != 3, select = "Masculine_look_Percent")
Both_Groupmate <- subset(Rhesus.Trials, Groupmate_Both_3_Masc_2_Fem_1_None_0 == 3, select = "Masculine_look_Percent")

Masc_GroupMate <- wilcox.test(Masc_Groupmate$Masculine_look_Percent, NoMasc_Groupmate$Masculine_look_Percent, paired = FALSE); Masc_GroupMate

Fem_GroupMate <- wilcox.test(Fem_Groupmate$Masculine_look_Percent, NoFem_Groupmate$Masculine_look_Percent, paired = FALSE); Fem_GroupMate

Both_GroupMate <- wilcox.test(Both_Groupmate$Masculine_look_Percent, No_Groupmate$Masculine_look_Percent, paired = FALSE); Both_GroupMate

# Load Facial masculinity data
# Subset into three dataframes: images used as any stimuli, as masculine stimuli, and as feminine stimuli
#Check for confounds of facial masculinity: age, color, and luminance

Rhesus.Masculinity <- as.data.frame(read.csv("Facial_Masculinity_Data.csv"))

Rhesus.Masculinity$Color <- as.numeric(as.character(Rhesus.Masculinity$Color))
Rhesus.Masculinity$Luminance <- as.numeric(as.character(Rhesus.Masculinity$Luminance))

Stimuli <- subset(Rhesus.Masculinity, M2_F1_Not0 >0, select = c("Age","Color","Luminance","Masculinity_score"))
Masc_Stimuli <- subset(Rhesus.Masculinity, M2_F1_Not0 >1, select = c("Age","Color","Luminance","Masculinity_score"))
Fem_Stimuli <- subset(Rhesus.Masculinity, M2_F1_Not0 == 1, select = c("Age","Color","Luminance","Masculinity_score"))

Confound_Age <- cor.test(Stimuli$Masculinity_score, Stimuli$Age, method = "spearman"); Confound_Age

Confound_Age2 <- wilcox.test(Masc_Stimuli$Age, Fem_Stimui$Age, paired = FALSE); Confound_Age2

Confound_Color <- cor.test(Stimuli$Masculinity_score, Stimuli$Color, method = "spearman"); Confound_Color

Confound_Color2 <- wilcox.test(Masc_Stimuli$Color, Fem_Stimui$Color, paired = FALSE); Confound_Color2

Confound_Luminance <- cor.test(Stimuli$Masculinity_score, Stimuli$Luminance, method = "spearman"); Confound_Luminance

Confound_Luminance2 <- wilcox.test(Masc_Stimuli$Luminance, Fem_Stimui$Luminance, paired = FALSE); Confound_Luminance2