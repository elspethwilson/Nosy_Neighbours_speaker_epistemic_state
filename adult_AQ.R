library(ggplot2)
library(boot)

adult_participant_Cond <-  aggregate(Response ~ Condition + Ignorant +AQ + ID, FUN = mean, data = adult_participant)

AQ_IG <- ggplot(subset(adult_participant_Cond, adult_participant_Cond$Condition=="C"), aes(Response, AQ, colour = Ignorant))
AQ_IG + geom_point() + facet_grid(~Ignorant) + geom_smooth(method = "glm")+
  ggtitle("Correct response in critical condition by AQ")

AQ_IG + geom_point() + scale_colour_discrete(guide="none")+ scale_fill_discrete(name="Speaker\nknowledge", labels=c("Knowledgable", "Ignorant")) +
  geom_smooth(method = "glm", aes(fill=Ignorant))+
  ggtitle("Correct response \nin critical condition by AQ") + ggsave("AQ_C_byIg.png")

AQ_IG_Scale <- ggplot(subset(adult_participant, adult_participant_Cond$Condition=="C"), aes(Response, AQ, colour = Ignorant))
AQ_IG_Scale + geom_point() + facet_grid(Scale~Ignorant) + geom_smooth(method = "glm") +
  scale_colour_discrete(name="Speaker\nknowledge", labels=c("Knowledgable", "Ignorant")) +
  ggtitle("Correct response in critical condition by AQ") + ggsave("IG_AQ_scale.png", width=7, height = 7)


IG_AQ <- ggplot(subset(adult_participant_Cond, adult_participant_Cond$Condition=="C"), aes( AQ, Response, colour = Ignorant))
IG_AQ + geom_point() +  scale_color_discrete(guide="none") + scale_fill_discrete(name="Speaker\nknowledge", labels=c("Knowledgable", "Ignorant")) + 
  geom_smooth(method = "glm", aes(fill=Ignorant))+
  ggtitle("Correct response in critical condition by AQ") + ggsave("IG_AQ.png", width=7, height = 7)

# all conditions
# responses too skewed to see any pattern with AQ 
AQ_IG <- ggplot(adult_participant_Cond, aes(Response, AQ, colour = Ignorant))
AQ_IG + geom_point() +  scale_color_discrete(guide="none") + scale_fill_discrete(name="Speaker\nknowledge", labels=c("Knowledgable", "Ignorant")) + 
  geom_smooth(method = "glm", aes(fill=Ignorant))+ facet_grid(~Condition) +
  ggtitle("Correct response in critical condition by AQ")

# analysis 

#AQ_IG <- glmer(Response ~ Ignorant * Condition * AQ + (1+Condition|Participant), family = "binomial", optimizer="bobyqa", control = glmerControl(optCtrl = list(maxfun = 200000)), data = adult_d)
#AQ_IF_all <- allFit(AQ_IG)
#summary(AQ_IF_all)
##fails to converge 
# AQ_IG <- glmer(Response ~ Ignorant * Condition * AQ + (1+Condition|Participant), family = "binomial", optimizer="bobyqa", control = glmerControl(optCtrl = list(maxfun = 200000)), data = subset(d_C_F1, d_C_F1$agegroup=="adult"))
# AQ_IG_all <- allFit(AQ_IG)
# summary(AQ_IG_all)
# summary(AQ_IG_all$bobyqa)
#AQ_IG_all_bob <- AQ_IG_all$bobyqa

# compare without AQ 
# AQ0_IG <- glmer(Response ~ Ignorant * Condition + (1+Condition|Participant), family = "binomial", optimizer="bobyqa", control = glmerControl(optCtrl = list(maxfun = 200000)), data = subset(d_C_F1, d_C_F1$agegroup=="adult"))
# AQ0_IG_all <- allFit(AQ0_IG)
# AQ0_IG_all_bob <- AQ0_IG_all$bobyqa
# 
# anova(AQ_IG_all_bob, AQ0_IG_all_bob)

# correlation 
# less = negative correlation (expect higher Response with lower AQ)

#knowledge
cor.test((subset(adult_participant_Cond, adult_participant_Cond$Condition=="C" & adult_participant_Cond$Ignorant==0)$Response), (subset(adult_participant_Cond, adult_participant_Cond$Condition=="C"& adult_participant_Cond$Ignorant==0)$AQ), alternative = "less", method = "kendall")


cor.test((subset(adult_participant_Cond, adult_participant_Cond$Condition=="C" & adult_participant_Cond$Ignorant==0)$Response), (subset(adult_participant_Cond, adult_participant_Cond$Condition=="C"& adult_participant_Cond$Ignorant==0)$AQ), alternative = "two.sided", method = "kendall")
#cor.test((subset(adult_participant_Cond, adult_participant_Cond$Condition=="C" & adult_participant_Cond$Ignorant==0)$AQ), (subset(adult_participant_Cond, adult_participant_Cond$Condition=="C"& adult_participant_Cond$Ignorant==0)$Response), alternative = "greater", method = "kendall")
# small but significant positive correlation - although this was not our hypothesis

# bootstrap this for knowledge
adult_participant_Cond_sub <- subset(adult_participant_Cond, adult_participant_Cond$Condition=="C" & adult_participant_Cond$Ignorant==0)
bootTau <- function(adult_participant_Cond_sub, i)cor(adult_participant_Cond_sub$Response[i], adult_participant_Cond_sub$AQ[i], use = "complete.obs", method = "kendall")
boot_Kendall_Kn_AQ <- boot(adult_participant_Cond_sub, bootTau, 2000)
boot_Kendall_Kn_AQ
boot.ci(boot_Kendall_Kn_AQ)
# the bootstrapped CIS unfortunately do cross 0, which means that the original conclusion might not stand

#ignorance
cor.test((subset(adult_participant_Cond, adult_participant_Cond$Condition=="C" & adult_participant_Cond$Ignorant==1)$Response), (subset(adult_participant_Cond, adult_participant_Cond$Condition=="C"& adult_participant_Cond$Ignorant==1)$AQ), alternative = "less", method = "kendall")
cor.test((subset(adult_participant_Cond, adult_participant_Cond$Condition=="C" & adult_participant_Cond$Ignorant==1)$Response), (subset(adult_participant_Cond, adult_participant_Cond$Condition=="C"& adult_participant_Cond$Ignorant==1)$AQ), alternative = "two.sided", method = "kendall")


# AQ Check scores

# d_adults_AQ_check <- d_adults
# AQ_scores <- read.csv("ASQ_CHECK.csv")
# colnames(AQ_scores)[1] <- "ID"
# 
# d_adults_ASQ_check <- merge(d_adults_AQ_check, AQ_scores, by = "ID")
