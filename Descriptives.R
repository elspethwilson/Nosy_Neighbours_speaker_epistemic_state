library(plyr)
library(ggplot2)
library(reshape2)

# data prep
# Import child data

child_d <- read.csv("kids_results.csv")
child_info <- read.csv("kids_info.csv")
adult_d <- read.csv("adults_results.csv")

# merge child results and info 
colnames(child_d)[1] <- "ID"
child <- merge.data.frame(child_d, child_info, by = "ID")

# Adult data - check headings
colnames(child)[c(4, 10, 11, 12, 14, 15)] <- c("Ignorant", "Item_no", "Item_order", "Answer", "Response", "Age")

colnames(adult_d)[6] <- "Item_no"
colnames(adult_d)[1] <- "Participant"

# add agegroup

child$agegroup <- "child"
adult_d$agegroup <- "adult"

colnames(child)
colnames(adult_d)

# merge child and adult

d_full  <- rbind.fill(child, adult_d)

# exclude participants 
d <- d_full[!d_full$ID=="IG01",]
d <- d[!d$ID=="IG11",]
d <- d[!d$ID=="IG17",]

# exclude adult participants - who did Ig in 'top up' but had already done Knowledge

d <- d[!(d$ID=="57274a95e386b9000f3eb600" & d$Ignorant==1),]
d <- d[!(d$ID=="574061ecd700ea000ef44a8f" & d$Ignorant==1),]

d$Ignorant <- as.factor(d$Ignorant)

d_adults <- subset(d, d$agegroup=="adult")
d_chil <- subset(d, d$agegroup=="child")

# descriptives by age and condition 

ms <- aggregate(Response ~ Ignorant + agegroup + Condition, data = d, FUN = mean)
ms

write.csv(ms, file = "NosyNeighbour_results_1.csv")

ms_scale <- aggregate(Response ~ Ignorant + agegroup + Condition + Scale, data = d, FUN = mean)
ms_scale

write.csv(ms_scale, file = "NosyNeighbour_results.csv")

# bar chart with bootstrapped 95% CIs 

ms_plot <- ggplot(data = d, aes (Condition, Response, fill = Ignorant))
ms_plot + stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2, position = position_dodge(width = .9)) +
  facet_wrap(~ agegroup) + ggtitle("Mean correct response by condition and agegroup") + ylab("Proportion correct response") + 
  scale_x_discrete(labels = c("weak/\nstrong", "strong/\nweak", "strong/\nstrong", "weak/\nweak")) + 
  xlab("Condition \n utterance/picture") + scale_fill_discrete(name="Speaker\n knowledge", labels=c("Knowledgable", "Ignorant")) +
  ggsave("Mean_condition.png", width = 8, height = 10)

ms_plot_line <- ggplot(data = d, aes (Condition, Response, colour = Ignorant))
ms_plot_line + stat_summary(fun.y = mean, geom = "line", aes(group=Ignorant)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2) +
  facet_wrap(~ agegroup) + ggtitle("Mean correct response by condition and agegroup") + ylab("Proportion correct response") + 
  scale_x_discrete(labels = c("weak/\nstrong", "strong/\nweak", "strong/\nstrong", "weak/\nweak")) + 
  xlab("Condition \n utterance/picture") + scale_colour_discrete(name="Speaker\n knowledge", labels=c("Knowledgable", "Ignorant")) +
  theme(text= element_text(size = 14)) +
  ggsave("Mean_condition_line.png", width = 8, height = 10)                                                        
                                                               
# ms_scale_plot <- ggplot(data = d, aes (Condition, Response, fill = Ignorant))
# ms_scale_plot + stat_summary(fun.y = mean, geom = "point", position = "dodge") +
#   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2, position = position_dodge(width = .9)) +
#   facet_wrap(Scale ~ agegroup) + ylab("Proportion correct response") + 
#   scale_x_discrete(labels = c("weak/\nstrong", "strong/\nweak", "strong/\nstrong", "weak/\nweak")) + 
#   xlab("Condition \n utterance/picture") + scale_fill_discrete(name="Speaker\n knowledge", labels=c("Knowledgable", "Ignorant")) + 
#   ggtitle("Mean correct response \nby condition, age, scale") + ggsave("Mean_condition_scale.png")

ms_scale_line <- ggplot(data = d, aes (Condition, Response, colour = Ignorant))
ms_scale_line + stat_summary(fun.y = mean, geom = "line", aes(group = Ignorant)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2) +
  facet_grid(agegroup ~ Scale) + ggtitle("Mean correct response by \ncondition, scale and agegroup") + ylab("Proportion correct response") + 
  scale_x_discrete(labels = c("weak/\nstrong", "strong/\nweak", "strong/\nstrong", "weak/\nweak")) + 
  xlab("Condition \n utterance/picture") + scale_colour_discrete(name="Speaker\n knowledge", labels=c("Knowledgable", "Ignorant")) +
  theme(text= element_text(size = 14), axis.text.x = element_text(size = 12)) +
  ggsave("Mean_Condition_scale.png", width = 10, height = 10)
  
ms_scale_line

# histograms # participants can score 0, .5 or 1 for Scale/Condition

# create labels for facet_wrap
condition_names <- c('C' = "weak/strong", 'F1' = "strong/weak",
                     'F2' = "strong/strong", 'F3' = "weak/weak")

adult_participant <- aggregate(Response ~ ID + Ignorant + Condition + Scale + AQ, data = d_adults, FUN = mean)
hist_adult_cond <- ggplot(data = adult_participant, aes (Response, fill = Ignorant))
hist_adult_cond + geom_histogram(position = "dodge", binwidth = .5) + scale_fill_brewer(name="Speaker\n knowledge", labels=c("Knowledgable", "Ignorant"), palette="Set2") +
  facet_wrap(Condition~Scale, ncol = 2, labeller = labeller(Condition = as_labeller(condition_names))) + ggtitle("Histogram of mean adult responses \nby condition & scale") + 
  ggsave("adult_histogram.png", height = 6, width = 4)

child_participant <- aggregate(Response ~ ID + Ignorant + Condition + Scale, data = subset(d, d$agegroup=="child"), FUN = mean)
hist_child_cond <- ggplot(data = child_participant, aes (Response, fill = Ignorant))
hist_child_cond + geom_histogram(position = "dodge", binwidth = .5) + scale_fill_brewer(name="Speaker\n knowledge", labels=c("Knowledgable", "Ignorant"), palette = "Set2") + 
  facet_wrap(Condition~Scale, ncol = 2, labeller = labeller(Condition = as_labeller(condition_names)))+ ggtitle("Histogram of mean child responses \nby condition & scale") + 
  ggsave("Child_histogram.png", height = 6, width = 4)


child_participant_cond <- aggregate(Response ~ ID + Ignorant + Condition, data = subset(d, d$agegroup=="child"), FUN = mean)
hist_child_cond2 <- ggplot(data = child_participant_cond, aes (Response, fill = Ignorant))
hist_child_cond2 + geom_histogram(position = "dodge", binwidth = .5) + scale_fill_discrete(name="Speaker\n knowledge", labels=c("Knowledgable", "Ignorant")) +
  facet_wrap(~Condition, ncol = 2)+ ggtitle("Histogram of mean child responses \nby condition ")

## subset only those adults who are at ceiling in F2 adn F3 

adult_ceiling <- dcast(adult_participant_Cond, ID + Ignorant + AQ ~ Condition, value =  "Response")
adult_ceiling <- subset(adult_ceiling, adult_ceiling$F2==1 & adult_ceiling$F3==1)
adult_ceiling$C_m_Ig <- mean(subset(adult_ceiling, adult_ceiling$Ignorant==1)$C) 
adult_ceiling$F1_m_Ig <- mean(subset(adult_ceiling, adult_ceiling$Ignorant==1)$F1)
adult_ceiling$C_m_Kn <- mean(subset(adult_ceiling, adult_ceiling$Ignorant==0)$C) 
adult_ceiling$F1_m_Kn <- mean(subset(adult_ceiling, adult_ceiling$Ignorant==0)$F1)

adult_ceiling_means <- adult_ceiling[,(8:11)]
adult_ceiling <- adult_ceiling[-c(8:11)]

adult_ceilingl <- melt(adult_ceiling, id =c("ID", "AQ", "Ignorant"), measured = c("C", "F1", "F2", "F3"))
colnames(adult_ceilingl) <- c("ID", "AQ", "Ignorant", "Condition", "Response")

adult_ceilinglong <- subset(adult_ceilingl, adult_ceilingl$Condition=="C" | adult_ceilingl$Condition=="F1")
adult_ceilinglong$Response <- as.numeric(adult_ceilinglong$Response)

adult_ceiling_plot <- ggplot(data = adult_ceilinglong, aes(Condition, Response, colour = Ignorant))  
adult_ceiling_plot + stat_summary(fun.y = mean, geom = "line", aes(group=Ignorant)) + ggtitle("Mean correct response by \ncondition, for control ceiling scorers") + ylab("Proportion correct response") + 
  scale_x_discrete(labels = c("weak/\nstrong", "strong/\nweak", "strong/\nstrong", "weak/\nweak")) + 
  xlab("Condition \n utterance/picture") + scale_colour_discrete(name="Speaker\n knowledge", labels=c("Knowledgable", "Ignorant")) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2) + ylim(0:1) + theme(text = element_text(size = 14), axis.text.x = element_text(size = 15)) +
  ggsave("Mean_Condition_ceiling.png", width = 10, height = 10)

adult_ceilinglong_bi <- merge(adult_ceilinglong, d_C_F1, by = "ID")


# item effects 

items <- ggplot(data = d, aes (Item_no, Response, fill = Ignorant))
items + stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  facet_wrap(~Condition)

items_AH <- ggplot(data = subset(d, d$Scale=="AH"), aes (Item_no, Response, fill = Ignorant))
items_AH + stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  facet_wrap(~Condition) + ggtitle("Mean response by item (Ad hocs)")+ 
  scale_fill_discrete(name="Speaker\nknowledge", labels=c("Knowledgable", "Ignorant")) +
  xlab("Item") + ggsave("Items_AH.png")

items_SI <- ggplot(data = subset(d, d$Scale=="SI"), aes (Item_no, Response, fill = Ignorant))
items_SI + stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  facet_wrap(~Condition)+ ggtitle("Mean response by item (Scalars)")+
  scale_fill_discrete(name="Speaker\nknowledge", labels=c("Knowledgable", "Ignorant")) +
  xlab("Item") + ggsave("Items_SI.png")

## adults
items_AH <- ggplot(data = subset(d_adults, d_adults$Scale=="AH"), aes (Item_no, Response, fill = Ignorant))
items_AH + stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  facet_wrap(~Condition) + ggtitle("Mean response by item (Ad hocs)")+ 
  scale_fill_discrete(name="Speaker\nknowledge", labels=c("Knowledgable", "Ignorant")) +
  xlab("Item") + ggsave("Items_AH_adults.png")

items_SI <- ggplot(data = subset(d_adults, d_adults$Scale=="SI"), aes (Item_no, Response, fill = Ignorant))
items_SI + stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  facet_wrap(~Condition)+ ggtitle("Mean response by item (Scalars)")+
  scale_fill_discrete(name="Speaker\nknowledge", labels=c("Knowledgable", "Ignorant")) +
  xlab("Item") + ggsave("Items_SI_adults.png")




