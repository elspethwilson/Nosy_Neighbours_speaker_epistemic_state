library(lme4)
require(optimx)
source(system.file("utils", "allFit.R", package = "lme4"))

# full model with main effects

# set contrast coding 
d$agegroup <- as.factor(d$agegroup)
d$Item_no <- as.factor(d$Item_no)

contrasts(d$Ignorant) = contr.sum(2)
contrasts(d$agegroup) = contr.sum(2)
contrasts(d$Condition) = contr.sum(4)
contrasts(d$Scale) = contr.sum(2)

#create subset with only C and F1 conditions
d_C_F1 <- subset(d, c(d$Condition=="C" | d$Condition=="F1"))
d_C_F1$Condition <- factor(d_C_F1$Condition)
contrasts(d_C_F1$Condition) <- contr.sum(2)
d_C_F1$agegroup <- as.factor(d_C_F1$agegroup)
contrasts(d_C_F1$agegroup) <- contr.sum(2)
contrasts(d_C_F1$Ignorant) <- contr.sum(2)
contrasts(d_C_F1$Scale) <- contr.sum(2)

# Speaker epistemic state, condition, and agegroup for just critical and first control conditions
# both random slopes 
model_C_F1_slopes <- glmer(Response ~ Ignorant * Condition * agegroup + (1+Condition|ID)+ (1+Condition|Item_no), family = "binomial", optimizer = "bobyqa", d_C_F1)
model_C_F1_slopes_all <- allFit(model_C_F1_slopes)
summary(model_C_F1_slopes_all$bobyqa)
C_F1_slopes_model <- capture.output(summary(model_C_F1_slopes_all$bobyqa))
write(C_F1_slopes_model, "C_F1_slopes_model.txt")

# with scale as well 
model_C_F1_slope_scale <- glmer(Response ~ Ignorant * Condition * agegroup * Scale + (1+Condition|ID), family = "binomial", optimizer = "bobyqa", d_C_F1)
model_C_F1_slope_scale_all <- allFit(model_C_F1_slope_scale)
summary(model_C_F1_slope_scale_all)
summary(model_C_F1_slope_scale_all$bobyqa)
C_F1_model_scale <- capture.output(summary(model_C_F1_slope_scale_all$bobyqa))
write(C_F1_model_scale, "C_F1_model_scale.txt")


#create subset with only C and F1 conditions with dummy coding
d_dummy <- d

contrasts(d_dummy$Ignorant) = contr.treatment(2)
contrasts(d_dummy$agegroup) = contr.treatment(2)
contrasts(d_dummy$Condition) = contr.treatment(4)
contrasts(d_dummy$Scale) = contr.treatment(2)

d_dummy_C_F1 <- subset(d_dummy, c(d$Condition=="C" | d$Condition=="F1"))
d_dummy_C_F1$Condition <- factor(d_C_F1$Condition)
contrasts(d_C_F1$Condition) <- contr.treatment(2)

model_dummy_C_F1_slopes <- glmer(Response ~ Ignorant * Condition * agegroup+ (1+ Condition | Item_no) + (1+Condition|ID), family = "binomial", optimizer = "bobyqa", d_dummy_C_F1)
model_dummy_C_F1_slopes_all <- allFit(model_dummy_C_F1_slopes)
summary(model_dummy_C_F1_slopes_all$bobyqa)
C_F1_dummy_model_slopes <- capture.output(summary(model_dummy_C_F1_slopes_all$bobyqa))
write(C_F1_dummy_model_slopes, "C_F1_dummy_slopes.txt")

model_dummy_C_F1_slope_scale <- glmer(Response ~ Ignorant * Condition * agegroup * Scale + (1+Condition|ID), family = "binomial", optimizer = "bobyqa", d_dummy_C_F1)
model_dummy_C_F1_slope_scale_all <- allFit(model_dummy_C_F1_slope_scale)
summary(model_dummy_C_F1_slope_scale_all)
summary(model_dummy_C_F1_slope_scale_all$bobyqa)
C_F1_scale_dummy_model <- capture.output(summary(model_dummy_C_F1_slope_scale_all$bobyqa))
write(C_F1_scale_dummy_model, "C_F1_scale_dummy_model.txt")


# remove knowledge/ignorance to see whether overall effect of scale # contrast coding
model_C_F1_slope_noIg <- glmer(Response ~  Condition * agegroup * Scale + (1+Condition|ID), family = "binomial", optimizer = "bobyqa", d_C_F1)
model_C_F1_slope_noIg_all <- allFit(model_C_F1_slope_noIg)
summary(model_C_F1_slope_noIg_all)
summary(model_C_F1_slope_noIg_all$bobyqa)
effect_scale <- capture.output(summary(model_C_F1_slope_noIg_all$bobyqa))
write(effect_scale, "effect_scale.txt")

# dummy coding 
model_C_F1_dummy_slope_noIg <- glmer(Response ~  Condition * agegroup * Scale + (1+Condition|ID), family = "binomial", optimizer = "bobyqa", d_dummy_C_F1)
model_C_F1_dummy_slope_noIg_all <- allFit(model_C_F1_dummy_slope_noIg)
summary(model_C_F1_dummy_slope_noIg_all)
summary(model_C_F1_dummy_slope_noIg_all$bobyqa)
effect_scale_dum <- capture.output(summary(model_C_F1_dummy_slope_noIg_all$bobyqa))
write(effect_scale_dum, "effect_scale_dum.txt")

# those who score at ceiling in F2 and F3
#model_ceiling <- glm(Response ~ Ignorant * Condition, family = "gaussian", adult_ceilinglong)
#summary(model_ceiling)

# contrasts
adult_ceilinglong_bi$Item_no <- as.factor(adult_ceilinglong_bi$Item_no)
contrasts(adult_ceilinglong_bi$Ignorant.y) = contr.sum(2)
contrasts(adult_ceilinglong_bi$Condition.y) = contr.sum(2)
contrasts(adult_ceilinglong_bi$Scale) = contr.sum(2)

# model
model_adult_ceiling <- glmer(Response.y ~ Ignorant.y * Condition.y + (1+Condition.y|ID)+ (1+Condition.y|Item_no), family = "binomial", optimizer = "bobyqa", adult_ceilinglong_bi)
model_adult_ceiling_all <- allFit(model_adult_ceiling)
summary(model_adult_ceiling_all$bobyqa)
model_adult_ceiling_C_F1 <- capture.output(summary(model_adult_ceiling_all$bobyqa))
write(model_adult_ceiling_C_F1, "model_adult_ceiling.txt")

# add Item_no as predictor and compare
#model_C_F1_slopes <- glmer(Response ~ Ignorant * Condition * agegroup + (1+Condition|ID)+ (1+Condition|Item_no), family = "binomial", optimizer = "bobyqa", d_C_F1)

Item_slopes <- glmer(Response ~ Ignorant * Condition * agegroup + Item_no + (1+Condition|ID)+ (1+Condition|Item_no), family = "binomial", optimizer = "bobyqa", d_C_F1)
Item_slopes_all <- allFit(Item_slopes)
summary(Item_slopes_all$bobyqa)

items_comp <- capture.output(anova(model_C_F1_slopes_all$bobyqa, Item_slopes_all$bobyqa))
write(items_comp, "items_comp.txt")
