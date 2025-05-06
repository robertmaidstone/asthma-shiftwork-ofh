#draft code for OFH data analysis

#import data
#%%bash
#dx download input_data/reads.fastq

#summary stats

data$Asthma |> table(useNA="always")
data$Shift_work |> table(useNA="always")
data$Age |> summary()

#logistic regression

glm(data,"Asthma ~ Shift_work + Age",family = binomial(link="logit")) -> mod1

summary(mod1)
exp(cbind(coef(mod3)[2:3],confint.default(mod3,2:3)))