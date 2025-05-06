#draft code for OFH data analysis

#import data
#%%bash
#dx download input_data/reads.fastq

#define variables

data$Asthma <-  grepl(",1111,",nCI_code)
data$Shift_work <-  ifelse(Job_involves_shift_work_int=="Never/rarely",
                           "No shift work",
                           Job_involves_night_shift_work_int)
data$Age <- 


#summary stats

data$Asthma |> table(useNA="always")
data$Shift_work |> table(useNA="always")
data$Age |> summary()

#missing data

data <- data[,(is.na(data$Asthma))|(is.na(data$Shift_work))|(is.na(data$Age))]

#logistic regression

glm(data,"Asthma ~ Shift_work + Age",family = binomial(link="logit")) -> mod1

summary(mod1)
exp(cbind(coef(mod3)[2:3],confint.default(mod3,2:3)))