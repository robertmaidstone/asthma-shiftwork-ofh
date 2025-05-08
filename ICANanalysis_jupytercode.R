
#define variables
data <- data_m

data$Sex <- data$demog_sex_2_1
data$Asthma <-  grepl("Asthma",data$diag_resp_1_m)
data$Lung_med <- grepl("Lung or breathing problems",data$medicat_1_m)
data$Asthma2 <- data$Asthma & data$Lung_med
data$Shift_work <-  ifelse(data$work_shifts_1_1=="Never/rarely",
                           "No shift work",
                           data$work_nights_1_1)
data$Shift_work <-  ifelse(data$Shift_work=="Usually",
                           "Sometimes",
                           data$Shift_work)
data$Shift_work <-  ifelse(data$Shift_work=="",
                           NA,
                           data$Shift_work)
data$Shift_work <- factor(data$Shift_work,levels=c("No shift work","Never/rarely","Sometimes","Always","Do not know","Prefer not to answer"))
data$Submission_year <- (data$submission_date |> strsplit("-") |> unlist())[(1:length(data$submission_date))*3-2]
data$Age <- as.numeric(data$Submission_year)-as.numeric(data$birth_year) #change to be based on months/days as well as year??

#missing data

data <- data[(!is.na(data$Asthma2))|(!is.na(data$Shift_work))|(!is.na(data$Age)),]
data <- data[!(data$Shift_work %in% c("Prefer not to answer","Do not know")),]

###

data <- data_a[data_a$Sex=="Female",]
data$gyn_contracept_pill_first_age_1_1 <-  ifelse(data$gyn_contracept_pill_first_age_1_1%in%c("","Prefer not to answer","Do not know"),
                                                  NA,
                                                  data$gyn_contracept_pill_first_age_1_1)
data$gyn_contracept_pill_last_age_1_1 <-  ifelse(data$gyn_contracept_pill_last_age_1_1%in%c("","Prefer not to answer","Do not know"),
                                                 NA,
                                                 data$gyn_contracept_pill_last_age_1_1)
data$gyn_contracept_pill_last_age_1_1 <-  ifelse(data$gyn_contracept_pill_last_age_1_1%in%c("Still taking the pill"),
                                                 data$Age,
                                                 data$gyn_contracept_pill_last_age_1_1)
data$timeonpill <- as.numeric(data$gyn_contracept_pill_last_age_1_1) - as.numeric(data$gyn_contracept_pill_first_age_1_1)
data <- data[!is.na(data$timeonpill),]

###


glm(data=data[(data$timeonpill<5),],formula="Asthma2 ~ Shift_work + Age",family = binomial(link="logit")) -> mod1
exp(cbind(coef(mod1)[2:4],confint.default(mod1,2:4))) -> OR
OR <- round(OR,2)
paste0(OR[,1]," (", OR[,2],"-",OR[,3],")") -> q1_a

glm(data=data[(data$timeonpill>=5)&(data$timeonpill<10),],formula="Asthma2 ~ Shift_work + Age",family = binomial(link="logit")) -> mod1
exp(cbind(coef(mod1)[2:4],confint.default(mod1,2:4))) -> OR
OR <- round(OR,2)
paste0(OR[,1]," (", OR[,2],"-",OR[,3],")") -> q2_a

glm(data=data[(data$timeonpill>=10)&(data$timeonpill<15),],formula="Asthma2 ~ Shift_work + Age",family = binomial(link="logit")) -> mod1
exp(cbind(coef(mod1)[2:4],confint.default(mod1,2:4))) -> OR
OR <- round(OR,2)
paste0(OR[,1]," (", OR[,2],"-",OR[,3],")") -> q3_a

glm(data=data[(data$timeonpill>=15)&(data$timeonpill<20),],formula="Asthma2 ~ Shift_work + Age",family = binomial(link="logit")) -> mod1
exp(cbind(coef(mod1)[2:4],confint.default(mod1,2:4))) -> OR
OR <- round(OR,2)
paste0(OR[,1]," (", OR[,2],"-",OR[,3],")") -> q4_a

glm(data=data[(data$timeonpill>=20),],formula="Asthma2 ~ Shift_work + Age",family = binomial(link="logit")) -> mod1
exp(cbind(coef(mod1)[2:4],confint.default(mod1,2:4))) -> OR
OR <- round(OR,2)
paste0(OR[,1]," (", OR[,2],"-",OR[,3],")") -> q5_a

rbind(q1_a,q2_a,q3_a,q4_a,q5_a)