library(tidyverse)

read.csv("~/OurFutureHealth/OFH_AsthmaShiftwork/data/OddsRatios_newBTSV2.csv") -> plot_data

plot_data %>% head

plot_data_sex <- filter(plot_data,X.1%in%c("Male","Female"))
plot_data_sex %>% head

plot_data_sex %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,c("Day workers","Male","1",NA,NA)) %>%
  rbind(.,c("Day workers","Female","1",NA,NA)) %>%
  dplyr::select(`Shift work`=X,Sex=X.1,LCI=X2.50.,UCI=X97.50.,OR=X.2) %>%
  mutate(`Shift work`=factor(`Shift work`,levels=c("Day workers","Shift_workNever/rarely","Shift_workSometimes","Shift_workAlways"),
                                         labels=c("Day workers (referent)","Shift work, but never or\nrarely night shifts",
                                                  "Irregular shift work\nincluding nights",
                                                  "Permanent night shift\nwork"))) %>%
  mutate(Sex=factor(Sex,levels=c("Female","Male"),labels=c("Female","Male"))) %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) -> plot_data_sex

cbPalette <- c("red","black","red")
pd_width <- 0.6

plot_data_sex %>%
  ggplot(aes(y=OR,x=`Shift work`,colour=Sex)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.25),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.6,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE))-> p_sex

ggsave(plot = p_sex, filename="OurFutureHealth/OFH_AsthmaShiftwork/plots/BTS_V2.png",width=5,height=4)

# Create the data frame
age_data <- data.frame(
  AgeGroup = c("18–29", "30–44", "45–59", "60+","18–29", "30–44", "45–59", "60+"),
  Count = c(27176, 112365, 133287, 62617,48784,157904,177407,61067),
  Sex = c("Male","Male","Male","Male","Female","Female","Female","Female")
)

library(scales)

# Create the bar chart
ggplot(age_data, aes(x = AgeGroup, y = Count,fill=Sex)) +
  geom_bar(stat = "identity",position="dodge",alpha=.5,colour="black") +
  scale_y_continuous(labels = label_comma()) +
  labs(
       y = "Count") +
  scale_fill_manual(values=c("red","black"))+
  theme_bw() -> p_bar

ggsave(plot = p_bar, filename="OurFutureHealth/OFH_AsthmaShiftwork/plots/BTS_bar_V2.png",width=5,height=4)

#read.csv("~/OurFutureHealth/OFH_AsthmaShiftwork/data/OddsRatios.csv") -> plot_data

plot_data_age1 <- filter(plot_data,X.1%in%c("AFemale","AMale"))
plot_data_age1 %>% head

day<-as.data.frame(cbind(cbind("A18-29",plot_data_age1$X.1 %>% unique),"1",NA,NA))
names(day)<- names(plot_data_age1)[1:5]

plot_data_age1 %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,day) %>%
  dplyr::select(`Age Range`=X,Sex=X.1,LCI=X2.50.,UCI=X97.50.,OR=X.2) %>%
  mutate(`Age Range`=factor(`Age Range`,levels=c("A18-29","A30-44","A45-59","A60-94"),
                             labels=c("18-29 (referent)","30-44",
                                      "45-59",
                                      "60+"))) %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(Sex=factor(Sex,levels=c("AFemale","AMale"),labels=c("Female","Male")))-> plot_data_age1

plot_data_age1 %>%
  ggplot(aes(y=OR,x=`Age Range`,colour=Sex)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.15, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.6,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) -> p_age1

ggsave(plot = p_age1, filename="OurFutureHealth/OFH_AsthmaShiftwork/plots/BTS_age_V2.png",width=5,height=4)

#####
plot_data_age <- filter(plot_data,X.1%in%c("Age q1 female","Age q2 female","Age q3 female","Age q4 female","Age q5 female",
                                           "Age q1 male","Age q2 male","Age q3 male","Age q4 male","Age q5 male"))
plot_data_age %>% head

day<-as.data.frame(cbind(cbind("Day workers",plot_data_age$X.1 %>% unique),"1",NA,NA))
names(day)<- names(plot_data_age)[1:5]

plot_data_age %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,day) %>%
  separate(X.1,into=c(NA,"Quantile","Sex"),sep = " ") %>%
  dplyr::select(`Shift work`=X,Quantile,Sex,LCI=X2.50.,UCI=X97.50.,OR=X.2) %>%
  mutate(`Shift work`=factor(`Shift work`,levels=c("Day workers","Shift_workNever/rarely","Shift_workSometimes","Shift_workAlways"),
                             labels=c("Day workers (referent)","Shift work, but never or\nrarely night shifts",
                                      "Irregular shift work\nincluding nights",
                                      "Permanent night shift\nwork"))) %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(Sex=factor(Sex,levels=c("female","male"),labels=c("Female","Male")))%>%
  mutate(Quantile=factor(Quantile,levels=c("q1","q2","q3","q4"),labels=c("18-29","30-44","45-59","60+")))-> plot_data_age

plot_data_age %>%
  ggplot(aes(y=OR,x=`Shift work`,colour=Sex)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.95, 0.2),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.5,2.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  facet_grid(~Quantile)-> p_age

ggsave(plot = p_age, filename="OurFutureHealth/OFH_AsthmaShiftwork/plots/BTS_agestrat_V2.png",width=10,height=4)


#####

#plot_data_compill <- filter(plot_data,X.1%in%c("Compill True","Compill False","No contraception"))
plot_data_compill <- filter(plot_data,X.1%in%c("Compill True","No contraception"))
plot_data_compill %>% head

day<-as.data.frame(cbind(cbind("Day workers",plot_data_compill$X.1 %>% unique),"1",NA,NA))
names(day)<- names(plot_data_compill)[1:5]

plot_data_compill %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,day) %>%
  dplyr::select(`Shift work`=X,Group=X.1,LCI=X2.50.,UCI=X97.50.,OR=X.2) %>%
  mutate(`Shift work`=factor(`Shift work`,levels=c("Day workers","Shift_workNever/rarely","Shift_workSometimes","Shift_workAlways"),
                             labels=c("Day workers (referent)","Shift work, but never or\nrarely night shifts",
                                      "Irregular shift work\nincluding nights",
                                      "Permanent night shift\nwork"))) %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(Group=factor(Group,levels=c("No contraception","Compill True"),labels = c("No contraception","Combined Pill"))) -> plot_data_compill

plot_data_compill %>%
  ggplot(aes(y=OR,x=`Shift work`,colour=Group)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.15),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.6,2)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) -> p_compill

ggsave(plot = p_compill, filename="OurFutureHealth/OFH_AsthmaShiftwork/plots/BTS_comppill_V2.png",width=5,height=4)

###

plot_data_compill <- filter(plot_data,X.1%in%c("contra"))
plot_data_compill %>% head

day<-as.data.frame(cbind(cbind("No contraception",plot_data_compill$X.1 %>% unique),"1",NA,NA))
names(day)<- names(plot_data_compill)[1:5]

plot_data_compill %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,day) %>%
  dplyr::select(`Shift work`=X,Group=X.1,LCI=X2.50.,UCI=X97.50.,OR=X.2) %>%
  mutate(`Shift work`=factor(`Shift work`,levels=c("No contraception","Compill","Injection","IUD","IUS","ProgPill","Patch","Ring","Other"),
                             labels=c("No contraception (referent)","Combined Pill",
                                      "Injection","IUD (coil)","IUS (hormonal coil)","Progesterone only pill","Patch","Vaginal Ring","Other not listed"))) %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR))  -> plot_data_compill

plot_data_compill %>%
  ggplot(aes(y=OR,x=`Shift work`)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.5,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data_compill$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) -> p_cont

ggsave(plot = p_cont, filename="OurFutureHealth/OFH_AsthmaShiftwork/plots/BTS_cont_V2.png",width=5,height=4)


#####

plot_data_men <- filter(plot_data,X.1%in%c("unsure","post","pre"))
plot_data_men %>% head

day<-as.data.frame(cbind(cbind("Day workers",plot_data_men$X.1 %>% unique),"1",NA,NA))
names(day)<- names(plot_data_men)[1:5]

plot_data_men %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,day) %>%
  dplyr::select(`Shift work`=X,Group=X.1,LCI=X2.50.,UCI=X97.50.,OR=X.2) %>%
  mutate(`Shift work`=factor(`Shift work`,levels=c("Day workers","Shift_work"),
                             labels=c("Day workers (referent)","Shift workers"))) %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(Group=factor(Group,levels=c("pre","post","unsure"),labels = c("Premenopausal","Postmenopausal","Unsure - Hysterectomy"))) -> plot_data_men

plot_data_men %>%
  ggplot(aes(y=OR,x=`Shift work`,colour=Group)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.15),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.8,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"blue","black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) -> p_men

ggsave(plot = p_men, filename="OurFutureHealth/OFH_AsthmaShiftwork/plots/BTS_men1_V2.png",width=5,height=4)
#####

plot_data_men <- filter(plot_data,X.1%in%c("unsure2","post2","pre2"))
plot_data_men %>% head

day<-as.data.frame(cbind(cbind("Day workers",plot_data_men$X.1 %>% unique),"1",NA,NA))
names(day)<- names(plot_data_men)[1:5]

plot_data_men %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,day) %>%
  dplyr::select(`Shift work`=X,Group=X.1,LCI=X2.50.,UCI=X97.50.,OR=X.2) %>%
  mutate(`Shift work`=factor(`Shift work`,levels=c("Day workers","Shift_work"),
                             labels=c("Day workers (referent)","Shift workers"))) %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(Group=factor(Group,levels=c("pre2","post2","unsure2"),labels = c("Premenopausal","Postmenopausal","Unsure - Hysterectomy"))) -> plot_data_men

plot_data_men %>%
  filter(Group!="Unsure - Hysterectomy") %>%
  ggplot(aes(y=OR,x=`Shift work`,colour=Group)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.15),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.8,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"blue","black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) -> p_men

ggsave(plot = p_men, filename="OurFutureHealth/OFH_AsthmaShiftwork/plots/BTS_men2_V2.png",width=5,height=4)
