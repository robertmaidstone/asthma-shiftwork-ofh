library(tidyverse)

read.csv("~/OurFutureHealth/OFH_AsthmaShiftwork/data/OddsRatios_model2.csv") -> plot_data

plot_data %>% head

plot_data_sex <- filter(plot_data,X.1%in%c("Male","Female"))
plot_data_sex %>% head

plot_data_sex %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,c("Day workers","Male","1",NA,NA)) %>%
  rbind(.,c("Day workers","Female","1",NA,NA)) %>%
  dplyr::select(`Shift work`=X,Sex=X.1,LCI=X2.5..,UCI=X97.5..,OR=X.2) %>%
  mutate(`Shift work`=factor(`Shift work`,levels=c("Day workers","Shift_workNever/rarely","Shift_workSometimes","Shift_workAlways"),
                                         labels=c("Day workers (referent)","Shift work, but never or\nrarely night shifts",
                                                  "Irregular shift work\nincluding nights",
                                                  "Permanent night shift\nwork"))) %>%
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
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.6,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE))-> p_sex

#ggsave(plot = p_sex, filename="OurFutureHealth/OFH_AsthmaShiftwork/plots/BTS.png",width=5,height=4)

read.csv("~/OurFutureHealth/OFH_AsthmaShiftwork/data/OddsRatios.csv") -> plot_data

plot_data %>% head

plot_data_sex <- filter(plot_data,X.1%in%c("Male","Female"))
plot_data_sex %>% head

plot_data_sex %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,c("Day workers","Male","1",NA,NA)) %>%
  rbind(.,c("Day workers","Female","1",NA,NA)) %>%
  dplyr::select(`Shift work`=X,Sex=X.1,LCI=X2.5..,UCI=X97.5..,OR=X.2) %>%
  mutate(`Shift work`=factor(`Shift work`,levels=c("Day workers","Shift_workNever/rarely","Shift_workSometimes","Shift_workAlways"),
                             labels=c("Day workers (referent)","Shift work, but never or\nrarely night shifts",
                                      "Irregular shift work\nincluding nights",
                                      "Permanent night shift\nwork"))) %>%
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
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.6,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE))-> p_sex2


plot_data_age <- filter(plot_data,X.1%in%c("Age q1 female","Age q2 female","Age q3 female","Age q4 female","Age q5 female",
                                           "Age q1 male","Age q2 male","Age q3 male","Age q4 male","Age q5 male"))
plot_data_age %>% head

day<-as.data.frame(cbind(cbind("Day workers",plot_data_age$X.1 %>% unique),"1",NA,NA))
names(day)<- names(plot_data_age)[1:5]

plot_data_age %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,day) %>%
  separate(X.1,into=c(NA,"Quantile","Sex"),sep = " ") %>%
  dplyr::select(`Shift work`=X,Quantile,Sex,LCI=X2.5..,UCI=X97.5..,OR=X.2) %>%
  mutate(`Shift work`=factor(`Shift work`,levels=c("Day workers","Shift_workNever/rarely","Shift_workSometimes","Shift_workAlways"),
                             labels=c("Day workers (referent)","Shift work, but never or\nrarely night shifts",
                                      "Irregular shift work\nincluding nights",
                                      "Permanent night shift\nwork"))) %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) -> plot_data_age

plot_data_age %>%
  ggplot(aes(y=OR,x=`Shift work`,colour=Sex)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.3,2)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  facet_grid(~Quantile)-> p_age

#####

plot_data_compill <- filter(plot_data,X.1%in%c("Compill True","Compill False","No contraception"))
plot_data_compill %>% head

day<-as.data.frame(cbind(cbind("Day workers",plot_data_compill$X.1 %>% unique),"1",NA,NA))
names(day)<- names(plot_data_compill)[1:5]

plot_data_compill %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,day) %>%
  dplyr::select(`Shift work`=X,Group=X.1,LCI=X2.5..,UCI=X97.5..,OR=X.2) %>%
  mutate(`Shift work`=factor(`Shift work`,levels=c("Day workers","Shift_workNever/rarely","Shift_workSometimes","Shift_workAlways"),
                             labels=c("Day workers (referent)","Shift work, but never or\nrarely night shifts",
                                      "Irregular shift work\nincluding nights",
                                      "Permanent night shift\nwork"))) %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) -> plot_data_compill

plot_data_compill %>%
  ggplot(aes(y=OR,x=`Shift work`,colour=Group)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.8,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) -> p_compill

######

plot_data_timepill <- filter(plot_data,X.1%in%c("TimeonPill"))
plot_data_timepill %>% head

day<-as.data.frame(cbind(cbind("timeonpill_bin<5",plot_data_timepill$X.1 %>% unique),"1",NA,NA))
names(day)<- names(plot_data_timepill)[1:5]

plot_data_timepill %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,day) %>%
  dplyr::select(`Time on Pill`=X,Group=X.1,LCI=X2.5..,UCI=X97.5..,OR=X.2) %>%
  mutate(`Time on Pill`=factor(`Time on Pill`,levels=c("timeonpill_bin<5","timeonpill_bin5<x<10","timeonpill_bin10<x<15","timeonpill_bin15<x<20","timeonpill_bin>20"),
                             labels=c("< 5","5 < x < 10",
                                      "10 < x < 15",
                                      "15 < x < 20", "> 20"))) %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) -> plot_data_timepill

plot_data_timepill %>%
  ggplot(aes(y=OR,x=`Time on Pill`)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.8,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Time on Pill`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) -> p_timepill

####

plot_data_swtime <- filter(plot_data,X.1%in%c("TimeonPill q1","TimeonPill q2","TimeonPill q3","TimeonPill q4","TimeonPill q5"))
plot_data_swtime %>% head

day<-as.data.frame(cbind(cbind("Day workers",plot_data_swtime$X.1 %>% unique),"1",NA,NA))
names(day)<- names(plot_data_swtime)[1:5]

plot_data_swtime %>% filter(X!="(Intercept)") %>%
  .[,1:5] %>%
  rbind(.,day) %>%
  separate(X.1,into=c(NA,"Quantile"),sep = " ") %>%
  dplyr::select(`Shift work`=X,Quantile,LCI=X2.5..,UCI=X97.5..,OR=X.2) %>%
  mutate(`Shift work`=factor(`Shift work`,levels=c("Day workers","Shift_workNever/rarely","Shift_workSometimes","Shift_workAlways"),
                             labels=c("Day workers (referent)","Shift work, but never or\nrarely night shifts",
                                      "Irregular shift work\nincluding nights",
                                      "Permanent night shift\nwork"))) %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) -> plot_data_swtime

plot_data_swtime %>%
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
  ylim(c(.3,2)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  facet_grid(~Quantile)-> p_age

