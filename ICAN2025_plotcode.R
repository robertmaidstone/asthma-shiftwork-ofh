library(tidyverse)

readxl::read_xlsx("~/OurFutureHealth/OFH_AsthmaShiftwork/data/ORdataforICAN_asthma2.xlsx") -> plot_data

plot_data %>% head

plot_data %>% mutate(`Shift work`=factor(`Shift work`,levels=c("Day workers","Never/rarely","Sometimes","Always"),
                                         labels=c("Day workers (referent)","Shift work, but never or\nrarely night shifts",
                                                  "Irregular shift work\nincluding nights",
                                                  "Permanent night shift\nwork"))) -> plot_data

cbPalette <- c("red","black","red")
pd_width <- 0.6

plot_data %>%
  filter(AnalysisID==7) %>%
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

plot_data %>%
  filter(AnalysisID%in%c(3,4)) %>%
  ggplot(aes(y=OR,x=`Shift work`,colour=`Sex`)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position="none")+
  ylab("Odds Ratio") +
  ylim(c(.4,1.6)) +
  coord_flip()+
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  #scale_colour_manual(values = c(cbPalette[c(2,3,4,5)],"black"),name = element_blank(),guide = guide_legend(reverse=TRUE))
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  #ggtitle("Males")+
  facet_grid(~`Age range`) -> p_age



library(patchwork)
p_sex + p_age


plot_data %>%
  filter(AnalysisID==8) %>%
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
    name = element_blank(),guide = guide_legend(reverse=TRUE))-> p_pill

plot_data %>%
  filter(AnalysisID%in%c(5)) %>%
  mutate(`Years on pill`=factor(`Years on pill`,levels = rev(c("0-7","4-12","12-18","18-63")))) %>%
  ggplot(aes(y=OR,x=`Shift work`,colour=`Years on pill`)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  #theme(axis.title.y = element_blank(),legend.position="none")+
  ylab("Odds Ratio") +
  #ylim(c(.4,1.6)) +
  coord_flip()+
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`))) -> p_timeonpill

plot_data %>%
  filter(AnalysisID%in%c(6)) %>%
  mutate(`Years on pill`=factor(`Years on pill`,levels = rev(c("0-5","5-10","10-15","15-20","20-63")))) %>%
  ggplot(aes(y=OR,x=`Shift work`,colour=`Years on pill`)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  #theme(axis.title.y = element_blank(),legend.position="none")+
  ylab("Odds Ratio") +
  #ylim(c(.4,1.6)) +
  coord_flip()+
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`))) -> p_timeonpill

###
