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
  filter(Sex=="All",`Age range`=="All") %>%
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
  ylim(c(.6,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE))


plot_data %>%
  filter(Sex=="All",`Age range`!="All") %>%
  ggplot(aes(y=OR,x=`Shift work`,group=`Age range`,colour=`Age range`)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.5,1.6)) +
  coord_flip()+
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  #scale_colour_manual(values = c(cbPalette[c(2,3,4,5)],"black"),name = element_blank(),guide = guide_legend(reverse=TRUE))
  scale_colour_discrete(name = element_blank(),guide = guide_legend(reverse=TRUE))-> p_all

plot_data %>%
  filter(Sex=="Male",`Age range`!="All") %>%
  ggplot(aes(y=OR,x=`Shift work`,group=`Age range`,colour=`Age range`)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.5,1.6)) +
  coord_flip()+
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  #scale_colour_manual(values = c(cbPalette[c(2,3,4,5)],"black"),name = element_blank(),guide = guide_legend(reverse=TRUE))
  scale_colour_discrete(name = element_blank(),guide = guide_legend(reverse=TRUE)) + ggtitle("Males")-> p_male

plot_data %>%
  filter(Sex=="Female",`Age range`!="All") %>%
  ggplot(aes(y=OR,x=`Shift work`,group=`Age range`,colour=`Age range`)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.5,1.6)) +
  coord_flip()+
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  #scale_colour_manual(values = c(cbPalette[c(2,3,4,5)],"black"),name = element_blank(),guide = guide_legend(reverse=TRUE))
  scale_colour_discrete(name = element_blank(),guide = guide_legend(reverse=TRUE))+ggtitle("Females")-> p_female

library(patchwork)
p_all + p_male + p_female
p_male + p_female

###

plot_data %>%
  filter(Sex=="Male",`Age range`!="All") %>%
  ggplot(aes(y=OR,group=`Shift work`,x=`Age range`,colour=`Shift work`)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.5,1.6)) +
  coord_flip()+
  scale_x_discrete(limits = rev(levels(plot_data$`Age range`)))+
  #scale_colour_manual(values = c(cbPalette[c(2,3,4,5)],"black"),name = element_blank(),guide = guide_legend(reverse=TRUE))
  scale_colour_discrete(name = element_blank(),guide = guide_legend(reverse=TRUE)) + ggtitle("Males")-> p_male

plot_data %>%
  filter(Sex=="Female",`Age range`!="All") %>%
  ggplot(aes(y=OR,x=`Shift work`,group=`Age range`,colour=`Age range`)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.5,1.6)) +
  coord_flip()+
  scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
  #scale_colour_manual(values = c(cbPalette[c(2,3,4,5)],"black"),name = element_blank(),guide = guide_legend(reverse=TRUE))
  scale_colour_discrete(name = element_blank(),guide = guide_legend(reverse=TRUE))+ggtitle("Females")-> p_female
