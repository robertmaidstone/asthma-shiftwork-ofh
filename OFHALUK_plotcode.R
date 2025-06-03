library(tidyverse)

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
  mutate(OR=as.numeric(OR)) -> plot_data

cbPalette <- c("red","black","red")
pd_width <- 0.6

plot_data %>%
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

ggsave(plot = p_sex, filename="OurFutureHealth/OFH_AsthmaShiftwork/plots/OFHALUKshowcase.png",width=5,height=4)
