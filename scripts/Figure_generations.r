library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(bayesplot)
library(patchwork)
library(data.table)
library(ggdist)
library(emmeans)
library(ggsignif)
library(posterior)
library(extrafont)
library(ggeasy)
library(ggrain)

setwd("data_dir")

#fig2.A
s <- read.csv("data_fig1.csv")

ps <- s  %>% 
  group_by(subN,conN) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=conN,y=n,group=conN,fill=conN))+
  geom_boxplot(aes(fill=conN),alpha=.15,outlier.colour = NA)+
  geom_point(aes(col=conN),position = position_jitterdodge(jitter.width = 0.25,
                                                           dodge.width = 0.7),
             size=2.5)+
  scale_fill_manual(values = c('#429EBE','#F7B04F'))+
  scale_color_manual(values = c('#429EBE','#F7B04F'))+
  theme_classic()+
  theme(
        text = element_text(size = 32),
        axis.title.x = element_blank(),
        axis.line = element_line(colour="black",size=1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.25, "cm"),
        legend.position = c(0.2, 1),
        legend.justification = c(1, 1),
        legend.title=element_blank())+
  labs(y="MW reports")

ts <- s%>% group_by(subN,conN) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = conN,
              values_from = n)

ggsave("fig1D.png",plot=ps, dpi = 1800, width = 12, height = 12)

#fig2.B
s <- s %>% filter(!is.na(Time))

meand <- s %>% 
  group_by(subN,conN,Emotion) %>% 
  filter(Emotion != "D192")%>% 
  summarise(ns = n()) %>% 
  group_by(conN,Emotion) %>% 
  summarise(nm = mean(ns),se = sd(ns)/sqrt(n()))

emos <- s  %>% 
  group_by(subN,conN,Emotion) %>% 
  filter(Emotion != "D192")%>% 
  summarise(nm = n()) %>% 
  ggplot(aes(y=nm,x=Emotion,col=conN,fill=conN))+
  geom_boxplot(aes(fill=conN),alpha=.15,outlier.colour = NA)+
  geom_point(aes(col=conN), alpha = .5,size=5,position = position_jitterdodge(jitter.width = 0.25,
                                                                               dodge.width = 0.7),
             size=2.5)+
  geom_line(data = meand, 
            aes(x = Emotion, 
                y = nm,group=conN), 
            color = "black",
            linewidth = 2,position = position_jitterdodge(jitter.width = 0,
                                                          dodge.width = 0.7))+
  geom_point(data = meand, 
             aes(x = Emotion, 
                 y = nm,   
                 fill = conN), 
             color = "black",
             size= 10,　
             shape = 21,
             stroke = 2,
             alpha=1.,position = position_jitterdodge(jitter.width = 0,
                                                      dodge.width = 0.7))+
  scale_color_manual(values = c('#429EBE','#F7B04F'))+
  scale_fill_manual(values = c('#429EBE','#F7B04F'))+
  theme_classic() +
  theme_classic()+
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())

meand <- s %>% 
  group_by(subN,conN,Time) %>% 
  summarise(ns = n()) %>% 
  group_by(conN,Time) %>% 
  summarise(nm = mean(ns),se = sd(ns)/sqrt(n()))

tims <- s  %>% 
  group_by(subN,conN,Time)%>% 
  summarise(nm = n()) %>% 
  ggplot(aes(y=nm,x=Time,col=conN,fill=conN))+
  geom_boxplot(aes(fill=conN),alpha=.15,outlier.colour = NA)+
  geom_point(aes(col=conN), alpha = .5,size=5,position = position_jitterdodge(jitter.width = 0.25,
                                                                              dodge.width = 0.7),
             size=2.5)+
  geom_line(data = meand, 
            aes(x = Time, 
                y = nm,group=conN), 
            color = "black",
            linewidth = 2,position = position_jitterdodge(jitter.width = 0,
                                                          dodge.width = 0.7))+
  geom_point(data = meand, 
             aes(x = Time, 
                 y = nm,   
                 fill = conN), 
             color = "black",
             size= 10,　
             shape = 21,
             stroke = 2,
             alpha=1.,position = position_jitterdodge(jitter.width = 0,
                                                      dodge.width = 0.7))+
  scale_color_manual(values = c('#429EBE','#F7B04F'))+
  scale_fill_manual(values = c('#429EBE','#F7B04F'))+
  theme_classic() +
  theme_classic()+
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())

ggsave("sfig1A.png",plot= emos, dpi = 1200, width = 10, height = 10)
ggsave("sfig1B.png",plot= tims, dpi = 1200, width = 10, height = 10)

#fig.2C
dd <- read.csv("data_fig1EF.csv")%>%
  mutate(type = str_replace_all(type, pattern = c("MW_Desc_s" = "MW", 
                                                  "Aware_Desc_s" = "Aware", 
                                                  "task_sound" = "Focus")))%>% 
  mutate(type  = factor(type , levels = c("MW",
                                          "Aware",
                                          "Focus")))
avedd <- dd %>% 
  group_by(type) %>% 
  summarise(aves = mean(ave))

ave_p <- dd %>% 
  group_by(sub,type)%>% 
  ggplot(aes(x=type,y=ave,group=type,col=type,fill=type))+
  geom_rain(point.args = list(color = "grey25",shape = 21, alpha = .5,size=5), 
            point.args.pos = rlang::list2(position = position_jitter(width = 0.05)),
            boxplot.args = list(fill = NA, alpha = 1.,outlier.shape = NA),
            violin.args = list(color = NA, alpha = 1.),
            violin.args.pos = rlang::list2(side = "l", width = 0.5, position = position_nudge(x = -0.05)))+
  geom_line(data = avedd, 
            aes(x = type, 
                y = aves,group=1), 
            color = "black",
            linewidth = 2)+
  geom_point(data = avedd, 
             aes(x = type, 
                 y = aves,   
                 fill = type), 
             color = "black",
             size= 10,　
             shape = 21,
             stroke = 2)+
  scale_color_manual(values = c('#F3663F','#2D7537',"#044C7C"))+
  scale_fill_manual(values = c('#F3663F','#2D7537',"#044C7C"))+
  theme_classic()+
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())+
  labs(y="Mean RT (ms)")

ggsave("fig1E.png",plot= ave_p, dpi = 1200, width = 10, height = 10)

rtcvdd <- dd %>% 
  group_by(type) %>% 
  summarise(aves = mean(rtcv))

sd_p <- dd %>% 
  group_by(sub,type)  %>% 
  ggplot(aes(x=type,y=rtcv,group=type,col=type,fill=type))+
  geom_rain(point.args = list(color = "grey25",shape = 21, alpha = .5,size=5), 
            point.args.pos = rlang::list2(position = position_jitter(width = 0.05)),
            boxplot.args = list(fill = NA, alpha = 1.,outlier.shape = NA),
            violin.args = list(color = NA, alpha = 1.),
            violin.args.pos = rlang::list2(side = "l", width = 0.5, position = position_nudge(x = -0.05)))+
  geom_line(data = rtcvdd, 
            aes(x = type, 
                y = aves,group=1), 
            color = "black",
            linewidth = 2)+
  geom_point(data = rtcvdd, 
             aes(x = type, 
                 y = aves,   
                 fill = type), 
             color = "black",
             size= 10,　
             shape = 21,
             stroke = 2)+
  scale_color_manual(values = c('#F3663F','#2D7537',"#044C7C"))+
  scale_fill_manual(values = c('#F3663F','#2D7537',"#044C7C"))+
  theme_classic()+
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())+
  labs(y="Mean RT (ms)")

ggsave("fig1F.png",plot= sd_p, dpi = 1200, width = 10, height = 10)

dd <- read.csv("data_fig3A.csv")%>%
  mutate(gr = str_replace_all(gr, pattern = c("mwr_Desc" = "MW", 
                                              "awr_Desc" = "Aware", 
                                              "task_R" = "Focus")))%>% 
  mutate(gr  = factor(gr , levels = c("MW",
                                      "Aware",
                                      "Focus")))%>%
  mutate(condN = str_replace_all(condN, pattern = c("bf" = "Breath", 
                                                  "sf" = "Sound")))
dd2 <- dd %>% 
  group_by(subN,condN,gr) %>% 
  summarise(means = mean(RR))  %>% 
  ggplot(aes(x=gr,y=means,fill=condN))+
  geom_boxplot(aes(fill=condN),alpha=.15,outlier.colour = NA)+
  geom_point(aes(col=condN), alpha = .5,size=5,position = position_jitterdodge(jitter.width = 0.25,
                                                            dodge.width = 0.7),
             size=2.5)+
  geom_line(data = aves, 
            aes(x = gr, 
                y = means,group=condN), 
            color = "black",
            linewidth = 2,position = position_jitterdodge(jitter.width = 0,
                                                          dodge.width = 0.7))+
  geom_point(data = aves, 
             aes(x = gr, 
                 y = means,   
                 fill = condN), 
             color = "black",
             size= 10,　
             shape = 21,
             stroke = 2,
             alpha=1.,position = position_jitterdodge(jitter.width = 0,
                                                       dodge.width = 0.7))+
  scale_color_manual(values = c('#429EBE','#F7B04F'))+
  scale_fill_manual(values = c('#429EBE','#F7B04F'))+
  theme_classic() +
  theme_classic()+
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())

ggsave("fig3A.png",plot= dd2, dpi = 1200, width = 12, height = 5)

################################################################################
#fig.2
mask.colors <- colorRampPalette(c("white","black"))

plotter <- function(stat_names,mask_names,freqs,mins,maxs){
  dm <- fread(mask_names)%>% 
    setnames(freqs %>% 
               as.character()) %>% 
    cbind(Ch = 1:65,.) %>% 
    pivot_longer(cols=2:ncol(.),
                 names_to = "freq")
  
  d<-fread(stat_names) %>% 
    setnames(freqs %>% 
               as.character()) %>% 
    cbind(Ch = 1:65,
          .) %>% 
    pivot_longer(cols=2:ncol(.),
                 names_to = "freq",
                 values_to = "t-val") %>% 
    cbind(
      mask = dm$value/4 + 0.75,.
    )
  
  ggplot(d,aes(x=as.numeric(freq),y=Ch,fill=ersp))+
    geom_tile(aes())+
    scale_x_continuous(trans = "log",breaks=c(5,10,20,30,40))+ 
    scale_fill_gradientn(colours = parula(100),
                         limits = c(mins, maxs),
                         breaks=c(mins,maxs,0))+
    theme_classic()+
    theme(
      text = element_text(size = 32),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(), 
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      axis.line = element_line(colour="black",size=1),
      axis.ticks = element_line(size = 1),
      axis.ticks.length = unit(0.25, "cm"),
      legend.position = "none",
      legend.justification = c(1, 1),
      legend.title=element_blank())
} 

maskmaker <- function(mask_names,freqs){
  dm <- fread(mask_names)%>% 
    setnames(freqs %>% 
               as.character()) %>% 
    cbind(Ch = 1:65,.) %>% 
    pivot_longer(cols=2:ncol(.),
                 names_to = "freq")
  
  ggplot(dm,aes(x=as.numeric(freq),y=Ch,fill=value))+
    geom_tile(aes())+
    scale_x_continuous(trans = "log",breaks=c(5,10,20,30,40))+
    scale_fill_gradientn(colours = mask.colors (10))+
    theme_classic()+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          text = element_text(size = 32),
          axis.line = element_line(colour="black",linewidth=1),
          axis.ticks = element_line(linewidth = 1),
          axis.ticks.length = unit(0.25, "cm"))+
    xlab("Frequency")
} 

freq <- fread("freq_lab.csv") %>% as.vector() %>% unlist()
stat <- "stat_bf_comp.csv"
mask <- "mask_bf_comp.csv"
a <- plotter(stat,mask,freq,-8.5,5) 
amask <- maskmaker(mask,freq)
ggsave("fig2_freqmt_bf.png",plot= a, dpi = 1200, width = 10, height = 6)

stat <- "stat_sf_comp.csv"
mask <- "mask_sf_comp.csv"
b <- plotter(stat,mask,freq,-8.5,5) 
bmask <- maskmaker(mask,freq)
ggsave("fig2_freqmt_sf.png",plot= b, dpi = 1200, width = 10, height = 6)

stat <- "stat_bf-sf.csv"
mask <- "mask_bf-sf.csv"
c <- plotter(stat,mask,freq,-8.5,5) + ggtitle("task_diff")
cmask <- maskmaker(mask,freq)

stat <- "stat_bf-sf_mw.csv"
mask <- "mask_bf-sf_mw.csv"
d <- plotter(stat,mask,freq,-8.5,5) + ggtitle("mw_diff")
dmask <- maskmaker(mask,freq)

stat <- "stat_bf_awcomp.csv"
mask <- "mask_bf_awcomp.csv"
e <- plotter(stat,mask,freq,-4,7.5) 
emask <- maskmaker(mask,freq)
ggsave("fig2_freqmt_awbf.png",plot= e, dpi = 1200, width = 10, height = 6)

stat <- "stat_sf_awcomp.csv"
mask <- "mask_sf_awcomp.csv"
f <- plotter(stat,mask,freq,-4,7.5) 
fmask <- maskmaker(mask,freq)
ggsave("fig2_freqmt_awsf.png",plot= f, dpi = 1200, width = 10, height = 6)

ggsave("fig5mask.png",plot= (amask/bmask), dpi = 1200, width = 9, height = 9)
ggsave("fig6Bmask.png",plot= (emask/fmask), dpi = 1200, width = 10, height = 10)

#fig6のtime-seriesでのalphaのデータを読み込む話
fit_alpha <- readRDS("alpha.Rds")
fit_beta <- readRDS("beta.Rds")

fit_sfalpha <- readRDS("alpha_sf.Rds")
fit_sfbeta <- readRDS("beta_sf.Rds")

da <- fread(file="freq_res.csv",header=T)

######extract frequency for analysis#####
d_alpha <- da %>% 
  filter(freq_lab == "alpha" & cond == "bf") %>% 
  dplyr::select(sub,time,ersp)%>% 
  group_by(sub,time) %>% 
  summarise(value = mean(ersp))%>% 
  filter(between(time,5,195))

d_beta <- da %>% 
  filter(freq_lab == "beta" & cond == "bf") %>% 
  dplyr::select(sub,time,ersp)%>% 
  group_by(sub,time) %>% 
  summarise(value = mean(ersp))%>% 
  filter(between(time,5,195))

dsf_alpha <- da %>% 
  filter(freq_lab == "alpha" & cond == "sf") %>% 
  dplyr::select(sub,time,ersp)%>% 
  group_by(sub,time) %>% 
  summarise(value = mean(ersp))%>% 
  filter(between(time,5,195))

dsf_beta <- da %>% 
  filter(freq_lab == "beta" & cond == "sf") %>% 
  dplyr::select(sub,time,ersp)%>% 
  group_by(sub,time) %>% 
  summarise(value = mean(ersp))%>% 
  filter(between(time,5,195))

draws_df <- as_draws(fit_alpha)
fitd <- draws_df
fit_summary <- fread(file="summ_alpha.csv",header=T)

draws_dfb <- as_draws(fit_beta)
fitd <- draws_dfb
fit_sumbeta <- fread(file="summ_beta.csv",header=T)

draws_dfsf <- as_draws(fit_sfalpha)
fitdsf <- draws_dfsf
fitsf_summary <- fread(file="summsf_alpha.csv",header=T)

draws_dfbsf <- as_draws(fit_sfbeta)
fitdsf <- draws_dfbsf
fitsf_sumbeta <- fread(file="summsf_beta.csv",header=T)

min_bp <- fit_summary %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(q5)%>% as.numeric()
min_bp <- -((195 - min_bp))*0.045 - 0.5

mean_bp <- fit_summary %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(mean)%>% as.numeric()
mean_bp <- -((195 - mean_bp))*0.045 - 0.5

max_bp <- fit_summary %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(q95)%>% as.numeric()
max_bp <- -((195 - max_bp))*0.045 - 0.5

d_alpha$time <- -((195 - d_alpha$time))*0.045 - 0.5

tsp_alpha <- d_alpha %>% 
  group_by(time) %>% 
  summarise(v = mean(value),std = sd(value)/sqrt(n())) %>% 
  ggplot()+
  geom_vline(xintercept = min_bp, linetype = "dashed",col="#F3663F",linewidth = 2)+
  geom_vline(xintercept = mean_bp, linetype = "dashed",linewidth = 2)+
  geom_vline(xintercept = max_bp, linetype = "dashed",col="#87AC75",linewidth = 2)+
  geom_ribbon(aes(x=time,y=v,
                  ymin = v - std, 
                  ymax = v + std),fill='#429EBE',
              alpha = .25)+
  geom_line(aes(x=time,y=v),
            linewidth=2,col='#429EBE') +
  theme_classic()+
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())+
  scale_x_continuous(breaks=c(-9.4,-7.5,-5.0,-2.5,0))

ggsave("fig2_alphafreq.png",plot= tsp_alpha, dpi = 1200, width = 10, height = 6)

min_bp <- fit_sumbeta %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(q5)%>% as.numeric()
min_bp <- -((195 - min_bp))*0.045 - 0.5

mean_bp <- fit_sumbeta %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(mean)%>% as.numeric()
mean_bp <- -((195 - mean_bp))*0.045 - 0.5

max_bp <- fit_sumbeta %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(q95)%>% as.numeric()
max_bp <- -((195 - max_bp))*0.045 - 0.5

d_beta$time <- -((195 - d_beta$time))*0.045 - 0.5

tsp_beta <- d_beta %>% 
  group_by(time) %>% 
  summarise(v = mean(value),std = sd(value)/sqrt(n())) %>% 
  ggplot()+
  geom_vline(xintercept = min_bp, linetype = "dashed",col="#F3663F",linewidth = 2)+
  geom_vline(xintercept = mean_bp, linetype = "dashed",linewidth = 2)+
  geom_vline(xintercept = max_bp, linetype = "dashed",col="#87AC75",linewidth = 2)+
  geom_ribbon(aes(x=time,y=v,
                  ymin = v - std, 
                  ymax = v + std),fill='#429EBE',
              alpha = .25)+
  geom_line(aes(x=time,y=v),
            linewidth=2,col='#429EBE') +
  theme_classic() +
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())+
  scale_x_continuous(breaks=c(-9.4,-7.5,-5.0,-2.5,0))+
  scale_y_continuous(breaks=c(12,13,14))

ggsave("fig2_betafreq.png",plot= tsp_beta, dpi = 1200, width = 10, height = 6)

min_bp <- fitsf_summary %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(q5)%>% as.numeric()
min_bp <- -((195 - min_bp))*0.045 - 0.5

mean_bp <- fitsf_summary %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(mean)%>% as.numeric()
mean_bp <- -((195 - mean_bp))*0.045 - 0.5

max_bp <- fitsf_summary %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(q95)%>% as.numeric()
max_bp <- -((195 - max_bp))*0.045 - 0.5

dsf_alpha$time <- -((195 - dsf_alpha$time))*0.045 - 0.5
tsp_alphasf <- dsf_alpha %>% 
  group_by(time) %>% 
  summarise(v = mean(value),std = sd(value)/sqrt(n())) %>% 
  ggplot() +
  annotate("rect",xmin=c(-9.4,max_bp),
           xmax=c(min_bp,0),
           ymin=-Inf,ymax=Inf, alpha=0.25, 
           fill=c("#F3663F","#87AC75"))+
  geom_ribbon(aes(x=time,y=v,
                  ymin = v - std, 
                  ymax = v + std),fill="grey50",
              alpha = .5)+
  geom_line(aes(x=time,y=v),
            linewidth=1,col="grey25") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 32),
        axis.line = element_line(colour="black",linewidth=1),
        axis.ticks = element_line(linewidth = 1),
        axis.ticks.length = unit(0.25, "cm"))+
  labs(x = "Time (s)",
       y = "ERSP",
       title = "alpha")+
  scale_x_continuous(breaks=c(-9.4,-7.5,-5.0,-2.5,0))

tsp_alphasf <- dsf_alpha %>% 
  group_by(time) %>% 
  summarise(v = mean(value),std = sd(value)/sqrt(n())) %>% 
  ggplot()+
  geom_vline(xintercept = min_bp, linetype = "dashed",col="#F3663F",linewidth = 2)+
  geom_vline(xintercept = mean_bp, linetype = "dashed",linewidth = 2)+
  geom_vline(xintercept = max_bp, linetype = "dashed",col="#87AC75",linewidth = 2)+
  geom_ribbon(aes(x=time,y=v,
                  ymin = v - std, 
                  ymax = v + std),fill='#F7B04F',
              alpha = .25)+
  geom_line(aes(x=time,y=v),
            linewidth=2,col='#F7B04F') +
  theme_classic() +
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())+
  scale_x_continuous(breaks=c(-9.4,-7.5,-5.0,-2.5,0))+
  scale_y_continuous(breaks=c(19,20,21))

ggsave("fig2_sfalphafreq.png",plot= tsp_alphasf, dpi = 1200, width = 10, height = 6)

min_bp <- fitsf_sumbeta %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(q5)%>% as.numeric()
min_bp <- -((195 - min_bp))*0.045 - 0.5

mean_bp <- fitsf_sumbeta %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(mean)%>% as.numeric()
mean_bp <- -((195 - mean_bp))*0.045 - 0.5

max_bp <- fitsf_sumbeta %>% 
  dplyr::filter(variable == "BP") %>% 
  dplyr::select(q95)%>% as.numeric()
max_bp <- -((195 - max_bp))*0.045 - 0.5

dsf_beta$time <- -((195 - dsf_beta$time))*0.045 - 0.5
dsf_beta %>% 
  group_by(time) %>% 
  summarise(v = mean(value),std = sd(value)/sqrt(n())) %>% 
  ggplot() +
  annotate("rect",xmin=c(-9.4,max_bp),
           xmax=c(min_bp,0),
           ymin=-Inf,ymax=Inf, alpha=0.25, 
           fill=c("#F3663F","#87AC75"))+
  geom_ribbon(aes(x=time,y=v,
                  ymin = v - std, 
                  ymax = v + std),fill="grey50",
              alpha = .5)+
  geom_line(aes(x=time,y=v),
            linewidth=1,col="grey25") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 32),
        axis.line = element_line(colour="black",linewidth=1),
        axis.ticks = element_line(linewidth = 1),
        axis.ticks.length = unit(0.25, "cm"))+
  labs(x = "Time (s)",
       y = "ERSP",
       title = "beta")+
  scale_x_continuous(breaks=c(-9.4,-7.5,-5.0,-2.5,0))

tsp_betasf <- dsf_beta %>%
  group_by(time) %>% 
  summarise(v = mean(value),std = sd(value)/sqrt(n())) %>% 
  ggplot()+
  geom_vline(xintercept = min_bp, linetype = "dashed",col="#F3663F",linewidth = 2)+
  geom_vline(xintercept = mean_bp, linetype = "dashed",linewidth = 2)+
  geom_vline(xintercept = max_bp, linetype = "dashed",col="#87AC75",linewidth = 2)+
  geom_ribbon(aes(x=time,y=v,
                  ymin = v - std, 
                  ymax = v + std),fill='#F7B04F',
              alpha = .25)+
  geom_line(aes(x=time,y=v),
            linewidth=2,col='#F7B04F') +
  theme_classic() +
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())+
  scale_x_continuous(breaks=c(-9.4,-7.5,-5.0,-2.5,0))+
  scale_y_continuous(breaks=c(12,13,14))

ggsave("fig2_sfbetafreq.png",plot= tsp_betasf, dpi = 1200, width = 10, height = 6)
  
del1 <- fit_alpha$draws(paste0("delta1_mu")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

del2 <- fit_alpha$draws(paste0("delta2_mu")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

del3<- fit_alpha$draws(paste0("diff")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

delta_hist <- rbind(cbind(del = "pre",
                          del1),
                    cbind(del = "post",
                          del2),
                    cbind(del = "diff",
                          del3))

deltp_alpha <- delta_hist%>%
  mutate(del = str_replace_all(del, pattern = c("pre" = "before", 
                                                  "post" = "after",
                                                "diff"="diff"))) %>% 
  ggplot(aes(x=value,fill=del)) +
  geom_histogram(position = "identity",alpha=.75)+
  scale_fill_manual(values = c("#87AC75","#F3663F","#429EBE"))+
  theme_classic()+
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())+
    scale_y_continuous(breaks=c(0,1000,3000,5000))+
  scale_x_continuous(breaks=c(-0.01,0,0.01))

ggsave("fig2_alphadiff.png",plot= deltp_alpha, dpi = 1200, width = 4, height =5)

del1 <- fit_beta$draws(paste0("delta1_mu")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

del2 <- fit_beta$draws(paste0("delta2_mu")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

del3<- fit_beta$draws(paste0("diff")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

delta_hist <- rbind(cbind(del = "pre",
                          del1),
                    cbind(del = "post",
                          del2),
                    cbind(del = "diff",
                          del3))

deltp_beta <- delta_hist%>%
  mutate(del = str_replace_all(del, pattern = c("pre" = "before", 
                                                "post" = "after",
                                                "diff"="diff"))) %>% 
  ggplot(aes(x=value,fill=del)) +
  geom_histogram(position = "identity",alpha=.75)+
  scale_fill_manual(values = c("#87AC75","#F3663F","#429EBE"))+
  theme_classic()+
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())+
  scale_y_continuous(breaks=c(0,1000,3000,5000))+
  scale_x_continuous(breaks=c(-0.01,0,0.01))

ggsave("fig2_betadiff.png",plot= deltp_beta, dpi = 1200, width = 4, height = 5)

del1sf <- fit_sfalpha$draws(paste0("delta1_mu")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

del2sf <- fit_sfalpha$draws(paste0("delta2_mu")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

del3<- fit_sfalpha$draws(paste0("diff")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

delta_sfhist <- rbind(cbind(del = "pre",
                          del1sf),
                    cbind(del = "post",
                          del2sf),
                    cbind(del = "diff",
                          del3))

deltp_sfalpha <- delta_hist%>%
  mutate(del = str_replace_all(del, pattern = c("pre" = "before", 
                                                "post" = "after",
                                                "diff"="diff"))) %>% 
  ggplot(aes(x=value,fill=del)) +
  geom_histogram(position = "identity",alpha=.75)+
  scale_fill_manual(values = c("#87AC75","#F3663F","#429EBE"))+
  theme_classic()+
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())+
  scale_y_continuous(breaks=c(0,1000,3000,5000))+
  scale_x_continuous(breaks=c(-0.01,0,0.01))

ggsave("fig2sf_alphadiff.png",plot= deltp_sfalpha, dpi = 1200, width = 4, height = 5)

del1sf <- fit_sfbeta$draws(paste0("delta1_mu")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

del2sf <- fit_sfbeta$draws(paste0("delta2_mu")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

del3<- fit_sfbeta$draws(paste0("diff")) %>% 
  as.data.frame() %>% 
  pivot_longer(cols=1:4)

delta_sfhist <- rbind(cbind(del = "pre",
                          del1sf),
                    cbind(del = "post",
                          del2sf),
                    cbind(del = "diff",
                          del3))

deltp_sfbeta <- delta_sfhist %>%
  mutate(del = str_replace_all(del, pattern = c("pre" = "before", 
                                                "post" = "after",
                                                "diff"="diff"))) %>% 
  ggplot(aes(x=value,fill=del)) +
  geom_histogram(position = "identity",alpha=.75)+
  scale_fill_manual(values = c("#87AC75","#F3663F","#429EBE"))+
  theme_classic()+
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=1),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.title=element_blank())+
  scale_y_continuous(breaks=c(0,1000,3000,5000))+
  scale_x_continuous(breaks=c(-0.01,0,0.01))

ggsave("fig2sf_betadiff.png",plot= deltp_sfbeta, dpi = 1200, width = 4, height = 5)

#BP等の時点のプロット
diffp_sfalpha <- fit_sfalpha$draws(paste0("diff")) %>% mcmc_areas(prob = .95)+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        text = element_text(size = 32),
        axis.line = element_line(colour="black",size=1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.25, "cm"))+
  labs(x = "Difference of δ parameter")+
  easy_remove_axes(which = "y", what = "text")+
  easy_remove_axes(which = "y", what = "ticks")+
  scale_x_continuous(breaks=c(0.01,0.015,0.02))

bpp_alpha <- -((200 - fit_alpha$draws(paste0("BP")))*0.045 + 0.5) %>% mcmc_areas(prob = .95)+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        text = element_text(size = 32),
        axis.line = element_line(colour="black",size=1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.25, "cm"))+
  labs(x = "Change Point (s)")+
  easy_remove_axes(which = "y", what = "text")+
  easy_remove_axes(which = "y", what = "ticks")

diffp_beta <- fit_beta$draws(paste0("diff")) %>% mcmc_areas(prob = .95)+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        text = element_text(size = 32),
        axis.line = element_line(colour="black",size=1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.25, "cm"))+
  labs(x = "Difference of δ parameter")+
  easy_remove_axes(which = "y", what = "text")+
  easy_remove_axes(which = "y", what = "ticks")+
  scale_x_continuous(breaks=c(0.005,0.01,0.015))

diffp_sfbeta <- fit_sfbeta$draws(paste0("diff")) %>% mcmc_areas(prob = .95)+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        text = element_text(size = 32),
        axis.line = element_line(colour="black",size=1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.25, "cm"))+
  labs(x = "Difference of δ parameter")+
  easy_remove_axes(which = "y", what = "text")+
  easy_remove_axes(which = "y", what = "ticks")+
  scale_x_continuous(breaks=c(0,0.005,0.01))

bpp_beta <- -((200 - fit_beta$draws(paste0("BP")))*0.045 + 0.5) %>% mcmc_areas(prob = .95)+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        text = element_text(size = 32),
        axis.line = element_line(colour="black",size=1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.25, "cm"))+
  labs(x = "Change Point (s)")+
  easy_remove_axes(which = "y", what = "text")+
  easy_remove_axes(which = "y", what = "ticks")

######breath cycle
all_cyc_res <- fread("check_cyc.csv")[,-1]

aves <- all_cyc_res %>% 
  dplyr::mutate(
    subN = substring(sub,3,5),
    cond = substring(sub,6,7)
  ) %>% 
  dplyr::select(subN,cond,cycles)  %>%
  mutate(cond = str_replace_all(cond, pattern = c("sf" = "SF",
                                                  "bf" = "BF")))%>% 
  mutate(cond = factor(cond, levels = c("SF","BF"))) %>% 
  group_by(cond) %>% 
  summarise(means=mean(cycles))

cyc_p <- all_cyc_res %>% 
  dplyr::mutate(
    subN = substring(sub,3,5),
    cond = substring(sub,6,7)
  ) %>% 
  dplyr::select(subN,cond,cycles)  %>%
  mutate(cond = str_replace_all(cond, pattern = c("sf" = "SF",
                                                  "bf" = "BF")))%>% 
  mutate(cond = factor(cond, levels = c("BF","SF")))%>%
  ggplot(aes(x=cond,y=cycles,fill=cond))+
  geom_rain(point.args = list(color = "grey25",shape = 21, alpha = .5,size=5), 
            point.args.pos = rlang::list2(position = position_jitter(width = 0.05)),
            boxplot.args = list(fill = NA, alpha = 1.,outlier.shape = NA),
            violin.args = list(color = NA, alpha = 1.),
            violin.args.pos = rlang::list2(side = "l", width = 0.75, position = position_nudge(x = -0.05)))+
  geom_line(data = aves, 
            aes(x = cond, 
                y = means,group=1), 
            color = "black",
            linewidth = 2)+
  geom_point(data = aves, 
             aes(x = cond, 
                 y = means,   
                 fill = cond), 
             color = "black",
             size= 10,　
             shape = 21,
             stroke = 2)+
  scale_fill_manual(values = c('#429EBE','#F7B04F'))+
  scale_color_manual(values = c('#429EBE','#F7B04F')) +
  theme_classic()+
  theme(
    text = element_text(size = 32),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.line = element_line(colour="black",size=2),
    axis.ticks = element_line(size = 2),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    legend.title=element_blank())

ggsave("fig4_A.png",plot= cyc_p, dpi = 1200, width = 10, height = 15)

cycd <- all_cyc_res %>% 
  dplyr::mutate(
    subN = substring(sub,3,5),
    cond = substring(sub,6,7)
  ) %>% 
  dplyr::select(subN,cond,cycles) 

finale <- fread("comp_resp.csv")
a <- finale %>% 
  group_by(subN,cond,phase) %>% 
  summarise(means=mean(parcs))%>% 
  filter(cond=="sf") %>% 
  ggplot(aes(x=means,y=phase,group=phase,fill=phase))+
  stat_halfeye(aes(fill=phase),
               point_color = NA, .width = 0, height = 0.6,
               position = position_nudge(y = 0.3)
  ) +
  geom_point(aes(col=phase),
             position = position_jitter(width = 0, height = 0.1, seed = 1),
             size=2.5) +
  geom_boxplot(
    position = position_nudge(y = 0.2),
    width = 0.1, outlier.shape = NA,outlier.colour = NA
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 32),
        axis.line = element_line(colour="black",size=1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")+ 
  geom_vline(xintercept=.616,linetype=2,alpha=0.7,linewidth=0.5,color='black')+
  ggtitle("sound_focus")

b <- finale %>% 
  group_by(subN,cond,phase) %>% 
  summarise(means=mean(parcs)) %>% 
  filter(cond=="bf") %>% 
  ggplot(aes(x=means,y=phase,group=phase,fill=phase))+
  stat_halfeye(aes(fill=phase),
               point_color = NA, .width = 0, height = 0.6,
               position = position_nudge(y = 0.3)
  ) +
  geom_point(aes(col=phase),
             position = position_jitter(width = 0, height = 0.1, seed = 1),
             size=2.5) +
  geom_boxplot(
    position = position_nudge(y = 0.2),
    width = 0.1, outlier.shape = NA,outlier.colour = NA
  )+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 32),
        axis.line = element_line(colour="black",size=1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.25, "cm"))+
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")+ 
  geom_vline(xintercept=.652,linetype=2,alpha=0.7,linewidth=0.5,color='black')+
  ggtitle("breath_focus")

ggsave("fig4B.png",plot= (cyc_p), dpi = 300, width = 12, height = 12)

#########################################################################
all_res <- fread("resp_alld.csv")

all_res_time <- all_res %>% pivot_longer(cols = c("inhale","exhale"),
                                         names_to = "phase",
                                         values_to = "val") %>% 
  mutate(time = as.numeric(time),
         val = as.numeric(val)) %>% 
  group_by(cond,time,phase) %>% 
  summarise(means = mean(val),se = sd(val)/sqrt(n())) 

all_res_time$time <- all_res_time$time * -1

bfd <- all_res_time %>% filter(phase == "inhale"&cond == "bf") %>% 
  ggplot(aes(x=time,y=means,group=cond,fill=cond,col=cond))+
  geom_point(size=5)+
  geom_errorbar(aes(ymin = means-se, ymax = means+se),
                width = .25,
                linewidth=2)+
  geom_line(linewidth=2)+ 
  geom_hline(yintercept=.651,linetype=2,alpha=0.7,linewidth=1,color='black')+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        text = element_text(size = 32),
        axis.line = element_line(colour="black",size=2),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.25, "cm"))+
  scale_x_continuous(breaks=seq(-10,0,2))+
  scale_color_manual(values = c('#429EBE'))+
  scale_y_continuous(breaks=seq(0.55,0.75,0.1))

sfd <- all_res_time %>% filter(phase == "inhale"&cond == "sf") %>% 
  ggplot(aes(x=time,y=means,group=cond,fill=cond,col=cond))+
  geom_point(size=5)+
  geom_errorbar(aes(ymin = means-se, ymax = means+se),
                width = .25,
                linewidth=2)+
  geom_line(linewidth=2)+ 
  geom_hline(yintercept=.616,linetype=2,alpha=0.7,linewidth=1,color='black')+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        text = element_text(size = 32),
        axis.line = element_line(colour="black",size=2),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.25, "cm"))+
  scale_x_continuous(breaks=seq(-10,0,2))+
  scale_color_manual(values = c('#F7B04F'))+
  scale_y_continuous(breaks=seq(0.60,0.65,0.05))

ggsave("fig4_B.png",plot= bfd, dpi = 1200, width = 10, height = 10)
ggsave("fig4_C.png",plot= sfd, dpi = 1200, width = 10, height = 10)

da <- read.csv("RR_timeseries.csv")
plotsa <-  da %>% 
  group_by(subN,condN,time_vals) %>% summarise(ave = mean(RR)) %>% 
  group_by(condN,time_vals) %>% 
  filter(condN=="bf")%>% 
  summarise(mean = mean(ave),se = sd(ave)/sqrt(n())) %>% 
  ggplot(aes(x=time_vals,y=mean,ymin=mean-se,ymax=mean+se,
             group=condN,col=condN,fill=condN))+
  geom_point(size=5)+
  geom_line(linewidth=2)+
  geom_errorbar(width = .25,
                linewidth=2)+
  scale_fill_manual(values = c('#429EBE'))+
  scale_color_manual(values = c('#429EBE'))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        text = element_text(size = 32),
        axis.line = element_line(colour="black",size=2),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.25, "cm"))+
  scale_x_continuous(breaks=seq(-10,0,2))+
  scale_y_continuous(breaks=seq(0.80,0.95,0.05))

plotsb <-   da %>% 
  group_by(subN,condN,time_vals) %>% summarise(ave = mean(RR)) %>% 
  group_by(condN,time_vals) %>% 
  filter(condN=="sf") %>% 
  summarise(mean = mean(ave),se = sd(ave)/sqrt(n())) %>% 
  ggplot(aes(x=time_vals,y=mean,ymin=mean-se,ymax=mean+se,
             group=condN,col=condN,fill=condN))+
  geom_point(size=5)+
  geom_line(linewidth=2)+
  geom_errorbar(width = .25,
                linewidth=2)+
  scale_fill_manual(values = c('#F7B04F'))+
  scale_color_manual(values = c('#F7B04F'))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        text = element_text(size = 32),
        axis.line = element_line(colour="black",size=2),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.25, "cm"))+
  scale_x_continuous(breaks=seq(-10,0,2))+
  scale_y_continuous(breaks=seq(0.75,0.90,0.05))

ggsave("fig4_D.png",plot= plotsa, dpi = 1200, width = 10, height = 10)
ggsave("fig4_E.png",plot= plotsb, dpi = 1200, width = 10, height = 10)
