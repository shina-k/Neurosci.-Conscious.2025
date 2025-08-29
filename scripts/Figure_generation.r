#HMMの事後処理
#library
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(Rcpp)
library(bayesplot)
library(patchwork)
library(ggridges)
library(data.table)
library(gridExtra)
library(viridis) 
library(tidyverse) 
library(rstatix)
library(multcomp)
library(lme4)
library(lmerTest)
library(emmeans)
library(permutes)
library(doParallel)
library(permuco)
library(buildmer)
library(akima)
library(scales)
library(mgcv)
library(png)
library(grid)
library(gganimate)
library(pals)
library(pals)

dir_r <- "G:/マイドライブ/1. exp/MW/MW_HEP/data/erp/plots/"
dir_r <- "I:/MW_HEP/erp/" 
ch_dir <- "G:/マイドライブ/1. exp/MW/MW_HEP/"
mark_use <- 19
mark_unuse<-1

baseline <- -200
forhz <- -196
step <- abs(abs(baseline)-abs(forhz))

point <- 1:250
# 色のリストから「n個の色を生成する関数」を作成する
jet_palette_function <- colorRampPalette(
  c("#00007F", "blue", "#007FFF", "cyan", 
    "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
)
parula_palette_function <- pals::parula(256)
palette <- parula_palette_function

#######################################################################
#channel情報の登録
electrodeLocs <- read_delim(paste0(ch_dir,"new_chanloc.loc"),
                            "\t",
                            escape_double = FALSE,
                            col_names = c("chanNo","theta","radius","electrode"),
                            trim_ws = TRUE)

electrodeLocs$radianTheta <- pi/180*electrodeLocs$theta

electrodeLocs <- electrodeLocs %>%
  mutate(x = .$radius*sin(.$radianTheta),
         y = .$radius*cos(.$radianTheta))

polar <- ggplot(electrodeLocs,
                aes(radianTheta, radius, label = electrode))+
  geom_text()+
  theme_bw()+
  coord_fixed(ratio = 6.75)

cartesian <- ggplot(electrodeLocs,
                    aes(x, y, label = electrode))+
  geom_text()+
  theme_bw()+
  coord_equal()

theme_topo <- function(base_size = 12)
{
  theme_bw(base_size = base_size) %+replace%
    theme(
      rect             = element_blank(),
      line             = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100) {
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

headShape <- circleFun(c(0, 0), round(max(electrodeLocs$x)), npoints = 100) # 0
nose <- data.frame(x = c(-0.075,0,.075),y=c(.495,.575,.495))
top_func_journal<-function(data, points, palette, minT, maxT,minv,maxv,yesm,nom,use){
  jet.colors <- palette
  pd_list <- list()
  i<-1

  data$time <- (data$time-1)*step + baseline
  
  singleTimepoint <- filter(data,between(time,minT,maxT)) %>% 
    group_by(electrode,chanNo,theta,radius,radianTheta,x,y) %>% 
    summarize(amplitude = mean(amplitude))
  
  
  gridRes <- 50 # Specify the number of points for each grid dimension i.e. the resolution/smoothness of the interpolation
  
  tmpTopo <- with(singleTimepoint,
                  interp(x = x, y = y, z = amplitude,
                         xo = seq(min(x)*2,
                                  max(x)*2,
                                  length = gridRes),
                         yo = seq(min(y)*2,
                                  max(y)*2,
                                  length = gridRes),
                         linear = FALSE,
                         extrap = TRUE)
  ) 
  
  interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)
  
  names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y
  
  interpTopo <- gather(interpTopo,
                       key = y,
                       value = amplitude,
                       -x,
                       convert = TRUE)
  
  interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle
  
  interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle
  
  maskRing <- circleFun(diameter = 1.42) #create a circle round the outside of the plotting area to mask the jagged edges of the interpolation
  
  akimaPlot <- ggplot(interpTopo,
                      aes(x = x, y = y, fill = amplitude)
  ) +
    geom_raster()  +
    theme_topo()+
    scale_fill_gradientn(colours = palette,
                         limits = c(-2,2),
                         guide = "colourbar",
                         oob = squish) + 
    geom_path(data = maskRing,
              aes(x, y, z = NULL, fill =NULL),
              colour = "white",
              linewidth = 6)+
    geom_point(data = singleTimepoint,
               aes(x, y),
               size = 1)+
    geom_path(data = headShape,
              aes(x, y, z = NULL, fill = NULL),
              linewidth = 1.5)+
    geom_path(data = nose,
              aes(x, y, z = NULL, fill = NULL),
              linewidth = 1.5)+
    coord_equal()
  
  splineSmooth <- gam(amplitude ~ s(x, y, bs = 'ts'),
                      data = singleTimepoint)
  
  GAMtopo <- data.frame(expand.grid(x = seq(min(singleTimepoint$x)*2,
                                            max(singleTimepoint$x)*2,
                                            length = gridRes),
                                    y = seq(min(singleTimepoint$y)*2,
                                            max(singleTimepoint$y)*2,
                                            length = gridRes)))
  
  GAMtopo$amplitude <-  predict(splineSmooth,
                                GAMtopo,
                                type = "response")
  
  GAMtopo$incircle <- (GAMtopo$x)^2 + (GAMtopo$y)^2 < .7^2 # mark
  pd_list[[i]] <- cbind(timep = rep(1,nrow(GAMtopo)),GAMtopo)
  GAPtp<-do.call(rbind,pd_list)
  
  singleTimepoint <- singleTimepoint %>% 
    mutate(use = ifelse(electrode %in% used_ch,"yes","no"))
  
  # マスク用のポリゴン（ドーナツ形状）を作成する
  # ----------------------------------------------------
  # 表示したい円の半径
  hole_radius <- 0.71 
  # マスクの外側の半径（プロット全体を覆うように十分に大きくする）
  mask_radius <- 2.0  
  
  # 円周上の点の座標を計算
  angles <- seq(0, 2 * pi, length.out = 200)
  # ドーナツの内側の円（穴）
  hole_df <- data.frame(
    x = hole_radius * cos(angles), 
    y = hole_radius * sin(angles)
  )
  # ドーナツの外側の円（rev()で座標の順序を逆にし、正しいポリゴンを描く）
  outer_df <- data.frame(
    x = mask_radius * cos(rev(angles)), 
    y = mask_radius * sin(rev(angles))
  )
  # 結合してドーナツ形状のデータフレームを作成
  mask_polygon <- rbind(hole_df, outer_df)
  # ----------------------------------------------------
  ggplot(GAPtp, aes(x, y, fill = amplitude)) + # ★ フィルタせず、全てのデータを渡す
    #geom_tile() + 
    geom_raster(interpolate=TRUE)+
    geom_polygon(data = mask_polygon, aes(x, y), fill = "white") +
    theme_topo() +
    scale_fill_gradientn(colours = palette,
                         limits = c(minv, maxv),
                         guide = "colourbar",
                         oob = squish) +
    geom_point(data = singleTimepoint,
               aes(x, y, shape = use),
               size = 3) +
    geom_path(data = headShape,
              aes(x, y, z = NULL, fill = NULL),
              linewidth = 1.5) +
    geom_path(data = nose,
              aes(x, y, z = NULL, fill = NULL),
              linewidth = 1.5) +
    coord_equal(xlim = c(-0.8, 0.8), ylim = c(-0.8, 0.8)) +
    scale_shape_manual(values = c("yes" = yesm,
                                  "no" = nom))
}

########################################################################

dir_c <- paste0(dir_r,"HEP_conts")  
dir_nh <- paste0(dir_r,"./plots/HEP_notime")  
dir_res <- paste0(dir_r,"HEP_resp")  

setwd(dir_nh)
nhep_ls <- list.files()
nh_ls <- list() 
nh_ls <- lapply(nhep_ls,fread)

setwd(dir_c)
cont_ls <- list.files()
co_ls <- list() 
co_ls <- lapply(cont_ls,fread)

setwd(dir_res)
resp_ls <- list.files()
res_ls <- list() 
res_ls <- lapply(resp_ls,fread)

notime_fun <- function(data_ls,fn,chan){
  data <- data_ls %>% t()
  colnames(data) <- data[1,]
  #この時点で時間，チャンネルのデータ
  data <- data[-1,c(1,(chan+1))]
  #MWかtaskかを入れる
  if(length(grep("MW",fn)) == 1){
    if(length(grep("sf",fn))==1){
      data<-cbind(state=rep("MW",nrow(data)),cond=rep("sf",nrow(data)),data)}
    else{
      data<-cbind(state=rep("MW",nrow(data)),cond=rep("bf",nrow(data)),data)}
  } 
  else if(length(grep("awr",fn)) == 1){
    if(length(grep("sf",fn))==1){
      data<-cbind(state=rep("AW",nrow(data)),cond=rep("sf",nrow(data)),data)}
    else{
      data<-cbind(state=rep("AW",nrow(data)),cond=rep("bf",nrow(data)),data)}
  } 
  else if(length(grep("Aware",fn)) == 1){
    if(length(grep("sf",fn))==1){
      data<-cbind(state=rep("AW",nrow(data)),cond=rep("sf",nrow(data)),data)}
    else{
      data<-cbind(state=rep("AW",nrow(data)),cond=rep("bf",nrow(data)),data)}
  } 
  else{
    if(length(grep("sf",fn)) ==1){
      data<-cbind(state=rep("task",nrow(data)),cond=rep("sf",nrow(data)),data)}
    else{
      data<-cbind(state=rep("task",nrow(data)),cond=rep("bf",nrow(data)),data)}
  }
  data <- cbind(sub=rep(str_sub(fn, start = -5, end = -3) ,nrow(data)),data)
}

#処理
#データの入力
new_nh_ls <- list()
#前頭用の電極
chans_nh_ls <- 1:65

for(i in 1:length(nh_ls)){
  new_nh_ls[[i]] <- notime_fun(nh_ls[[i]],
                               nhep_ls[[i]],
                               chans_nh_ls)
}

notHEP <- do.call(rbind,new_nh_ls) %>% as.data.frame() %>% 
  pivot_longer(cols = 5:(length(chans_nh_ls)+4), names_to = "chans")
notHEP$value <- notHEP$value %>% as.numeric()
notHEP$Time <- notHEP$Time %>% as.numeric()
notHEP <- cbind(newc = paste0(notHEP$state,"_",notHEP$cond),
                notHEP)

plotter <- function(data,time_min,time_max,col_m){
  data %>% group_by(newc,Time) %>% 
    summarise(mean=mean(value),se=sd(value)/sqrt(n())) %>% 
    ggplot(aes(x=Time,y=mean,ymin = mean - se,ymax = mean + se))+
    theme_classic()+ 
    annotate("rect", 
             xmin = time_min, 
             xmax = time_max, 
             ymin = -Inf, ymax = Inf, alpha = 0.2)+
    theme(text = element_text(size = 16),                                                       legend.position = "none")+
    geom_ribbon(aes(fill=newc),alpha = 0.25)+ 
    geom_line(aes(col=newc),linewidth=2)+
    geom_hline(yintercept = 0, color = "black", linetype = 2,size=1)+
    geom_vline(xintercept = 0, color = "black", linetype = 2,size=1)+
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
      legend.title=element_blank())+
    xlim(-200,600)+
    scale_fill_manual(values = col_m)+
    scale_color_manual(values = col_m)
}

######################################################################
#cluster analysis BF Focus vs MW
posc <- paste0("E",c(2,3,5,6,8,9,10,11,12,13,14,19,59,60))
time_min <- 310
time_max <- 360
col_m <- c("#F3663F",
           #"#F7B04F",
           "#044C7C"
           #"#429EBE"
           )

p1 <- notHEP %>% filter(chans %in% posc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "AW") %>% 
  plotter(time_min,time_max,col_m)
notHEP %>% filter(chans %in% posc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "AW")%>% group_by(newc,Time) %>% 
  summarise(mean=mean(value),se=sd(value)/sqrt(n())) %>% 
  ggplot(aes(x=Time,y=mean,ymin = mean - se,ymax = mean + se))+
  annotate("rect", 
           xmin = time_min, 
           xmax = time_max, 
           ymin = -Inf, ymax = Inf, alpha = 0.2)+
  geom_ribbon(aes(fill=newc),alpha = 0.25)+ 
  geom_line(aes(col=newc),linewidth=2)

ggsave("../../fig3c1_time.png",plot= p1, dpi = 1200, width = 10, height = 6)

#topomap
range_min <- -0.5
range_max <- 0.5
used_ch <- posc

#datasetを，time, electrode, amplitudeの順に入れる
dataset <- notHEP %>% filter(cond == "bf") %>% 
  filter(state %in% c("task", "MW")) %>% 
  pivot_wider(
    id_cols = c(Time, chans),  
    names_from = state,       
    values_from = value,
    values_fn = mean
  ) %>% 
  mutate(amplitude = .data[["task"]] - .data[["MW"]])%>%
  dplyr::select(
    time = Time, 
    electrode = chans, 
    amplitude
  )
  
allData <- dataset %>% 
  left_join(electrodeLocs, by = "electrode")%>%
  mutate(time = as.numeric(as.character(time)))

p1t <- top_func_journal(allData,point,palette, 
                 time_min, time_max,range_min,
                 range_max,mark_use,mark_unuse,used_ch)

ggsave("../../fig3c1_topo.png",plot= p1t, dpi = 1200, width = 4, height = 4)

#cluster analysis BF Focus vs MW
negc <- paste0("E",c(22,25,26,27,28,31,33,34,36,38,40,42,45,46))
time_min <- 240
time_max <- 300
col_m <- c("#F3663F",
           #"#F7B04F",
           "#044C7C"
           #"#429EBE"
)

p2 <- notHEP %>% filter(chans %in% negc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "AW") %>% 
  plotter(time_min,time_max,col_m)
notHEP %>% filter(chans %in% negc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "AW")%>% group_by(newc,Time) %>% 
  summarise(mean=mean(value),se=sd(value)/sqrt(n())) %>% 
  ggplot(aes(x=Time,y=mean,ymin = mean - se,ymax = mean + se))+
  annotate("rect", 
           xmin = time_min, 
           xmax = time_max, 
           ymin = -Inf, ymax = Inf, alpha = 0.2)+
  geom_ribbon(aes(fill=newc),alpha = 0.25)+ 
  geom_line(aes(col=newc),linewidth=2)

ggsave("../../fig3c2_time.png",plot= p2, dpi = 1200, width = 10, height = 6)

#topomap
range_min <- -0.35
range_max <- 0.35
used_ch <- negc

#datasetを，time, electrode, amplitudeの順に入れる
dataset <- notHEP %>% filter(cond == "bf") %>% 
  filter(state %in% c("task", "MW")) %>% 
  pivot_wider(
    id_cols = c(Time, chans),  
    names_from = state,       
    values_from = value,
    values_fn = mean
  ) %>% 
  mutate(amplitude = .data[["task"]] - .data[["MW"]])%>%
  dplyr::select(
    time = Time, 
    electrode = chans, 
    amplitude
  )

allData <- dataset %>% 
  left_join(electrodeLocs, by = "electrode")%>%
  mutate(time = as.numeric(as.character(time)))

p2t <- top_func_journal(allData,point,palette, 
                 time_min, time_max,range_min,
                 range_max,mark_use,mark_unuse,used_ch)

ggsave("../../fig3c2_topo.png",plot= p2t, dpi = 1200, width = 4, height = 4)

#cluster analysis BF Focus vs SF Focus
negc <- paste0("E",c(22,24,25,26,27,29,30,32))
time_min <- 200
time_max <- 230
col_m <- c(#"#F3663F",
           #"#F7B04F",
           "#044C7C",
           "#429EBE")

p3 <- notHEP %>% filter(chans %in% negc) %>% 
  filter(state != "MW") %>% 
  filter(state != "AW") %>% 
  plotter(time_min,time_max,col_m)

notHEP %>% filter(chans %in% negc) %>% 
  filter(state != "MW") %>% 
  filter(state != "AW")%>% group_by(newc,Time) %>% 
  summarise(mean=mean(value),se=sd(value)/sqrt(n())) %>% 
  ggplot(aes(x=Time,y=mean,ymin = mean - se,ymax = mean + se))+
  annotate("rect", 
           xmin = time_min, 
           xmax = time_max, 
           ymin = -Inf, ymax = Inf, alpha = 0.2)+
  geom_ribbon(aes(fill=newc),alpha = 0.25)+ 
  geom_line(aes(col=newc),linewidth=2)

ggsave("../../fig3c3_time.png",plot= p3, dpi = 1200, width = 10, height = 6)

#topomap
range_min <- -0.35
range_max <- 0.35
used_ch <- negc

#datasetを，time, electrode, amplitudeの順に入れる
dataset <- notHEP %>% 
  filter(state == "task") %>% 
  pivot_wider(
    id_cols = c(Time, chans),  
    names_from = cond,       
    values_from = value,
    values_fn = mean
  ) %>% 
  mutate(amplitude = .data[["bf"]] - .data[["sf"]])%>%
  dplyr::select(
    time = Time, 
    electrode = chans, 
    amplitude
  )

allData <- dataset %>% 
  left_join(electrodeLocs, by = "electrode")%>%
  mutate(time = as.numeric(as.character(time)))

p3t <- top_func_journal(allData,point,palette, 
                 time_min, time_max,range_min,
                 range_max,mark_use,mark_unuse,used_ch)

ggsave("../../fig3c3_topo.png",plot= p3t, dpi = 1200, width = 4, height = 4)

##########################################################################
#cluster analysis BF Aware vs MW
posc <- paste0("E",c(2,3,5,6,8,9,10,11,12,13,14,19,60))
time_min <- 300
time_max <- 390

col_m <- c("#2D7537",
           #"#87AC75",
           "#F3663F"
           #"#F7B04F"
           )

p4<-notHEP %>% filter(chans %in% posc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "task") %>% 
  plotter(time_min,time_max,col_m)

notHEP %>% filter(chans %in% posc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "task")%>% group_by(newc,Time) %>% 
  summarise(mean=mean(value),se=sd(value)/sqrt(n())) %>% 
  ggplot(aes(x=Time,y=mean,ymin = mean - se,ymax = mean + se))+
  annotate("rect", 
           xmin = time_min, 
           xmax = time_max, 
           ymin = -Inf, ymax = Inf, alpha = 0.2)+
  geom_ribbon(aes(fill=newc),alpha = 0.25)+ 
  geom_line(aes(col=newc),linewidth=2)

ggsave("../../fig3c4_time.png",plot= p4, dpi = 1200, width = 10, height = 6)

###################topo
range_min <- -0.4
range_max <- 0.4
used_ch <- posc

#datasetを，time, electrode, amplitudeの順に入れる
dataset <- notHEP %>% filter(cond == "bf") %>% 
  filter(state %in% c("AW", "MW")) %>% 
  pivot_wider(
    id_cols = c(Time, chans),  
    names_from = state,       
    values_from = value,
    values_fn = mean
  ) %>% 
  mutate(amplitude = .data[["AW"]] - .data[["MW"]])%>%
  dplyr::select(
    time = Time, 
    electrode = chans, 
    amplitude
  )

allData <- dataset %>% 
  left_join(electrodeLocs, by = "electrode")%>%
  mutate(time = as.numeric(as.character(time)))

p4t<-top_func_journal(allData,point,palette, 
                 time_min, time_max,range_min,
                 range_max,mark_use,mark_unuse,used_ch)

ggsave("../../fig3c4_topo.png",plot= p4t, dpi = 1200, width = 4, height = 4)

#negative
negc <- paste0("E",c(26,27,28,31,33,34,36,38,40,
                     41,42,45,46,48,49,50,51))
time_min <- 240
time_max <- 350

col_m <- c("#2D7537",
           #"#87AC75",
           "#F3663F"
           #"#F7B04F"
)

p5<-notHEP %>% filter(chans %in% negc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "task") %>% 
  plotter(time_min,time_max,col_m)
notHEP %>% filter(chans %in% negc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "task")%>% group_by(newc,Time) %>% 
  summarise(mean=mean(value),se=sd(value)/sqrt(n())) %>% 
  ggplot(aes(x=Time,y=mean,ymin = mean - se,ymax = mean + se))+
  annotate("rect", 
           xmin = time_min, 
           xmax = time_max, 
           ymin = -Inf, ymax = Inf, alpha = 0.2)+
  geom_ribbon(aes(fill=newc),alpha = 0.25)+ 
  geom_line(aes(col=newc),linewidth=2)
ggsave("../../fig3c5_time.png",plot= p5, dpi = 1200, width = 10, height = 6)

###################topo
range_min <- -0.35
range_max <- 0.35
used_ch <- negc

#datasetを，time, electrode, amplitudeの順に入れる
dataset <- notHEP %>% filter(cond == "bf") %>% 
  filter(state %in% c("AW", "MW")) %>% 
  pivot_wider(
    id_cols = c(Time, chans),  
    names_from = state,       
    values_from = value,
    values_fn = mean
  ) %>% 
  mutate(amplitude = .data[["AW"]] - .data[["MW"]])%>%
  dplyr::select(
    time = Time, 
    electrode = chans, 
    amplitude
  )

allData <- dataset %>% 
  left_join(electrodeLocs, by = "electrode")%>%
  mutate(time = as.numeric(as.character(time)))

time_min <- 260
time_max <- 300

p5t <- top_func_journal(allData,point,palette, 
                 time_min, time_max,range_min,
                 range_max,mark_use,mark_unuse,used_ch)
ggsave("../../fig3c5_topo.png",plot= p5t, dpi = 1200, width = 4, height = 4)

##########################################################################
#cluster analysis SF Aware vs MW
posc <- paste0("E",c(2,3,5,6,8,9,10,11,12,56,57,59,60))
time_min <- 360
time_max <- 450

col_m <- c(#"#2D7537",
           "#87AC75",
           #"#F3663F"
           "#F7B04F")

p6<-notHEP %>% filter(chans %in% posc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "task") %>% 
  plotter(time_min,time_max,col_m)
notHEP %>% filter(chans %in% posc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "task")%>% group_by(newc,Time) %>% 
  summarise(mean=mean(value),se=sd(value)/sqrt(n())) %>% 
  ggplot(aes(x=Time,y=mean,ymin = mean - se,ymax = mean + se))+
  annotate("rect", 
           xmin = time_min, 
           xmax = time_max, 
           ymin = -Inf, ymax = Inf, alpha = 0.2)+
  geom_ribbon(aes(fill=newc),alpha = 0.25)+ 
  geom_line(aes(col=newc),linewidth=2)
ggsave("../../fig3c6_time.png",plot= p6, dpi = 1200, width = 10, height = 6)

###################topo
range_min <- -0.35
range_max <- 0.35
used_ch <- posc

#datasetを，time, electrode, amplitudeの順に入れる
dataset <- notHEP %>% filter(cond == "bf") %>% 
  filter(state %in% c("AW", "MW")) %>% 
  pivot_wider(
    id_cols = c(Time, chans),  
    names_from = state,       
    values_from = value,
    values_fn = mean
  ) %>% 
  mutate(amplitude = .data[["AW"]] - .data[["MW"]])%>%
  dplyr::select(
    time = Time, 
    electrode = chans, 
    amplitude
  )

allData <- dataset %>% 
  left_join(electrodeLocs, by = "electrode")%>%
  mutate(time = as.numeric(as.character(time)))

time_min <- 360
time_max <- 400

p6t<-top_func_journal(allData,point,palette, 
                 time_min, time_max,range_min,
                 range_max,mark_use,mark_unuse,used_ch)
ggsave("../../fig3c6_topo.png",plot= p6t, dpi = 1200, width = 4, height = 4)

############################negative
negc <- paste0("E",c(25,26,27,28,31,33,34,36,38,40,
                     42,43,44,45,46,47,48,52))
time_min <- 300
time_max <- 410

p7<-notHEP %>% filter(chans %in% negc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "task") %>% 
  plotter(time_min,time_max,col_m)
notHEP %>% filter(chans %in% negc) %>% 
  filter(cond == "bf") %>% 
  filter(state != "task")%>% group_by(newc,Time) %>% 
  summarise(mean=mean(value),se=sd(value)/sqrt(n())) %>% 
  ggplot(aes(x=Time,y=mean,ymin = mean - se,ymax = mean + se))+
  annotate("rect", 
           xmin = time_min, 
           xmax = time_max, 
           ymin = -Inf, ymax = Inf, alpha = 0.2)+
  geom_ribbon(aes(fill=newc),alpha = 0.25)+ 
  geom_line(aes(col=newc),linewidth=2)
ggsave("../../fig3c7_time.png",plot= p7, dpi = 1200, width = 10, height = 6)

###################topo
range_min <- -0.35
range_max <- 0.35
used_ch <- negc

#datasetを，time, electrode, amplitudeの順に入れる
dataset <- notHEP %>% filter(cond == "bf") %>% 
  filter(state %in% c("AW", "MW")) %>% 
  pivot_wider(
    id_cols = c(Time, chans),  
    names_from = state,       
    values_from = value,
    values_fn = mean
  ) %>% 
  mutate(amplitude = .data[["AW"]] - .data[["MW"]])%>%
  dplyr::select(
    time = Time, 
    electrode = chans, 
    amplitude
  )

allData <- dataset %>% 
  left_join(electrodeLocs, by = "electrode")%>%
  mutate(time = as.numeric(as.character(time)))

time_min <- 360
time_max <- 400

p7t<-top_func_journal(allData,point,palette, 
                 time_min, time_max,range_min,
                 range_max,mark_use,mark_unuse,used_ch)
ggsave("../../fig3c7_topo.png",plot= p7t, dpi = 1200, width = 4, height = 4)

################################################################################
resp_fun <- function(data_ls,fn,chan){
  data <- data_ls %>% t()
  colnames(data) <- data[1,]
  #この時点で時間，チャンネルのデータ
  data <- data[-1,c(1,(chan+1))]
  #MWかtaskかを入れる
  if(length(grep("Ex_R",fn)) == 1){
    if(length(grep("sf",fn))==1){
      data<-cbind(state=rep("Ex",nrow(data)),cond=rep("sf",nrow(data)),data)}
    else{
      data<-cbind(state=rep("Ex",nrow(data)),cond=rep("bf",nrow(data)),data)}
  } 
  else if(length(grep("In_R",fn)) == 1){
    if(length(grep("sf",fn))==1){
      data<-cbind(state=rep("In",nrow(data)),cond=rep("sf",nrow(data)),data)}
    else{
      data<-cbind(state=rep("In",nrow(data)),cond=rep("bf",nrow(data)),data)}
  } 
  else if(length(grep("MW_R",fn)) == 1){
    if(length(grep("sf",fn))==1){
      data<-cbind(state=rep("MW",nrow(data)),cond=rep("sf",nrow(data)),data)}
    else{
      data<-cbind(state=rep("MW",nrow(data)),cond=rep("bf",nrow(data)),data)}
  } 
  data <- cbind(sub=rep(str_sub(fn, start = -5, end = -3) ,nrow(data)),data)
}

#処理
#データの入力
new_resp_ls <- list()
#前頭用の電極
chans_resp_ls <- 1:65

for(i in 1:length(res_ls)){
  new_resp_ls[[i]] <- resp_fun(res_ls[[i]],
                               resp_ls[[i]],
                               chans_resp_ls)
}

respHEP <- do.call(rbind,new_resp_ls) %>% as.data.frame() %>% 
  pivot_longer(cols = 5:(length(chans_nh_ls)+4), names_to = "chans")
respHEP$value <- respHEP$value %>% as.numeric()
respHEP$Time <- respHEP$Time %>% as.numeric()
respHEP <- cbind(newc = paste0(respHEP$state,"_",respHEP$cond),
                respHEP)

posc <- paste0("E",c(7,15,16,20,21,22,25,26,
                     27,28,31,33,34,36,38))
time_min <- 140
time_max <- 210

col_m <- c("#3A64BD","#429EBE")
           #"#F7B04F",
           
           #"#F7804F")

p8<-respHEP %>% filter(chans %in% posc) %>% 
  filter(state != "In") %>% 
  filter(state != "MW") %>% 
  plotter(time_min,time_max,col_m)

respHEP %>% filter(chans %in% posc) %>% 
  filter(state != "In") %>% 
  filter(state != "MW")%>% group_by(newc,Time) %>% 
  summarise(mean=mean(value),se=sd(value)/sqrt(n())) %>% 
  ggplot(aes(x=Time,y=mean,ymin = mean - se,ymax = mean + se))+
  annotate("rect", 
           xmin = time_min, 
           xmax = time_max, 
           ymin = -Inf, ymax = Inf, alpha = 0.2)+
  geom_ribbon(aes(fill=newc),alpha = 0.25)+ 
  geom_line(aes(col=newc),linewidth=2)
ggsave("../../fig3c8_time.png",plot= p8, dpi = 1200, width = 10, height = 6)

###################topo
range_min <- -1.5
range_max <- 0.75
used_ch <- posc

#datasetを，time, electrode, amplitudeの順に入れる
dataset <- respHEP  %>% 
  filter(state != "In") %>% 
  filter(state != "MW") %>% 
  pivot_wider(
    id_cols = c(Time, chans),  
    names_from = cond,       
    values_from = value,
    values_fn = mean
  ) %>% 
  mutate(amplitude = .data[["bf"]] - .data[["sf"]])%>%
  dplyr::select(
    time = Time, 
    electrode = chans, 
    amplitude
  )

allData <- dataset %>% 
  left_join(electrodeLocs, by = "electrode")%>%
  mutate(time = as.numeric(as.character(time)))

time_min <- 140
time_max <- 220

p8t<-top_func_journal(allData,point,palette, 
                 time_min, time_max,range_min,
                 range_max,mark_use,mark_unuse,used_ch)
ggsave("../../fig3c8_topo.png",plot= p8t, dpi = 1200, width = 4, height = 4)

########################################################################################
cont_fun <- function(data_ls,fn,chan){
  data <- data_ls %>% t()
  colnames(data) <- data[1,]
  #この時点で時間，チャンネルのデータ
  data <- data[-1,c(1,(chan+1))]
  #MWかtaskかを入れる
  if(length(grep("emo",fn)) == 1){
    if(length(grep("DI96",fn))==1){
      data<-cbind(state=rep("emo",nrow(data)),cond=rep("nega",nrow(data)),data)}
    else if(length(grep("D128",fn)) == 1){
      data<-cbind(state=rep("emo",nrow(data)),cond=rep("neut",nrow(data)),data)}
    else{
      data<-cbind(state=rep("emo",nrow(data)),cond=rep("posi",nrow(data)),data)}
  } else if(length(grep("time",fn)) == 1){
    if(length(grep("DI96",fn))==1){
      data<-cbind(state=rep("time",nrow(data)),cond=rep("past",nrow(data)),data)}
    else if(length(grep("D128",fn)) == 1){
      data<-cbind(state=rep("time",nrow(data)),cond=rep("now",nrow(data)),data)}
    else if(length(grep("D160",fn)) == 1){
      data<-cbind(state=rep("time",nrow(data)),cond=rep("futu",nrow(data)),data)}
    else{
      data<-cbind(state=rep("time",nrow(data)),cond=rep("none",nrow(data)),data)}
  } 
  data <- cbind(sub=rep(str_sub(fn, start = -5, end = -3) ,nrow(data)),data)
}

#処理
#データの入力
new_cont_ls <- list()
chans_cont_ls <- 1:65

for(i in 1:length(co_ls)){
  new_cont_ls[[i]] <- cont_fun(co_ls[[i]],
                               cont_ls[[i]],
                               chans_cont_ls)
}

contHEP <- do.call(rbind,new_cont_ls) %>% as.data.frame() %>% 
  pivot_longer(cols = 5:(length(chans_nh_ls)+4), names_to = "chans")
contHEP$value <- contHEP$value %>% as.numeric()
contHEP$Time <- contHEP$Time %>% as.numeric()
contHEP <- cbind(newc = paste0(contHEP$state,"_",contHEP$cond),
                 contHEPHEP)








