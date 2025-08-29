#Plot HEP and topomap
#library
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(tidyverse) 
library(akima)
library(scales)
library(mgcv)
library(grid)
library(pals)

#settings
dir_r <- "root dir" 
ch_dir <- "channel dir"

mark_use <- 19
mark_unuse<-1

baseline <- -200
forhz <- -196
step <- abs(abs(baseline)-abs(forhz))

point <- 1:250

jet_palette_function <- colorRampPalette(
  c("#00007F", "blue", "#007FFF", "cyan", 
    "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
)
parula_palette_function <- pals::parula(256)
palette <- parula_palette_function

#######################################################################
#ready for topomap
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
  
  hole_radius <- 0.71 
  
  mask_radius <- 2.0  

  angles <- seq(0, 2 * pi, length.out = 200)

  hole_df <- data.frame(
    x = hole_radius * cos(angles), 
    y = hole_radius * sin(angles)
  )
  
  outer_df <- data.frame(
    x = mask_radius * cos(rev(angles)), 
    y = mask_radius * sin(rev(angles))
  )
  
  mask_polygon <- rbind(hole_df, outer_df)
  #################################################
  ggplot(GAPtp, aes(x, y, fill = amplitude)) + 
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

dir_nh <- paste0(dir_r,"./HEP")  

setwd(dir_nh)
nhep_ls <- list.files()
nh_ls <- list() 
nh_ls <- lapply(nhep_ls,fread)

notime_fun <- function(data_ls,fn,chan){
  data <- data_ls %>% t()
  colnames(data) <- data[1,]
  data <- data[-1,c(1,(chan+1))]
  if(length(grep("MW",fn)) == 1){
    if(length(grep("sf",fn))==1){
      data<-cbind(state=rep("MW",nrow(data)),cond=rep("sf",nrow(data)),data)}
    else{
      data<-cbind(state=rep("MW",nrow(data)),cond=rep("bf",nrow(data)),data)}
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


new_nh_ls <- list()
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
#plot example BF Focus vs MW
sig_chs <- fread("sig_chs.csv")
posc <- sig_chs
time_min <- 310
time_max <- 360
col_m <- c("#F3663F",
           "#044C7C"
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

ggsave("../../fig3B.png",plot= p1, dpi = 1200, width = 10, height = 6)

#topomap
range_min <- -0.5
range_max <- 0.5
used_ch <- posc

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

ggsave("../../fig3B_topo.png",plot= p1t, dpi = 1200, width = 4, height = 4)


