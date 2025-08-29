#topomap generation for fig 2
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(patchwork)
library(ggridges)
library(data.table)
library(gridExtra)
library(viridis) 
library(tidyverse) 
library(permuco)
library(buildmer)
library(akima)
library(scales)
library(mgcv)
library(png)
library(grid)

setwd("data dir")
freq <- fread("freq_lab.csv") %>% as.vector() %>% unlist()

ch_dir <- "ch_dir"


mark_use <- 19
mark_unuse<-1
used_ch<- paste0("E",1:65)

jet_palette_function <- colorRampPalette(
  c("#00007F", "blue", "#007FFF", "cyan", 
    "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
)
parula_palette_function <- pals::parula(256)
palette <- parula_palette_function
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

top_func_journal<-function(data, palette, minF, maxF,minv,maxv,yesm,nom,use){
  jet.colors <- palette
  pd_list <- list()
  i<-1
  
  singleTimepoint <- filter(data,between(freq,minF,maxF)) %>% 
    group_by(electrode,chanNo,theta,radius,radianTheta,x,y) %>% 
    summarize(amplitude = mean(amplitude))
  
  gridRes <- 50 
  
  tmpTopo <- with(singleTimepoint,
                  interp(x = x, y = y, z = amplitude,
                         xo = seq(min(x)*2,
                                  max(x)*2,
                                  length = gridRes),
                         yo = seq(min(y)*2,
                                  max(y)*2,
                                  length = gridRes),
                         linear = FALSE,
                         extrap = TRUE)) 
  
  interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)
  
  names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y
  
  interpTopo <- gather(interpTopo,
                       key = y,
                       value = amplitude,
                       -x,
                       convert = TRUE)
  
  interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7
  
  interpTopo <- interpTopo[interpTopo$incircle,] 
  
  maskRing <- circleFun(diameter = 1.42) 
  
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
               size = 2) +
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

dataset1 <- cbind(chans = paste0("E",1:65),
                  fread("stat_sf_comp.csv")) %>% 
  set_names(c("chans", freq)) %>% 
  pivot_longer(cols=2:76,
               names_to = "freq",
               values_to = "amplitude") %>% 
  mutate(freq = freq %>% as.numeric(),
         electrode = chans) %>% 
  dplyr::select(electrode,freq,amplitude)

allData1 <- dataset1 %>% 
  left_join(electrodeLocs, by = "electrode") 

dataset2 <- cbind(chans = paste0("E",1:65),
                  fread("stat_bf_awcomp.csv")) %>% 
  set_names(c("chans", freq)) %>% 
  pivot_longer(cols=2:76,
               names_to = "freq",
               values_to = "amplitude") %>% 
  mutate(freq = freq %>% as.numeric(),
         electrode = chans) %>% 
  dplyr::select(electrode,freq,amplitude)

allData2 <- dataset2 %>% 
  left_join(electrodeLocs, by = "electrode") 

#topomap
range_min <- -8.5
range_max <- 5
freq_min <- 15
freq_max <- 16
topo1 <- top_func_journal(allData1,palette, 
                 freq_min, freq_max,range_min,
                 range_max,mark_use,mark_unuse,used_ch)

freq_min <- 8
freq_max <- 10
topo2 <- top_func_journal(allData1,palette, 
                 freq_min, freq_max,range_min,
                 range_max,mark_use,mark_unuse,used_ch)

#topomap
range_min <- -4.0
range_max <- 7.5
freq_min <- 10
freq_max <- 11
topo3 <- top_func_journal(allData2,palette, 
                freq_min, freq_max,range_min,
                range_max,mark_use,mark_unuse,used_ch)

range_min <- -4.0
range_max <- 7.5
freq_min <- 20
freq_max <- 21
topo4 <- top_func_journal(allData2,palette, 
                 freq_min, freq_max,range_min,
                 range_max,mark_use,mark_unuse,used_ch)

ggsave("fig2_topo1.png",plot= topo1, dpi = 1200, width = 4, height = 4)
ggsave("fig2_topo2.png",plot= topo2, dpi = 1200, width = 4, height = 4)
ggsave("fig2_topo3.png",plot= topo3, dpi = 1200, width = 4, height = 4)
ggsave("fig2_topo4.png",plot= topo4, dpi = 1200, width = 4, height = 4)




