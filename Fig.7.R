# Evapotranspiration dynamics and partitioning in a grassed vineyard: 
# ecophysiological and computational modelling approaches
# 
# Flávio Bastos Campos
# 
# code for Fig. 7


# Libraries & functions -------------------------------

library(tidyverse)
library(egg)          # to use ggarrange function
library(scales)       # to use the function date_format() in ggplot


# Fig. 7 -------------------------------

setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/only_RDS_Data_ETpaper_review_2014_02_10")
data <- readRDS("Vineyard_data_Fig_7.rds")

# mean patterns over a day
lt <- c("dashed", "dashed", "solid", "solid")
sz <- c(1.50, 1.30, 1.50, 1.50)

plot_A <- ggplot(data, aes(hour))+
  
  geom_line(aes(y= Tv, color= "Tv"), linetype="solid", size=1.5)+
  geom_line(aes(y= ETu, color= "ETu"), linetype="solid", size=1.3)+
  geom_line(aes(y= F_EPA, color= "F_EPA"), linetype="dashed", size=1.5)+
  geom_line(aes(y= ET, color= "ET"), linetype="dashed", size=1.5)+
  
  geom_ribbon(aes(y= Tv, ymin= Tv - Tv_sd, ymax= Tv + Tv_sd, fill= "Tv"), alpha=0.25)+
  geom_ribbon(aes(y= ETu, ymin= ETu - ETu_sd, ymax= ETu + ETu_sd, fill= "ETu"), alpha=0.25)+
  geom_ribbon(aes(y= F_EPA, ymin= F_EPA - F_EPA_sd, ymax= F_EPA + F_EPA_sd, fill= "F_EPA"), alpha=0.25)+
  geom_ribbon(aes(y= ET, ymin= ET - ET_sd, ymax= ET + ET_sd, fill= "ET"), alpha=0.25)+
  
  # define colors
  scale_color_manual(values= c("blue", "red", "black", "darkgreen"), 
                     labels= c(expression(ET[EC]), 
                               expression(ET[u]), 
                               expression(ET[EPA]),
                               expression(Tv[(mod)])))+
  
  scale_fill_manual(values= c("blue", "red", "black", "darkgreen"), 
                    labels= c(expression(ET[EC]), 
                              expression(ET[u]), 
                              expression(ET[EPA]),
                              expression(Tv[(mod)])),
                    guide= "none")+

    # axis
  scale_y_continuous(expression(Flux~(mm~h^-~1)),
                     breaks=seq(0.0, 0.7, 0.2),
                     limits= c(0.0, 0.60))+
  scale_x_continuous(expression("Hour of the day"~(h)), breaks= seq(6, 18, 6),
                     limits = c(5, 19))+
  # lines
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  
  # layout
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 11),
        axis.title.x= element_text(size= 11, color="black"),
        axis.text.x= element_text(size= 11),
        axis.title.y= element_text(size= 11, color="black"),
        axis.text.y= element_text(size= 11),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        axis.ticks.y= element_blank(),
        legend.title= element_text(color= "white", size= 10),
        legend.position= c(0.50, 0.96),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 12, colour= "black"),
        legend.key.height= unit(0.7,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.spacing.y = unit(0.0, 'cm'))

# plot_A

# Save
setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/out")
ggsave("Fig_7.png", plot= plot_A, width= 11.0, height= 10.0, units= "cm")


# done.


