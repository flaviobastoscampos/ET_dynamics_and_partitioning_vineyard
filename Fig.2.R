# Evapotranspiration dynamics and partitioning in a grassed vineyard: 
# ecophysiological and computational modelling approaches
# 
# Flávio Bastos Campos
# 
# code for Fig. 2


# Libraries & functions -------------------------------

library(tidyverse)
library(egg)          # to use ggarrange function
library(scales)       # to use the function date_format() in ggplot


# Fig. 2 -------------------------------

setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/only_RDS_Data_ETpaper_review_2014_02_10")

data_d <- readRDS("Vineyard_data_Fig_2.rds") %>% as_tibble(.)


# rename to plot
names(data_d)[names(data_d)=="Rn_MJ"] <- "Rn"
names(data_d)[names(data_d)=="Tair"] <- "Tair"
names(data_d)[names(data_d)=="Precip"] <- "Precip"
names(data_d)[names(data_d)=="VPD_KPa"] <- "VPD"
names(data_d)[names(data_d)=="ET_EC_mmd_f"] <- "ET"
names(data_d)[names(data_d)=="GPP_nt_gCd"] <- "GPP"


# re-scale for plotting
data_d$VPD <- 10.0*(data_d$VPD) 
data_d$Precip <- 1.0*(data_d$Precip) 
data_d$ET <- 3.2*(data_d$ET)


# pt 1  --------------------------------------

Plt_Tair_VPD_d <- data_d %>%
  pivot_longer(c(`Tair`,`VPD`), names_to= "Variable", values_to= "Value")

lt <- c("solid", "solid")
sz <- c(0.6, 0.6)

# for the barplot
Plt_Precip_irrig_d <- data_d %>%
  pivot_longer(c(`Precip`,`irrig`), names_to= "Variable", values_to= "Value")

my_colors <- c(Precip= "blue", irrig= "magenta")

my_labels <- c(expression(Irrigation), expression(Rainfall))


plot_Plt_Tair_VPD <- ggplot(Plt_Tair_VPD_d, aes(date, Value))+
  geom_line(aes(color= Variable, size= Variable, linetype= Variable))+
  scale_color_manual(values= c("red", "black"))+
  scale_size_manual(values= sz)+ scale_linetype_manual(values= lt)+
  scale_y_continuous(expand = c(0, 0),
                     expression(Precip~(mm~d^-~1)~Irrig~(mm~d^-~1)~"; "~"T"[air]~("°C")),
                     breaks=seq(0.0, 50.0, 10.0),
                     sec.axis= sec_axis(~./10.0, name= expression(VPD~(KPa)),
                                        breaks= seq(0.0, 4.0, 1.0)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("month"),
                   labels= date_format("%b", tz="GMT"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2021-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                     as.POSIXct("2022-08-31 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))))+
  
  # bars
  geom_bar(data= Plt_Precip_irrig_d, aes(x=date, y= Value, fill= Variable),
           stat="identity", alpha=0.6)+
  scale_fill_manual(values= my_colors, labels= my_labels)+
  
  geom_text(x=as.POSIXct("2021-05-12 04:00:00", tz="GMT"), 
            y=50.00,
            size=7.0, label= "A", parse= F)+
  
  # years
  geom_text(x=as.POSIXct("2021-06-25 04:00:00", tz="GMT"), 
            y=49, 
            size=4.5, label= "2021", parse= F)+
  
  geom_text(x=as.POSIXct("2022-01-25 04:00:00", tz="GMT"), 
            y=49, 
            size=4.5, label= "2022", parse= F)+
  
  
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 11),
        axis.title.x = element_text(size= 11, color="white"),
        axis.text.x= element_text(size= 11),
        axis.text.y= element_text(size= 11),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.title= element_text(color= "white", size= 10),
        legend.position= c(0.850, 0.910),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 12, colour= "black"),
        legend.key.height= unit(0.7,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.direction="vertical",
        legend.box="horizontal",
        legend.spacing.y = unit(0.0, 'cm'))

# plot_Plt_Tair_VPD



# pt 2 ------------

# colors
my_colors <- c(ET= "blue", Rn= "#fdbb84")

my_labels <- c(expression(ET[EC]), expression(Rn))

# plot
plot_Plt_ET_GPP_Rn <- ggplot(data_d)+
  # start with blank
  geom_blank()+
  # add area
  geom_area(aes(x=date, y=Rn), fill="#fdbb84", color= "transparent", alpha= 1.0)+
  # add ET - continuous timeline
  geom_line(aes(x=date, y=Rn, color= "Rn"))+
  geom_line(aes(x=date, y=ET, color= "ET"), size=0.6)+
  # define colors
  scale_color_manual(values= my_colors, labels= my_labels)+
  # axes
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("month"),
                   labels= date_format("%b", tz="GMT"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2021-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                     as.POSIXct("2022-08-31 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))))+
  
  scale_y_continuous(expand = c(0, 0),
                     expression(Rn~(MJ~m^-~2~d^-~1)),
                     breaks=seq(0.0, 30.0, 5.0),
                     limits= c(-3.5, 20.5),
                     sec.axis= sec_axis(~./3.2, name= expression(ET~(mm~d^-~1)),  # careful
                                        breaks= seq(0.0, 7.0, 1.0)))+
  
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  
  geom_text(x=as.POSIXct("2021-05-12 04:00:00", tz="GMT"), 
            y=19.50, 
            size=7.0, label= "B", parse= F)+
  
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 11),
        axis.title.x = element_text(size= 11, color="white"),
        axis.text.x= element_text(size= 11),
        axis.text.y= element_text(size= 11),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.title= element_text(color= "white", size= 10),
        legend.position= c(0.62, 0.92),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 12, colour= "black"),
        legend.key.height= unit(0.7,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.spacing.y = unit(0.0, 'cm'))

# plot_Plt_ET_GPP_Rn


# panel ----------------------------

plot_meteo_season_2021_2022 <- ggarrange(plot_Plt_Tair_VPD, 
                                         plot_Plt_ET_GPP_Rn, nrow=2, ncol=1)

setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/out")
ggsave("Fig_2.png", plot= plot_meteo_season_2021_2022, width= 25.0, height= 18.0, units= "cm")


# done.


