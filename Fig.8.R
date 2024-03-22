# Evapotranspiration dynamics and partitioning in a grassed vineyard: 
# ecophysiological and computational modelling approaches
# 
# Flávio Bastos Campos
# 
# code for Fig. 8


# Libraries & functions -------------------------------

library(tidyverse)
library(egg)          # to use ggarrange function
library(scales)       # to use the function date_format() in ggplot


# Fig. 8 -----------------------------


# 2021 -----------------------------

setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/only_RDS_Data_ETpaper_review_2014_02_10")

data_d <- readRDS("Vineyard_data_Figs_3_4_8.rds") %>% as_tibble(.)


# filter by period
data_d <- data_d %>% filter(date>=as.POSIXct("2021-05-01 00:00:00", tz="GMT") & 
                              date<=as.POSIXct("2021-08-31 23:30:00", tz="GMT")) 


# for plotting aesthetics we assign NA to ET in the end of Nov 2021
data_d$ET_EC_mmd_f[data_d$date > as.POSIXct("2021-08-31 00:00:00", 
                                            format="%Y-%m-%d %H:%M:%S", 
                                            tz="GMT")] <- NA

# not showing precipitaiton with < 3 mm/d in the stripe plot
data_d <- mutate_at(data_d, c("Precip"), funs(ifelse(.<3, NA, .)))
data_d <- mutate_at(data_d, c("irrig"), funs(ifelse(.<3, NA, .)))

# re-scale for the ruller
# re-scale for the ruller
data_d$irrig <- ifelse(data_d$irrig>0, -0.15, .)
data_d$Precip <- ifelse(data_d$Precip>0, -0.15, .)


# the plot -------------------

# colors
my_colors <- c(Tec_ET= "darkblue", Tv_mod_ET= "darkorange", Tu_ET= "darkred")
my_labels <- c(expression("T"[EC]~"/ ET"[EC]),
               expression("T"[u]~"/ ET"[EC]),
               expression("Tv"[(mod)]~"/ ET"[EC]))

# for the barplot
Plt_Precip_irrig_d <- data_d %>%
  pivot_longer(c(`Precip`,`irrig`), names_to= "Variable", values_to= "Value")
my_colors_bars <- c(Precip= "blue", irrig= "magenta")
my_labels_bars <- c(expression(Irrig.), expression(Rain))

# plot
Fig_8_2021 <- ggplot(data_d, aes(x=date))+
  
  # add ratios
  geom_line(aes(y=Tec_ET, color= "Tec_ET"), size=0.6)+
  geom_line(aes(y=Tv_mod_ET, color= "Tv_mod_ET"), size=0.75)+
  geom_line(aes(y=Tu_ET, color= "Tu_ET"), size=0.4)+
  
  # define colors
  scale_color_manual(values= my_colors, 
                     labels= my_labels)+

  # bars
  geom_bar(data= Plt_Precip_irrig_d, aes(x=date, y= Value, fill= Variable),
           stat="identity", alpha=1.0, position="dodge")+
  scale_fill_manual(values= my_colors_bars, 
                    labels= my_labels_bars,
                    guide= "none")+

  # axis
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("month"),
                   labels= date_format("%b", tz="GMT"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2021-06-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                     as.POSIXct("2021-09-30 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))))+
  
  scale_y_continuous(expression("Fluxes / ET"[EC]),
                     limits= c(-0.15, 1.25),
                     breaks= seq(0, 1, 0.5))+
  
  # add lines and annotations
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  geom_hline(yintercept=1, linetype="dashed", color= "darkgrey", size= 0.5)+
  
  # years
  geom_text(x=as.POSIXct("2021-06-15 04:00:00", tz="GMT"), 
            y=1.10, 
            size=4.5, label= "2021", parse= F)+
  # layout
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 12),
        axis.title.x= element_text(size= 12, color="white"),
        axis.text.x= element_text(size= 12),
        axis.title.y= element_text(size= 12, color="black"),
        axis.text.y= element_text(size= 12),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        axis.ticks.y= element_blank(),
        legend.title= element_text(color= "white", size= 10),
        legend.position= c(0.500, 0.93),   # 0.975
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 12, colour= "black"),
        legend.key.height= unit(0.7,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.spacing.y = unit(0.0, 'cm'))

# Fig_8_2021


# 2022 -----------------------------

setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/only_RDS_Data_ETpaper_review_2014_02_10")

data_d <- readRDS("Vineyard_data_Figs_3_4_8.rds") %>% as_tibble(.)


# filter by period
data_d <- data_d %>% filter(date>=as.POSIXct("2022-05-01 00:00:00", tz="GMT") & 
                              date<=as.POSIXct("2022-08-31 23:30:00", tz="GMT")) 

# get Tu
data_d <- data_d %>% mutate(Tu= ifelse(is.na(T_TEA) | is.na(SF_fitted) | is.na(ET_EC_mmd_f), 
                                       NA, 
                                       T_TEA - SF_fitted))

# not showing precipitaiton with < 3 mm/d in the stripe plot
data_d <- mutate_at(data_d, c("Precip"), funs(ifelse(.<3, NA, .)))
data_d <- mutate_at(data_d, c("irrig"), funs(ifelse(.<3, NA, .)))

# calculate the ratios
data_d <- data_d %>% mutate(
  Tec_ET= T_TEA/ET_EC_mmd_f,
  Tv_obs_ET= SF_ud_mean/ET_EC_mmd_f,
  Tv_mod_ET= SF_fitted/ET_EC_mmd_f,
  Tu_ET= Tu/ET_EC_mmd_f)

# re-scale for the ruller
data_d$irrig <- ifelse(data_d$irrig>0, -0.15, .)
data_d$Precip <- ifelse(data_d$Precip>0, -0.15, .)


# the plot -------------------

# colors
my_colors <- c(Tec_ET= "darkblue", Tv_mod_ET= "darkorange", Tu_ET= "darkred")
my_labels <- c(expression("T"[(ec)]~"/ ET"[EC]),
               expression("T"[(u)]~"/ ET"[EC]),
               expression("Tv"[(mod)]~"/ ET"[EC]))

# for the barplot
Plt_Precip_irrig_d <- data_d %>%
  pivot_longer(c(`Precip`,`irrig`), names_to= "Variable", values_to= "Value")
my_colors_bars <- c(Precip= "blue", irrig= "magenta")
my_labels_bars <- c(expression(Irrig.), expression(Rain))

# plot
Fig_8_2022 <- ggplot(data_d, aes(x=date))+
  
  # add ratios
  geom_line(aes(y=Tec_ET, color= "Tec_ET"), size=0.6)+
  geom_line(aes(y=Tv_mod_ET, color= "Tv_mod_ET"), size=0.75)+
  geom_line(aes(y=Tu_ET, color= "Tu_ET"), size=0.4)+
  
  # define colors
  scale_color_manual(values= my_colors, 
                     labels= my_labels)+

  # bars
  geom_bar(data= Plt_Precip_irrig_d, aes(x=date, y= Value, fill= Variable),
           stat="identity", alpha=1.0, position="dodge")+
  scale_fill_manual(values= my_colors_bars, 
                    labels= my_labels_bars,
                    guide= "none")+
  # axis
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("month"),
                   labels= date_format("%b", tz="GMT"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2022-06-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                     as.POSIXct("2022-08-31 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))))+
  
  scale_y_continuous(expression( ),
                     limits= c(-0.15, 1.25),
                     breaks= seq(0, 1, 0.5))+
  
  # add lines and annotations
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  geom_hline(yintercept=1, linetype="dashed", color= "darkgrey", size= 0.5)+
  
  # years
  geom_text(x=as.POSIXct("2022-06-18 04:00:00", tz="GMT"), 
            y=1.10, 
            size=4.5, label= "2022", parse= F)+
  
  # layout
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 12),
        axis.title.x = element_text(size= 12, color="white"),
        axis.text.x= element_text(size= 12),
        axis.text.y= element_blank(),
        axis.line.y.right= element_blank(),
        axis.line.y.left= element_blank(),
        axis.title.y= element_blank(),
        axis.ticks.y= element_blank(),
        legend.position= "none")

# Fig_8_2022


# boxplot -----------------------------

# load
setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/only_RDS_Data_ETpaper_review_2014_02_10")

data_d <- readRDS("Vineyard_data_Figs_3_4_8.rds") %>% as_tibble(.)

# get Year and month
data_d <- data_d %>% mutate(
  Year= year(date),
  month= month(date))

# get months 5 to 8
dat_box <- data_d %>% filter(
  (Year=2021 & (month>=5 & month<=8)) |
    (Year=2022 & (month>=5 & month<=8)))

dat_box$Year <- as.factor(dat_box$Year)


# get Tu
data_d <- data_d %>% mutate(Tu= T_TEA - SF_fitted)

# calculate the ratios
data_d <- data_d %>% mutate(
  Tec_ET= T_TEA/ET_EC_mmd_f,
  Tv_obs_ET= SF_ud_mean/ET_EC_mmd_f,
  Tv_mod_ET= SF_fitted/ET_EC_mmd_f,
  Tu_ET= Tu/ET_EC_mmd_f)


# merge data_d_2021 with data_d_2022
dat_box <- data_d %>% filter(
  (Year=2021 & (month>=6 & month<=9)) |
    (Year=2022 & (month>=6 & month<=8)))

dat_box$Year <- as.factor(dat_box$Year)

# pivot
dat_box_piv <- dat_box %>%
  pivot_longer(c(`Tec_ET`, `Tv_mod_ET`, `Tu_ET`), 
               names_to= "Variable", values_to= "Value")


my_colors <- c(Tec_ET= "darkblue",
               Tv_mod_ET= "darkorange", Tu_ET= "darkred")

# plot
Fig_8_box <- ggplot(dat_box_piv, aes(x=factor(Year), y=Value, fill=factor(Variable))) +
  geom_boxplot(width= 0.50, outlier.colour = "darkgrey", outlier.shape= 1)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red",
               position= position_dodge2(width= 0.50, preserve = "single"))+
  scale_color_manual(values= rep("black", times=5))+
  scale_fill_manual(values= my_colors)+
  scale_alpha_manual(values= rep(0.7, times=5))+
  
  # y axis
  scale_y_continuous(name=NULL,
                     limits= c(-0.15, 1.25),    # does not work inside 2nd axis function
                     sec.axis= sec_axis(~., 
                                        name= expression(),
                                        breaks= seq(0, 1, 0.5)))+
  guides(y= "none")+
  
  # add lines and annotations
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  geom_hline(yintercept=1, linetype="dashed", color= "darkgrey", size= 0.5)+
  
  # layout
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 12),
        axis.title.x = element_text(size= 10, color="white"),
        axis.text.x= element_text(size= 10),
        axis.text.y= element_text(size= 12),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.position="none")

# Fig_8_box


# panel ------------------------------

Fig_8_panel <- ggarrange(Fig_8_2021, Fig_8_2022, Fig_8_box,
                         heights= c(1.0), widths= c(1.0, (3.0/4.0), 0.30),
                         nrow=1, ncol=3)

# Save

setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/out")
ggsave("Fig_8.png", plot= Fig_8_panel, width= 27.0, height= 10.0, units= "cm")


# done.

