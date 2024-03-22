# Evapotranspiration dynamics and partitioning in a grassed vineyard: 
# ecophysiological and computational modelling approaches
# 
# Flávio Bastos Campos
# 
# code for Fig. 4


# Libraries & functions -------------------------------

library(tidyverse)
library(egg)          # to use ggarrange function
library(scales)       # to use the function date_format() in ggplot


# Fig. 4 -------------------------------


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
data_d$irrig <- ifelse(data_d$irrig>0, -0.75, .)
data_d$Precip <- ifelse(data_d$Precip>0, -0.75, .)


# the plot -------------------

# colors
my_colors <- c(ET_EC_mmd_f= "lightblue", SF_fitted= "darkorange")
my_labels <- c(expression(ET[EC]), expression(Tv[(mod)]))

# for the barplot
Plt_Precip_irrig_d <- data_d %>%
  pivot_longer(c(`Precip`,`irrig`), names_to= "Variable", values_to= "Value")
my_colors_bars <- c(Precip= "blue", irrig= "magenta")
my_labels_bars <- c(expression(Irrig.), expression(Rain))


# plot
plot_Fig_4A_2021 <- ggplot(data_d, aes(x=date))+
  
  geom_area(aes(y=ET_EC_mmd_f), fill= "lightblue", color= "lightblue", alpha= 0.4)+
  
  # add fluxes
  geom_line(aes(y=ET_EC_mmd_f, color= "ET_EC_mmd_f"))+
  geom_line(aes(y=SF_fitted, color= "SF_fitted"), size=0.6)+
  
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
                     as.POSIXct("2021-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                     as.POSIXct("2021-08-31 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))))+
  
  scale_y_continuous(expression(Fluxes~(mm~d^-~1)),
                     limits= c(-0.75, 6.5),
                     breaks= seq(0, 6, 1.5))+
  
  # add lines and annotations
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  
  # years
  geom_text(x=as.POSIXct("2021-05-25 04:00:00", tz="GMT"), 
            y=6.0, 
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
        legend.position="none")

#  plot_Fig_4A_2021



# 2022 -----------------------------

setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/only_RDS_Data_ETpaper_review_2014_02_10")

data_d <- readRDS("Vineyard_data_Figs_3_4_8.rds") %>% as_tibble(.)


# filter by period
data_d <- data_d %>% filter(date>=as.POSIXct("2022-05-01 00:00:00", tz="GMT") & 
                              date<=as.POSIXct("2022-08-31 23:30:00", tz="GMT")) 

# not showing precipitaiton with < 3 mm/d in the stripe plot
data_d <- mutate_at(data_d, c("Precip"), funs(ifelse(.<3, NA, .)))
data_d <- mutate_at(data_d, c("irrig"), funs(ifelse(.<3, NA, .)))

# re-scale for the ruller
data_d$irrig <- ifelse(data_d$irrig>0, -0.75, .)
data_d$Precip <- ifelse(data_d$Precip>0, -0.75, .)



# the plot -------------------

# colors
my_colors <- c(ET_EC_mmd_f= "lightblue", SF_ud_mean= "darkgreen", 
               SF_fitted= "darkorange")

my_labels <- c(expression(ET[EC]), expression(Tv[(mod)]),
               expression(Tv[(obs)]))

# for the barplot
Plt_Precip_irrig_d <- data_d %>%
  pivot_longer(c(`Precip`,`irrig`), names_to= "Variable", values_to= "Value")
my_colors_bars <- c(Precip= "blue", irrig= "magenta")
my_labels_bars <- c(expression(Irrig.), expression(Rain))

# plot
plot_Fig_4A_2022 <- ggplot(data_d, aes(x=date))+
  
  # add ET_EC_mmd
  geom_area(aes(y=ET_EC_mmd_f), fill= "lightblue", color= "lightblue", alpha= 0.4)+
  
  # add fluxes
  geom_line(aes(y=ET_EC_mmd_f, color= "ET_EC_mmd_f"))+ 
  geom_line(aes(y=SF_fitted, color= "SF_fitted"), size=0.6)+
  geom_line(aes(y=SF_ud_mean, color= "SF_ud_mean"), size=0.6)+
  
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
                     as.POSIXct("2022-05-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                     as.POSIXct("2022-08-31 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))))+
  
  scale_y_continuous(expression(),
                     limits= c(-0.75, 6.5),
                     breaks= seq(0, 6, 1.5))+
  
  # add lines and annotations
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  
  # years
  geom_text(x=as.POSIXct("2022-05-25 04:00:00", tz="GMT"), 
            y=6.0, 
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
        legend.title= element_text(color= "white", size= 10),
        legend.position= c(0.670, 0.975),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 12, colour= "black"),
        legend.key.height= unit(0.7,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.spacing.y = unit(0.0, 'cm'))

# plot_Fig_4A_2022



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

dat_box_stats <- dat_box


# the plot -------------------

# pivot
dat_box_piv <- dat_box %>%
  pivot_longer(c(`ET_EC_mmd_f`, `SF_ud_mean`, `SF_fitted`), 
               names_to= "Variable", values_to= "Value")

my_colors <- c(ET_EC_mmd_f= "lightblue", SF_ud_mean= "darkgreen",
               SF_fitted= "darkorange")

# plot
Fig_4_box <- ggplot(dat_box_piv, aes(x=factor(Year), y=Value, fill=factor(Variable)))+
  
  geom_boxplot(width=0.50, outlier.colour="darkgrey", outlier.shape=1)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red",
               position= position_dodge2(width= 0.50, preserve="single"))+
  
  scale_color_manual(values= rep("black", times=5))+
  scale_fill_manual(values= my_colors)+
  scale_alpha_manual(values= rep(0.7, times=5))+
  
  # y axis
  scale_y_continuous(name=NULL,
                     limits= c(-0.75, 6.5),
                     sec.axis= sec_axis(~., 
                                        name= expression(),
                                        breaks= seq(0, 6, 1.5)))+
  guides(y= "none")+
  
  # add lines and annotations
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  
  # layout
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 12),
        axis.title.x = element_text(size= 12, color="white"),
        axis.text.x= element_text(size= 12),
        axis.text.y= element_text(size= 12),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.position="none")

# Fig_4_box

# panel
Fig_4_panel <- ggarrange(plot_Fig_4A_2021, plot_Fig_4A_2022, Fig_4_box,
                         heights= c(1.0), widths= c(1.0, 1.0, 0.40),
                         nrow=1, ncol=3)


setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/out")
ggsave("Fig_4.png", plot= Fig_4_panel, width= 27.0, height= 9.0, units= "cm")


# done.



