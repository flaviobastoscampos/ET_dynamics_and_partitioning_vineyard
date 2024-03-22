# Evapotranspiration dynamics and partitioning in a grassed vineyard: 
# ecophysiological and computational modelling approaches
# 
# Flávio Bastos Campos
# 
# code for Fig. 5 (campaigns panel)


# Libraries & functions -------------------------------

library(tidyverse)
library(egg)          # to use ggarrange function
library(scales)       # to use the function date_format() in ggplot


# Fig. 5 -------------------------------


# initial loop ----------------------------------

for (i in c(1:5)) {
  # Load
  setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/only_RDS_Data_ETpaper_review_2014_02_10")
  Camp_list <- readRDS("Vineyard_data_list_Fig_5.rds")
  
  data <- Camp_list[[i]]
  
  # rename to plot
  names(data)[names(data)=="ETu_mmh_av_act"] <- "ETu"
  names(data)[names(data)=="ET_EC_mmh_f"] <- "ET"
  names(data)[names(data)=="Precip"] <- "Precip"
  
  # in order to not show Precip=0 in the plots, give them NA
  data <- mutate_at(data, c("Precip"), funs(ifelse(.==0, NA, .)))
  
  # re-scale
  data$Precip <- (data$Precip)/11.0

  assign(paste("data_Camp_", i, sep=""), data )
  }


# plots ---------------------------

# _1 ---------------------------

# set the data
data <- data_Camp_1

plot_Precip_ETu_1 <- ggplot(data, aes(x=date, y=ET))+
  
  # start with blank
  geom_blank()+
  
  # D area (from 7:00 to 19:00)
  annotate("rect", ymin= 0.00, ymax= 0.75, alpha=0.10, fill= "blue",
           xmin= as.POSIXct("2021-07-07 07:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"),
           xmax= as.POSIXct("2021-07-07 19:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+
  
  # add Precip
  geom_col(data= data, aes(x=date, y=Precip, fill="Precip"), alpha=0.5, colour= "blue")+
  scale_fill_manual(values="blue")+
  
  # add ET
  geom_line(data=data, aes(x=date, y=ET, color= "ET"), alpha=1.0, size=1.0)+
  
  # add ETu
  geom_line(data=data, aes(x=date, y=ETu, color= "ETu"), alpha=1.0, size=1.5)+
  
  # add line colours
  scale_color_manual(name = "Colors", 
                     values = c("ET" = "black", "ETu" = "darkorange"))+
  
  # axis
  scale_y_continuous(expand = c(0, 0),
                     expression( ),
                     breaks=seq(0.00, 0.75, 0.25),
                     sec.axis= sec_axis(~.*11.0, name= expression( ),    # adjust scale here
                                        breaks= seq(0.0, 9.25, 2.5)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("day"),
                   labels= date_format("%b %d", tz="GMT"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2021-07-03 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                     as.POSIXct("2021-07-09 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))))+

  geom_text(x=as.POSIXct("2021-07-03 18:00:00", tz="GMT"), 
            y=0.72, 
            size=5.0, label= "Camp 1", parse=F, col="blue")+
  
  geom_hline(yintercept=0, linetype= "dashed", color= "darkgrey", size= 0.5)+
  
  # D label
  geom_text(x=as.POSIXct("2021-07-07 13:00:00", tz="GMT"), 
            y=0.70, 
            size=6.0, label= "D3", parse=F, color="darkred")+

  # Day label
  geom_text(x=as.POSIXct("2021-07-07 13:00:00", tz="GMT"), 
            y=0.60, 
            size=4.0, label= "(day 1)", parse=F, color="black")+

  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 13),
        axis.title.x = element_text(size= 13, color="white"),
        axis.text.x= element_text(size= 13),
        axis.text.y= element_text(size= 13),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.position="none")

# plot_Precip_ETu_1



# _2 ---------------------------

# set the data
data <- data_Camp_2

plot_Precip_ETu_2 <- ggplot(data, aes(x=date, y=ET))+
  
  # start with blank
  geom_blank()+
  
  # D area (from 7:00 to 19:00)
  annotate("rect", ymin= 0.00, ymax= 0.75, alpha=0.10, fill= "blue",
           xmin= as.POSIXct("2021-07-21 07:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"),
           xmax= as.POSIXct("2021-07-21 19:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+

    # add Precip
  geom_col(data= data, aes(x=date, y=Precip, fill="Precip"), alpha=0.5, colour= "blue")+
  scale_fill_manual(values="blue")+
  
  # add ET
  geom_line(data=data, aes(x=date, y=ET, color= "ET"), alpha=1.0, size=1.0)+
  
  # add ETu
  geom_line(data=data, aes(x=date, y=ETu, color= "ETu"), alpha=1.0, size=1.5)+
  
  # add line colours
  scale_color_manual(name = "Colors", 
                     values = c("ET" = "black", "ETu" = "darkorange"))+
  
  # axis
  scale_y_continuous(expand = c(0, 0),
                     expression(Fluxes~(mm~h^-~1)),
                     breaks=seq(0.00, 0.75, 0.25),
                     sec.axis= sec_axis(~.*11.0, name= expression( ),    # adjust scale here
                                        breaks= seq(0.0, 9.25, 2.5)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("day"),
                   labels= date_format("%b %d", tz="GMT"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2021-07-17 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                     as.POSIXct("2021-07-23 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))))+

    geom_text(x=as.POSIXct("2021-07-17 18:00:00", tz="GMT"), 
            y=0.72, 
            size=5.0, label= "Camp 2", parse=F, col="blue")+
  
  geom_hline(yintercept=0, linetype= "dashed", color= "darkgrey", size= 0.5)+
  
  # D label
  geom_text(x=as.POSIXct("2021-07-21 13:00:00", tz="GMT"), 
            y=0.70, 
            size=6.0, label= "D3+", parse=F, color="darkred")+
  
  # Day label
  geom_text(x=as.POSIXct("2021-07-21 13:00:00", tz="GMT"), 
            y=0.60, 
            size=4.0, label= "(day 2)", parse=F, color="black")+
  
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 13),
        axis.title.x = element_text(size= 13, color="white"),
        axis.text.x= element_text(size= 13),
        axis.text.y= element_text(size= 13),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.position="none")

# plot_Precip_ETu_2



# _3 ---------------------------

# set the data
data <- data_Camp_3

plot_Precip_ETu_3 <- ggplot(data, aes(x=date, y=ET))+
  
  # start with blank
  geom_blank()+
  
  # D area (from 7:00 to 19:00)
  annotate("rect", ymin= 0.00, ymax= 0.75, alpha=0.10, fill= "blue",
           xmin= as.POSIXct("2021-08-09 07:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"),
           xmax= as.POSIXct("2021-08-09 19:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+
  
  # D area (from 7:00 to 19:00)
  annotate("rect", ymin= 0.00, ymax= 0.75, alpha=0.10, fill= "blue",
           xmin= as.POSIXct("2021-08-10 07:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"),
           xmax= as.POSIXct("2021-08-10 19:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+
  
  # D area (from 7:00 to 19:00)
  annotate("rect", ymin= 0.00, ymax= 0.75, alpha=0.10, fill= "blue",
           xmin= as.POSIXct("2021-08-11 07:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"),
           xmax= as.POSIXct("2021-08-11 19:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+
  
  # D label
  geom_text(x=as.POSIXct("2021-08-09 13:00:00", tz="GMT"), 
            y=0.70, 
            size=6.0, label= "D2", parse=F, color="darkred")+
  
  # Day label
  geom_text(x=as.POSIXct("2021-08-09 13:00:00", tz="GMT"), 
            y=0.60, 
            size=4.0, label= "(day 3)", parse=F, color="black")+
  
  
  # D label
  geom_text(x=as.POSIXct("2021-08-10 13:00:00", tz="GMT"), 
            y=0.70, 
            size=6.0, label= "D3", parse=F, color="darkred")+
  
  # Day label
  geom_text(x=as.POSIXct("2021-08-10 13:00:00", tz="GMT"), 
            y=0.60, 
            size=4.0, label= "(day 4)", parse=F, color="black")+
  
  # D label
  geom_text(x=as.POSIXct("2021-08-11 13:00:00", tz="GMT"), 
            y=0.70, 
            size=6.0, label= "D3+", parse=F, color="darkred")+
  
  # Day label
  geom_text(x=as.POSIXct("2021-08-11 13:00:00", tz="GMT"), 
            y=0.60, 
            size=4.0, label= "(day 5)", parse=F, color="black")+
  
  # add Precip
  geom_col(data= data, aes(x=date, y=Precip, fill="Precip"), alpha=0.5, colour= "blue")+
  scale_fill_manual(values="blue")+
  
  # add ET
  geom_line(data=data, aes(x=date, y=ET, color= "ET"), alpha=1.0, size=1.0)+
  
  # add ETu
  geom_line(data=data, aes(x=date, y=ETu, color= "ETu"), alpha=1.0, size=1.5)+
  
  # add line colours
  scale_color_manual(name = "Colors", 
                     values = c("ET" = "black", "ETu" = "darkorange"))+
  
  # axis
  scale_y_continuous(expand = c(0, 0),
                     expression( ),
                     breaks=seq(0.00, 0.75, 0.25),
                     sec.axis= sec_axis(~.*11.0, name= expression( ),    # adjust scale here
                                        breaks= seq(0.0, 9.25, 2.5)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("day"),
                   labels= date_format("%b %d", tz="GMT"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2021-08-06 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                     as.POSIXct("2021-08-12 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))))+

  geom_text(x=as.POSIXct("2021-08-06 18:00:00", tz="GMT"), 
            y=0.72, 
            size=5.0, label= "Camp 3", parse=F, col="blue")+
  
  geom_hline(yintercept=0, linetype= "dashed", color= "darkgrey", size= 0.5)+
  
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 13),
        axis.title.x = element_text(size= 13, color="white"),
        axis.text.x= element_text(size= 13),
        axis.text.y= element_text(size= 13),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.position="none")

# plot_Precip_ETu_3



# _4 ---------------------------

# set the data
data <- data_Camp_4

plot_Precip_ETu_4 <- ggplot(data, aes(x=date, y=ET))+
  
  # start with blank
  geom_blank()+
  
  # D area (from 7:00 to 19:00)
  annotate("rect", ymin= 0.00, ymax= 0.75, alpha=0.10, fill= "blue",
           xmin= as.POSIXct("2021-08-31 07:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"),
           xmax= as.POSIXct("2021-08-31 19:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+
  
  # D area (from 7:00 to 19:00)
  annotate("rect", ymin= 0.00, ymax= 0.75, alpha=0.10, fill= "blue",
           xmin= as.POSIXct("2021-09-01 07:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"),
           xmax= as.POSIXct("2021-09-01 19:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+
  
  # D label
  geom_text(x=as.POSIXct("2021-08-31 13:00:00", tz="GMT"), 
            y=0.70, 
            size=6.0, label= "D1", parse=F, color="darkred")+
  
  # Day label
  geom_text(x=as.POSIXct("2021-08-31 13:00:00", tz="GMT"), 
            y=0.60, 
            size=4.0, label= "(day 6)", parse=F, color="black")+
  
  
  # D label
  geom_text(x=as.POSIXct("2021-09-01 13:00:00", tz="GMT"), 
            y=0.70, 
            size=6.0, label= "D2", parse=F, color="darkred")+
  
  # Day label
  geom_text(x=as.POSIXct("2021-09-01 13:00:00", tz="GMT"), 
            y=0.60, 
            size=4.0, label= "(day 7)", parse=F, color="black")+
  
  # add Precip
  geom_col(data= data, aes(x=date, y=Precip, fill="Precip"), alpha=0.5, colour= "blue")+
  scale_fill_manual(values="blue")+
  
  # add ET
  geom_line(data=data, aes(x=date, y=ET, color= "ET"), alpha=1.0, size=1.0)+
  
  # add ETu
  geom_line(data=data, aes(x=date, y=ETu, color= "ETu"), alpha=1.0, size=1.5)+
  
  # add line colours
  scale_color_manual(name = "Colors", 
                     values = c("ET" = "black", "ETu" = "darkorange"))+
  
  # axis
  scale_y_continuous(expand = c(0, 0),
                     expression( ),
                     breaks=seq(0.00, 0.75, 0.25),
                     sec.axis= sec_axis(~.*11.0, name= expression( ),    # adjust scale here
                                        breaks= seq(0.0, 9.25, 2.5)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("day"),
                   labels= date_format("%b %d", tz="GMT"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2021-08-27 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                     as.POSIXct("2021-09-02 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))))+

  geom_text(x=as.POSIXct("2021-09-02 11:00:00", tz="GMT"),
            y=0.72, 
            size=5.0, label= "Camp 4", parse=F, col="blue")+
  
  geom_hline(yintercept=0, linetype= "dashed", color= "darkgrey", size= 0.5)+
  
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 13),
        axis.title.x = element_text(size= 13, color="white"),
        axis.text.x= element_text(size= 13),
        axis.text.y= element_text(size= 13),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.position="none")

# plot_Precip_ETu_4



# _5 ---------------------------

# set the data
data <- data_Camp_5

plot_Precip_ETu_5 <- ggplot(data, aes(x=date, y=ET))+
  
  # start with blank
  geom_blank()+
  
  # D area (from 7:00 to 19:00)
  annotate("rect", ymin= 0.00, ymax= 0.75, alpha=0.10, fill= "blue",
           xmin= as.POSIXct("2021-09-21 07:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"),
           xmax= as.POSIXct("2021-09-21 19:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+

    # D area (from 7:00 to 19:00)
  annotate("rect", ymin= 0.00, ymax= 0.75, alpha=0.10, fill= "blue",
           xmin= as.POSIXct("2021-09-22 07:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"),
           xmax= as.POSIXct("2021-09-22 19:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+
  
  # D label
  geom_text(x=as.POSIXct("2021-09-21 13:00:00", tz="GMT"), 
            y=0.70, 
            size=6.0, label= "D1", parse=F, color="darkred")+
  
  # Day label
  geom_text(x=as.POSIXct("2021-09-21 13:00:00", tz="GMT"), 
            y=0.60, 
            size=4.0, label= "(day 8)", parse=F, color="black")+
  
  # D label
  geom_text(x=as.POSIXct("2021-09-22 13:00:00", tz="GMT"), 
            y=0.70, 
            size=6.0, label= "D2", parse=F, color="darkred")+
  
  # Day label
  geom_text(x=as.POSIXct("2021-09-22 13:00:00", tz="GMT"), 
            y=0.60, 
            size=4.0, label= "(day 9)", parse=F, color="black")+
  
  # add Precip
  geom_col(data= data, aes(x=date, y=Precip, fill="Precip"), alpha=0.5, colour= "blue")+
  scale_fill_manual(values="blue")+
  
  # add ET
  geom_line(data=data, aes(x=date, y=ET, color= "ET"), alpha=1.0, size=1.0)+
  
  # add ETu
  geom_line(data=data, aes(x=date, y=ETu, color= "ETu"), alpha=1.0, size=1.5)+
  
  # add line colours
  scale_color_manual(name = "Colors", 
                     values = c("ET" = "black", "ETu" = "darkorange"))+
  
  # axis
  scale_y_continuous(expand = c(0, 0),
                     expression( ),
                     breaks=seq(0.00, 0.75, 0.25),
                     sec.axis= sec_axis(~.*11.0, name= expression(Precipitation~(mm~h^-~1)),    # adjust scale here
                                        breaks= seq(0.0, 9.25, 2.5)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("day"),
                   labels= date_format("%b %d", tz="GMT"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2021-09-17 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                     as.POSIXct("2021-09-23 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))))+

  geom_text(x=as.POSIXct("2021-09-23 11:00:00", tz="GMT"), 
            y=0.72, 
            size=5.0, label= "Camp 5", parse=F, col="blue")+
  
  geom_hline(yintercept=0, linetype= "dashed", color= "darkgrey", size= 0.5)+
  
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 13),
        axis.title.x = element_text(size= 13, color="white"),
        axis.text.x= element_text(size= 13),
        axis.text.y= element_text(size= 13),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.position="none")

# plot_Precip_ETu_5



# adjustments to plot ---------------------------


# for Camp. 4:
plot_Precip_ETu_4 <- plot_Precip_ETu_4+
  theme(legend.title= element_text(color= "white", size= 12),
        legend.position= c(0.27, 0.89),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 18, colour= "black"),
        legend.key.height= unit(0.7,"line"),
        legend.key.width= unit(1.1,"line"),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.spacing.y = unit(0.0, 'cm'))


# panel ---------------------------

panel <- ggarrange(plot_Precip_ETu_1, plot_Precip_ETu_4,
                   plot_Precip_ETu_2, plot_Precip_ETu_5,
                   plot_Precip_ETu_3,
                   nrow=3, ncol=2)


setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/out")
ggsave("Fig_5.png", plot= panel, width= 42.0, height= 21.5, units= "cm")


# done.



