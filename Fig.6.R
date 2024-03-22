# Evapotranspiration dynamics and partitioning in a grassed vineyard: 
# ecophysiological and computational modelling approaches
# 
# Flávio Bastos Campos
# 
# code for Fig. 6 (approaches' agreement)


# Libraries & functions -------------------------------

library(tidyverse)
library(egg)          # to use ggarrange function
library(scales)       # to use the function date_format() in ggplot


# computes the RMSE
# between two vectors of same length
RMSE_fc <- function(Actual, Predicted) {
  diff_x_y <- (Actual - Predicted)^2
  RMSE <- (sum(diff_x_y)/length(Actual))^0.5
  return(RMSE)
}


# Fig. 6 -------------------------------

setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/only_RDS_Data_ETpaper_review_2014_02_10")

dat <- readRDS("Vineyard_data_Fig_6.rds") %>% as_tibble(.)


# Metrics -----------------------

reg <- lm(ET ~ ET_EPA, dat)   # Y~X
b <- round(unname(coef(reg)[2]), digits= 3)  # Y= a + bx
a <- round(unname(coef(reg)[1]), digits= 3)
R2 <- round(summary(reg)$r.squared, digits= 3)
p_value <- round(summary(reg)$coefficients[,4][[2]], 8)

dat_noNA <- dat[,c("ET_EPA", "ET")] %>% filter(!is.na(ET_EPA) & !is.na(ET))
RMSE <- RMSE_fc(dat_noNA$ET_EPA, dat_noNA$ET) %>% round(., 3)


my_text= list(ET[EC]~"= 1.095*."~ET[EPA]~+0.007^"ns",
              R^2~"="~0.85,
              RMSE~"="~0.065~mm~h^-~1)

# day_label
day_label <- c("07/07/2021", "21/07/2021", "09/08/2021", "10/08/2021", "11/08/2021", 
               "31/08/2021", "01/09/2021", "21/09/2021", "22/09/2021")

max_y <- 0.5235


# Plot -----------------------

ET_ET_plot <- ggplot(dat, aes(ET_EPA, ET, shape=day_label))+
  geom_point(size= 2.5)+
  
  scale_shape_manual(values= c(0, 1, 2, 4, 15, 16, 17, 18, 10, 12),
                     name=" ", 
                     labels= day_label)+
  
  # 1:1
  geom_abline(slope=1, intercept=0, color="darkgrey", linetype="solid", lwd= 1.0)+
  
  # reg line
  geom_abline(slope=b, intercept=a, color="blue", linetype="solid", lwd= 1.0)+
  
  scale_x_continuous(expand = c(0, 0),
                     expression(ET[EPA]~(mm~h^-~1)),
                     breaks=seq(0, max_y, 0.25),
                     limits = c(0, 0.6))+
  scale_y_continuous(expand = c(0, 0),
                     expression(ET[EC]~(mm~h^-~1)), 
                     breaks=seq(0, max_y, 0.25),
                     limits = c(0, 0.6))+
  
  # add the plot reference
  annotate(geom= "text", label= my_text,
           x=0.25, y= seq(0.14, 0.06, length=3),
           size=3.3, color= "black", parse=T, hjust= 0)+
  
  # add layout
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 11), 
        axis.text.x= element_text(size= 11),
        axis.line.x.bottom= element_line(color= "black"),
        axis.text.y= element_text(size= 11),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.title= element_text(color= "white", size= 10),
        legend.position= c(0.18, 0.85),
        legend.key= element_rect(fill= "white", colour= "white"),
        legend.text= element_text(size= 8, colour= "black"),
        legend.key.height= unit(0.5,"line"),
        legend.key.width= unit(1.0,"line"))

# ET_ET_plot

# Save
setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/out")
ggsave("Fig_6.png", plot= ET_ET_plot, width= 10.8, height= 10.0, units= "cm")


# done.



