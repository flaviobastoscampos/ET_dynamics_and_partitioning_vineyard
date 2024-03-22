# Evapotranspiration dynamics and partitioning in a grassed vineyard: 
# ecophysiological and computational modelling approaches
# 
# Flávio Bastos Campos
# 
# code for Fig. 9


# Libraries & functions -------------------------------

library(tidyverse)
library(egg)          # to use ggarrange function
library(scales)       # to use the function date_format() in ggplot


# Fig. 9 -----------------------------

# Part 1 ----------------------

setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/only_RDS_Data_ETpaper_review_2014_02_10")
data <- readRDS("Vineyard_data_Fig_9_season.rds")

# Rename
colnames(data)[colnames(data) == "ET_EC_mmd_f"] <- "ETec"
colnames(data)[colnames(data) == "T_TEA"] <- "Tec"
colnames(data)[colnames(data) == "SF_fitted"] <- "Tv"

# compute fluxes
data <- data %>% mutate(
  Tu= Tec - Tv,
  Eec= ETec - Tec)

# filter for the period
data <- data %>% filter((date>=as.POSIXct("2021-05-01 00:00:00", tz="GMT") & 
                                 date<=as.POSIXct("2021-08-31 23:30:00", tz="GMT")) |
                                (date>=as.POSIXct("2022-05-01 00:00:00", tz="GMT") & 
                                   date<=as.POSIXct("2022-08-31 23:30:00", tz="GMT")))


# Step 1: Create Precip_ind column
data <- data %>% 
  mutate(Precip_ind = ifelse(is.na(Precip), NA, ifelse(Precip > 0, 1, 0)))

# Step 2: Create and initialize index "D"
data$D <- 0

# Step 3: Update index "D" according to the conditions
for (i in 2:nrow(data)) {
  if (is.na(data$Precip_ind[i])) {
    data$D[i] <- NA
  } else if (is.na(data$D[i - 1]) && data$Precip_ind[i] == 0) {
    data$D[i] <- NA
  } else if (is.na(data$D[i - 1]) && data$Precip_ind[i] == 1) {
    data$D[i] <- max(data$D, na.rm = TRUE) + 1
  } else if (data$Precip_ind[i] == 0) {
    data$D[i] <- data$D[i - 1]
  } else if (data$Precip_ind[i - 1] == 0 && data$Precip_ind[i] == 1) {
    data$D[i] <- data$D[i - 1] + 1
  } else if (data$Precip_ind[i - 1] == 1 && data$Precip_ind[i] == 1) {
    data$D[i] <- data$D[i - 1]
    data$D[i - 1] <- -50
  }
}


# Part 2 ----------------------

# Create a list to store data frames
data_frames_list <- list()

# Loop through unique D values
unique_D <- unique(subset(data$D, !is.na(data$D) & data$D >= 1))

for (d in unique_D) {
  # Subset data for each D value
  subset_data <- data[!is.na(data$D) & data$D == d, ]
  
  # Create TAPD as an index following the row number
  subset_data$TAPD <- seq_len(nrow(subset_data))
  
  #
  subset_data <- subset_data %>% mutate(
    Tec_ET= Tec/ETec,
    ETu_ET= ETu/ETec,
    Tv_ET= Tv/ETec,
    Eec_ET= Eec/ETec,
    Tu_ET= Tu/ETec)
  
  # Rename the columns
  for (col_name in c("ETec", "Tec", "ETu", "Tv", "Eec", "Tu", 
                     "Tec_ET", "ETu_ET", "Tv_ET", "Eec_ET", "Tu_ET")) {
    colnames(subset_data)[colnames(subset_data) == col_name] <- paste0(col_name, "_", d)
  }
  
  # Store the subset data frame in the list
  data_frames_list[[as.character(d)]] <- subset_data
}

# Create data frame data_F with only TAPD column from 1:100
data_F <- data.frame(TAPD = 1:100)

# Join all individual data frames by TAPD
for (d in unique_D) {
  if (as.character(d) %in% names(data_frames_list)) {
    # Subset the columns to merge
    columns_to_merge <- c("TAPD", paste0(c("ETec", "Tec", "ETu", "Tv", "Eec", "Tu",
                                           "Tec_ET", "ETu_ET", "Tv_ET", "Eec_ET", "Tu_ET"), "_", d))
    
    # Merge the subset of columns
    data_F <- merge(data_F, data_frames_list[[as.character(d)]][columns_to_merge], by = "TAPD", all.x = TRUE)
  }
}

# Print the resulting data frame data_F
# data_F
as_tibble(data_F) %>% head(.)


# Part 3 ----------------------

# Initialize lists for each variable
ETec_list <- list()
Tec_list <- list()
ETu_list <- list()
Tv_list <- list()
Tu_list <- list()
Eec_list <- list()

Tec_ET_list <- list()
ETu_ET_list <- list()
Tv_ET_list <- list()
Tu_ET_list <- list()
Eec_ET_list <- list()

# Identify columns in data_F corresponding to each variable and add them to the lists
ETec_cols <- grep("^ETec_", names(data_F))       # example: ^ETec_1 column is number 2 in the df
Tec_cols <- grep("^Tec_", names(data_F))
ETu_cols <- grep("^ETu_", names(data_F))
Tv_cols <- grep("^Tv_", names(data_F))
Tu_cols <- grep("^Tu_", names(data_F))
Eec_cols <- grep("^Eec_", names(data_F))

Tec_ET_cols <- grep("^Tec_ET_", names(data_F))
ETu_ET_cols <- grep("^ETu_ET_", names(data_F))
Tv_ET_cols <- grep("^Tv_ET_", names(data_F))
Tu_ET_cols <- grep("^Tu_ET_", names(data_F))
Eec_ET_cols <- grep("^Eec_ET_", names(data_F))


ETec_list <- data_F[, c(1, ETec_cols)]
Tec_list <- data_F[, c(1, Tec_cols)]
ETu_list <- data_F[, c(1, ETu_cols)]
Tv_list <- data_F[, c(1, Tv_cols)]
Tu_list <- data_F[, c(1, Tu_cols)]
Eec_list <- data_F[, c(1, Eec_cols)]

Tec_ET_list <- data_F[, c(1, Tec_ET_cols)]
ETu_ET_list <- data_F[, c(1, ETu_ET_cols)]
Tv_ET_list <- data_F[, c(1, Tv_ET_cols)]
Tu_ET_list <- data_F[, c(1, Tu_ET_cols)]
Eec_ET_list <- data_F[, c(1, Eec_ET_cols)]


# 1 - analysis to get mean and sd of each flux per D after rain -------------------

# Calculate mean and standard deviation along rows for each variable
ETec_list$Mean <- rowMeans(ETec_list[, -1], na.rm=T)
ETec_list$SD <- apply(ETec_list[, -1], 1, sd, na.rm=T)

Tec_list$Mean <- rowMeans(Tec_list[, -1], na.rm=T)
Tec_list$SD <- apply(Tec_list[, -1], 1, sd, na.rm=T)

ETu_list$Mean <- rowMeans(ETu_list[, -1], na.rm=T)
ETu_list$SD <- apply(ETu_list[, -1], 1, sd, na.rm=T)

Tv_list$Mean <- rowMeans(Tv_list[, -1], na.rm=T)
Tv_list$SD <- apply(Tv_list[, -1], 1, sd, na.rm=T)

Tu_list$Mean <- rowMeans(Tu_list[, -1], na.rm=T)
Tu_list$SD <- apply(Tu_list[, -1], 1, sd, na.rm=T)

Eec_list$Mean <- rowMeans(Eec_list[, -1], na.rm=T)
Eec_list$SD <- apply(Eec_list[, -1], 1, sd, na.rm=T)

# Count non-NA values in each row of ETec
ETec_nonNA_counter <- apply(ETec_list[,2:length(ETec_list)], 1, function(row) sum(!is.na(row)))
Tec_nonNA_counter <- apply(Tec_list[,2:length(Tec_list)], 1, function(row) sum(!is.na(row)))
ETu_nonNA_counter <- apply(ETu_list[,2:length(ETu_list)], 1, function(row) sum(!is.na(row)))
Tv_nonNA_counter <- apply(Tv_list[,2:length(Tv_list)], 1, function(row) sum(!is.na(row)))
Tu_nonNA_counter <- apply(Tu_list[,2:length(Tu_list)], 1, function(row) sum(!is.na(row)))
Eec_nonNA_counter <- apply(Eec_list[,2:length(Eec_list)], 1, function(row) sum(!is.na(row)))

# prepare and join
ETec_F <- as_tibble(ETec_list[,c("TAPD", "Mean", "SD")])
colnames(ETec_F) <- c("TAPD", "ETec", "ETec_sd")

Tec_F <- as_tibble(Tec_list[,c("TAPD", "Mean", "SD")])
colnames(Tec_F) <- c("TAPD", "Tec", "Tec_sd")

ETu_F <- as_tibble(ETu_list[,c("TAPD", "Mean", "SD")])
colnames(ETu_F) <- c("TAPD", "ETu", "ETu_sd")

Tv_F <- as_tibble(Tv_list[,c("TAPD", "Mean", "SD")])
colnames(Tv_F) <- c("TAPD", "Tv", "Tv_sd")

Tu_F <- as_tibble(Tu_list[,c("TAPD", "Mean", "SD")])
colnames(Tu_F) <- c("TAPD", "Tu", "Tu_sd")

Eec_F <- as_tibble(Eec_list[,c("TAPD", "Mean", "SD")])
colnames(Eec_F) <- c("TAPD", "Eec", "Eec_sd")



data_Fluxes <- ETec_F %>% 
  left_join(Tec_F, by="TAPD") %>% 
  left_join(ETu_F, by="TAPD") %>% 
  left_join(Tv_F, by="TAPD") %>% 
  left_join(Tu_F, by="TAPD") %>% 
  left_join(Eec_F, by="TAPD")

# function: replace non-numeric or 0 values with NA
replace_non_numeric_zero <- function(x) {
  x <- as.numeric(x)                   # Convert values to numeric
  x[x == 0 | is.na(x)] <- NA           # Replace 0 or NA values with NA
  return(x)
}

# replace
data_Fluxes <- data_Fluxes %>%
  mutate_all(.funs=replace_non_numeric_zero)

# get relative fluxes (F/ETec)
data_F_ET <- data_Fluxes[,c("TAPD", "Tec", "ETu", "Tv", "Tu", "Eec", "ETec")] %>% 
  mutate(TAPD= TAPD,
         Tec= Tec/ETec,
         ETu= ETu/ETec,
         Tv= Tv/ETec,
         Tu= Tu/ETec,
         Eec= Eec/ETec,
         ETec_orig=ETec,
         ETec= 1)

# add D
data_F_ET <- data_F_ET %>% 
  mutate(D= as_factor(c(c("D1", "D2", "D3"), 
                        rep("D3+", 5), 
                        rep(NA, times=(nrow(data_F_ET)-8)))))


# 2 - analysis to (bar)plot all values of each flux per D after rain -------------------

# Tec_ET
Tec_ET_long <- Tec_ET_list[1:6,]

Tec_ET_long <- Tec_ET_long %>%
  pivot_longer(cols= starts_with("Tec_ET_"), names_to= "nameS", values_to= "Tec_ET_values")

# plot(Tec_ET_long$TAPD, Tec_ET_long$Tec_ET_values)

# Tu_ET
Tu_ET_long <- Tu_ET_list[1:6,]

Tu_ET_long <- Tu_ET_long %>%
  pivot_longer(cols= starts_with("Tu_ET_"), names_to= "nameS", values_to= "Tu_ET_values")

# plot(Tu_ET_long$TAPD, Tu_ET_long$Tu_ET_values)

# ETu_ET
ETu_ET_long <- ETu_ET_list[1:6,]

ETu_ET_long <- ETu_ET_long %>%
  pivot_longer(cols= starts_with("ETu_ET_"), names_to= "nameS", values_to= "ETu_ET_values")

# plot(ETu_ET_long$TAPD, ETu_ET_long$ETu_ET_values)

# Tv_ET
Tv_ET_long <- Tv_ET_list[1:6,]

Tv_ET_long <- Tv_ET_long %>%
  pivot_longer(cols= starts_with("Tv_ET_"), names_to= "nameS", values_to= "Tv_ET_values")

# plot(Tv_ET_long$TAPD, Tv_ET_long$Tv_ET_values)

# Eec_ET
Eec_ET_long <- Eec_ET_list[1:6,]

Eec_ET_long <- Eec_ET_long %>%
  pivot_longer(cols= starts_with("Eec_ET_"), names_to= "nameS", values_to= "Eec_ET_values")

# plot(Eec_ET_long$TAPD, Eec_ET_long$Eec_ET_values)

# merge
data_Fluxes <- Tec_ET_long[,c("TAPD", "Tec_ET_values")] %>% 
  mutate(Tu_ET_values= Tu_ET_long$Tu_ET_values, 
         ETu_ET_values= ETu_ET_long$ETu_ET_values, 
         Tv_ET_values= Tv_ET_long$Tv_ET_values, 
         Eec_ET_values= Eec_ET_long$Eec_ET_values)

# get D
data_Fluxes <- data_Fluxes %>% 
  mutate(D= ifelse(TAPD==1, "D1", 
                   ifelse(TAPD==2, "D2", 
                          ifelse(TAPD==3, "D3", "D3+")))) %>% mutate(D= factor(D))

# rename
colnames(data_Fluxes) <- c("TAPD", "Tec", "Tu", "ETu", "Tv", "Eec", "D")



# load ETu ----------------------------

setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/only_RDS_Data_ETpaper_review_2014_02_10")
TAPD_data_d_N <- readRDS("Vineyard_data_Fig_9_ETu.rds")

TAPD_data_d_N <- TAPD_data_d_N[,c("D", "ETu")] %>%
  mutate(D= factor(D),
         Variable= "ETu",
         Value= ETu)


# plot ----------------------------

# pivot for plot
data_F_ET_plot <- data_Fluxes %>%
  pivot_longer(c(`Tu`,`Tec`, `Tv`,`ETu`,`Eec`), names_to= "Variable", values_to= "Value")

# colors and labels
my_colors <- c(Eec= "#2ECB94", ETu= "red", 
               Tec= "darkblue",
               Tu= "magenta", Tv= "darkorange")

my_labels <- c(expression(E[EC]), expression("ET"[u]),
               expression("T"[EC]), expression("T"[u]),
               expression(Tv[(mod)]))

# plot
Fig_9 <- ggplot(data_F_ET_plot, aes(D, Value))+
  geom_boxplot(aes(colour= Variable), 
               outlier.shape=NA, width = 0.55)+
  
  geom_point(data=TAPD_data_d_N, aes(x= D, y= Value, colour= Variable), size= 1.8)+
               
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", linewidth= 0.5)+
  geom_hline(yintercept=1, linetype="dashed", color= "darkgrey", linewidth= 0.5)+
  
  scale_color_manual(values= my_colors, labels= my_labels)+
  
  scale_x_discrete(expression(Time~after~rainfall~(days)))+
  scale_y_continuous(expression("Contribution of fluxes to ET"),
                     breaks= seq(0.0, 1.0, 0.25),
                     limits= c(-0.15, 1.12))+
  
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size= 12), 
        axis.text.x= element_text(size= 10),
        axis.text.y= element_text(size= 10),
        axis.title.x= element_text(size= 12, color= "black"),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.title= element_text(color= "white", size= 10),
        legend.position= c(0.85, 0.20),
        legend.key= element_rect(fill= "white", colour= "white"),
        legend.text= element_text(size= 13, colour= "black"),
        legend.key.height= unit(0.5,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.direction="vertical",
        legend.box="vertical",
        legend.spacing.y = unit(0.0, 'cm'))+
  facet_wrap(~Variable)


# Fig_9

# save plots
setwd("C:/Users/bastosca/Desktop/Scripts_for_Figures/out")
ggsave("Fig_9.png", plot= Fig_9, width= 18.0, height= 13.0, units= "cm")


# done.


