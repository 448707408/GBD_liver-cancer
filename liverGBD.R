save(list = ls(), file = "all_data1.RData")
setwd("E:/GBD_liver_cancer")



download_GBD(url = "https://vizhub.healthdata.org/gbd-results/result/443d21c671fde9083821ba18b4e4a741",
             dest = "E:/GBD_liver_cancer",num =75 )

GBD_edition(2021)
data(GBDRegion2021)
str(GBDRegion2021)
df <- GBDread(zipfile, folder = T, foldername = "E:/GBD_liver_cancer/primary_data")
library(easyGBDR)
library(dplyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggsci)
library(splines)
colnames(df)
str(df)
unique(df$measure)
unique(df$age)
sort(unique(df$location))
list(unique(df$measure),
     unique(df$location),
     unique(df$sex),
     unique(df$age),
     unique(df$metric),
     unique(df$cause),
     unique(df$rei),
     unique(df$year))
sort(unique(df$year))


datfigure2021<- dplyr::filter(df,df$age == "Age-standardized",
                          df$year == "2021",
                          df$measure== "DALYs (Disability-Adjusted Life Years)",
                          df$sex== "Female")
write.csv(datfigure2021,"datfigure2021.csv")
write.csv(GBDRegion2021,"GBDRegion2021.csv")
length(datfigure2021$location)
length(GBDRegion2021$location)
merged_data2021 <- merge(datfigure2021, GBDRegion2021, by = "location")
write.csv(merged_data2021,"merged_data2021.csv")

datfigure<- dplyr::filter(df,df$measure== "DALYs (Disability-Adjusted Life Years)",
                          df$sex== "Female")
write.csv(datfigure,"datfigure.all.age.csv")

merged_data <- merge(datfigure, GBDRegion2021, by = "location")
write.csv(merged_data,"merged_data_all.age.csv")

datfigure1990<- dplyr::filter(df,df$age == "Age-standardized",
                              df$year == "1990",
                              df$measure== "DALYs (Disability-Adjusted Life Years)",
                              df$sex== "Female")
write.csv(datfigure1990,"datfigure1990.csv")
length(datfigure1990$location)

merged_data1990 <- merge(datfigure1990, GBDRegion2021, by = "location")
write.csv(merged_data1990,"merged_data1990.csv")



dat_liver<- dplyr::filter(merged_data,merged_data$cause == "Liver cancer")
dat_B <- dplyr::filter(merged_data,merged_data$cause == "Liver cancer due to hepatitis B")
dat_C <- dplyr::filter(merged_data,merged_data$cause == "Liver cancer due to hepatitis C")
dat_A <- dplyr::filter(merged_data,merged_data$cause == "Liver cancer due to alcohol use")
dat_O<- dplyr::filter(merged_data,merged_data$cause == "Liver cancer due to other causes")
dat_N<- dplyr::filter(merged_data,merged_data$cause == "Liver cancer due to NASH")
dat_H<- dplyr::filter(merged_data,merged_data$cause == "Hepatoblastoma")


dat_temp_B_1 <- dplyr::filter(df,df$location == "Global",
                              df$measure== "DALYs (Disability-Adjusted Life Years)",
                              df$sex== "Female")

common_cols <- intersect(names(merged_data), names(dat_temp_B_1))
merged_data_common <- merged_data[, common_cols, drop = FALSE]
dat_temp_B_1_common <- dat_temp_B_1[, common_cols, drop = FALSE]

dat_temp_B_all <- rbind(merged_data_common,dat_temp_B_1_common)
unique(dat_temp_B_all$location)
write.csv(dat_temp_B_all,"dat_temp_B_all.age.csv")

dat_liver_all<- dplyr::filter(dat_temp_B_all,
                          dat_temp_B_all$cause == "Liver cancer")
dat_liver_all_female<- dplyr::filter(dat_temp_B_all,
                              dat_temp_B_all$cause == "Liver cancer",
                              dat_temp_B_all$sex=="Female")
dat_B_all <- dplyr::filter(dat_temp_B_all,
                       dat_temp_B_all$cause == "Liver cancer due to hepatitis B")
dat_B_all_female <- dplyr::filter(dat_temp_B_all,
                                  dat_temp_B_all$cause == "Liver cancer due to hepatitis B",
                                  dat_temp_B_all$sex=="Female")
dat_C_all <- dplyr::filter(dat_temp_B_all,
                       dat_temp_B_all$cause == "Liver cancer due to hepatitis C")
dat_C_all_female <- dplyr::filter(dat_temp_B_all,
                                  dat_temp_B_all$cause == "Liver cancer due to hepatitis C",
                                  dat_temp_B_all$sex=="Female")
dat_A_all <- dplyr::filter(dat_temp_B_all,
                       dat_temp_B_all$cause == "Liver cancer due to alcohol use")
dat_A_all_female <- dplyr::filter(dat_temp_B_all,
                                  dat_temp_B_all$cause == "Liver cancer due to alcohol use",
                                  dat_temp_B_all$sex=="Female")
dat_O_all<- dplyr::filter(dat_temp_B_all,
                      dat_temp_B_all$cause == "Liver cancer due to other causes")
dat_O_all_female <- dplyr::filter(dat_temp_B_all,
                                  dat_temp_B_all$cause == "Liver cancer due to other causes",
                                  dat_temp_B_all$sex=="Female")
dat_N_all<- dplyr::filter(dat_temp_B_all,
                      dat_temp_B_all$cause == "Liver cancer due to NASH")
dat_N_all_female <- dplyr::filter(dat_temp_B_all,
                                  dat_temp_B_all$cause == "Liver cancer due to NASH",
                                  dat_temp_B_all$sex=="Female")
dat_H_all<- dplyr::filter(dat_temp_B_all,
                      dat_temp_B_all$cause == "Hepatoblastoma")
dat_H_all_female <- dplyr::filter(dat_temp_B_all,
                                  dat_temp_B_all$cause == "Hepatoblastoma",
                                  dat_temp_B_all$sex=="Female")


unique(dat_A_all$metric)
age_name <- c("<5","5 to 9","10 to 14","15 to 19",
              "20 to 24","25 to 29","30 to 34",
              "35 to 39","40 to 44", "45 to 49",
              "50 to 54","55 to 59", "60 to 64", "65 to 69",
              "70 to 74","75 to 79","80 to 84",
              "85 to 89","90 to 94","95 plus" )

figure1_a2021 <- ggpyramid(
  data = dat_A_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Liver cancer due to alcohol use",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 2021,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)
unique(dat_A_all_female$sex)
temp1 <- dat_A_all_female
temp2 <- dat_A_all_female
temp1$sex <- "Male"
temp2$sex <- "Both"
temp3 <- rbind(dat_A_all_female,temp1)
dat_A_all_femaleX2 <- rbind(temp3,temp2)
figure1_a2021_female <- ggpyramid(
          data = dat_A_all_femaleX2,
          group = "cause",
          x_axis = "age",
          percent = F,
          location_name = "Global",
          cause_name= "Liver cancer due to alcohol use",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          year_name = 2021,
          age_name = age_name,
          metric_name = "Rate",
          rei_name = NULL
)

figure1_b2021 <- ggpyramid(
  data = dat_B_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Liver cancer due to hepatitis B",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 2021,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)

unique(dat_B_all_female$sex)
temp1 <- dat_B_all_female
temp2 <- dat_B_all_female
temp1$sex <- "Male"
temp2$sex <- "Both"
temp3 <- rbind(dat_B_all_female,temp1)
dat_B_all_femaleX2 <- rbind(temp3,temp2)
figure1_b2021_female <- ggpyramid(
          data = dat_B_all_femaleX2,
          group = "cause",
          x_axis = "age",
          percent = F,
          location_name = "Global",
          cause_name = "Liver cancer due to hepatitis B",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          year_name = 2021,
          age_name = age_name,
          metric_name = "Rate",
          rei_name = NULL
)


figure1_c2021 <- ggpyramid(
  data = dat_C_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Liver cancer due to hepatitis C",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 2021,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)
unique(dat_C_all_female$sex)
temp1 <- dat_C_all_female
temp2 <- dat_C_all_female
temp1$sex <- "Male"
temp2$sex <- "Both"
temp3 <- rbind(dat_C_all_female,temp1)
dat_C_all_femaleX2 <- rbind(temp3,temp2)
figure1_c2021_female <- ggpyramid(
          data = dat_C_all_femaleX2,
          group = "cause",
          x_axis = "age",
          percent = F,
          location_name = "Global",
          cause_name = "Liver cancer due to hepatitis C",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          year_name = 2021,
          age_name = age_name,
          metric_name = "Rate",
          rei_name = NULL
)

figure1_O2021 <- ggpyramid(
  data = dat_O_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Liver cancer due to other causes",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 2021,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)

unique(dat_O_all_female$sex)
temp1 <- dat_O_all_female
temp2 <- dat_O_all_female
temp1$sex <- "Male"
temp2$sex <- "Both"
temp3 <- rbind(dat_O_all_female,temp1)
dat_O_all_femaleX2 <- rbind(temp3,temp2)
figure1_O2021_female <- ggpyramid(
          data = dat_O_all_femaleX2,
          group = "cause",
          x_axis = "age",
          percent = F,
          location_name = "Global",
          cause_name = "Liver cancer due to other causes",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          year_name = 2021,
          age_name = age_name,
          metric_name = "Rate",
          rei_name = NULL
)


figure1_N2021 <- ggpyramid(
  data = dat_N_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Liver cancer due to NASH",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 2021,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)

unique(dat_N_all_female$sex)
temp1 <- dat_N_all_female
temp2 <- dat_N_all_female
temp1$sex <- "Male"
temp2$sex <- "Both"
temp3 <- rbind(dat_N_all_female,temp1)
dat_N_all_femaleX2 <- rbind(temp3,temp2)
figure1_N2021_female <- ggpyramid(
          data = dat_N_all_femaleX2,
          group = "cause",
          x_axis = "age",
          percent = F,
          location_name = "Global",
          cause_name = "Liver cancer due to NASH",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          year_name = 2021,
          age_name = age_name,
          metric_name = "Rate",
          rei_name = NULL
)


figure1_H2021 <- ggpyramid(
  data = dat_H_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Hepatoblastoma",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 2021,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)

unique(dat_H_all_female$sex)
temp1 <- dat_H_all_female
temp2 <- dat_H_all_female
temp1$sex <- "Male"
temp2$sex <- "Both"
temp3 <- rbind(dat_H_all_female,temp1)
dat_H_all_femaleX2 <- rbind(temp3,temp2)
figure1_H2021_female <- ggpyramid(
          data = dat_H_all_femaleX2,
          group = "cause",
          x_axis = "age",
          percent = F,
          location_name = "Global",
          cause_name = "Hepatoblastoma",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          year_name = 2021,
          age_name = age_name,
          metric_name = "Rate",
          rei_name = NULL
)


figure1_liver2021 <- ggpyramid(
  data = dat_liver_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Liver cancer",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 2021,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)


figure1_H1990 <- ggpyramid(
  data = dat_H_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Hepatoblastoma",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 1990,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)

figure1_N1990 <- ggpyramid(
  data = dat_N_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Liver cancer due to NASH",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 1990,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)
figure1_O1990 <- ggpyramid(
  data = dat_O_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Liver cancer due to other causes",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 1990,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)
figure1_c1990 <- ggpyramid(
  data = dat_C_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Liver cancer due to hepatitis C",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 1990,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)
figure1_b1990 <- ggpyramid(
  data = dat_B_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Liver cancer due to hepatitis B",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 1990,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)
figure1_a1990 <- ggpyramid(
  data = dat_A_all,
  group = "cause",
  x_axis = "age",
  percent = F,
  location_name = "Global",
  cause_name = "Liver cancer due to alcohol use",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  year_name = 1990,
  age_name = age_name,
  metric_name = "Rate",
  rei_name = NULL
)

combined_data_figure1 <- bind_rows(
  dat_H_all %>% mutate(cause = "Hepatoblastoma"),
  dat_N_all %>% mutate(cause = "Liver cancer due to NASH"),
  dat_O_all %>% mutate(cause = "Liver cancer due to other causes"),
  dat_C_all %>% mutate(cause = "Liver cancer due to hepatitis C"),
  dat_B_all %>% mutate(cause = "Liver cancer due to hepatitis B"),
  dat_A_all %>% mutate(cause = "Liver cancer due to alcohol use")
)

combined_data_figure1_female <- bind_rows(
          dat_H_all_female %>% mutate(cause = "Hepatoblastoma"),
          dat_N_all_female %>% mutate(cause = "Liver cancer due to NASH"),
          dat_O_all_female %>% mutate(cause = "Liver cancer due to other causes"),
          dat_C_all_female %>% mutate(cause = "Liver cancer due to hepatitis C"),
          dat_B_all_female %>% mutate(cause = "Liver cancer due to hepatitis B"),
          dat_A_all_female %>% mutate(cause = "Liver cancer due to alcohol use")
)

combined_data_figure_1 <- dplyr::filter(combined_data_figure1,combined_data_figure1$measure == "DALYs (Disability-Adjusted Life Years)",
                                        combined_data_figure1$metric== "Rate",
                                        combined_data_figure1$location== "Global",
                                        combined_data_figure1$year== 1990)

combined_data_figure_1_female <- dplyr::filter(combined_data_figure1_female,combined_data_figure1_female$measure == "DALYs (Disability-Adjusted Life Years)",
                                        combined_data_figure1_female$metric== "Rate",
                                        combined_data_figure1_female$location== "Global",
                                        combined_data_figure1_female$year== 1990)
age_groups <- c("<5", "5 to 9", "10 to 14", "15 to 19",
                "20 to 24", "25 to 29", "30 to 34",
                "35 to 39", "40 to 44", "45 to 49",
                "50 to 54", "55 to 59", "60 to 64",
                "65 to 69", "70 to 74", "75 to 79",
                "80 to 84", "85 to 89", "90 to 94", "95 plus")
filtered_data_figure_1 <- combined_data_figure_1 %>% 
  filter(age %in% age_groups & val > 0)

filtered_data_figure_1_female <- combined_data_figure_1_female %>% 
          filter(age %in% age_groups & val > 0)

filtered_data_figure_1$age <- factor(filtered_data_figure_1$age, levels = age_groups)
lancet_palette <- c("#EAB8C7", "#7E9FC3", "#F6A600", "#1F78B4", "#B5D09A", "#FF6F61")

filtered_data_figure_1_female$age <- factor(filtered_data_figure_1_female$age, levels = age_groups)
lancet_palette <- c("#EAB8C7", "#7E9FC3", "#F6A600", "#1F78B4", "#B5D09A", "#FF6F61")

figure_combined_figure_1_1990 <- ggplot(filtered_data_figure_1, aes(x = val, y = age, fill = cause)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = lancet_palette) +
  labs(title = "(1990)",
       y = "Age (year)",
       x = "DALYs rate (per 1000000 populations)") +
  theme_minimal(base_family = "Times New Roman")+
  theme(
    text = element_text(family = "Helvetica", size = 18), 
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.background = element_rect(fill = "white"),  
    panel.background = element_rect(fill = "white"),
    axis.title.x = element_text(margin = margin(t = 11)),  
    axis.title.y = element_text(margin = margin(r = 11)),  
    axis.title = element_text(size = 18),  
    axis.text = element_text(size = 18) ,
    legend.text = element_text(size = 15),
    legend.position = c(0.8, 0.2))

ggsave(filename = "Global estimates of DALYs (1990).tiff",
       plot = figure_combined_figure_1_1990 ,
       height = 10,
       width = 14,
       dpi = 300)

figure_combined_figure_1_1990_female <- ggplot(filtered_data_figure_1_female, aes(x = val, y = age, fill = cause)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = lancet_palette) +
          labs(title = "(1990)",
               y = "Age (year)",
               x = "DALYs rate (per 1000000 populations)") +
          theme_minimal(base_family = "Times New Roman")+
          theme(
                    text = element_text(family = "Helvetica", size = 18),  
                    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                    plot.background = element_rect(fill = "white"), 
                    panel.background = element_rect(fill = "white"),
                    axis.title.x = element_text(margin = margin(t = 11)), 
                    axis.title.y = element_text(margin = margin(r = 11)),  
                    axis.title = element_text(size = 18),  
                    axis.text = element_text(size = 18) ,
                    legend.text = element_text(size = 15),
                    legend.position = c(0.8, 0.2))+theme(
                      text = element_text(family = "Times New Roman")
                    ) 

figure_combined_figure_1_1990_female <- ggplot(filtered_data_figure_1_female, aes(x = val, y = age, fill = cause)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = lancet_palette) +
  labs(title = "(1990)",
       y = "Age (year)",
       x = "DALYs rate (per 1000000 populations)") +
  theme_minimal(base_family = "Times New Roman")+
  theme(
    text = element_text(family = "Helvetica", size = 18),  
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.background = element_rect(fill = "white",color = NA), 
    panel.background = element_rect(fill = "white",color = NA),
    panel.border = element_blank(),
    axis.title.x = element_text(margin = margin(t = 11)),  
    axis.title.y = element_text(margin = margin(r = 11)),   
    axis.title = element_text(size = 18), 
    axis.text = element_text(size = 18) ,
    legend.text = element_text(size = 15), plot.margin = margin(0, 0, 0, 0),  
    panel.spacing = unit(0, "lines"),
    legend.position = c(0.8, 0.2))+theme(
      text = element_text(family = "Times New Roman")
    ) 
ggsave(filename = "Global estimates of DALYs (1990)female.tiff",
       plot = figure_combined_figure_1_1990_female ,
       height = 10,
       width = 14,
       dpi = 300)



combined_data_figure_2 <- dplyr::filter(combined_data_figure1,combined_data_figure1$measure == "DALYs (Disability-Adjusted Life Years)",
                                        combined_data_figure1$metric== "Rate",
                                        combined_data_figure1$location== "Global",
                                        combined_data_figure1$year== 2021)

combined_data_figure_2_female <- dplyr::filter(combined_data_figure1_female,combined_data_figure1_female$measure == "DALYs (Disability-Adjusted Life Years)",
                                        combined_data_figure1_female$metric== "Rate",
                                        combined_data_figure1_female$location== "Global",
                                        combined_data_figure1_female$year== 2021)

age_groups <- c("<5", "5 to 9", "10 to 14", "15 to 19",
                "20 to 24", "25 to 29", "30 to 34",
                "35 to 39", "40 to 44", "45 to 49",
                "50 to 54", "55 to 59", "60 to 64",
                "65 to 69", "70 to 74", "75 to 79",
                "80 to 84", "85 to 89", "90 to 94", "95 plus")
filtered_data_figure_2 <- combined_data_figure_2 %>% 
  filter(age %in% age_groups & val > 0)

filtered_data_figure_2_female <- combined_data_figure_2_female %>% 
          filter(age %in% age_groups & val > 0)

filtered_data_figure_2$age <- factor(filtered_data_figure_2$age, levels = age_groups)
lancet_palette <- c("#EAB8C7", "#7E9FC3", "#F6A600", "#1F78B4", "#B5D09A", "#FF6F61")

filtered_data_figure_2_female$age <- factor(filtered_data_figure_2_female$age, levels = age_groups)
lancet_palette <- c("#EAB8C7", "#7E9FC3", "#F6A600", "#1F78B4", "#B5D09A", "#FF6F61")

figure_combined_figure_2_2021 <- ggplot(filtered_data_figure_2, aes(x = val, y = age, fill = cause)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = lancet_palette) +
  labs(title = "(2021)",
       y = "Age (year)",
       x = "DALYs rate (per 1000000 populations)") +
  theme_minimal(base_family = "Times New Roman")+
  theme(
    text = element_text(family = "Helvetica", size = 18),  
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.title.x = element_text(margin = margin(t = 11)),  
    axis.title.y = element_text(margin = margin(r = 11)),  
    axis.title = element_text(size = 18),  
    axis.text = element_text(size = 18) ,
    legend.text = element_text(size = 15),
    legend.position = c(0.8, 0.2))

ggsave(filename = "Global estimates of DALYs (2021).tiff",
       plot = figure_combined_figure_2_2021 ,
       height = 10,
       width = 14,
       dpi = 300)


figure_combined_figure_2_2021_female <- ggplot(filtered_data_figure_2_female, aes(x = val, y = age, fill = cause)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = lancet_palette) +
          labs(title = "(2021)",
               y = "Age (year)",
               x = "DALYs rate (per 1000000 populations)") +
          theme_minimal(base_family = "Times New Roman")+
  theme(
    text = element_text(family = "Helvetica", size = 18), 
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.background = element_rect(fill = "white",color = NA), 
    panel.background = element_rect(fill = "white",color = NA),
    panel.border = element_blank(),
    axis.title.x = element_text(margin = margin(t = 11)),  
    axis.title.y = element_text(margin = margin(r = 11)),   
    axis.title = element_text(size = 18), 
    axis.text = element_text(size = 18) ,
    legend.text = element_text(size = 15), plot.margin = margin(0, 0, 0, 0), 
    panel.spacing = unit(0, "lines"),
    legend.position = c(0.8, 0.2))+theme(
      text = element_text(family = "Times New Roman")
    ) 

ggsave(filename = "Global estimates of DALYs (2021)female.tiff",
       plot = figure_combined_figure_2_2021_female ,
       height = 10,
       width = 14,
       dpi = 300)

combined_plot <- (figure_combined_figure_1_1990 + figure_combined_figure_2_2021) +
  plot_annotation(title = "The distribution of global DALYs rates for liver cancer", 
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          size = 24, 
                                                          face = "bold",
                                                          margin = margin(t = 20, b = 20))))+
  plot_layout(widths = c(1, 1), 
              guides = "collect") + 
  theme(plot.margin = margin(0, 50, 0, 50),
        legend.position = c(0.9, 0.1))
  

ggsave(filename = "The distribution of global DALYs rates for liver cancer.tiff",
       plot = combined_plot,
       height = 10,
       width = 18,
       dpi = 300)


data_combined_H <- dat_H_all %>%
  filter(year %in% c(1990, 2021)) %>%
  mutate(year = factor(year))

data_filtered_H <- data_combined_H[data_combined_H$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_H <- dplyr::filter(data_filtered_H,data_filtered_H$location == "Global",
                           data_filtered_H$metric == "Rate",
                           data_filtered_H$age%in%  c("<5","5 to 9"))


data_combined_H_female <- dat_H_all_female %>%
          filter(year %in% c(1990, 2021)) %>%
          mutate(year = factor(year))

data_filtered_H_female <- data_combined_H_female[data_combined_H_female$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_H_female <- dplyr::filter(data_filtered_H_female,data_filtered_H_female$location == "Global",
                             data_filtered_H_female$metric == "Rate",
                             data_filtered_H_female$age%in% c("<5","5 to 9"))


data_date_H$age <- factor(data_date_H$age, levels = c("<5", "5 to 9"))


figure_combined_H <- ggplot(data_date_H, aes(x = age, y = val, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
  labs(
    title = "Comparison of Hepatoblastoma DALYs (1990 vs 2021)",
    x = "Age (year)",
    y = "DALYs rate (per 1000000 populations)",
    fill = "Year"
  ) +
  theme_minimal(base_family = "Times New Roman") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  
    plot.background = element_rect(fill = "white"),  
    panel.background = element_rect(fill = "white"), 
    axis.title = element_text(size = 25),  
    axis.text.x = element_text(size = 25),  
    axis.text.y = element_text(size = 25),  
    legend.position = c(0.9, 0.9),  
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 18) 
  )
  
data_date_H_female$age <- factor(data_date_H_female$age, levels = c("<5", "5 to 9"))

figure_combined_H_female <- ggplot(data_date_H_female, aes(x = age, y = val, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
  labs(
    title = "Comparison of Hepatoblastoma DALYs (1990 vs 2021)",
    x = "Age (year)",
    y = "DALYs rate (per 1000000 populations)",
    fill = "Year"
  ) +
  theme_minimal(base_family = "Times New Roman") + theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  
    plot.background = element_rect(fill = "white"),  
    panel.background = element_rect(fill = "white"),  
    axis.title = element_text(size = 25),  
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 25),  
    legend.position = c(0.9, 0.9),  
    legend.text = element_text(size = 18), 
    legend.title = element_text(size = 18)
  )



ggsave(filename = "Comparison of Hepatoblastoma DALYs.tiff",
       plot = figure_combined_H,
       height = 10,
       width = 14,
       dpi = 300)

ggsave(filename = "Comparison of Hepatoblastoma DALYsfemale.tiff",#jpeg，png，tif等，具体格式在画图时代码也要相应调整
       plot = figure_combined_H_female1,
       height = 10,
       width = 14,
       dpi = 300)


data_combined_B <- dat_B_all %>%
  filter(year %in% c(1990, 2021)) %>%
  mutate(year = factor(year))

data_filtered_B <- data_combined_B[data_combined_B$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_B <- dplyr::filter(data_filtered_B,data_filtered_B$location == "Global",
                             data_filtered_B$metric == "Rate",
                             data_filtered_B$age %in% c("10 to 14","15 to 19",
                                                     "20 to 24","25 to 29","30 to 34",
                                                     "35 to 39","40 to 44", "45 to 49",
                                                     "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                     "70 to 74","75 to 79","80 to 84",
                                                     "85 to 89","90 to 94","95 plus" ))

data_combined_B_female <- dat_B_all_female %>%
          filter(year %in% c(1990, 2021)) %>%
          mutate(year = factor(year))

data_filtered_B_female <- data_combined_B_female[data_combined_B_female$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_B_female <- dplyr::filter(data_filtered_B_female,data_filtered_B_female$location == "Global",
                             data_filtered_B_female$metric == "Rate",
                             data_filtered_B_female$age %in% c("10 to 14","15 to 19",
                                                        "20 to 24","25 to 29","30 to 34",
                                                        "35 to 39","40 to 44", "45 to 49",
                                                        "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                        "70 to 74","75 to 79","80 to 84",
                                                        "85 to 89","90 to 94","95 plus" ))



data_date_B$age <- factor(data_date_B$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                      "20 to 24", "25 to 29", "30 to 34",
                                                      "35 to 39", "40 to 44", "45 to 49",
                                                      "50 to 54", "55 to 59", "60 to 64",
                                                      "65 to 69", "70 to 74", "75 to 79",
                                                      "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_B <- ggplot(data_date_B, aes(x = age, y = val, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
  labs(
    title = "Comparison of Liver cancer due to hepatitis B DALYs (1990 vs 2021)",
    x = "Age (year)",
    y = "DALYs rate (per 1000000 populations)",
    fill = "Year"
  ) +
  theme_minimal(base_family = "Times New Roman") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  
    plot.background = element_rect(fill = "white"),  
    panel.background = element_rect(fill = "white"), 
    axis.title = element_text(size = 25),  
    axis.text.y = element_text(size = 25), 
    legend.position = c(0.9, 0.9),  
    legend.text = element_text(size = 18), 
    legend.title = element_text(size = 18)
  )


data_date_B_female$age <- factor(data_date_B_female$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                      "20 to 24", "25 to 29", "30 to 34",
                                                      "35 to 39", "40 to 44", "45 to 49",
                                                      "50 to 54", "55 to 59", "60 to 64",
                                                      "65 to 69", "70 to 74", "75 to 79",
                                                      "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_B_female <- ggplot(data_date_B_female, aes(x = age, y = val, fill = year)) + 
          geom_bar(stat = "identity", position = "dodge") + 
          scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
          labs(
                    title = "Comparison of Liver cancer due to hepatitis B DALYs (1990 vs 2021)",
                    x = "Age (year)",
                    y = "DALYs rate (per 1000000 populations)",
                    fill = "Year"
          ) +
          theme_minimal(base_family = "Times New Roman") + 
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  
    plot.background = element_rect(fill = "white",color = NA),  
    panel.background = element_rect(fill = "white",color = NA), 
    panel.border = element_blank(), 
    axis.title = element_text(size = 25),  
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 25),  
    legend.position = c(0.8, 0.9),  
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 18),  
    plot.margin = margin(0, 0, 0, 0),  
    panel.spacing = unit(0, "lines") 
  )



ggsave(filename = "Comparison of Liver cancer due to hepatitis B DALYs.tiff",
       plot = figure_combined_B,
       height = 10,
       width = 14,
       dpi = 300)

ggsave(filename = "Comparison of Liver cancer due to hepatitis B DALYsfemale.tiff",
       plot = figure_combined_B_female,
       height = 10,
       width = 14,
       dpi = 300)


data_combined_C <- dat_C_all %>%
  filter(year %in% c(1990, 2021)) %>%
  mutate(year = factor(year))

data_filtered_C <- data_combined_C[data_combined_C$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_C <- dplyr::filter(data_filtered_C,data_filtered_C$location == "Global",
                             data_filtered_C$metric == "Rate",
                             data_filtered_C$age %in% c("10 to 14","15 to 19",
                                                        "20 to 24","25 to 29","30 to 34",
                                                        "35 to 39","40 to 44", "45 to 49",
                                                        "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                        "70 to 74","75 to 79","80 to 84",
                                                        "85 to 89","90 to 94","95 plus" ))

data_combined_C_female <- dat_C_all_female %>%
          filter(year %in% c(1990, 2021)) %>%
          mutate(year = factor(year))

data_filtered_C_female <- data_combined_C_female[data_combined_C_female$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_C_female <- dplyr::filter(data_filtered_C_female,data_filtered_C_female$location == "Global",
                             data_filtered_C_female$metric == "Rate",
                             data_filtered_C_female$age %in% c("10 to 14","15 to 19",
                                                        "20 to 24","25 to 29","30 to 34",
                                                        "35 to 39","40 to 44", "45 to 49",
                                                        "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                        "70 to 74","75 to 79","80 to 84",
                                                        "85 to 89","90 to 94","95 plus" ))

data_date_C$age <- factor(data_date_C$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                      "20 to 24", "25 to 29", "30 to 34",
                                                      "35 to 39", "40 to 44", "45 to 49",
                                                      "50 to 54", "55 to 59", "60 to 64",
                                                      "65 to 69", "70 to 74", "75 to 79",
                                                      "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_C <- ggplot(data_date_C, aes(x = age, y = val, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
  labs(
    title = "Comparison of Liver cancer due to hepatitis C DALYs (1990 vs 2021)",
    x = "Age (year)",
    y = "DALYs rate (per 1000000 populations)",
    fill = "Year"
  ) +
  theme_minimal(base_family = "Times New Roman") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5), 
    plot.background = element_rect(fill = "white"),  
    panel.background = element_rect(fill = "white"),  
    axis.title = element_text(size = 25),  
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 25),  
    legend.position = c(0.8, 0.9),  
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 18)
  )

data_date_C_female$age <- factor(data_date_C_female$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                      "20 to 24", "25 to 29", "30 to 34",
                                                      "35 to 39", "40 to 44", "45 to 49",
                                                      "50 to 54", "55 to 59", "60 to 64",
                                                      "65 to 69", "70 to 74", "75 to 79",
                                                      "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_C_female <- ggplot(data_date_C_female, aes(x = age, y = val, fill = year)) + 
          geom_bar(stat = "identity", position = "dodge") + 
          scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
          labs(
                    title = "Comparison of Liver cancer due to hepatitis C DALYs (1990 vs 2021)",
                    x = "Age (year)",
                    y = "DALYs rate (per 1000000 populations)",
                    fill = "Year"
          ) +
          theme_minimal(base_family = "Times New Roman") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  
    plot.background = element_rect(fill = "white",color = NA),  
    panel.background = element_rect(fill = "white",color = NA), 
    panel.border = element_blank(), 
    axis.title = element_text(size = 25),  
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 25), 
    legend.position = c(0.8, 0.9),  
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 18),  
    plot.margin = margin(0, 0, 0, 0),  
    panel.spacing = unit(0, "lines") 
  )


ggsave(filename = "Comparison of Liver cancer due to hepatitis C DALYs.tiff",
       plot = figure_combined_C,
       height = 10,
       width = 14,
       dpi = 300)
ggsave(filename = "Comparison of Liver cancer due to hepatitis C DALYsfemale.tiff",
       plot = figure_combined_C_female,
       height = 10,
       width = 14,
       dpi = 300)


data_combined_A <- dat_A_all %>%
  filter(year %in% c(1990, 2021)) %>%
  mutate(year = factor(year))

data_filtered_A <- data_combined_A[data_combined_A$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_A <- dplyr::filter(data_filtered_A,data_filtered_A$location == "Global",
                             data_filtered_A$metric == "Rate",
                             data_filtered_A$age %in% c("<5","5 to 9","10 to 14","15 to 19",
                                                        "20 to 24","25 to 29","30 to 34",
                                                        "35 to 39","40 to 44", "45 to 49",
                                                        "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                        "70 to 74","75 to 79","80 to 84",
                                                        "85 to 89","90 to 94","95 plus" ))
data_date_A<- dplyr::filter(data_filtered_A,
                             location == "Global",
                             metric == "Rate",
                             age %in% c("<5", "5 to 9", "10 to 14", "15 to 19",
                                        "20 to 24", "25 to 29", "30 to 34",
                                        "35 to 39", "40 to 44", "45 to 49",
                                        "50 to 54", "55 to 59", "60 to 64", 
                                        "65 to 69", "70 to 74", "75 to 79", 
                                        "80 to 84", "85 to 89", "90 to 94", "95 plus"),
                             val > 0) 

data_combined_A_female <- dat_A_all_female %>%
          filter(year %in% c(1990, 2021)) %>%
          mutate(year = factor(year))

data_filtered_A_female <- data_combined_A_female[data_combined_A_female$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_A_female <- dplyr::filter(data_filtered_A_female,data_filtered_A_female$location == "Global",
                             data_filtered_A_female$metric == "Rate",
                             data_filtered_A_female$age %in% c("<5","5 to 9","10 to 14","15 to 19",
                                                        "20 to 24","25 to 29","30 to 34",
                                                        "35 to 39","40 to 44", "45 to 49",
                                                        "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                        "70 to 74","75 to 79","80 to 84",
                                                        "85 to 89","90 to 94","95 plus" ))
data_date_A_female<- dplyr::filter(data_filtered_A_female,
                            location == "Global",
                            metric == "Rate",
                            age %in% c("<5", "5 to 9", "10 to 14", "15 to 19",
                                       "20 to 24", "25 to 29", "30 to 34",
                                       "35 to 39", "40 to 44", "45 to 49",
                                       "50 to 54", "55 to 59", "60 to 64", 
                                       "65 to 69", "70 to 74", "75 to 79", 
                                       "80 to 84", "85 to 89", "90 to 94", "95 plus"),
                            val > 0) 


data_date_A$age <- factor(data_date_A$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                      "20 to 24", "25 to 29", "30 to 34",
                                                      "35 to 39", "40 to 44", "45 to 49",
                                                      "50 to 54", "55 to 59", "60 to 64",
                                                      "65 to 69", "70 to 74", "75 to 79",
                                                      "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_A <- ggplot(data_date_A, aes(x = age, y = val, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
  labs(
    title = "Comparison of Liver cancer due to alcohol use DALYs (1990 vs 2021)",
    x = "Age (year)",
    y = "DALYs rate (per 1000000 populations)",
    fill = "Year"
  ) +
  theme_minimal(base_family = "Times New Roman") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5), 
    plot.background = element_rect(fill = "white"), 
    panel.background = element_rect(fill = "white"),  
    axis.title = element_text(size = 25), 
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 25),  
    legend.position = c(0.9, 0.9),  
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 18)
  )


data_date_A_female$age <- factor(data_date_A_female$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                      "20 to 24", "25 to 29", "30 to 34",
                                                      "35 to 39", "40 to 44", "45 to 49",
                                                      "50 to 54", "55 to 59", "60 to 64",
                                                      "65 to 69", "70 to 74", "75 to 79",
                                                      "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_A_female <- ggplot(data_date_A_female, aes(x = age, y = val, fill = year)) + 
          geom_bar(stat = "identity", position = "dodge") + 
          scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
          labs(
                    title = "Comparison of Liver cancer due to alcohol use DALYs (1990 vs 2021)",
                    x = "Age (year)",
                    y = "DALYs rate (per 1000000 populations)",
                    fill = "Year"
          ) +
          theme_minimal(base_family = "Times New Roman") + 
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  
    plot.background = element_rect(fill = "white",color = NA),  
    panel.background = element_rect(fill = "white",color = NA), 
    panel.border = element_blank(), 
    axis.title = element_text(size = 25), 
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 25), 
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 18),  
    plot.margin = margin(0, 0, 0, 0),  
    panel.spacing = unit(0, "lines") 
  )



ggsave(filename = "Comparison of Liver cancer due to alcohol use DALYs.tiff",
       plot = figure_combined_A,
       height = 10,
       width = 14,
       dpi = 300)
ggsave(filename = "Comparison of Liver cancer due to alcohol use DALYsfemale.tiff",
       plot = figure_combined_A_female,
       height = 10,
       width = 14,
       dpi = 300)


data_combined_O <- dat_O_all %>%
  filter(year %in% c(1990, 2021)) %>%
  mutate(year = factor(year))

data_filtered_O <- data_combined_O[data_combined_O$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_O <- dplyr::filter(data_filtered_O,data_filtered_O$location == "Global",
                             data_filtered_O$metric == "Rate",
                             data_filtered_O$age %in% c("<5","5 to 9","10 to 14","15 to 19",
                                                        "20 to 24","25 to 29","30 to 34",
                                                        "35 to 39","40 to 44", "45 to 49",
                                                        "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                        "70 to 74","75 to 79","80 to 84",
                                                        "85 to 89","90 to 94","95 plus" ))
data_date_O<- dplyr::filter(data_filtered_O,
                            location == "Global",
                            metric == "Rate",
                            age %in% c("<5", "5 to 9", "10 to 14", "15 to 19",
                                       "20 to 24", "25 to 29", "30 to 34",
                                       "35 to 39", "40 to 44", "45 to 49",
                                       "50 to 54", "55 to 59", "60 to 64", 
                                       "65 to 69", "70 to 74", "75 to 79", 
                                       "80 to 84", "85 to 89", "90 to 94", "95 plus"),
                            val > 0) 

data_combined_O_female <- dat_O_all_female %>%
          filter(year %in% c(1990, 2021)) %>%
          mutate(year = factor(year))

data_filtered_O_female <- data_combined_O_female[data_combined_O_female$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_O_female <- dplyr::filter(data_filtered_O_female,data_filtered_O_female$location == "Global",
                             data_filtered_O_female$metric == "Rate",
                             data_filtered_O_female$age %in% c("<5","5 to 9","10 to 14","15 to 19",
                                                        "20 to 24","25 to 29","30 to 34",
                                                        "35 to 39","40 to 44", "45 to 49",
                                                        "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                        "70 to 74","75 to 79","80 to 84",
                                                        "85 to 89","90 to 94","95 plus" ))
data_date_O_female<- dplyr::filter(data_filtered_O_female,
                            location == "Global",
                            metric == "Rate",
                            age %in% c("<5", "5 to 9", "10 to 14", "15 to 19",
                                       "20 to 24", "25 to 29", "30 to 34",
                                       "35 to 39", "40 to 44", "45 to 49",
                                       "50 to 54", "55 to 59", "60 to 64", 
                                       "65 to 69", "70 to 74", "75 to 79", 
                                       "80 to 84", "85 to 89", "90 to 94", "95 plus"),
                            val > 0) 


data_date_O$age <- factor(data_date_O$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                      "20 to 24", "25 to 29", "30 to 34",
                                                      "35 to 39", "40 to 44", "45 to 49",
                                                      "50 to 54", "55 to 59", "60 to 64",
                                                      "65 to 69", "70 to 74", "75 to 79",
                                                      "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_O <- ggplot(data_date_O, aes(x = age, y = val, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
  labs(
    title = "Comparison of Liver cancer due to other causes DALYs (1990 vs 2021)",
    x = "Age (year)",
    y = "DALYs rate (per 1000000 populations)",
    fill = "Year"
  ) +
  theme_minimal(base_family = "Times New Roman") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5), 
    plot.background = element_rect(fill = "white"), 
    panel.background = element_rect(fill = "white"),  
    axis.title = element_text(size = 25),  
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 25),  
    legend.position = c(0.9, 0.9),  
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 18)
  )

data_date_O_female$age <- factor(data_date_O_female$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                      "20 to 24", "25 to 29", "30 to 34",
                                                      "35 to 39", "40 to 44", "45 to 49",
                                                      "50 to 54", "55 to 59", "60 to 64",
                                                      "65 to 69", "70 to 74", "75 to 79",
                                                      "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_O_female <- ggplot(data_date_O_female, aes(x = age, y = val, fill = year)) + 
          geom_bar(stat = "identity", position = "dodge") + 
          scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
          labs(
                    title = "Comparison of Liver cancer due to other causes DALYs (1990 vs 2021)",
                    x = "Age (year)",
                    y = "DALYs rate (per 1000000 populations)",
                    fill = "Year"
          ) +theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5), 
    plot.background = element_rect(fill = "white",color = NA),  
    panel.background = element_rect(fill = "white",color = NA), 
    panel.border = element_blank(), 
    axis.title = element_text(size = 25),  
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 25),  
    legend.position = c(0.8, 0.9),  
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 18),  
    plot.margin = margin(0, 0, 0, 0),  
    panel.spacing = unit(0, "lines") 
  )


ggsave(filename = "Comparison of Liver cancer due to other causes DALYs.tiff",
       plot = figure_combined_O,
       height = 10,
       width = 14,
       dpi = 300)
ggsave(filename = "Comparison of Liver cancer due to other causes DALYsfemale.tiff",
       plot = figure_combined_O_female,
       height = 10,
       width = 14,
       dpi = 300)


data_combined_N <- dat_N_all %>%
  filter(year %in% c(1990, 2021)) %>%
  mutate(year = factor(year))

data_filtered_N <- data_combined_N[data_combined_N$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_N <- dplyr::filter(data_filtered_N,data_filtered_N$location == "Global",
                             data_filtered_N$metric == "Rate",
                             data_filtered_N$age %in% c("<5","5 to 9","10 to 14","15 to 19",
                                                        "20 to 24","25 to 29","30 to 34",
                                                        "35 to 39","40 to 44", "45 to 49",
                                                        "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                        "70 to 74","75 to 79","80 to 84",
                                                        "85 to 89","90 to 94","95 plus" ))
data_date_N<- dplyr::filter(data_filtered_N,
                            location == "Global",
                            metric == "Rate",
                            age %in% c("<5", "5 to 9", "10 to 14", "15 to 19",
                                       "20 to 24", "25 to 29", "30 to 34",
                                       "35 to 39", "40 to 44", "45 to 49",
                                       "50 to 54", "55 to 59", "60 to 64", 
                                       "65 to 69", "70 to 74", "75 to 79", 
                                       "80 to 84", "85 to 89", "90 to 94", "95 plus"),
                            val > 0) 


data_combined_N_female <- dat_N_all_female %>%
          filter(year %in% c(1990, 2021)) %>%
          mutate(year = factor(year))

data_filtered_N_female <- data_combined_N_female[data_combined_N_female$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_N_female <- dplyr::filter(data_filtered_N_female,data_filtered_N_female$location == "Global",
                             data_filtered_N_female$metric == "Rate",
                             data_filtered_N_female$age %in% c("<5","5 to 9","10 to 14","15 to 19",
                                                        "20 to 24","25 to 29","30 to 34",
                                                        "35 to 39","40 to 44", "45 to 49",
                                                        "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                        "70 to 74","75 to 79","80 to 84",
                                                        "85 to 89","90 to 94","95 plus" ))
data_date_N_female<- dplyr::filter(data_filtered_N_female,
                            location == "Global",
                            metric == "Rate",
                            age %in% c("<5", "5 to 9", "10 to 14", "15 to 19",
                                       "20 to 24", "25 to 29", "30 to 34",
                                       "35 to 39", "40 to 44", "45 to 49",
                                       "50 to 54", "55 to 59", "60 to 64", 
                                       "65 to 69", "70 to 74", "75 to 79", 
                                       "80 to 84", "85 to 89", "90 to 94", "95 plus"),
                            val > 0) 

data_date_N$age <- factor(data_date_N$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                      "20 to 24", "25 to 29", "30 to 34",
                                                      "35 to 39", "40 to 44", "45 to 49",
                                                      "50 to 54", "55 to 59", "60 to 64",
                                                      "65 to 69", "70 to 74", "75 to 79",
                                                      "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_N <- ggplot(data_date_N, aes(x = age, y = val, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
  labs(
    title = "Comparison of Liver cancer due to NASH DALYs (1990 vs 2021)",
    x = "Age (year)",
    y = "DALYs rate (per 1000000 populations)",
    fill = "Year"
  ) +
  theme_minimal(base_family = "Times New Roman") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  
    plot.background = element_rect(fill = "white"), 
    panel.background = element_rect(fill = "white"),  
    axis.title = element_text(size = 25),  
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 25),  
    legend.position = c(0.95, 0.95),  
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 18)
  )

data_date_N_female$age <- factor(data_date_N_female$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                      "20 to 24", "25 to 29", "30 to 34",
                                                      "35 to 39", "40 to 44", "45 to 49",
                                                      "50 to 54", "55 to 59", "60 to 64",
                                                      "65 to 69", "70 to 74", "75 to 79",
                                                      "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_N_female <- ggplot(data_date_N_female, aes(x = age, y = val, fill = year)) + 
          geom_bar(stat = "identity", position = "dodge") + 
          scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
          labs(
                    title = "Comparison of Liver cancer due to NASH DALYs (1990 vs 2021)",
                    x = "Age (year)",
                    y = "DALYs rate (per 1000000 populations)",
                    fill = "Year"
          ) +
          theme_minimal(base_family = "Times New Roman") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  
    plot.background = element_rect(fill = "white",color = NA), 
    panel.background = element_rect(fill = "white",color = NA), 
    panel.border = element_blank(), 
    axis.title = element_text(size = 25),  
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 25),  
    legend.position = c(0.8, 0.9),  
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 18),  
    plot.margin = margin(0, 0, 0, 0), 
    panel.spacing = unit(0, "lines") 
  )


ggsave(filename = "Comparison of Liver cancer due to NASH DALYs.tiff",
       plot = figure_combined_N,
       height = 10,
       width = 14,
       dpi = 300)
ggsave(filename = "Comparison of Liver cancer due to NASH DALYsfemale.tiff",
       plot = figure_combined_N_female,
       height = 10,
       width = 14,
       dpi = 300)


data_combined_liver <- dat_liver_all %>%
  filter(year %in% c(1990, 2021)) %>%
  mutate(year = factor(year))

data_filtered_liver <- data_combined_liver[data_combined_liver$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_liver <- dplyr::filter(data_filtered_liver,data_filtered_liver$location == "Global",
                             data_filtered_liver$metric == "Rate",
                             data_filtered_liver$age %in% c("<5","5 to 9","10 to 14","15 to 19",
                                                        "20 to 24","25 to 29","30 to 34",
                                                        "35 to 39","40 to 44", "45 to 49",
                                                        "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                        "70 to 74","75 to 79","80 to 84",
                                                        "85 to 89","90 to 94","95 plus" ))
data_date_liver<- dplyr::filter(data_filtered_liver,
                            location == "Global",
                            metric == "Rate",
                            age %in% c("<5", "5 to 9", "10 to 14", "15 to 19",
                                       "20 to 24", "25 to 29", "30 to 34",
                                       "35 to 39", "40 to 44", "45 to 49",
                                       "50 to 54", "55 to 59", "60 to 64", 
                                       "65 to 69", "70 to 74", "75 to 79", 
                                       "80 to 84", "85 to 89", "90 to 94", "95 plus"),
                            val > 0) 

data_combined_liver_female <- dat_liver_all_female %>%
          filter(year %in% c(1990, 2021)) %>%
          mutate(year = factor(year))

data_filtered_liver_female <- data_combined_liver_female[data_combined_liver_female$measure == "DALYs (Disability-Adjusted Life Years)", ]

data_date_liver_female <- dplyr::filter(data_filtered_liver_female,data_filtered_liver_female$location == "Global",
                                 data_filtered_liver_female$metric == "Rate",
                                 data_filtered_liver_female$age %in% c("<5","5 to 9","10 to 14","15 to 19",
                                                                "20 to 24","25 to 29","30 to 34",
                                                                "35 to 39","40 to 44", "45 to 49",
                                                                "50 to 54","55 to 59", "60 to 64", "65 to 69",
                                                                "70 to 74","75 to 79","80 to 84",
                                                                "85 to 89","90 to 94","95 plus" ))
data_date_liver_female<- dplyr::filter(data_filtered_liver_female,
                                location == "Global",
                                metric == "Rate",
                                age %in% c("<5", "5 to 9", "10 to 14", "15 to 19",
                                           "20 to 24", "25 to 29", "30 to 34",
                                           "35 to 39", "40 to 44", "45 to 49",
                                           "50 to 54", "55 to 59", "60 to 64", 
                                           "65 to 69", "70 to 74", "75 to 79", 
                                           "80 to 84", "85 to 89", "90 to 94", "95 plus"),
                                val > 0)

data_date_liver$age <- factor(data_date_liver$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                      "20 to 24", "25 to 29", "30 to 34",
                                                      "35 to 39", "40 to 44", "45 to 49",
                                                      "50 to 54", "55 to 59", "60 to 64",
                                                      "65 to 69", "70 to 74", "75 to 79",
                                                      "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_liver <- ggplot(data_date_liver, aes(x = age, y = val, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
  labs(
    title = "Comparison of Liver cancer DALYs (1990 vs 2021)",
    x = "Age (year)",
    y = "DALYs rate (per 1000000 populations)",
    fill = "Year"
  ) +
  theme_minimal(base_family = "Times New Roman") +  
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  
    plot.background = element_rect(fill = "white"), 
    panel.background = element_rect(fill = "white"),  
    axis.title = element_text(size = 25),  
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 25), 
    legend.position = c(0.9, 0.9),  
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 18)
  )

data_date_liver_femaale$age <- factor(data_date_liver_female$age, levels = c("<5", "5 to 9", "10 to 14", "15 to 19",
                                                              "20 to 24", "25 to 29", "30 to 34",
                                                              "35 to 39", "40 to 44", "45 to 49",
                                                              "50 to 54", "55 to 59", "60 to 64",
                                                              "65 to 69", "70 to 74", "75 to 79",
                                                              "80 to 84", "85 to 89", "90 to 94", "95 plus"))
figure_combined_liver_female <- ggplot(data_date_liver_female, aes(x = age, y = val, fill = year)) + 
          geom_bar(stat = "identity", position = "dodge") + 
          scale_fill_manual(values = c("#377EB8","#E41A1C"), labels = c("1990", "2021")) +
          labs(
                    title = "Comparison of Liver cancer DALYs (1990 vs 2021)",
                    x = "Age (year)",
                    y = "DALYs rate (per 1000000 populations)",
                    fill = "Year"
          ) +
          theme_minimal(base_family = "Times New Roman") + 
          theme(
                    plot.title = element_text(size = 30, face = "bold", hjust = 0.5), 
                    plot.background = element_rect(fill = "white"),  
                    panel.background = element_rect(fill = "white"),  
                    axis.title = element_text(size = 25), 
                    axis.text.x = element_text(size = 14),  
                    axis.text.y = element_text(size = 25),  
                    legend.position = c(0.9, 0.9),  
                    legend.text = element_text(size = 18),  
                    legend.title = element_text(size = 18)
          )



ggsave(filename = "Comparison of Liver cancer DALYs.tiff",
       plot = figure_combined_N,
       height = 10,
       width = 14,
       dpi = 300)
ggsave(filename = "Comparison of Liver cancer DALYsfemale.tiff",
       plot = figure_combined_N_female,
       height = 10,
       width = 14,
       dpi = 300)



config_stata(path = "E:/New_Folder/stata16",
             version = 16, 
             stata_type = c("SE"))
SI_SDI_B <- GBDslope_index(
  data=df |>
    filter(year %in% c(1990,2021)),
  all_age_range=NULL, 
  SDI = T, 
  GBDregion = F, 
  SuperGBDregion = F)
slope <- SI_SDI_B$slope
unique(df$location)
write.csv(slope,"slope.csv")

SI_SDI_ALL <- GBDslope_index(
          data=df |>
                    filter(year %in% c(1990,2021)),
          all_age_range=NULL, 
          SDI = T, 
          GBDregion = T, 
          SuperGBDregion = T)#
slope <- SI_SDI_ALL$slope
unique(df$location)
write.csv(slope,"slope_all.csv")


slope_plot_B <- ggslope_index(
  data = SI_SDI_B, 
  model = "rlm",
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Liver cancer due to hepatitis B",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+05
)

ggsave("Liver cancer due to hepatitis B.tiff", plot = slope_plot_B, device = "tiff", dpi = 300, width = 7, height = 5)


slope_plot_B_female <- ggslope_index(
          data = SI_SDI_B, 
          model = "rlm",
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer due to hepatitis B",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05
)

ggsave("Liver cancer due to hepatitis Bfemale.tiff", plot = slope_plot_B_female, device = "tiff", dpi = 300, width = 7, height = 5)

ggsave("Liver_cancer_due_to_hepatitis_B_female.pdf", 
       plot = slope_plot_B_female, 
       device = "pdf", 
       dpi = 300,  
       width = 7, 
       height = 5)

slope_plot_H <- ggslope_index(
  data = SI_SDI_B, 
  model = "rlm",
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Hepatoblastoma",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+05
)

ggsave("Hepatoblastoma.tiff", plot = slope_plot_H, device = "tiff", dpi = 300, width = 7, height = 5)

slope_plot_H_female <- ggslope_index(
          data = SI_SDI_B, 
          model = "rlm",
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Hepatoblastoma",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05
)

ggsave("Hepatoblastoma.female.tiff", plot = slope_plot_H_female, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("Hepatoblastoma.female.pdf", 
       plot = slope_plot_H_female, 
       device = "pdf", 
       dpi = 300,  
       width = 7, 
       height = 5)

slope_plot_N <- ggslope_index(
  data = SI_SDI_B, 
  model = "rlm",
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Liver cancer due to NASH",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+05
)

ggsave("Liver cancer due to NASH.tiff", plot = slope_plot_N, device = "tiff", dpi = 300, width = 7, height = 5)

slope_plot_N_female <- ggslope_index(
          data = SI_SDI_B, 
          model = "rlm",
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer due to NASH",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05
)

ggsave("Liver cancer due to NASHfemale.tiff", plot = slope_plot_N_female, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("Liver cancer due to NASHfemale.pdf", 
       plot = slope_plot_N_female, 
       device = "pdf", 
       dpi = 300, 
       width = 7, 
       height = 5)

slope_plot_O <- ggslope_index(
  data = SI_SDI_B, 
  model = "rlm",
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  cause_name = "Liver cancer due to other causes",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+05
)

ggsave("Liver cancer due to other causes.tiff", plot = slope_plot_O, device = "tiff", dpi = 300, width = 7, height = 5)

slope_plot_O_female <- ggslope_index(
          data = SI_SDI_B, 
          model = "rlm",
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer due to other causes",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05
)

ggsave("Liver cancer due to other causes.female.tiff", plot = slope_plot_O_female, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("Liver cancer due to other causes.female.pdf", 
       plot = slope_plot_O_female, 
       device = "pdf", 
       dpi = 300,  
       width = 7, 
       height = 5)

slope_plot_C <- ggslope_index(
  data = SI_SDI_B, 
  model = "rlm",
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Liver cancer due to hepatitis C",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+05
)

ggsave("Liver cancer due to hepatitis C.tiff", plot = slope_plot_C, device = "tiff", dpi = 300, width = 7, height = 5)

slope_plot_C_female <- ggslope_index(
          data = SI_SDI_B, 
          model = "rlm",
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer due to hepatitis C",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05
)

ggsave("Liver cancer due to hepatitis Cfemale.tiff", plot = slope_plot_C_female, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("Liver cancer due to hepatitis Cfemale.pdf", 
       plot = slope_plot_C_female, 
       device = "pdf", 
       dpi = 300,  
       width = 7, 
       height = 5)
slope_plot_Liver_cancer <- ggslope_index(
  data = SI_SDI_B, 
  model = "rlm",
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Liver cancer",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  population_count = 1e+05
)

ggsave("Liver cancer.tiff", plot = slope_plot_Liver_cancer, device = "tiff", dpi = 300, width = 7, height = 5)

slope_plot_Liver_cancer_female <- ggslope_index(
          data = SI_SDI_B, 
          model = "rlm",
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05
)

ggsave("Liver cancer.female.tiff", plot = slope_plot_Liver_cancer_female, device = "tiff", dpi = 300, width = 7, height = 5)

slope_plot_A_female <- ggslope_index(
          data = SI_SDI_B, 
          model = "rlm",
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer due to alcohol use",
          rei_name = NULL,
          age_name = "Age-standardized",#
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05
)

ggsave("Liver cancer due to alcohol use.female.tiff", plot = slope_plot_A_female, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("Liver cancer due to alcohol use.female.pdf", 
       plot = slope_plot_A_female, 
       device = "pdf", 
       dpi = 300,  
       width = 7, 
       height = 5)

CI_SDI_B <- GBDconcentration_index(
  data= df|>
    filter(year %in% c(1990,2021)),
  all_age_range = NULL,
  SDI = T,
  GBDregion = F, 
  SuperGBDregion = F)
concentration_index <- CI_SDI_B $Concentration_index
write.csv(concentration_index,"concentration_index.csv")

CI_SDI_ALL <- GBDconcentration_index(
          data= df|>
                    filter(year %in% c(1990,2021)),
          all_age_range = NULL,
          SDI = T, 
          GBDregion = T, 
          SuperGBDregion = T)
concentration_index_all <- CI_SDI_ALL$Concentration_index
write.csv(concentration_index_all,"concentration_index_all.csv")

library(splines)
slopt_concentration_H <- ggconcentration_index(
  data = CI_SDI_B,
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Hepatoblastoma",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+05,
  line_type = c("geom_smooth")
)
ggsave("Hepatoblastoma.tiff", plot = slopt_concentration_H, device = "tiff", dpi = 300, width = 7, height = 5)


slopt_concentration_H_female <- ggconcentration_index(
          data = CI_SDI_B,#
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Hepatoblastoma",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05,
          line_type = c("geom_smooth")
)
ggsave("Hepatoblastoma.female.tiff", plot = slopt_concentration_H_female, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("Hepatoblastoma.female.pdf", 
       plot = slopt_concentration_H_female, 
       device = "pdf", 
       dpi = 300, 
       height = 5)
slopt_concentration_N <- ggconcentration_index(
  data = CI_SDI_B,
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Liver cancer due to NASH",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+05,
  line_type = c("geom_smooth")
)
ggsave("Liver cancer due to NASH.tiff", plot = slopt_concentration_N, device = "tiff", dpi = 300, width = 7, height = 5)


slopt_concentration_N_female <- ggconcentration_index(
          data = CI_SDI_B,
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer due to NASH",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05,
          line_type = c("geom_smooth")
)
ggsave("Liver cancer due to NASH.female.tiff", plot = slopt_concentration_N_female, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("Liver cancer due to NASH.female.pdf", 
       plot = slopt_concentration_N_female, 
       device = "pdf", 
       dpi = 300, 
       width = 7, 
       height = 5)
slopt_concentration_O <- ggconcentration_index(
  data = CI_SDI_B,
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Liver cancer due to other causes",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),,
  country_label = NULL,
  population_count = 1e+05,
  line_type = c("geom_smooth")
)
ggsave("Liver cancer due to other causes.tiff", plot = slopt_concentration_O, device = "tiff", dpi = 300, width = 7, height = 5)


slopt_concentration_O_female <- ggconcentration_index(
          data = CI_SDI_B,
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer due to other causes",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05,
          line_type = c("geom_smooth")
)
ggsave("Liver cancer due to other causes.female.tiff", plot = slopt_concentration_O_female, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("Liver cancer due to other causes.female.pdf", 
       plot = slopt_concentration_O_female, 
       device = "pdf", 
       dpi = 300, 
       width = 7, 
       height = 5)
slopt_concentration_C <- ggconcentration_index(
  data = CI_SDI_B,
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Liver cancer due to hepatitis C",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+05,
  line_type = c("geom_smooth")
)
ggsave("Liver cancer due to hepatitis C.tiff", plot = slopt_concentration_C, device = "tiff", dpi = 300, width = 7, height = 5)


slopt_concentration_C_female <- ggconcentration_index(
          data = CI_SDI_B,
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer due to hepatitis C",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05,
          line_type = c("geom_smooth")
)
ggsave("Liver cancer due to hepatitis C.female.tiff", plot = slopt_concentration_C_female, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("Liver cancer due to hepatitis C.female.pdf", 
       plot = slopt_concentration_C_female, 
       device = "pdf", 
       dpi = 300,  
       width = 7, 
       height = 5)
slopt_concentration_B <- ggconcentration_index(
  data = CI_SDI_B,
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Liver cancer due to hepatitis B",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+05,
  line_type = c("geom_smooth")
)
ggsave("Liver cancer due to hepatitis B.tiff", plot = slopt_concentration_B, device = "tiff", dpi = 300, width = 7, height = 5)

slopt_concentration_B_female <- ggconcentration_index(
          data = CI_SDI_B,
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer due to hepatitis B",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05,
          line_type = c("geom_smooth")
)
ggsave("Liver cancer due to hepatitis B.female.tiff", plot = slopt_concentration_B_female, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("Liver cancer due to hepatitis B.female.pdf", 
       plot = slopt_concentration_B_female, 
       device = "pdf", 
       dpi = 300,  
       width = 7, 
       height = 5)

slopt_concentration_A <- ggconcentration_index(
  data = CI_SDI_B,
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Liver cancer due to alcohol use",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+05,
  line_type = c("geom_smooth")
)
ggsave("Liver cancer due to hepatitis A.tiff", plot = slopt_concentration_A, device = "tiff", dpi = 300, width = 7, height = 5)


slopt_concentration_A_female <- ggconcentration_index(
          data = CI_SDI_B,
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer due to alcohol use",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05,
          line_type = c("geom_smooth")
)
ggsave("Liver cancer due to hepatitis A.female.tiff", plot = slopt_concentration_A_female, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("Liver cancer due to hepatitis A.female.pdf", 
       plot = slopt_concentration_A_female, 
       device = "pdf", 
       dpi = 300,  
       width = 7, 
       height = 5)
slopt_concentration_liver_cancer <- ggconcentration_index(
  data = CI_SDI_B,
  color_name = c("#377EB8","#E41A1C"),
  group_name = c("year"),
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Liver cancer",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+05,
  line_type = c("geom_smooth")
)
ggsave("Liver cancer.tiff", plot = slopt_concentration_liver_cancer, device = "tiff", dpi = 300, width = 7, height = 5)

slopt_concentration_liver_cancer_female <- ggconcentration_index(
          data = CI_SDI_B,
          color_name = c("#377EB8","#E41A1C"),
          group_name = c("year"),
          region_name = "All included",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          sex_name = "Female",
          cause_name = "Liver cancer",
          rei_name = NULL,
          age_name = "Age-standardized",
          year_name = c(1990, 2021),
          country_label = NULL,
          population_count = 1e+05,
          line_type = c("geom_smooth")
)
ggsave("Liver cancer.female.tiff", plot = slopt_concentration_liver_cancer_female, device = "tiff", dpi = 300, width = 7, height = 5)

CI_compare <- GBDconcentration_compare(
          data = CI_SDI_B,
          group_name = c("year"),
          region_name = "All included",
          measure_name = "Deaths",
          sex_name = "Female",
          cause_name = "Liver cancer due to alcohol use",
          rei_name = NULL,
          age_name = "All ages",
          year_name = c(1990, 2021),
          digits = 2
)
?GBDconcentration_compare


decomposition_H <- df[!(df$age %in% c("All ages", "Age-standardized")) & 
                        df$cause == "Hepatoblastoma" & 
                        df$measure == "DALYs (Disability-Adjusted Life Years)" & 
                        df$val > 0, ]

decomposition_N <- df[!(df$age %in% c("All ages", "Age-standardized")) & 
                         df$cause == "Liver cancer due to NASH" & 
                         df$measure == "DALYs (Disability-Adjusted Life Years)"& 
                        df$val > 0, ]

decomposition_O <- df[!(df$age %in% c("All ages", "Age-standardized")) & 
                         df$cause == "Liver cancer due to other causes" & 
                         df$measure == "DALYs (Disability-Adjusted Life Years)"& 
                        df$val > 0, ]

decomposition_C <- df[!(df$age %in% c("All ages", "Age-standardized")) & 
                         df$cause == "Liver cancer due to hepatitis C" & 
                         df$measure == "DALYs (Disability-Adjusted Life Years)"& 
                        df$val > 0, ]

decomposition_B <- df[!(df$age %in% c("All ages", "Age-standardized")) & 
                         df$cause == "Liver cancer due to hepatitis B" & 
                         df$measure == "DALYs (Disability-Adjusted Life Years)"& 
                        df$val > 0, ]

decomposition_A <- df[!(df$age %in% c("All ages", "Age-standardized")) & 
                         df$cause == "Liver cancer due to alcohol use" & 
                         df$measure == "DALYs (Disability-Adjusted Life Years)"& 
                        df$val > 0, ]

decomposition_liver <- df[!(df$age %in% c("All ages", "Age-standardized")) & 
                         df$cause == "Liver cancer" & 
                         df$measure == "DALYs (Disability-Adjusted Life Years)"& 
                           df$val > 0, ]
decomposition_result_H <- GBDdecomposition(
  data = decomposition_H, 
  byear = 1990,
  compareyear = 2021, 
  startage = 0, 
  endage = 5,
  percent = "overall difference"
)
decom_table <- GBDdecomposition_table(
  data=decomposition_result_H,
  digits = 2,
  measure_name = unique(decomposition_H$measure),
  location_name= unique(decomposition_H$location),
  sex_name=unique(decomposition_H$sex),
  rei_name = NULL,
  cause_name=unique(decomposition_H$cause)
)

write.csv(decom_table,"decomposition_result_H.csv", row.names = F)

plot_H <- ggdecomposition(
  data = decomposition_result_H,
  measure_name = unique(decomposition_result_H$measure),
  location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
  sex_name = unique(decomposition_H$sex),
  rei_name = NULL,
  cause_name = unique(decomposition_H$cause),
  percent = FALSE 
) + 
  facet_wrap(vars(sex)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Hepatoblastoma",  
    x = "Location",
    y = "DALYs rate (per 1,000,000 populations)"  
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)), 
    axis.text.x = element_text(margin = margin(t = 5))  
  ) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0"))

ggsave("decomposition_result_H.tiff", plot = plot_H, device = "tiff", dpi = 300, width = 12, height = 5)

plot_H_female <- ggdecomposition(
          data = decomposition_result_H,
          measure_name = unique(decomposition_result_H$measure),
          location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
          sex_name = "Female",
          rei_name = NULL,
          cause_name = unique(decomposition_H$cause),
          percent = FALSE 
) + 
          facet_wrap(vars(sex)) +
          scale_y_continuous(labels = scales::comma) +
          labs(
                    title = "Hepatoblastoma",  
                    x = "Location",
                    y = "DALYs rate (per 1,000,000 populations)"  
          ) +
          theme_minimal(base_size = 14) +
          theme(
                    text = element_text(family = "Times New Roman"),
                    panel.grid.major = element_line(color = "gray80"),
                    panel.grid.minor = element_blank(),
                    legend.position = "bottom",
                    plot.title = element_text(hjust = 0.5),
                    axis.title.x = element_text(margin = margin(t = 10)),  
                    axis.text.x = element_text(margin = margin(t = 5))  
          ) +
          scale_color_manual(values = c("#E41A1C","#377EB8", "#4DAF4A"))

ggsave("decomposition_result_H_female.tiff", plot = plot_H_female, device = "tiff", dpi = 300, width = 12, height = 5)

ggsave("plot_H_female.png", plot_H_female, bg = "white",dpi = 300, width = 12, height = 5)



decomposition_result_N <- GBDdecomposition(
  data = decomposition_N, 
  byear = 1990,
  compareyear = 2021, 
  startage = 15, 
  endage = 95,
  percent = "overall difference"
)

decom_table2 <- GBDdecomposition_table(
  data=decomposition_result_N,
  digits = 2,
  measure_name = unique(decomposition_N$measure),
  location_name= unique(decomposition_N$location),
  sex_name=unique(decomposition_N$sex),
  rei_name = NULL,
  cause_name=unique(decomposition_N$cause)
)

write.csv(decom_table2,"decomposition_result_N.csv", row.names = F)

plot_N <- ggdecomposition(
  data = decomposition_result_N,
  measure_name = unique(decomposition_result_N$measure),
  location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
  sex_name = unique(decomposition_N$sex),
  rei_name = NULL,
  cause_name = unique(decomposition_N$cause),
  percent = FALSE 
) + 
  facet_wrap(vars(sex)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Liver cancer due to NASH",  
    x = "Location",
    y = "DALYs rate (per 1,000,000 populations)"  
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)), 
    axis.text.x = element_text(margin = margin(t = 5))  
  ) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0"))

ggsave("decomposition_result_N.tiff", plot = plot_N, device = "tiff", dpi = 300, width = 12, height = 5)


plot_N_female <- ggdecomposition(
          data = decomposition_result_N,
          measure_name = unique(decomposition_result_N$measure),
          location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
          sex_name = "Female",
          rei_name = NULL,
          cause_name = unique(decomposition_N$cause),
          percent = FALSE 
) + 
          facet_wrap(vars(sex)) +
          scale_y_continuous(labels = scales::comma) +
          labs(
                    title = "Liver cancer due to NASH",  
                    x = "Location",
                    y = "DALYs rate (per 1,000,000 populations)"  
          ) +
          theme_minimal(base_size = 14) +
          theme(
                    text = element_text(family = "Times New Roman"),
                    panel.grid.major = element_line(color = "gray80"),
                    panel.grid.minor = element_blank(),
                    legend.position = "bottom",
                    plot.title = element_text(hjust = 0.5),
                    axis.title.x = element_text(margin = margin(t = 10)),  
                    axis.text.x = element_text(margin = margin(t = 5)) 
          ) +
          scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0"))

ggsave("decomposition_result_N_female.tiff", plot = plot_N_female, device = "tiff", dpi = 300, width = 12, height = 5)
ggsave("plot_N_female.png", plot_N_female, bg = "white",dpi = 300, width = 12, height = 5)


decomposition_result_O <- GBDdecomposition(
  data = decomposition_O, 
  byear = 1990,
  compareyear = 2021, 
  startage = 10, 
  endage = 95,
  percent = "overall difference"
)

decom_table3 <- GBDdecomposition_table(
  data=decomposition_result_O,
  digits = 2,
  measure_name = unique(decomposition_O$measure),
  location_name= unique(decomposition_O$location),
  sex_name=unique(decomposition_O$sex),
  rei_name = NULL,
  cause_name=unique(decomposition_O$cause)
)

write.csv(decom_table3,"decomposition_result_O.csv", row.names = F)

plot_O <- ggdecomposition(
  data = decomposition_result_O,
  measure_name = unique(decomposition_result_O$measure),
  location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
  sex_name = unique(decomposition_O$sex),
  rei_name = NULL,
  cause_name = unique(decomposition_O$cause),
  percent = FALSE 
) + 
  facet_wrap(vars(sex)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Liver cancer due to other causes",  
    x = "Location",
    y = "DALYs rate (per 1,000,000 populations)"  
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.text.x = element_text(margin = margin(t = 5))  
  ) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0"))

ggsave("decomposition_result_O.tiff", plot = plot_O, device = "tiff", dpi = 300, width = 12, height = 5)


plot_O_female <- ggdecomposition(
          data = decomposition_result_O,
          measure_name = unique(decomposition_result_O$measure),
          location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
          sex_name = "Female",
          rei_name = NULL,
          cause_name = unique(decomposition_O$cause),
          percent = FALSE 
) + 
          facet_wrap(vars(sex)) +
          scale_y_continuous(labels = scales::comma) +
          labs(
                    title = "Liver cancer due to other causes",  
                    x = "Location",
                    y = "DALYs rate (per 1,000,000 populations)"  
          ) +
          theme_minimal(base_size = 14) +
          theme(
                    text = element_text(family = "Times New Roman"),
                    panel.grid.major = element_line(color = "gray80"),
                    panel.grid.minor = element_blank(),
                    legend.position = "bottom",
                    plot.title = element_text(hjust = 0.5),
                    axis.title.x = element_text(margin = margin(t = 10)), 
                    axis.text.x = element_text(margin = margin(t = 5))  
          ) +
          scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0"))

ggsave("decomposition_result_O_femle.tiff", plot = plot_O_female, device = "tiff", dpi = 300, width = 12, height = 5)
ggsave("plot_O_female.png", plot_O_female, bg = "white",dpi = 300, width = 12, height = 5)

decomposition_result_C <- GBDdecomposition(
  data = decomposition_C, 
  byear = 1990,
  compareyear = 2021, 
  startage = 10, 
  endage = 95,
  percent = "overall difference"
)

decom_table4 <- GBDdecomposition_table(
  data=decomposition_result_C,
  digits = 2,
  measure_name = unique(decomposition_C$measure),
  location_name= unique(decomposition_C$location),
  sex_name=unique(decomposition_C$sex),
  rei_name = NULL,
  cause_name=unique(decomposition_C$cause)
)

write.csv(decom_table4,"decomposition_result_C.csv", row.names = F)

plot_C <- ggdecomposition(
  data = decomposition_result_C,
  measure_name = unique(decomposition_result_C$measure),
  location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
  sex_name = unique(decomposition_C$sex),
  rei_name = NULL,
  cause_name = unique(decomposition_C$cause),
  percent = FALSE 
) + 
  facet_wrap(vars(sex)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Liver cancer due to hepatitis C",  
    x = "Location",
    y = "DALYs rate (per 1,000,000 populations)"  
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)), 
    axis.text.x = element_text(margin = margin(t = 5))  
  ) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0"))

ggsave("decomposition_result_C.tiff", plot = plot_C, device = "tiff", dpi = 300, width = 12, height = 5)


plot_C_female <- ggdecomposition(
          data = decomposition_result_C,
          measure_name = unique(decomposition_result_C$measure),
          location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
          sex_name = "Female",
          rei_name = NULL,
          cause_name = unique(decomposition_C$cause),
          percent = FALSE 
) + 
          facet_wrap(vars(sex)) +
          scale_y_continuous(labels = scales::comma) +
          labs(
                    title = "Liver cancer due to hepatitis C",  
                    x = "Location",
                    y = "DALYs rate (per 1,000,000 populations)"  
          ) +
          theme_minimal(base_size = 14) +
          theme(
                    text = element_text(family = "Times New Roman"),
                    panel.grid.major = element_line(color = "gray80"),
                    panel.grid.minor = element_blank(),
                    legend.position = "bottom",
                    plot.title = element_text(hjust = 0.5),
                    axis.title.x = element_text(margin = margin(t = 10)), 
                    axis.text.x = element_text(margin = margin(t = 5))  
          ) +
          scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0"))

ggsave("decomposition_result_C_female.tiff", plot = plot_C_female, device = "tiff", dpi = 300, width = 12, height = 5)
ggsave("plot_C_female.png", plot_C_female, bg = "white",dpi = 300, width = 12, height = 5)

decomposition_result_B<- GBDdecomposition(
  data = decomposition_B,
  byear = 1990,
  compareyear = 2021, 
  startage = 10, 
  endage = 95,
  percent = "overall difference"
)

decom_table5 <- GBDdecomposition_table(
  data=decomposition_result_B,
  digits = 2,
  measure_name = unique(decomposition_B$measure),
  location_name= unique(decomposition_B$location),
  sex_name=unique(decomposition_B$sex),
  rei_name = NULL,
  cause_name=unique(decomposition_B$cause)
)

write.csv(decom_table5,"decomposition_result_B.csv", row.names = F)

plot_B <- ggdecomposition(
  data = decomposition_result_B,
  measure_name = unique(decomposition_result_B$measure),
  location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
  sex_name = unique(decomposition_B$sex),
  rei_name = NULL,
  cause_name = unique(decomposition_B$cause),
  percent = FALSE 
) + 
  facet_wrap(vars(sex)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Liver cancer due to hepatitis B",  
    x = "Location",
    y = "DALYs rate (per 1,000,000 populations)"  
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)), 
    axis.text.x = element_text(margin = margin(t = 5))  
  ) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0"))

ggsave("decomposition_result_B.tiff", plot = plot_B, device = "tiff", dpi = 300, width = 12, height = 5)


plot_B_female <- ggdecomposition(
          data = decomposition_result_B,
          measure_name = unique(decomposition_result_B$measure),
          location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
          sex_name = "Female",
          rei_name = NULL,
          cause_name = unique(decomposition_B$cause),
          percent = FALSE 
) + 
          facet_wrap(vars(sex)) +
          scale_y_continuous(labels = scales::comma) +
          labs(
                    title = "Liver cancer due to hepatitis B",  
                    x = "Location",
                    y = "DALYs rate (per 1,000,000 populations)"  
          ) +
          theme_minimal(base_size = 14) +
          theme(
                    text = element_text(family = "Times New Roman"),
                    panel.grid.major = element_line(color = "gray80"),
                    panel.grid.minor = element_blank(),
                    legend.position = "bottom",
                    plot.title = element_text(hjust = 0.5),
                    axis.title.x = element_text(margin = margin(t = 10)),  
                    axis.text.x = element_text(margin = margin(t = 5))  
          ) +
          scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0"))

ggsave("decomposition_result_B_female.tiff", plot = plot_B_female, device = "tiff", dpi = 300, width = 12, height = 5)
ggsave("plot_B_female.png", plot_B_female, bg = "white",dpi = 300, width = 12, height = 5)

decomposition_result_A<- GBDdecomposition(
  data = decomposition_A, 
  byear = 1990,
  compareyear = 2021, 
  startage = 15,
  endage = 95,
  percent = "overall difference"
)

decom_table6 <- GBDdecomposition_table(
  data=decomposition_result_A,
  digits = 2,
  measure_name = unique(decomposition_A$measure),
  location_name= unique(decomposition_A$location),
  sex_name=unique(decomposition_A$sex),
  rei_name = NULL,
  cause_name=unique(decomposition_A$cause)
)

write.csv(decom_table6,"decomposition_result_A.csv", row.names = F)

plot_A <- ggdecomposition(
  data = decomposition_result_A,
  measure_name = unique(decomposition_result_A$measure),
  location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
  sex_name = unique(decomposition_A$sex),
  rei_name = NULL,
  cause_name = unique(decomposition_A$cause),
  percent = FALSE 
) + 
  facet_wrap(vars(sex)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Liver cancer due to hepatitis B",  
    x = "Location",
    y = "DALYs rate (per 1,000,000 populations)"  
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)), 
    axis.text.x = element_text(margin = margin(t = 5))  
  ) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0"))

ggsave("decomposition_result_A.tiff", plot = plot_A, device = "tiff", dpi = 300, width = 12, height = 5)

plot_A_female <- ggdecomposition(
          data = decomposition_result_A,
          measure_name = unique(decomposition_result_A$measure),
          location_name = c("Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
          sex_name = "Female",
          rei_name = NULL,
          cause_name = unique(decomposition_A$cause),
          percent = FALSE 
) + 
          facet_wrap(vars(sex)) +
          scale_y_continuous(labels = scales::comma) +
          labs(
                    title = "Liver cancer due to hepatitis A",  
                    x = "Location",
                    y = "DALYs rate (per 1,000,000 populations)"  
          ) +
          theme_minimal(base_size = 14) +
          theme(
                    text = element_text(family = "Times New Roman"),
                    panel.grid.major = element_line(color = "gray80"),
                    panel.grid.minor = element_blank(),
                    legend.position = "bottom",
                    plot.title = element_text(hjust = 0.5),
                    axis.title.x = element_text(margin = margin(t = 10)),  
                    axis.text.x = element_text(margin = margin(t = 5)) 
          ) +
          scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0"))

ggsave("decomposition_result_A_female.tiff", plot = plot_A_female, device = "tiff", dpi = 300, width = 12, height = 5)
ggsave("plot_A_female.png", plot_A_female, bg = "white",dpi = 300, width = 12, height = 5)
unique(decomposition_A$age)
unique(decomposition_N$age)


str(dd)


dd1 <- dat_temp_B %>%
  filter(age %in% c("Age-standardized", "All ages"))

Boostrap_H <- GBDfrontier(
  data= df|>
    filter(location %in% GBDRegion2021$location),
  sex_name = "Both",
  cause_name = "Hepatoblastoma",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  rei_name = NULL,
  age_name = "Age-standardized",
  boot = 100,
  cpu_num = 10)

frontier_table_H <- GBDfrontier_table(
  frontier_result = Boostrap_H |>
    filter(location %in% GBDRegion2021$location),
  data = df|>
    filter(location %in% GBDRegion2021$location),
  digits = 2,#
  sex_name = "Both",
  cause_name = "Hepatoblastoma",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  age_name = "Age-standardized",
  rei_name = NULL)
write_csv(frontier_table_H, "frintier_table_H.csv")

front_Ha  <- ggfrontier(
  frontier_result = Boostrap_H|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("all years"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_H_a.tiff", plot = front_Ha, device = "tiff", dpi = 300, width = 7, height = 5)

front_Hb <- ggfrontier(
  frontier_result = Boostrap_H|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("single year"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_H_b.tiff", plot = front_Hb, device = "tiff", dpi = 300, width = 7, height = 5)


str(dd1)
unique(dd1$sex)
sort(unique(dd1$year))

library(parallel)
library(doParallel)
remove(Boostrap_H_female)
Boostrap_H_female <- GBDfrontier(
  data= df|>
    filter(location %in% GBDRegion2021$location),
  sex_name = "Female",
  cause_name = "Hepatoblastoma",#
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  rei_name = NULL,
  age_name = "Age-standardized",
  boot = 100,
  cpu_num = 10)

frontier_table_H_female <- GBDfrontier_table(
          frontier_result = Boostrap_H_female |>
                    filter(location %in% GBDRegion2021$location),
          data = df|>
                    filter(location %in% GBDRegion2021$location),
          digits = 2,#
          sex_name = "Female",
          cause_name = "Hepatoblastoma",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          age_name = "Age-standardized",
          rei_name = NULL)
write_csv(frontier_table_H_female, "frintier_table_H_female.csv")

front_Ha_female  <- ggfrontier(
          frontier_result = Boostrap_H_female|>
                    filter(location %in% GBDRegion2021$location),
          smooth_span = 0.3,
          type = c("all years"),
          high_SDI = 0.85,
          low_SDI = 0.5
)
ggsave("front_H_a_female.tiff", plot = front_Ha_female, device = "tiff", dpi = 300, width = 7, height = 5)

front_Hb_female <- ggfrontier(
          frontier_result = Boostrap_H_female|>
                    filter(location %in% GBDRegion2021$location),
          smooth_span = 0.3,
          type = c("single year"),
          high_SDI = 0.85,
          low_SDI = 0.5
)
ggsave("front_H_b_female.tiff", plot = front_Hb_female, device = "tiff", dpi = 300, width = 7, height = 5)



Boostrap_B <- GBDfrontier(
  data= df|>
            filter(location %in% GBDRegion2021$location),
  sex_name = "Both",
  cause_name = "Liver cancer due to hepatitis B",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  rei_name = NULL,
  age_name = "Age-standardized",
  boot = 100,
  cpu_num = 10)

frontier_table_B <- GBDfrontier_table(
          frontier_result = Boostrap_B |>
                    filter(location %in% GBDRegion2021$location),
          data = df|>
                    filter(location %in% GBDRegion2021$location),
          digits = 2,#
          sex_name = "Both",
          cause_name = "Liver cancer due to hepatitis B",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          age_name = "Age-standardized",
          rei_name = NULL)
write_csv(frontier_table_B, "frintier_table_B.csv")

front_Ba  <- ggfrontier(
  frontier_result = Boostrap_B|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("all years"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_B_a.tiff", plot = front_Ba, device = "tiff", dpi = 300, width = 7, height = 5)

front_Bb <- ggfrontier(
  frontier_result = Boostrap_B|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("single year"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_B_b.tiff", plot = front_Bb, device = "tiff", dpi = 300, width = 7, height = 5)


Boostrap_B_female <- GBDfrontier(
          data= df|>
                    filter(location %in% GBDRegion2021$location),
          sex_name = "Female",
          cause_name = "Liver cancer due to hepatitis B",#
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          rei_name = NULL,
          age_name = "Age-standardized",
          boot = 100,
          cpu_num = 10)

frontier_table_B_female <- GBDfrontier_table(
  frontier_result = Boostrap_B_female |>
    filter(location %in% GBDRegion2021$location),
  data = df|>
    filter(location %in% GBDRegion2021$location),
  digits = 2,#
  sex_name = "Female",
  cause_name = "Liver cancer due to hepatitis B",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  age_name = "Age-standardized",
  rei_name = NULL)

write_csv(frontier_table_B_female, "frintier_table_B_female.csv")

front_Ba_female  <- ggfrontier(
  frontier_result = Boostrap_B_female|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("all years"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_B_a_female.tiff", plot = front_Ba_female, device = "tiff", dpi = 300, width = 7, height = 5)

front_Bb_female <- ggfrontier(
  frontier_result = Boostrap_B_female|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("single year"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_B_b_female.tiff", plot = front_Bb_female, device = "tiff", dpi = 300, width = 7, height = 5)



Boostrap_C <- GBDfrontier(
          data= df|>
                    filter(location %in% GBDRegion2021$location),
          sex_name = "Both",
          cause_name = "Liver cancer due to hepatitis C",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          rei_name = NULL,
          age_name = "Age-standardized",
          boot = 100,
          cpu_num = 10)

frontier_table_C <- GBDfrontier_table(
  frontier_result = Boostrap_C |>
    filter(location %in% GBDRegion2021$location),
  data = df|>
    filter(location %in% GBDRegion2021$location),
  digits = 2,#
  sex_name = "Both",
  cause_name = "Liver cancer due to hepatitis C",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  age_name = "Age-standardized",
  rei_name = NULL)
write_csv(frontier_table_C, "frintier_table_C.csv")

front_Ca  <- ggfrontier(
  frontier_result = Boostrap_C|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("all years"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_C_a.tiff", plot = front_Ca, device = "tiff", dpi = 300, width = 7, height = 5)

front_Cb <- ggfrontier(
  frontier_result = Boostrap_C|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("single year"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_C_b.tiff", plot = front_Cb, device = "tiff", dpi = 300, width = 7, height = 5)


Boostrap_C_female <- GBDfrontier(
  data= df|>
    filter(location %in% GBDRegion2021$location),
  sex_name = "Female",
  cause_name = "Liver cancer due to hepatitis C",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  rei_name = NULL,
  age_name = "Age-standardized",
  boot = 100,
  cpu_num = 10) 


frontier_table_C_female <- GBDfrontier_table(
  frontier_result = Boostrap_C_female |>
    filter(location %in% GBDRegion2021$location),
  data = df|>
    filter(location %in% GBDRegion2021$location),
  digits = 2,#
  sex_name = "Female",
  cause_name = "Liver cancer due to hepatitis C",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  age_name = "Age-standardized",
  rei_name = NULL)
write_csv(frontier_table_C_female, "frintier_table_C_female.csv")

front_Ca_female  <- ggfrontier(
  frontier_result = Boostrap_C_female|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("all years"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_C_a_female.tiff", plot = front_Ca_female, device = "tiff", dpi = 300, width = 7, height = 5)

front_Cb_female <- ggfrontier(
  frontier_result = Boostrap_C_female|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("single year"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_C_b_female.tiff", plot = front_Cb_female, device = "tiff", dpi = 300, width = 7, height = 5)


Boostrap_A <- GBDfrontier(
          data= df|>
                    filter(location %in% GBDRegion2021$location),
          sex_name = "Both",
          cause_name = "Liver cancer due to alcohol use",#
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          rei_name = NULL,
          age_name = "Age-standardized",
          boot = 100,
          cpu_num = 10)

frontier_table_A <- GBDfrontier_table(
          frontier_result = Boostrap_A |>
                    filter(location %in% GBDRegion2021$location),
          data = df|>
                    filter(location %in% GBDRegion2021$location),
          digits = 2,#
          sex_name = "Both",
          cause_name = "Liver cancer due to alcohol use",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          age_name = "Age-standardized",
          rei_name = NULL)
write_csv(frontier_table_A, "frintier_table_A.csv")

front_Aa  <- ggfrontier(
          frontier_result = Boostrap_A|>
                    filter(location %in% GBDRegion2021$location),
          smooth_span = 0.3,
          type = c("all years"),
          high_SDI = 0.85,
          low_SDI = 0.5
)
ggsave("front_A_a.tiff", plot = front_Aa, device = "tiff", dpi = 300, width = 7, height = 5)

front_Ab <- ggfrontier(
          frontier_result = Boostrap_A|>
                    filter(location %in% GBDRegion2021$location),
          smooth_span = 0.3,
          type = c("single year"),
          high_SDI = 0.85,
          low_SDI = 0.5
)
ggsave("front_A_b.tiff", plot = front_Ab, device = "tiff", dpi = 300, width = 7, height = 5)


Boostrap_A_female <- GBDfrontier(
  data= df|>
    filter(location %in% GBDRegion2021$location),
  sex_name = "Female",
  cause_name = "Liver cancer due to alcohol use",#
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  rei_name = NULL,
  age_name = "Age-standardized",
  boot = 100,
  cpu_num = 10)

frontier_table_A <- GBDfrontier_table(
  frontier_result = Boostrap_A |>
    filter(location %in% GBDRegion2021$location),
  data = df|>
    filter(location %in% GBDRegion2021$location),
  digits = 2,#
  sex_name = "Both",
  cause_name = "Liver cancer due to alcohol use",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  age_name = "Age-standardized",
  rei_name = NULL)
write_csv(frontier_table_A, "frintier_table_A.csv")

front_Aa  <- ggfrontier(
  frontier_result = Boostrap_A|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("all years"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_A_a.tiff", plot = front_Aa, device = "tiff", dpi = 300, width = 7, height = 5)

front_Ab <- ggfrontier(
  frontier_result = Boostrap_A|>
    filter(location %in% GBDRegion2021$location),
  smooth_span = 0.3,
  type = c("single year"),
  high_SDI = 0.85,
  low_SDI = 0.5
)
ggsave("front_A_b.tiff", plot = front_Ab, device = "tiff", dpi = 300, width = 7, height = 5)


Boostrap_O <- GBDfrontier(
          data= df|>
                    filter(location %in% GBDRegion2021$location),
          sex_name = "Both",
          cause_name = "Liver cancer due to other causes",#
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          rei_name = NULL,
          age_name = "Age-standardized",
          boot = 100,
          cpu_num = 10)

frontier_table_O <- GBDfrontier_table(
          frontier_result = Boostrap_O |>
                    filter(location %in% GBDRegion2021$location),
          data = df|>
                    filter(location %in% GBDRegion2021$location),
          digits = 2,#
          sex_name = "Both",
          cause_name = "Liver cancer due to other causes",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          age_name = "Age-standardized",
          rei_name = NULL)
write_csv(frontier_table_O, "frintier_table_O.csv")

front_Oa  <- ggfrontier(
          frontier_result = Boostrap_O|>
                    filter(location %in% GBDRegion2021$location),
          smooth_span = 0.3,
          type = c("all years"),
          high_SDI = 0.85,
          low_SDI = 0.5
)
ggsave("front_O_a.tiff", plot = front_Oa, device = "tiff", dpi = 300, width = 7, height = 5)

front_Ob <- ggfrontier(
          frontier_result = Boostrap_O|>
                    filter(location %in% GBDRegion2021$location),
          smooth_span = 0.3,
          type = c("single year"),
          high_SDI = 0.85,
          low_SDI = 0.5
)
ggsave("front_O_b.tiff", plot = front_Ob, device = "tiff", dpi = 300, width = 7, height = 5)


Boostrap_N <- GBDfrontier(
          data= df|>
                    filter(location %in% GBDRegion2021$location),
          sex_name = "Both",
          cause_name = "Liver cancer due to NASH",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          rei_name = NULL,
          age_name = "Age-standardized",
          boot = 100,
          cpu_num = 10)

frontier_table_N <- GBDfrontier_table(
          frontier_result = Boostrap_N |>
                    filter(location %in% GBDRegion2021$location),
          data = df|>
                    filter(location %in% GBDRegion2021$location),
          digits = 2,#
          sex_name = "Both",
          cause_name = "Liver cancer due to NASH",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          age_name = "Age-standardized",
          rei_name = NULL)
write_csv(frontier_table_N, "frintier_table_N.csv")

front_Na  <- ggfrontier(
          frontier_result = Boostrap_N|>
                    filter(location %in% GBDRegion2021$location),
          smooth_span = 0.3,
          type = c("all years"),
          high_SDI = 0.85,
          low_SDI = 0.5
)
ggsave("front_N_a.tiff", plot = front_Na, device = "tiff", dpi = 300, width = 7, height = 5)

front_Nb <- ggfrontier(
          frontier_result = Boostrap_N|>
                    filter(location %in% GBDRegion2021$location),
          smooth_span = 0.3,
          type = c("single year"),
          high_SDI = 0.85,
          low_SDI = 0.5
)
ggsave("front_N_b.tiff", plot = front_Nb, device = "tiff", dpi = 300, width = 7, height = 5)


Boostrap_N_female <- GBDfrontier(
          data= df|>
                    filter(location %in% GBDRegion2021$location),
          sex_name = "Female",
          cause_name = "Liver cancer due to NASH",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          rei_name = NULL,
          age_name = "Age-standardized",
          boot = 100,
          cpu_num = 10)

frontier_table_N_female <- GBDfrontier_table(
          frontier_result = Boostrap_N_female |>
                    filter(location %in% GBDRegion2021$location),
          data = df|>
                    filter(location %in% GBDRegion2021$location),
          digits = 2,#
          sex_name = "Female",
          cause_name = "Liver cancer due to NASH",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          age_name = "Age-standardized",
          rei_name = NULL)
write_csv(frontier_table_N_female, "frintier_table_N_female.csv")

front_Na_female  <- ggfrontier(
          frontier_result = Boostrap_N_female|>
                    filter(location %in% GBDRegion2021$location),
          smooth_span = 0.3,
          type = c("all years"),
          high_SDI = 0.85,
          low_SDI = 0.5
)
ggsave("front_N_a_female.tiff", plot = front_Na_female, device = "tiff", dpi = 300, width = 7, height = 5)

front_Nb_female <- ggfrontier(
          frontier_result = Boostrap_N_female|>
                    filter(location %in% GBDRegion2021$location),
          smooth_span = 0.3,
          type = c("single year"),
          high_SDI = 0.85,
          low_SDI = 0.5
)
ggsave("front_N_b_female.tiff", plot = front_Nb_female, device = "tiff", dpi = 300, width = 7, height = 5)


GBD_edition(2021)


df_pointB<- dplyr::filter(df,df$sex == "Female",
                          df$measure=="DALYs (Disability-Adjusted Life Years)",
                          df$cause=="Liver cancer due to hepatitis B")
temp <- df %>% dplyr::filter(location=="Global",
                             age=="Age-standardized",
                             measure=="Deaths",
                             cause=="Liver cancer",
                             sex=="Both")
result_B <- GBDASR_aapc(
  data=df_pointB,
  startyear = 1990,
  endyear = 2021,
  model = "ln",
  joinpoints = 5, 
  rei_included = F,
  CI = TRUE,
  digits = 2,
  sep = " to ",
  constant_variance = T,
  AAPCrange = NULL)


AAPC_B <- result_B $AAPC
write.csv(AAPC_B,
          "AAPC_B.csv", 
          row.names = F)

APC_B <- result_B$APC
write.csv(APC_B,
          "APC_B.csv", 
          row.names = F)
figB <- ggjoinpoint_apc(
          data = result_B,
          location_name = "Global",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to hepatitis B",
          age_name = "Age-standardized",
          rei_name = NULL,
          nudge_y = -0.5,
          facet_name = NULL,
          point_color = "black",
          joinpoint_color = "black",
          line_size = 1
)
figB
remove(figB_SDI)
figB_SDI <- ggjoinpoint_compare(
  data = result_B, 
  group_name = "location",
  location_name = c("Global","High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI",
                    "Low SDI"),
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  cause_name = "Liver cancer due to hepatitis B",
  nudge_y = -0.5,
  sex_name = "Female",
  age_name = "Age-standardized",
  rei_name = NULL,
  facet_name = NULL,
  color_name = pal_lancet("lanonc")(9)[1:6], 
  shape_name = c(14, 15, 16, 17, 18,19),
  line_size = 1
)+
  theme(
    text = element_text(family = "Times New Roman")  
  )
ggsave("joinpoint_B.tiff", plot = figB_SDI, device = "tiff", dpi = 300, width = 14, height = 10)

ggsave("joinpoint_B.pdf", 
       plot = figB_SDI, 
       device = "pdf", 
       dpi = 300,  
       width = 14, 
       height = 10)


df_pointC<- dplyr::filter(df,df$sex == "Female",
                          df$measure=="DALYs (Disability-Adjusted Life Years)",
                          df$cause=="Liver cancer due to hepatitis C")

result_C <- GBDASR_aapc(
          data=df_pointC,
          startyear = 1990, endyear = 2021,
          model = "ln",
          joinpoints = 5, 
          rei_included = F,
          CI = TRUE,
          digits = 2,
          sep = " to ",
          constant_variance = T,
          AAPCrange = NULL )

AAPC_C <- result_C $AAPC
write.csv(AAPC_C,
          "AAPC_C.csv", 
          row.names = F)

APC_C <- result_C$APC
write.csv(APC_C,
          "APC_C.csv", 
          row.names = F)
figC <- ggjoinpoint_apc(
          data = result_C,
          location_name = "Global",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to hepatitis C",
          sex_name = "Female",
          age_name = "Age-standardized",
          rei_name = NULL,
          nudge_y = -0.5,
          facet_name = NULL,
          point_color = "black",
          joinpoint_color = "black",
          line_size = 1
)+
  theme(
    text = element_text(family = "Times New Roman") 
  )

figC

figC_SDI <- ggjoinpoint_compare(
  data = result_C,
  group_name = "location",
  location_name = c("Global","High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI",
                    "Low SDI"),
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  cause_name = "Liver cancer due to hepatitis C",
  nudge_y = -0.5,
  sex_name = "Female",
  age_name = "Age-standardized",
  rei_name = NULL,
  facet_name = NULL,
  color_name = pal_lancet("lanonc")(9)[1:6], 
  shape_name = c(14, 15, 16, 17, 18,19),
  line_size = 1
)+
  theme(
    text = element_text(family = "Times New Roman")  
  )
ggsave("joinpoint_C.tiff", plot = figC_SDI, device = "tiff", dpi = 300, width = 14, height = 10)



df_pointH<- dplyr::filter(df,df$sex == "Female",
                          df$measure=="DALYs (Disability-Adjusted Life Years)",
                          df$cause=="Hepatoblastoma")

result_H <- GBDASR_aapc(
          data=df_pointH,
          startyear = 1990,
          endyear = 2021,
          model = "ln",
          joinpoints = 5, 
          rei_included = F,
          CI = TRUE,
          digits = 2,
          sep = " to ",
          constant_variance = T,
          AAPCrange = NULL )

AAPC_H <- result_H $AAPC
write.csv(AAPC_H,
          "AAPC_H.csv", 
          row.names = F)

APC_H <- result_H$APC
write.csv(APC_H,
          "APC_H.csv", 
          row.names = F)
figH <- ggjoinpoint_apc(
          data = result_H,
          location_name = "Global",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Hepatoblastoma",
          sex_name = "Female",
          age_name = "Age-standardized",
          rei_name = NULL,
          nudge_y = -0.5,
          facet_name = NULL,
          point_color = "black",
          joinpoint_color = "black",
          line_size = 1
)



figH_SDI <- ggjoinpoint_compare(
  data = result_H, 
  group_name = "location",
  location_name = c("Global","High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI",
                    "Low SDI"),
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  cause_name = "Hepatoblastoma",
  nudge_y = -0.5,
  sex_name = "Female",
  age_name = "Age-standardized",
  rei_name = NULL,
  facet_name = NULL,
  color_name = pal_lancet("lanonc")(9)[1:6], 
  shape_name = c(14, 15, 16, 17, 18,19),
  line_size = 1
)+
  theme(
    text = element_text(family = "Times New Roman")  
  )
ggsave("joinpoint_H.tiff", plot = figH_SDI, device = "tiff", dpi = 300, width = 14, height = 10)


df_pointN<- dplyr::filter(df,df$sex == "Female",
                          df$measure=="DALYs (Disability-Adjusted Life Years)",
                          df$cause=="Liver cancer due to NASH")

result_N <- GBDASR_aapc(
          data=df_pointN,
          startyear = 1990,
          endyear = 2021,
          model = "ln",
          joinpoints = 5, 
          rei_included = F,
          CI = TRUE,
          digits = 2,
          sep = " to ",
          constant_variance = T,
          AAPCrange = NULL )

AAPC_N <- result_N $AAPC
write.csv(AAPC_N,
          "AAPC_N.csv", 
          row.names = F)

APC_N <- result_N$APC
write.csv(APC_N,
          "APC_N.csv", 
          row.names = F)
figN <- ggjoinpoint_apc(
          data = result_N,
          location_name = "Global",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to NASH",
          sex_name = "Female",
          age_name = "Age-standardized",
          rei_name = NULL,
          nudge_y = -0.5,
          facet_name = NULL,
          point_color = "black",
          joinpoint_color = "black",
          line_size = 1
)



figN_SDI <- ggjoinpoint_compare(
  data = result_N,
  group_name = "location",
  location_name = c("Global","High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI",
                    "Low SDI"),
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  cause_name = "Liver cancer due to NASH",
  nudge_y = -0.5,
  sex_name = "Female",
  age_name = "Age-standardized",
  rei_name = NULL,
  facet_name = NULL,
  color_name = pal_lancet("lanonc")(9)[1:6], 
  shape_name = c(14, 15, 16, 17, 18,19),
  line_size = 1
)+
  theme(
    text = element_text(family = "Times New Roman")  
  )
ggsave("joinpoint_N.tiff", plot = figN_SDI, device = "tiff", dpi = 300, width = 14, height = 10)


df_pointO<- dplyr::filter(df,df$sex == "Female",
                          df$measure=="DALYs (Disability-Adjusted Life Years)",
                          df$cause=="Liver cancer due to other causes")

result_O <- GBDASR_aapc(
          data=df_pointO,
          startyear = 1990,
          endyear = 2021,
          model = "ln",
          joinpoints = 5, 
          rei_included = F,
          CI = TRUE,
          digits = 2,
          sep = " to ",
          constant_variance = T,
          AAPCrange = NULL )

AAPC_O <- result_O $AAPC
write.csv(AAPC_O,
          "AAPC_O.csv", 
          row.names = F)

APC_O <- result_O$APC
write.csv(APC_O,
          "APC_O.csv", 
          row.names = F)
figO <- ggjoinpoint_apc(
          data = result_O,
          location_name = "Global",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to other causes",
          sex_name = "Female",
          age_name = "Age-standardized",
          rei_name = NULL,
          nudge_y = -0.5,
          facet_name = NULL,
          point_color = "black",
          joinpoint_color = "black",
          line_size = 1
)


figO_SDI <- ggjoinpoint_compare(
  data = result_O, 
  group_name = "location",
  location_name = c("Global","High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI",
                    "Low SDI"),
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  cause_name = "Liver cancer due to other causes",
  nudge_y = -0.5,
  sex_name = "Female",
  age_name = "Age-standardized",
  rei_name = NULL,
  facet_name = NULL,
  color_name = pal_lancet("lanonc")(9)[1:6], 
  shape_name = c(14, 15, 16, 17, 18,19),
  line_size = 1
)+
  theme(
    text = element_text(family = "Times New Roman")  
  )
ggsave("joinpoint_O.tiff", plot = figO_SDI, device = "tiff", dpi = 300, width = 14, height = 10)


df_pointA<- dplyr::filter(df,df$sex == "Female",
                          df$measure=="DALYs (Disability-Adjusted Life Years)",
                          df$cause=="Liver cancer due to alcohol use")

result_A <- GBDASR_aapc(
          data=df_pointA,
          startyear = 1990,
          endyear = 2021,
          model = "ln",
          joinpoints = 5, 
          rei_included = F,
          CI = TRUE,
          digits = 2,
          sep = " to ",
          constant_variance = T,
          AAPCrange = NULL )

AAPC_A <- result_A $AAPC
write.csv(AAPC_A,
          "AAPC_A.csv", 
          row.names = F)

APC_A <- result_A$APC
write.csv(APC_A,
          "APC_A.csv", 
          row.names = F)
figA <- ggjoinpoint_apc(
          data = result_A,
          location_name = "Global",
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to alcohol use",
          sex_name = "Female",
          age_name = "Age-standardized",
          rei_name = NULL,
          nudge_y = -0.5,
          facet_name = NULL,
          point_color = "black",
          joinpoint_color = "black",
          line_size = 1
)



figA_SDI <- ggjoinpoint_compare(
  data = result_A, 
  group_name = "location",
  location_name = c("Global","High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI",
                    "Low SDI"),
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  cause_name = "Liver cancer due to alcohol use",
  nudge_y = -0.5,
  sex_name = "Female",
  age_name = "Age-standardized",
  rei_name = NULL,
  facet_name = NULL,
  color_name = pal_lancet("lanonc")(9)[1:6], 
  shape_name = c(14, 15, 16, 17, 18,19),
  line_size = 1
)+
  theme(
    text = element_text(family = "Times New Roman")  
  )
ggsave("joinpoint_A.tiff", plot = figA_SDI, device = "tiff", dpi = 300, width = 14, height = 10)

ggsave("figA.tiff", plot = figA, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("figB.tiff", plot = figB, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("figC.tiff", plot = figC, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("figH.tiff", plot = figH, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("figN.tiff", plot = figN, device = "tiff", dpi = 300, width = 7, height = 5)
ggsave("figO.tiff", plot = figO, device = "tiff", dpi = 300, width = 7, height = 5)


bapc_results_B <- GBDbapc_prediction(
          data = df,
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to hepatitis B",
          location_name = "Global",
          rei_name = NULL,
          By_sex = F,
          predyear = 2044,
          full_age_adjusted = T,
          rate_lessen = NULL,
          pop_predict = "GBD"
)
age_number_data_B <- bapc_results_B[[1]]
ASR_rate_data_B <- bapc_results_B[[3]]
write.csv(age_number_data_B, "bapc_results_B_age_number.csv", row.names = FALSE)
write.csv(ASR_rate_data_B, "bapc_results_B_ASR_rate.csv", row.names = FALSE)


bapc_results_B_figure <- ggprediction_Dx(
          data = bapc_results_B,
          ratio = "auto", 
          CI = T, 
          predict_start = 2022,
          group_name = "location",
          location_name = "Global", 
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to hepatitis B",
          rei_name = NULL,
          sex_name = "Female" 
) +
  ggtitle("Liver cancer due to hepatitis B") +  
  theme(
    text = element_text(family = "Times New Roman"),  
    plot.title = element_text(family = "Times New Roman", size = 16, hjust = 0.5)  
  )
ggsave("bapc_results_B.tiff", plot = bapc_results_B_figure, device = "tiff", dpi = 300, width = 7, height = 5)

bapc_results_B_china <- GBDbapc_prediction(
          data = df,
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to hepatitis B",
          location_name = c("China","India"),
          rei_name = NULL,
          By_sex = F,
          predyear = 2044,
          full_age_adjusted = T,
          rate_lessen = NULL,
          pop_predict = "GBD"
)
bapc_results_B_figure_china <- ggprediction_Dx(
          data = bapc_results_B_china,
          ratio = "auto", 
          CI = T, 
          predict_start = 2022,
          group_name = "location",
          location_name = c("China","India"), 
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to hepatitis B",
          rei_name = NULL,
          sex_name = "Female" 
)
Bchina <- bapc_results_B_figure_china + 
          scale_color_manual(values = c("China" = "#E41A1C", "India" = "#377EB8")) + 
          theme(legend.position = "right") +  
          labs(color = "group")

ggsave("bapc_results_B_china.tiff", plot = bapc_results_B_figure_china, device = "tiff", dpi = 300, width = 7, height = 5)

ggsave(filename = "bapc_results_B_china.pdf",
       plot = Bchina,
       device = cairo_pdf,
       height = 7,
       width = 5,
       dpi = 300)


bapc_results_C <- GBDbapc_prediction(
          data = df,
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to hepatitis C",
          location_name = "Global",
          rei_name = NULL,
          By_sex = F,
          predyear = 2044,
          full_age_adjusted = T,
          rate_lessen = NULL,
          pop_predict = "GBD"
)
age_number_data_C <- bapc_results_C[[1]]
ASR_rate_data_C <- bapc_results_C[[3]]
write.csv(age_number_data_C, "bapc_results_C_age_number.csv", row.names = FALSE)
write.csv(ASR_rate_data_C, "bapc_results_C_ASR_rate.csv", row.names = FALSE)

bapc_results_C_figure <- ggprediction_Dx(
          data = bapc_results_C,
          ratio = "auto", 
          CI = T,
          predict_start = 2022,
          group_name = "location",
          location_name = "Global", 
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to hepatitis C",
          rei_name = NULL,
          sex_name = "Female" 
)+
  ggtitle("Liver cancer due to hepatitis C") +  
  theme(
    text = element_text(family = "Times New Roman"),  
    plot.title = element_text(family = "Times New Roman", size = 16, hjust = 0.5),  
ggsave("bapc_results_C.tiff", plot = bapc_results_C_figure, device = "tiff", dpi = 300, width = 7, height = 5))


bapc_results_C_china <- GBDbapc_prediction(
          data = df,
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to hepatitis C",
          location_name = c("China","India"),
          rei_name = NULL,
          By_sex = F,
          predyear = 2044,
          full_age_adjusted = T,
          rate_lessen = NULL,
          pop_predict = "GBD"
)
bapc_results_C_figure_china <- ggprediction_Dx(
          data = bapc_results_C_china,
          ratio = "auto", 
          CI = T, 
          predict_start = 2022,
          group_name = "location",
          location_name = c("China","India"), 
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to hepatitis C",
          rei_name = NULL,
          sex_name = "Female" 
)
ggsave("bapc_results_C_china.tiff", plot = bapc_results_C_figure_china, device = "tiff", dpi = 300, width = 7, height = 5)


bapc_results_A <- GBDbapc_prediction(
          data = df,
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to alcohol use",
          location_name = "Global",
          rei_name = NULL,
          By_sex = F,
          predyear = 2044,
          full_age_adjusted = T,
          rate_lessen = NULL,
          pop_predict = "GBD"
)

age_number_data_A <- bapc_results_A[[1]]
ASR_rate_data_A <- bapc_results_A[[3]]
write.csv(age_number_data_A, "bapc_results_A_age_number.csv", row.names = FALSE)
write.csv(ASR_rate_data_A, "bapc_results_A_ASR_rate.csv", row.names = FALSE)

bapc_results_A_figure <- ggprediction_Dx(
          data = bapc_results_A,
          ratio = "auto", 
          CI = T,
          predict_start = 2022,
          group_name = "location",
          location_name = "Global", 
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to alcohol use",
          rei_name = NULL,
          sex_name = "Female" 
)+
  ggtitle("Liver cancer due to alcohol use") +  
  theme(
    text = element_text(family = "Times New Roman"),  
    plot.title = element_text(family = "Times New Roman", size = 16, hjust = 0.5)  
  )
ggsave("bapc_results_A.tiff", plot = bapc_results_A_figure, device = "tiff", dpi = 300, width = 7, height = 5)

bapc_results_A_china <- GBDbapc_prediction(
          data = df,
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to alcohol use",
          location_name = c("China","India"),
          rei_name = NULL,
          By_sex = F,
          predyear = 2044,
          full_age_adjusted = T,
          rate_lessen = NULL,
          pop_predict = "GBD"
)
bapc_results_A_figure_china <- ggprediction_Dx(
          data = bapc_results_A_china,
          ratio = "auto", 
          CI = T, 
          predict_start = 2022,
          group_name = "location",
          location_name = c("China","India"), 
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to alcohol use",
          rei_name = NULL,
          sex_name = "Female" 
)
ggsave("bapc_results_A_china.tiff", plot = bapc_results_A_figure_china, device = "tiff", dpi = 300, width = 7, height = 5)



bapc_results_H <- GBDbapc_prediction(
          data = df,
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Hepatoblastoma",
          location_name = "Global",
          rei_name = NULL,
          By_sex = F,
          predyear = 2044,
          full_age_adjusted = T,
          rate_lessen = NULL,
          pop_predict = "GBD"
)


age_number_data_H <- bapc_results_H[[1]]
ASR_rate_data_H <- bapc_results_H[[3]]
write.csv(age_number_data_H, "bapc_results_H_age_number.csv", row.names = FALSE)
write.csv(ASR_rate_data_H, "bapc_results_H_ASR_rate.csv", row.names = FALSE)


bapc_results_H_figure <- ggprediction_Dx(
          data = bapc_results_H,
          ratio = "auto", 
          CI = T, 
          predict_start = 2022,
          group_name = "location",
          location_name = "Global", 
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Hepatoblastoma",
          rei_name = NULL,
          sex_name = "Female" 
)+ 
  ggtitle("Hepatoblastoma") +  
  theme(
    text = element_text(family = "Times New Roman"),  
    plot.title = element_text(family = "Times New Roman", size = 16, hjust = 0.5)  
  )
ggsave("bapc_results_H.tiff", plot = bapc_results_H_figure, device = "tiff", dpi = 300, width = 7, height = 5)


bapc_results_H_china <- GBDbapc_prediction(
          data = df,
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Hepatoblastoma",
          location_name = c("China","India"),
          rei_name = NULL,
          By_sex = F,
          predyear = 2044,
          full_age_adjusted = T,
          rate_lessen = NULL,
          pop_predict = "GBD"
)
bapc_results_H_figure_china <- ggprediction_Dx(
          data = bapc_results_H_china,
          ratio = "auto", 
          CI = T, 
          predict_start = 2022,
          group_name = "location",
          location_name = c("China","India"), 
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Hepatoblastoma",
          rei_name = NULL,
          sex_name = "Female" 
)
ggsave("bapc_results_H_china.tiff", plot = bapc_results_H_figure_china, device = "tiff", dpi = 300, width = 7, height = 5)


bapc_results_O <- GBDbapc_prediction(
          data = df,
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to other causes",
          location_name = "Global",
          rei_name = NULL,
          By_sex = F,
          predyear = 2044,
          full_age_adjusted = T,
          rate_lessen = NULL,
          pop_predict = "GBD"
)

age_number_data_O <- bapc_results_O[[1]]
ASR_rate_data_O <- bapc_results_O[[3]]
write.csv(age_number_data_O, "bapc_results_O_age_number.csv", row.names = FALSE)
write.csv(ASR_rate_data_O, "bapc_results_O_ASR_rate.csv", row.names = FALSE)

bapc_results_O_figure <- ggprediction_Dx(
          data = bapc_results_O,
          ratio = "auto", 
          CI = T, 
          predict_start = 2022,
          group_name = "location",
          location_name = "Global", 
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to other causes",
          rei_name = NULL,
          sex_name = "Female" 
)+ 
  ggtitle("Liver cancer due to other causes") +  
  theme(
    text = element_text(family = "Times New Roman"),  
    plot.title = element_text(family = "Times New Roman", size = 16, hjust = 0.5)  
  )
ggsave("bapc_results_O.tiff", plot = bapc_results_O_figure, device = "tiff", dpi = 300, width = 7, height = 5)


remove(DFDF)

library(dplyr)

DFDF <- df %>%
          filter(measure == "DALYs (Disability-Adjusted Life Years)",
                 cause == "Liver cancer due to NASH",
                 location == "Global",
                 age != "<5",          
                 age != "5 to 9") 

print(DFDF)




library(dplyr)
remove(DFDF)

bapc_results_N <- GBDbapc_prediction(
          data = DFDF,
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to NASH",
          location_name = "Global",
          rei_name = NULL,
          By_sex = F,
          predyear = 2044,
          full_age_adjusted = T,
          rate_lessen = NULL,
          pop_predict = "GBD"
)

age_number_data_N <- bapc_results_N[[1]]
ASR_rate_data_N <- bapc_results_N[[3]]
write.csv(age_number_data_N, "bapc_results_N_age_number.csv", row.names = FALSE)
write.csv(ASR_rate_data_N, "bapc_results_N_ASR_rate.csv", row.names = FALSE)


bapc_results_N_figure <- ggprediction_Dx(
  data = bapc_results_N,  
  ratio = "auto", 
  CI = TRUE,  
  predict_start = 2022,  
  group_name = "location",
  location_name = "Global", 
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  cause_name = "Liver cancer due to NASH",
  rei_name = NULL,
  sex_name = "Female"
) + 
  ggtitle("Liver cancer due to NASH") +  
  theme(
    text = element_text(family = "Times New Roman"),  
    plot.title = element_text(family = "Times New Roman", size = 16, hjust = 0.5)  
  )

ggsave("bapc_results_N.tiff", plot = bapc_results_N_figure, device = "tiff", dpi = 300, width = 7, height = 5)




bapc_results_B_231 <- GBDbapc_prediction(
          data = df,
          measure_name = "DALYs (Disability-Adjusted Life Years)",
          cause_name = "Liver cancer due to hepatitis B",
          location_name = unique(df$location),
          rei_name = NULL,
          By_sex = F,
          predyear = 2044,
          full_age_adjusted = T,
          rate_lessen = NULL,
          pop_predict = "GBD"
)
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE, type = "binary")

c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#A156B0")
