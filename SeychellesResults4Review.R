#LOAD DATA and LIBRARIES first:=======
if (!require(ggpmisc)) install.packages('ggpmisc')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(gridExtra)) install.packages('gridExtra')
if (!require(svglite)) install.packages('svglite')
if (!require(readxl)) install.packages('readxl')

library(svglite)
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpmisc)

library(sjstats)
library(sjmisc)
library(sjPlot)

library(gridExtra)

getwd()#see where your working directory is. Then set it to your fave place:
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Seychelles/DATA")
#Replace (CTRL+H) spaces,with _, % with percent and -, (, ) with nothing in Excel first in a new sheet.

mg1 <- read_excel("Soil_inner_NOV22_Pawel_2811.xlsx", sheet = "LOI550inner") %>%
  select(Plot_ID,SliceLength_cm,Compaction_factor, Island_Name,sample_depth_, Carbon_density_g_cm3,Bulk_Density_Corrected_g_C_m3,OC_percent, P2 )%>%
  mutate(Location = "Inner")

mg1$Depth <- fct_rev(mg1$sample_depth_) #Reversing depth intervals so that 0-10 is on top

mg2 <- read_excel("Soil_outer_NOV22_Pawel_2811.xlsx", sheet = "LOI550outer") %>%
  select(Plot_ID,SliceLength_cm,Compaction_factor, Island_Name,sample_depth_, Carbon_density_g_cm3,Bulk_Density_Corrected_g_C_m3,OC_percent,P2 )%>%
  mutate(Location = "Outer")

mg2$Depth <- fct_rev(mg2$sample_depth_)

#Binding data from inner and outer islands:
mg <- rbind(mg1,mg2)
mg$Island_Name <- as.factor(as.character(mg$Island_Name))
names(mg)

#DBD ========
# Perform ANOVA
dbd_isl_model_aov <- aov(Bulk_Density_Corrected_g_C_m3 ~ Island_Name, data = mg)
summary(dbd_isl_model_aov) # Summary of the ANOVA model
tukey_result <- TukeyHSD(dbd_isl_model_aov) # Perform Tukey's HSD test
tukey_result
# Print Tukey's test results

# Convert Tukey's HSD results into a data frame
tukey_summary <- as.data.frame(tukey_result$Island_Name)

# Rename columns for better clarity
colnames(tukey_summary) <- c( "Difference", "Lower CI", "Upper CI", "p-value")

# Round the numeric values in Tukey's summary to 3 decimal places
tukey_summary[] <- lapply(tukey_summary, function(x) if(is.numeric(x)) round(x, 3) else x)

# Perform ANOVA summary and round the numeric values
aov_summary <- summary(dbd_isl_model_aov)



#DBD PLOTt:============
mg$sample_depth_ <- as.factor(as.character(mg$sample_depth_))
mg_dbd_sum <- mg %>% #getting mean values for he results
  group_by(Island_Name,sample_depth_ ) %>% 
  summarise(AV = mean(Bulk_Density_Corrected_g_C_m3, na.rm=T),
            N = n(),
            SD = sd(Bulk_Density_Corrected_g_C_m3, na.rm=T),
            SE = SD/sqrt(N))

mg_dbd_sum

# Convert sample_depth_ to numeric midpoints 
mg_dbd_sum$sample_depth_numeric <- as.numeric(gsub(" -.*", "", mg_dbd_sum$sample_depth_)) + 5

# Create the plot
p1 <- ggplot(mg_dbd_sum, aes(x = sample_depth_numeric, y = AV, col = Island_Name)) +
  geom_point(size = 3) +  # Plot points with consistent size
  geom_line(aes(group = Island_Name), linewidth = 1) +  # Connect points for each Island_Name
  labs(y = "", x = "Soil depth (cm)") +
  
  ggtitle(bquote("Dry bulk density" ~ (g ~ cm^-3))) +  # Title
  
  scale_y_continuous(breaks = seq(0, 2, by = 0.2))  +  # Reverse y-axis and add ticks
  scale_x_reverse(  # Move x-axis to the top
      breaks = seq(5, 100, by = 10),   # Set 10 breaks on the reversed x-axis
      labels = c("0 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", 
                 "50 - 60", "60 - 70", "70 - 80", "80 - 90", "90 - 100")) +
  
  coord_flip() +  # Flip coordinates
  
  theme_classic() +  # Use a clean theme
  theme(
    axis.text.x = element_text(vjust = 0.5, size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20, vjust = -0.5),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5))  # Center-align and set title size

p1


#OC% (organic Carbon) stats ===========
oc_model <- lm(OC_percent ~ Location , data = mg)
tab_model(oc_model,show.se = T, show.ci = F)

oc_isl_model <- lm(OC_percent ~ Island_Name , data = mg)
tab_model(oc_isl_model,show.se = T, show.ci = F)


#SOC PLOT:==========
mg$sample_depth_ <- as.factor(as.character(mg$sample_depth_))
mg_SOC_sum <- mg %>% #getting mean values for he results
  group_by(Island_Name,sample_depth_ ) %>% 
  summarise(AV = mean(OC_percent, na.rm=T),
            N = n(),
            SD = sd(OC_percent, na.rm=T),
            SE = SD/sqrt(N))

mg_SOC_sum
range( mg_SOC_sum$AV)#4.669123 29.103962

# Convert sample_depth_ to numeric midpoints 
mg_SOC_sum$sample_depth_numeric <- as.numeric(gsub(" -.*", "", mg_SOC_sum$sample_depth_)) + 5
mg_SOC_sum$sample_depth_numeric

# Create the plot
p2 <- ggplot(mg_SOC_sum, aes(x = sample_depth_numeric, y = AV, col = Island_Name)) +
  geom_point(size = 3) +  # Plot points with consistent size
  geom_line(aes(group = Island_Name), linewidth = 1) +  # Connect points for each Island_Name
  labs(y = "", x = "Soil depth (cm)") +
  
  ggtitle("C content (%)") +  # Title
  
  scale_y_continuous(breaks = seq(0, 30, by = 5))  +  # Reverse y-axis and add ticks
  scale_x_reverse(  # Move x-axis to the top
      breaks = seq(5, 100, by = 10),   # Set 10 breaks on the reversed x-axis
      labels = c("0 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", 
                 "50 - 60", "60 - 70", "70 - 80", "80 - 90", "90 - 100")) +
  
  coord_flip() +  # Flip coordinates
  
  theme_classic() +  # Use a clean theme
  theme(
    axis.text.x = element_text(vjust = 0.5, size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20, vjust = -0.5),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5))  # Center-align and set title size

p2



# Perform ANOVA
soc_isl_model_aov <- aov(OC_percent ~ Island_Name, data = mg)
summary(soc_isl_model_aov) # Summary of the ANOVA model
tukey_result <- TukeyHSD(soc_isl_model_aov) # Perform Tukey's HSD test
tukey_result
# Print Tukey's test results

# Convert Tukey's HSD results into a data frame
tukey_summary <- as.data.frame(tukey_result$Island_Name)

# Rename columns for better clarity
colnames(tukey_summary) <- c( "Difference", "Lower CI", "Upper CI", "p-value")

# Round the numeric values in Tukey's summary to 3 decimal places
tukey_summary[] <- lapply(tukey_summary, function(x) if(is.numeric(x)) round(x, 3) else x)

# Perform ANOVA summary and round the numeric values
aov_summary <- summary(soc_isl_model_aov)
aov_summary





# CD PLOT=======
mg_CD_sum <- mg %>% #getting mean values for he results
  group_by(Island_Name, sample_depth_) %>% 
  summarise(AV = mean(Carbon_density_g_cm3 * 1000, na.rm=T),
            N = n(),
            SD = sd(Carbon_density_g_cm3*1000, na.rm=T),
            SE = SD/sqrt(N))

mg_CD_sum
range(mg_CD_sum$AV)# 32.55108 56.68010


#PLOT Carbon Density
# Convert sample_depth_ to numeric midpoints 
mg_CD_sum$sample_depth_numeric <- as.numeric(gsub(" -.*", "", mg_CD_sum$sample_depth_)) + 5
mg_CD_sum$sample_depth_numeric

# Create the plot
p3 <- ggplot(mg_CD_sum, aes(x = sample_depth_numeric, y = AV, col = Island_Name)) +
  geom_point(size = 3) +  # Plot points with consistent size
  geom_line(aes(group = Island_Name), linewidth = 1) +  # Connect points for each Island_Name
  labs(y = "", x = "Soil depth (cm)", col = "Island") +
  
  ggtitle(bquote("Carbon density" ~ (mgC ~ cm^-3))) +  # Title
  
  scale_y_continuous(breaks = seq(30, 60, by = 5), limits = c(30,60))  +  # Reverse y-axis and add ticks
  scale_x_reverse(  # Move x-axis to the top
      breaks = seq(5, 95, by = 10),   # Set 10 breaks on the reversed x-axis
      labels = c("0 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", 
                 "50 - 60", "60 - 70", "70 - 80", "80 - 90", "90 - 100")) +
  
  coord_flip() +  # Flip coordinates
  
  theme_classic() +  # Use a clean theme
  theme(
    axis.text.x = element_text(vjust = 0.5, size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20, vjust = -0.5),
    legend.position =  "right",  #c(0.9, 0.75),
      legend.text = element_text(size = 16),  # Double the legend text size (from default 16)
    legend.title = element_text(size = 16),  # Double the legend title size (from default 16)
    plot.title = element_text(size = 20, hjust = 0.5))  # Center-align and set title size

p3


# Perform ANOVA
soc_isl_model_aov <- aov(Carbon_density_g_cm3 ~ Island_Name, data = mg)
summary(soc_isl_model_aov) # Summary of the ANOVA model
tukey_result <- TukeyHSD(soc_isl_model_aov) # Perform Tukey's HSD test
tukey_result # Print Tukey's test results

# Convert Tukey's HSD results into a data frame
tukey_summary <- as.data.frame(tukey_result$Island_Name)

# Rename columns for better clarity
colnames(tukey_summary) <- c( "Difference", "Lower CI", "Upper CI", "p-value")

# Round the numeric values in Tukey's summary to 3 decimal places
tukey_summary[] <- lapply(tukey_summary, function(x) if(is.numeric(x)) round(x, 3) else x)
tukey_summary

# Perform ANOVA summary and round the numeric values
aov_summary <- summary(soc_isl_model_aov)
aov_summary




#Combine Plots==========
plot3 <- grid.arrange(p1,p2,p3, ncol = 3)
plot3
ggsave(plot3 , dpi=600, width = 17, height = 9, filename = "Plot_Seychelles_BySection3.png")
ggsave(plot3, dpi=1200, width = 17, height = 9,filename = "SVG_Plot_Seychelles_BySection3.svg")
