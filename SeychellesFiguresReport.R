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

getwd()
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Seychelles/DATA")
#Replace (CTRL+H) spaces,with _, % with percent and -, (, ) with nothing in Excel first in a new sheet.

mg1 <- read_excel("Soil_inner_NOV22_Pawel_2811.xlsx", sheet = "LOI550inner") %>%
  select(Island_Name,sample_depth_, Carbon_density_g_cm3,Bulk_Density_Corrected_g_C_m3,OC_percent )%>%
  mutate(Location = "Inner")

mg1$Depth <- fct_rev(mg1$sample_depth_) #Reversing depth intervals so that 0-10 is on top

mg2 <- read_excel("Soil_outer_NOV22_Pawel_2811.xlsx", sheet = "LOI550outer") %>%
  select(Island_Name,sample_depth_, Carbon_density_g_cm3,Bulk_Density_Corrected_g_C_m3,OC_percent )%>%
  mutate(Location = "Outer")

mg2$Depth <- fct_rev(mg2$sample_depth_)

#Binding data from inner and outer islands:
mg <- rbind(mg1,mg2)
mg$Island_Name <- as.factor(as.character(mg$Island_Name))
names(mg)


#STATS:===========
library(sjPlot)
library(sjstats)
library(sjmisc)
dbd_model <- lm(Bulk_Density_Corrected_g_C_m3 ~ Location, data = mg)
isl_model <- lm(Bulk_Density_Corrected_g_C_m3 ~ Island_Name , data = mg)
oc_model <- lm(Bulk_Density_Corrected_g_C_m3 ~ Location , data = mg)
cd_model <- lm(Carbon_density_g_cm3*1000 ~ Location , data = mg)

tab_model(dbd_model)
tab_model(isl_model)
tab_model(oc_model)
tab_model(cd_model)


#RESULTS NUMBERS======
mg_dbd_sum <- mg %>% #getting mean values for he results
  group_by(Island_Name) %>% 
  summarise(AV = mean(Bulk_Density_Corrected_g_C_m3, na.rm=T),
            N = n(),
            SD = sd(Bulk_Density_Corrected_g_C_m3, na.rm=T),
            SE = SD/sqrt(N))

mg_dbd_sum

mg_SOC_sum <- mg %>% #getting mean values for he results
  group_by(Island_Name) %>% 
  summarise(AV = mean(OC_percent, na.rm=T),
            N = n(),
            SD = sd(OC_percent, na.rm=T),
            SE = SD/sqrt(N))

mg_SOC_sum

mg_CD_sum <- mg %>% #getting mean values for he results
  group_by(Island_Name) %>% 
  summarise(AV = mean(Carbon_density_g_cm3*1000, na.rm=T),
            N = n(),
            SD = sd(Carbon_density_g_cm3*1000, na.rm=T),
            SE = SD/sqrt(N))

mg_CD_sum

#Plot inner and outer islands (Soil by Island)========
a <-ggplot(mg, aes(x=Island_Name, y=Carbon_density_g_cm3*1000, fill=Island_Name))+ #, fill = Island_Name
    geom_boxplot(outlier.shape = NA) +

  scale_y_continuous(limits = c(0,80))+
  xlab("")+ ylab(bquote("Soil carbon density "  (mg*~cm^-3)))+
  ggtitle("")+
 theme_classic()+
 theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
    
        rect = element_rect(fill = "transparent"), #turning background transparent
    
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))+
  
   geom_rect(aes(xmin=0, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey", alpha=0.01)+
      geom_boxplot(outlier.shape = NA) 

  
a #Soil carbon density 


# Assuming 'Island_Name' is a factor variable
#mg$Island_Name <- factor(mg$Island_Name, levels = unique(mg$Island_Name))

b <-ggplot(mg, aes(x=Island_Name, y=Bulk_Density_Corrected_g_C_m3, fill=Island_Name))+  #, fill = Island_Name
  geom_boxplot(outlier.shape = NA) +
  xlab("")+ ylab(bquote("Soil bulk density "  (g*~cm^-3)))+
  ggtitle("")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        
        rect = element_rect(fill = "transparent"), #turning background transparent
    
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))+
  
     geom_rect(aes(xmin=0, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey", alpha=0.01)+
      geom_boxplot(outlier.shape = NA) 


b

c<-ggplot(mg, aes(x=Island_Name, y=OC_percent, fill=Island_Name))+  #, fill = Island_Name
  geom_boxplot(outlier.shape = NA) +
  xlab("")+
  ylab(bquote("Soil organic carbon content (%) \n"))+ 
  ggtitle("")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        
        rect = element_rect(fill = "transparent"), #turning background transparent
    
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))+
  
     geom_rect(aes(xmin=0, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey", alpha=0.01)+
      geom_boxplot(outlier.shape = NA) 



c

#Join all plots together:
plot1 <- grid.arrange(b,c,a, ncol = 1)

ggsave(plot1 , dpi=1200, width = 12, height = 15,filename = "PlotSoil_InnerOuter_2024_inColour_1200DPI_GreyShade_V2.png")
ggsave(plot1 , dpi=1200, width = 12, height = 15,filename = "PlotSoil_InnerOuter_2024_inColour_1200DPI_GreyShade_V4.svg")


#PLOT outer 2 (Soil By Depth, Fig_S2 in MS)=======
names(mg2)

o1<-ggplot(mg2, aes(y=Depth, x=Carbon_density_g_cm3*1000))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_Name))+  #aes(color = Island_Name)
  xlab(bquote("Soil carbon density "  (mg*~cm^-3)))+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("C)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size=12),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


o1

o2<-ggplot(mg2, aes(y=Depth, x=Bulk_Density_Corrected_g_C_m3))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_Name))+   #aes(color = Island_Name)
  xlab(bquote("Soil bulk density "  (g*~cm^-3)))+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("A)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size=12),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


o2

o3<-ggplot(mg2, aes(y=Depth, x=OC_percent))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_Name))+    #aes(color = Island_Name)
  xlab("Soil organic carbon content (%) ")+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("B)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "bold", size=10),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


o3

plot_outer_depth <- grid.arrange(o2,o3,o1, ncol = 1)

ggsave(plot_outer_depth , dpi=600, width = 12, height = 15,
       filename = "FIG_plot_outer_depth_29Apr24.png")

ggsave(plot_outer_depth , dpi=600, width = 12, height = 15,
       filename = "FIG_plot_outer_depth_light.pdf")

#send it as svg file - seems highest quality compared to png.
ggsave(plot_outer_depth, dpi=1200, width = 12, height = 15,filename = "FIG_S2_plot_outer_depth_29Apr24.svg")


#write.csv(mg, file = "mg.csv", row.names = F) #Excel DATA for summary pivots



#PLOT inner 2 (Soil By Depth, Fig S3 in MS)=======
names(mg)

i1<-ggplot(mg1, aes(y=Depth, x=Carbon_density_g_cm3*1000))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_Name))+  #aes(color = Island_Name)
  xlab(bquote("Soil carbon density "  (mg*~cm^-3)))+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("C)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size=10),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


i1

i2<-ggplot(mg1, aes(y=Depth, x=Bulk_Density_Corrected_g_C_m3))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_Name))+   #
  xlab(bquote("Soil bulk density "  (g*~cm^-3)))+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("A)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size=12),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


i2

i3<-ggplot(mg, aes(y=Depth, x=OC_percent))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_Name))+    #aes(color = Island_Name)
  xlab("Soil organic carbon content (%) ")+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("B)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size=12),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


i3

plot_inner_depth <- grid.arrange(i2,i3,i1, ncol = 1)

ggsave(plot_inner_depth , dpi=600, width = 12, height = 15,
       filename = "FIG_plot_inner_depth_30April24.png")

ggsave(plot_inner_depth , dpi=600, width = 12, height = 15,
       filename = "FIG_plot_inner_depth_2811.pdf")

ggsave(plot_inner_depth, dpi=1200, width = 12, height = 15,filename = "FIG_S3_plot_inner_depth_30Apr24.svg")

# TREE DENSITY:=============
#Height, DBH, Density by mangrove species:
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Seychelles")
mg <- read_excel("SeychellesMangroveReview.xlsx", sheet = "Mangrove_BC_stock")

mg_height <- select(mg, Height_m,DBH_cm,TreeDensity_ha, Species)%>%
  group_by(Species) %>%
    summarise(AV_Height_m = mean(Height_m, na.rm = T),
            SD = sd(AV_Height_m, na.rm = T),
            N  = length(AV_Height_m),
            SE = SD / sqrt(N)) %>%
   arrange(desc(AV_Height_m))

mg_DBH_cm <- select(mg, Height_m,DBH_cm,TreeDensity_ha, Species)%>%
  group_by(Species) %>%
    summarise(AV_DBH_cm  = mean(Height_m, na.rm = T),
            SD = sd(AV_DBH_cm , na.rm = T),
            N  = length(AV_DBH_cm ),
            SE = SD / sqrt(N)) %>%
   arrange(desc(AV_DBH_cm ))

mg_TreeDensity_ha <- select(mg, Height_m,TreeDensity_ha,TreeDensity_ha, Species)%>%
  group_by(Species) %>%
    summarise(AV_TreeDensity_ha  = mean(TreeDensity_ha, na.rm = T),
            SD = sd(AV_TreeDensity_ha , na.rm = T),
            N  = length(AV_TreeDensity_ha ),
            SE = SD / sqrt(N)) %>%
   arrange(desc(AV_TreeDensity_ha ))


mg_TreeDensity_ha

max_sp_above_Height_m<-  round(max(mg$Height_m, na.rm=T),1)
max_mg_Height_m_sp1  <- mg[which.max(mg$Height_m),"Species"]

max_sp_above_DBH_cm <-  round(max(mg$DBH_cm, na.rm=T),1)
max_mg_height_sp1  <- mg[which.max(mg$DBH_cm),"Species"]


#Stats:
mg_TreeDensity_ha <- select(mg, Height_m,TreeDensity_ha,Species)%>%
  group_by(Species) %>%
    summarise(AV_TreeDensity_ha  = mean(TreeDensity_ha, na.rm = T),
            SD = sd(AV_TreeDensity_ha , na.rm = T),
            N  = length(AV_TreeDensity_ha ),
            SE = SD / sqrt(N)) %>%
   arrange(desc(AV_TreeDensity_ha ))


den1_model <- lm(TreeDensity_ha ~ Location, data = mg)
den2_model <- lm(DBH_cm ~ Location , data = mg)
hei_model <- lm(as.numeric(Height_m) ~ Location , data = mg)

tab_model(den1_model)
tab_model(den2_model)
tab_model(hei_model)

