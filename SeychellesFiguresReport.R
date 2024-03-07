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

mg1$Depth <- fct_rev(mg1$sample_depth_)

mg2 <- read_excel("Soil_outer_NOV22_Pawel_2811.xlsx", sheet = "LOI550outer") %>%
  select(Island_Name,sample_depth_, Carbon_density_g_cm3,Bulk_Density_Corrected_g_C_m3,OC_percent )%>%
  mutate(Location = "Outer")

mg2$Depth <- fct_rev(mg2$sample_depth_)

mg <- rbind(mg1,mg2)
mg$Island_Name <- as.factor(as.character(mg$Island_Name))

names(mg)

#Plot inner and outer 1 (Soil by Island)========
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


#PLOT inner 2 (Soil By Depth)=======
names(mg)

i1<-ggplot(mg1, aes(y=Depth, x=Carbon_density_g_cm3*1000))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_Name))+  #aes(color = Island_Name)
  xlab(bquote("Soil carbon density "  (mg*~cm^-3)))+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("a)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "top",
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
  ggtitle("b)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size=12),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


i2

i3<-ggplot(mg, aes(y=Depth, x=OC_percent))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_Name))+    #aes(color = Island_Name)
  xlab("Soil carbon content (%) ")+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("c)")+
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

plot_inner_depth <- grid.arrange(i1,i2,i3, ncol = 1)

ggsave(plot_inner_depth , dpi=600, width = 12, height = 15,
       filename = "FIG_plot_inner_depth_2811.png")

ggsave(plot_inner_depth , dpi=600, width = 12, height = 15,
       filename = "FIG_plot_inner_depth_2811.pdf")

#PLOT outer 2 (Soil By Depth)=======
names(mg2)

o1<-ggplot(mg2, aes(y=Depth, x=Carbon_density_g_cm3*1000))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_Name))+  #aes(color = Island_Name)
  xlab(bquote("Soil carbon density "  (mg*~cm^-3)))+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("a)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "top",
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
  ggtitle("b)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size=12),
        plot.title = element_text(hjust = 0.0,lineheight=1.2, face="bold",size=20))


o2

o3<-ggplot(mg2, aes(y=Depth, x=OC_percent))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Island_Name))+    #aes(color = Island_Name)
  xlab("Soil carbon content (%) ")+
  ylab("Soil depth (cm) ")+
  labs(color = "Island Name:")+
  ggtitle("c)")+
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

plot_outer_depth <- grid.arrange(o1,o2,o3, ncol = 1)

ggsave(plot_outer_depth , dpi=600, width = 12, height = 15,
       filename = "FIG_plot_outer_depth_2811.png")

ggsave(plot_outer_depth , dpi=600, width = 12, height = 15,
       filename = "FIG_plot_outer_depth_2811.pdf")

#send it as svg file - seems highest quality compared to png.


#write.csv(mg, file = "mg.csv", row.names = F) #Excel DATA for summary pivots

