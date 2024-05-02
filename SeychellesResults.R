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


#MAX CORE DEPTH============
#P2 is our max depth =  the PipeLength- Compaction_out (how deeply the pipe was inserted into the sediments )
MaxDepth <- mg %>%
        group_by(Plot_ID,Island_Name, Location) %>%
        summarise(MaxCoreDepth = max(P2))
MaxDepth
hist(MaxDepth$MaxCoreDepth)

MaxDepthPlot <- ggplot(MaxDepth, aes(x=Island_Name, y=MaxCoreDepth, fill=Location))+ #, fill = Island_Name
    geom_boxplot(outlier.shape = NA) +
  geom_jitter()+

  xlab("Island")+ ylab(bquote("Maximum core depth (cm)"))+
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
      geom_boxplot(outlier.shape = NA,alpha=0.1) 

MaxDepthPlot
#ggsave(MaxDepthPlot, dpi=300, width = 16, height = 9,filename = "MaxCoreDepth2.png")
#ggsave(MaxDepthPlot, dpi=1200, width = 16, height = 9,filename = "MaxCoreDepth.svg")

#STATS on ma dept:
max_model <- lm(MaxCoreDepth ~ Location, data = MaxDepth)
tab_model(max_model,show.se = T, show.ci = F)

max_model <- lm(MaxCoreDepth ~ Island_Name, data = MaxDepth)
tab_model(max_model,show.se = T, show.ci = F)

#STATS:===========
library(sjPlot)
library(sjstats)
library(sjmisc)


#DBD ========
dbd_model <- lm(Bulk_Density_Corrected_g_C_m3 ~ Location, data = mg)
tab_model(dbd_model,show.se = T, show.ci = F)

dbd_isl_model <- lm(Bulk_Density_Corrected_g_C_m3 ~ Island_Name , data = mg)
tab_model(dbd_isl_model,show.se = T, show.ci = F)


#OC% (organic Carbon) stats ===========
oc_model <- lm(OC_percent ~ Location , data = mg)
tab_model(oc_model,show.se = T, show.ci = F)

oc_isl_model <- lm(OC_percent ~ Island_Name , data = mg)
tab_model(oc_isl_model,show.se = T, show.ci = F)

#Stock==========
mg$CarbonStock.Mgha <- mg$Carbon_density_g_cm3 * 100 * mg$SliceLength_cm * mg$Compaction_factor

stock <- mg %>%
  group_by(Plot_ID, Island_Name, Location)%>%
  summarise(CoreStock_Mgha = sum(CarbonStock.Mgha))

stock_model <- lm(CoreStock_Mgha ~ Location , data = stock)
tab_model(stock_model,show.se = T, show.ci = F)

isl_stock_model <- lm(CoreStock_Mgha ~ Island_Name , data = stock)
tab_model(isl_stock_model,show.se = T, show.ci = F)


#PLOT:
StockPlot <- ggplot(stock , aes(x=Island_Name, y=CoreStock_Mgha, fill=Location))+ #, fill = Island_Name
    geom_boxplot(outlier.shape = NA) +
  geom_jitter()+

  xlab("Island")+ ylab(bquote("Soil carbon stock "  (Mg*~ha^-2)))+
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
      geom_boxplot(outlier.shape = NA,alpha=0.1) 

StockPlot
ggsave(StockPlot, dpi=300, width = 16, height = 9,filename = "StockPlot2.png")
ggsave(StockPlot, dpi=1200, width = 16, height = 9,filename = "StockPlot.svg")





#Average DBD, SOC, CD======
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



# Seychelles TREE DENSITY:=============
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/Seychelles/DATA")
t <- read_excel("COMBINED_BC_dataset_Seychelles_NOV22.xlsx", sheet = "Above_Belowground")
names(t)

tree<- t %>% select(PlotID,Density_tree_ha, IslandName,Elevation_Value_m )%>%
  group_by(PlotID, IslandName) %>%
  summarise(tree_density_ha = sum(Density_tree_ha)) %>%
  mutate(Location = ifelse(IslandName == "Cosmoledo" | IslandName =="Aldabra", "Outer","Inner"))

#Tree Stats =========
tree_model <- lm(tree_density_ha ~ Location , data = tree)
tab_model(tree_model,show.se = T, show.ci = F)

tree_isl_model <- lm(tree_density_ha ~ IslandName , data = tree)
tab_model(tree_isl_model,show.se = T, show.ci = F)


#PLOT:
TreePlot <- ggplot(tree, aes(x=IslandName, y=tree_density_ha, fill=Location))+ #, fill = Island_Name
    geom_boxplot(outlier.shape = NA) +
  geom_jitter()+

  xlab("Island")+ ylab(bquote("Tree density "  (~ha^-1)))+
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
      geom_boxplot(outlier.shape = NA,alpha=0.1) 

TreePlot
ggsave(TreePlot, dpi=300, width = 16, height = 9,filename = "TreePlot.png")
ggsave(TreePlot, dpi=1200, width = 16, height = 9,filename = "TreePlot.svg")
