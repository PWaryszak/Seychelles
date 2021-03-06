#FIG_MangrovePlant_AGC_BGC_by_Country (mean +- SE):=========
#LOAD DATA and LIBRARIES first:
if (!require(tidyverse)) library(tidyverse) # it loads/updates packages if not loaded/updated in your R environment yet
library(tidyverse)

if (!require(readxl)) library(readxl) # it loads/updates packages if not loaded/updated in your R environment yet
library(readxl)

mg <- read_excel("SeychellesMangroveReview.xlsx", sheet = "Mangrove_BC_stock")
sg <- read_excel("SeychellesSeagrassReview.xlsx", sheet = "Seagrass_BC_stock")


mg_plant_above <- select (mg, Country, AGC_MgCha, AGB_MgDWha) %>%
  #If "AGC_MgCha" present keep it  and if not we convert aboveground biomass to C by * 0.48
  #*Most papers say "Stocks were determined from AGB and BGB using a carbon fraction of 0.48 for AGB and 0.39 for BGB
  #(Kauffman and Donato 2012; Abino et al. 2014).
  mutate(Plant_C_Mgha = ifelse(is.na(AGC_MgCha)=="FALSE", AGC_MgCha, 0.48 * AGB_MgDWha)) %>%
  filter(Plant_C_Mgha >0)%>%  #I had to do filter here as NA-s were kept and counted against N
  
  group_by(Country) %>%    #delete this line to get overall plant_C average
  summarise(AV=mean(Plant_C_Mgha, na.rm = T),
            SD=sd(Plant_C_Mgha, na.rm = T),
            N = length(Plant_C_Mgha),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Aboveground")

#Belowground Carbon:
mg_plant_below <- select (mg, Country,  BGC_MgCha, BGB_MgDWha ) %>%
  #If "BGC_MgCha" present iwe keep it  and if not we convert belowground biomass to C by * 0.394
  #*Most papers say "Stocks were determined from AGB and BGB using a carbon fraction of 0.48 for AGB and 0.39 for BGB
  #(Kauffman and Donato 2012; Abino et al. 2014).
  mutate(Plant_C_Mgha = ifelse(is.na(BGC_MgCha)=="FALSE", BGC_MgCha, 0.39 * BGB_MgDWha)) %>%
  filter(Plant_C_Mgha >0)%>%  #I had to do filter here as NA-s were kept and counted against N

  group_by(Country) %>%    #delete this line to get overall plant_C average
  summarise(AV = mean(Plant_C_Mgha, na.rm = T),
            SD = sd(Plant_C_Mgha, na.rm = T),
            N  = length(Plant_C_Mgha),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Belowground")

mg_AB_Country <- rbind (mg_plant_above, mg_plant_below)%>%
  #Turning belowground stock into negative values for plotting.
  mutate(AV= ifelse(Stock =="Aboveground",AV, AV*-1)) %>%
  na.omit()

#Draw a figure:
MyBreaks <- c(-300, -200,-100, -50, 0, 50, 100, 200,300)

plot_mangrove_plant_byCountry <- ggplot(mg_AB_Country, aes(x=as.factor(Country), y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-300,300))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("#00FF00","#006633"))+
  xlab("\n Country")+ ylab(bquote("Plant carbon stock "  (Mg*~ha^-1)))+
  geom_text(aes(label=paste0( "N = ",N)),vjust = 0, nudge_y = c(25,25,25,25,-50,-100,-50,-50), color = "black", size = 3.5)+
  ggtitle("Mangroves")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12, angle = 90),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        legend.position = c(.2, .9),
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))


plot_mangrove_plant_byCountry #Merge plant above and below carbon data together:

write.csv(mg_AB_Country, row.names = F, file = "FIG_MangrovePlant_AGC_BGC_byCountry_WIthN.csv")

ggsave(plot_mangrove_plant_byCountry , dpi=600, width = 7, height = 5,
       filename = "FIG_MangrovePlant_AGC_BGC_byCountry_WithN.png")

----------------------------------------------------------------------------------------
  
#TABLE_MangrovePlant_AGC_BGC (mean +- SE):=========

mg <- read_excel("SeychellesMangroveReview.xlsx", sheet = "Mangrove_BC_stock")

mg_plant_above_table <- select(mg,Reference,Country,Location, Species,Height_m,Height_m_SE,
                         DBH_cm,DBH_cm_SE,TreeDensity_ha,TreeDensity_ha_SE,
                         AGB_MgDWha,AGC_MgCha,BGB_MgDWha,BGC_MgCha) %>%
  #If "AGC_MgCha" present keep it  and if not we convert aboveground biomass to C by * 0.48
  #*Most papers say "Stocks were determined from AGB and BGB using a carbon fraction of 0.48 for AGB and 0.39 for BGB
  #(Kauffman and Donato 2012; Abino et al. 2014).
  mutate(Plant_AGC_Mgha = ifelse(is.na(AGC_MgCha)=="FALSE", AGC_MgCha, 0.48 * as.numeric(AGB_MgDWha)))  %>%
  mutate(Plant_BGC_Mgha = ifelse(is.na(BGC_MgCha)=="FALSE", BGC_MgCha, 0.39 * as.numeric(BGB_MgDWha)))
  

#Change to numeric:
mg_plant_above_table$Height_m_SE <- as.numeric(mg_plant_above_table$Height_m_SE)
mg_plant_above_table$Height_m <- as.numeric(mg_plant_above_table$Height_m)

mg_plant_above_table$TreeDensity_ha_SE <- as.numeric(mg_plant_above_table$TreeDensity_ha_SE)
mg_plant_above_table$DBH_cm_SE <- as.numeric(mg_plant_above_table$DBH_cm_SE)
mg_plant_above_table$TreeDensity_ha_SE <- as.numeric(mg_plant_above_table$TreeDensity_ha_SE)


#Round all numeric values:
mg_plant_above_table_round <- mg_plant_above_table %>% 
  mutate_if(is.numeric, round, digits=1)

  
#Replace NA with blanks (numeric back to character to do so):
mg_plant_above_table_round<- sapply(mg_plant_above_table_round, as.character)
mg_plant_above_table_round[is.na(mg_plant_above_table_round)] <- " "

View(mg_plant_above_table_round )


write.csv(mg_plant_above_table_round, row.names = F, file = "TABLE_Mangrove_AGC_BGC.csv")


----------------------------------------------------------------------------------------
  
  
#FIG_MangrovePlant_AGC_BGC_bySpecies (mean +- SE):=========
#AboveGround Carbon:
mg_species_above <- mg %>% select (Species,AGC_MgCha, AGB_MgDWha) %>%
  mutate(Plant_C_Mgha = ifelse( is.na(AGC_MgCha)=="FALSE", AGC_MgCha, 0.48 * AGB_MgDWha)) %>%
  filter(Plant_C_Mgha >0) %>%  #I had to do filter here as NA-s were kept and counted against N
  group_by(Species) %>%    #delete this line to get overall plant_C average
  summarise(AV=mean(Plant_C_Mgha, na.rm = T),
            SD=sd(Plant_C_Mgha, na.rm = T),
            N = length(Plant_C_Mgha),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Aboveground") %>% #needed for mering above and below later on
  na.omit() 

#Belowground Carbon:
mg_species_below <-select(mg, Species,BGC_MgCha, BGB_MgDWha) %>%
  #we convert aboveground biomass to C by * 0.48 if not reported as BGC_MgCha:
  mutate(Plant_C_Mgha = ifelse(is.na(BGC_MgCha)=="FALSE", BGC_MgCha, 0.39 * BGB_MgDWha)) %>%
  filter(Plant_C_Mgha >0) %>%  #I had to do filter here as NA-s were kept and counted against N
  group_by(Species) %>%    #delete this line to get overall plant_C average
  summarise(AV = mean(Plant_C_Mgha, na.rm = T),
            SD = sd(Plant_C_Mgha, na.rm = T),
            N  = length(Plant_C_Mgha),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Belowground")%>%
  na.omit() 

#Merge plant above and below carbo data together:
mg_AB_species <- rbind (mg_species_above, mg_species_below)%>%
  #Turning belowground stock into negative values for plotting.
  mutate(AV= ifelse(Stock =="Aboveground",AV, AV*-1)) %>%
  na.omit()


#Relevel factor Species to move "Mixed" to the end:
mg_AB_species_orig_levels = mg_AB_species$Species #Original levels are ordered alphanumerically
mg_AB_species_new_levels = c(setdiff(mg_AB_species_orig_levels, "Mixed"), "Mixed") #We want to move "Mixed to the end.
mg_AB_species$Species <- factor(mg_AB_species$Species, levels = mg_AB_species_new_levels) #Move "Mixed" to the end of factor levels and start plotting
levels(mg_AB_species$Species) #Check if "Mixed" is the last level. YES!

#Set the distance from bars for "N = ?" on the plot
mg_AB_species$nudge_text_by <- ifelse (mg_AB_species$AV>0,  mg_AB_species$SE + 5, -mg_AB_species$SE -20 )
nudge_text_by <- as.vector(mg_AB_species$nudge_text_by)
                                    
#Draw a figure:
MyBreaks_sp <- c( -100, -50, 0, 50, 100)

plot_mangrove_plant_BySpecies <- ggplot(mg_AB_species, aes(x=as.factor(Species), y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks_sp,labels = abs(MyBreaks_sp), limits = c(-200,200))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("#00FF00","#006633"))+
  geom_text(aes(label=paste0( "N = ",N)),vjust = 0,
            nudge_y = nudge_text_by,
            color = "black", size = 3.5)+
  
  xlab("")+ ylab(bquote("Plant carbon stock "  (Mg*~ha^-1)))+
  ggtitle("Mangroves")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12,angle=90,face=ifelse(mg_AB_species$Species !="X.granatum","italic","bold")),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        legend.position = c(.2, .9),
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))

plot_mangrove_plant_BySpecies

ggsave(plot_mangrove_plant_BySpecies, dpi=600, width = 9, height = 5,
       filename = "FIG_MangrovePlant_AGC_BGC_bySpeciesWithN.png")

write.csv(mg_AB_species, row.names = F, file = "FIG_MangrovePlant_AGC_BGC_bySpecies_WithN.csv")

-----------------------------------------------------------------------------------------

#Boxplot of mangrove SOC ranked by core depth:======
str(mg)

mg_soil_C_boxplot<-select (mg,DownToDepth_Rank,Country,Reference,Species,SOC_MgCha,Bulkdensity_gDWmlorcm3,SOC_Percent,DownToDepth_cm) %>%
  mutate(Soil_C_Mgha = ifelse(is.na(SOC_MgCha)=="FALSE", SOC_MgCha,Bulkdensity_gDWmlorcm3*SOC_Percent*DownToDepth_cm*100)) %>%
  filter(Soil_C_Mgha>0)
#PLOT:
ggplot(mg_soil_C_boxplot,  aes(x=as.factor(Species), y=Soil_C_Mgha,))+
  geom_boxplot()+geom_jitter(aes(color = DownToDepth_Rank))+
  ggtitle("Soil Carbon Stock in Mangroves ranked by core depth")

ggsave( dpi=600, width = 7, height = 5, filename = "FIG_Mangrove_SOC_by_Country.png")

#REFs for different depths examples:
mg_ref_depth <- mg_soil_C_boxplot %>%
  select(Reference, DownToDepth_Rank,DownToDepth_cm) %>%
  group_by(DownToDepth_Rank,Reference,DownToDepth_cm)%>%
  summarise(PErDepth_Study = n_distinct(Reference))

--------------------------------------------------------------------------------------------------
  
  
#Fig_Mangrove_SOC_by_SpeciesCountry (mean +- SE):=========
#Averages by Species:
mg_soil_C_Species <-select (mg, Species, SOC_MgCha, Bulkdensity_gDWmlorcm3, SOC_Percent,  DownToDepth_cm) %>%
  mutate(Soil_C_Mgha = ifelse( is.na(SOC_MgCha)=="FALSE", SOC_MgCha,
                               Bulkdensity_gDWmlorcm3*SOC_Percent*DownToDepth_cm)) %>%
  filter(DownToDepth_cm >60) %>% #keep only deep cores as all but one were deep (see mg_soil_C_boxplot).
  filter(Soil_C_Mgha >0) %>%  #I had to do filter here as NA-s were kept and counted against N
  group_by(Species) %>%    #delete this line to get overall Soil_C average
  summarise(AV= mean(Soil_C_Mgha, na.rm = T),
            SD= sd(Soil_C_Mgha, na.rm = T),
            N = length(Soil_C_Mgha),
            SE= SD / sqrt(N)) %>%
  mutate(data = "Species") 


#Averages by Country:
mg_soil_C_Country <-select (mg, Country, SOC_MgCha,Bulkdensity_gDWmlorcm3, SOC_Percent,  DownToDepth_cm) %>%
  mutate(Soil_C_Mgha = ifelse( is.na(SOC_MgCha)=="FALSE", SOC_MgCha, 
                              Bulkdensity_gDWmlorcm3*SOC_Percent*DownToDepth_cm)) %>%
  
  filter(DownToDepth_cm >60) %>% #keep only deep cores as all but one were deep (see mg_soil_C_boxplot).
  filter(Soil_C_Mgha >0) %>%  #I had to do filter here as NA-s were kept and counted against N
  
  group_by(Country) %>%    #delete this line to get overall Soil_C average
  summarise(AV= mean(Soil_C_Mgha, na.rm = T),
            SD= sd(Soil_C_Mgha, na.rm = T),
            N = length(Soil_C_Mgha),
            SE= SD / sqrt(N))  %>%
  mutate(data = "Country") %>%
  rename(Species=Country) 

#Merge Soil above and below carbo data together:
mg_AB_soil <- rbind (mg_soil_C_Country, mg_soil_C_Species) %>%na.omit()
mg_AB_soil$Species <- as.factor(mg_AB_soil$Species)
levels(mg_AB_soil$Species)

#Relevel factor Species to move "Mixed" to the end:
orig_levels = mg_AB_soil$Species #Origninal levels are ordered alphanumerically
new_levels = c(setdiff(orig_levels, "Mixed"), "Mixed") #WE want to move "Mixed to the end.
mg_AB_soil$Species <- factor(mg_AB_soil$Species, levels = new_levels) #Move "Mixed" to the end of factor levels and start plotting


#Set the distance from bars for "N = ?" on the plot
mg_AB_soil$nudge_text_by <-  mg_AB_soil$SE + 20
nudge_text_by2 <- as.vector(mg_AB_soil$nudge_text_by)

plot_mangrove_soil <- ggplot(mg_AB_soil, aes(x=Species, y=AV, fill=data))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  scale_fill_manual(values = c("#494928","#949447"))+
  facet_grid (.~data, scales = "free", space = "free")+
  geom_text(aes(label=paste0( "N = ",N)), vjust = 0,
            nudge_y = nudge_text_by2,
            color = "black", size = 3.5)+
  
  xlab("")+
  ylab(bquote("Soil carbon stock "  (Mg*~ha^-1)))+
  ggtitle("Mangroves")+
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5, size=12, angle=90, face="italic"),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.position = "none",
        legend.text = element_text(size = 9),
        strip.text = element_text(size=15),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))

plot_mangrove_soil

ggsave(plot_mangrove_soil, dpi=600, width = 7, height = 5,
       filename = "Fig_Mangrove_SOC_by_SpeciesCountry_WithN.png")

write.csv(mg_AB_soil, row.names = F, file = "Fig_Mangrove_SOC_by_SpeciesCountry_WithN.csv" )

-------------------------------------------------------------------------------------------------

#FIG_Seagrass_Plant_AGC_BGC_by_Country (mean +- SE):=========
sg <- read_excel("SeychellesSeagrassReview.xlsx", sheet = "Seagrass_BC_stock")

#AboveGround Carbon:
sg_plant_above_Country <- select (sg, Country,  BGC_MgCha, AGC_MgCha, AGB_gDWm2, BGB_gDWm2)%>%
  #If "AGC_MgCha" present keep it  and if not we convert aboveground biomass to C by * 0.35
  mutate(Plant_C_Mgha = ifelse(is.na(AGC_MgCha)=="FALSE", AGC_MgCha, AGB_gDWm2*0.35/100 )) %>% #/100 to convert g/m2 to Mg/ha
  filter(Plant_C_Mgha >0) %>%  #I had to do filter here as NA-s were kept and counted against N
  group_by(Country) %>%    #delete this line to get overall plant_C average
  summarise(AV=mean(Plant_C_Mgha, na.rm = T),
            SD=sd(Plant_C_Mgha, na.rm = T),
            N = length(Plant_C_Mgha),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Aboveground")

#Belowground Carbon:
sg_plant_below_Country <- select (sg, Country, BGC_MgCha, BGB_gDWm2) %>%
  #convert biomass to C by * 0.35 as per paper:
  #By Fourqurean2012 "Seagrass ecosystems as a globally significantcarbon stock"
  mutate(Plant_C_Mgha = ifelse(is.na(BGC_MgCha)=="FALSE", BGC_MgCha, BGB_gDWm2*0.35/100)) %>%
  filter(Plant_C_Mgha >0) %>%  #I had to do filter here as NA-s were kept and counted against N
  group_by(Country) %>%    #delete this line to get overall plant_C average
  summarise(AV = mean(Plant_C_Mgha, na.rm = T),
            SD = sd(Plant_C_Mgha, na.rm = T),
            N  = length(Plant_C_Mgha),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Belowground")


#Merge plant above and below carbo data together:
ab_sg_plant_Country <- rbind (sg_plant_above_Country, sg_plant_below_Country)%>%
  #Turning belowground stock into negative values for plotting.
  mutate(AV= ifelse(Stock =="Aboveground",AV, AV*-1)) #%>%  na.omit()

#Set the distance from bars for "N = ?" on the plot
ab_sg_plant_Country$nudge_text_by <- ifelse (ab_sg_plant_Country$AV>0,  ab_sg_plant_Country$SE + 0.5, -ab_sg_plant_Country$SE -0.5 )
nudge_text_by3 <- as.vector(ab_sg_plant_Country$nudge_text_by)

#Draw a figure:
MyBreaks <- c(-6,-5,-4,-3, -2,-1,  0, 1,2,3)

plot_Seagrass_plant_Country <- ggplot(ab_sg_plant_Country, aes(x=as.factor(Country), y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-7,3))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("#00FF00","#006633"))+
  xlab("Country")+ ylab(bquote("Plant carbon stock "  (Mg*~ha^-1)))+
  ggtitle("Seagrasses")+
  geom_text(aes(label=paste0( "N = ",N)),vjust = 0,
            nudge_y = nudge_text_by3,
            color = "black", size = 3.5)+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12, angle=90),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        legend.position = c(.4, .3),
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))


plot_Seagrass_plant_Country

ggsave(plot_Seagrass_plant_Country,dpi=600, width = 9, height = 5, filename = "FIG_Seagrass_Plant_AGC_BGC_by_CountryWithN.png")

write.csv(ab_sg_plant_Country, row.names = F, file = "FIG_Seagrass_Plant_AGC_BGC_by_Country_WithN.csv")

-------------------------------------------------------------------------------------------------

#TABLE_Seagrass_AGC_BGC (mean +- SE):=========

sg <- read_excel("SeychellesSeagrassReview.xlsx", sheet = "Seagrass_BC_stock")

sg_plant_above_table <- select(sg,Reference,Country,Location, Species,
                               Height_m,Height_m_SE,ShootDensity_m2_SE,ShootDensity_m2,
                               AGC_MgCha, AGB_gDWm2, BGC_MgCha,BGB_gDWm2)%>%
  
  #If "AGC_MgCha" or "BGC_Mgha is present keep it  and if not we convert aboveground biomass to C by * 0.35
  mutate(Plant_AGC_Mgha = ifelse(is.na(AGC_MgCha)=="FALSE", AGC_MgCha, 0.35 * as.numeric(AGB_gDWm2)/100)) %>%
  mutate(Plant_BGC_Mgha = ifelse(is.na(BGC_MgCha)=="FALSE", BGC_MgCha, 0.35 * as.numeric(BGB_gDWm2) /100 ))


#Change to numeric:
sg_plant_above_table$Height_m_SE <- as.numeric(sg_plant_above_table$Height_m_SE)
sg_plant_above_table$ShootDensity_m2_SE <- as.numeric(sg_plant_above_table$ShootDensity_m2_SE)


#Round all numeric values:
sg_plant_above_table_round <- sg_plant_above_table %>% 
  mutate_if(is.numeric, round, digits=1)


#Replace NA with blanks (numeric back to character to do so):
sg_plant_above_table_round<- sapply(sg_plant_above_table_round, as.character)
sg_plant_above_table_round[is.na(sg_plant_above_table_round)] <- " "

View(sg_plant_above_table_round )

write.csv(sg_plant_above_table_round, row.names = F, file = "TABLE_Seagrass_AGC_BGC.csv")


----------------------------------------------------------------------------------------

#FIG_Seagrass_Plant_AGC_BGC_by_Species (mean +- SE):=========
#AboveGround Carbon:
sg_plant_above_Species<-select(sg,Species, AGC_MgCha, AGB_gDWm2)%>%
  #If "AGC_MgCha" present keep it  and if not we convert aboveground biomass to C by * 0.35
  mutate(Plant_C_Mgha = ifelse( is.na(AGC_MgCha)=="FALSE", AGC_MgCha, AGB_gDWm2*0.35/100 )) %>% #/100 to convert g/m2 to Mg/ha
  filter(Plant_C_Mgha >0) %>%  #I had to do filter here as NA-s were kept and counted against Ngroup_by(Species) %>%    #delete this line to get overall plant_C average
  group_by(Species)%>%
  summarise(AV=mean(Plant_C_Mgha, na.rm = T),
            SD=sd(Plant_C_Mgha, na.rm = T),
            N = length(Plant_C_Mgha),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Aboveground")

#Belowground Carbon:
sg_plant_below_Species <- select (sg, Species,  BGC_MgCha, BGB_gDWm2) %>%
  # convert biomass to C by * 0.35 as per paper:
  #By Fourqurean2012 "Seagrass ecosystems as a globally significantcarbon stock"
  mutate(Plant_C_Mgha = ifelse(is.na(BGC_MgCha)=="FALSE", BGC_MgCha, BGB_gDWm2*0.35/100)) %>%
  filter(Plant_C_Mgha >0) %>%  #I had to do filter here as NA-s were kept and counted against N
  
  group_by(Species) %>%    #delete this line to get overall plant_C average
  summarise(AV = mean(Plant_C_Mgha, na.rm = T),
            SD = sd(Plant_C_Mgha, na.rm = T),
            N  = length(Plant_C_Mgha),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Belowground")

#Merge plant above and below carbo data together:
ab_sg_plant_Species <- rbind (sg_plant_above_Species, sg_plant_below_Species)%>%
  #Turning belowground stock into negative values for plotting.
  mutate(AV= ifelse(Stock =="Aboveground",AV, AV*-1))  %>%   na.omit()

#Relevel factor Species to move "Mixed" to the end:
ab_sg_plant_Species_orig_levels = ab_sg_plant_Species$Species #Original levels are ordered alphanumerically
ab_sg_plant_Species_new_levels = c(setdiff(ab_sg_plant_Species_orig_levels, "Mixed"), "Mixed") #We want to move "Mixed to the end.
ab_sg_plant_Species$Species <- factor(ab_sg_plant_Species$Species, levels = ab_sg_plant_Species_new_levels) #Move "Mixed" to the end of factor levels and start plotting
levels(ab_sg_plant_Species$Species) #Check if "Mixed" is the last level. YES!
#Set the distance from bars for "N = ?" on the plot
ab_sg_plant_Species$nudge_text_by <- ifelse (ab_sg_plant_Species$AV>0,
                                             ab_sg_plant_Species$SE + 0.5,
                                             -ab_sg_plant_Species$SE - 1 )
nudge_text_by4 <- as.vector(ab_sg_plant_Species$nudge_text_by)


#Draw a figure:
MyBreaks <- c(-9,-7,-5,-3, -2,-1,  0, 1,2,3)

plot_Seagrass_plant_Species <- ggplot(ab_sg_plant_Species, aes(x=as.factor(Species), y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-10,3))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("#00FF00","#006633"))+
  xlab("Species")+ ylab(bquote("Plant carbon stock "  (Mg*~ha^-1)))+
  ggtitle("Seagrasses")+
  geom_text(aes(label=paste0( "N = ",N)),vjust = 0,
            nudge_y = nudge_text_by4,
            color = "black", size = 3.5)+
  
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12,angle = 90, face="italic"),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        legend.position = c(.8, .2),
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))

plot_Seagrass_plant_Species

ggsave(plot_Seagrass_plant_Species,dpi=600, width = 9, height = 5, filename = "FIG_Seagrass_Plant_AGC_BGC_by_SpeciesWithN.png")
write.csv(ab_sg_plant_Species, row.names = F, file = "FIG_Seagrass_Plant_AGC_BGC_by_Species_WithN.csv")

----------------------------------------------------------------------------------------------------

  
#Boxplot of seagrass SOC ranked by core depth:===========

sg_soil_C_boxplot <-select (sg,DownToDepth_Rank, Country,Species, SOC_MgCha, SoilDensity_gDWml, SOC_Percent,  DownToDepth_cm, SOM_Percent) %>%
  
  mutate(DownToDepth_Rank2 = ifelse(DownToDepth_cm <30, "Shallow", 
                                    ifelse(DownToDepth_cm < 60, "Intermediate", 
                                     ifelse(DownToDepth_cm < 201, "Deep", DownToDepth_cm)))) %>%
  
  mutate(Soil_C_Mgha = ifelse(SOC_MgCha > 0, SOC_MgCha, 
                              SoilDensity_gDWml*SOC_Percent*DownToDepth_cm*100)) %>%
  
  #mutate againg to include converstion from OM to OC:
  #Soil organic matter generally contains approximately 56% organic carbon (Brady, 1990). The following #equation was therefore used to estimate percentage soil organic carbon (SOC) from total soil organic matter #(Brady, 1990): SOC% = SOM x 0.56
  
  mutate(Soil_C_Mgha2 = ifelse(Soil_C_Mgha > 0, Soil_C_Mgha, 
                               SoilDensity_gDWml * SOM_Percent* 0.56 * DownToDepth_cm * 100)) %>% 
  filter(Soil_C_Mgha >0)   #I had to do filter here as NA-s were kept and counted against N

  
  #PLOT:
ggplot(sg_soil_C_boxplot,  aes(x= "Seagrass Species", y=Soil_C_Mgha2))+
  geom_boxplot(outlier.shape = NA)+geom_jitter(aes(color = DownToDepth_Rank2))+
  ggtitle("Soil Carbon Stock in Seagrasses ranked by core depth")

ggsave( dpi=600, width = 7, height = 5, filename = "BOXPLOT_Seagrass_SOC_by_CoreDepth.png")

----------------------------------------------------------------------------------------------------

#FIG_Seagrass_Soil_OC_by_Species_ByDepthRank (mean +- SE):=========

#First dataset to plot points must be raw
sg_raw_soil <-select (sg, Species, SOC_MgCha, SoilDensity_gDWml, SOC_Percent,  DownToDepth_cm, SOM_Percent) %>%
  
  mutate(Soil_C_Mgha = ifelse(is.na(SOC_MgCha)=="FALSE", SOC_MgCha, 
                              SoilDensity_gDWml*SOC_Percent*DownToDepth_cm)) %>%
  
  #mutate againg to include converstion from OM to OC:
  #Soil organic matter generally contains approximately 56% organic carbon (Brady, 1990). The following #equation was therefore used to estimate percentage soil organic carbon (SOC) from total soil organic matter #(Brady, 1990): SOC% = SOM x 0.56
  mutate(Soil_C_Mgha2 = ifelse(is.na(Soil_C_Mgha)=="FALSE", Soil_C_Mgha, 
                               SoilDensity_gDWml*SOM_Percent*.56*DownToDepth_cm))  %>%
  
  
  #Rank cores by their depths (Rank function in excel turned all blank cells in "Shallow" instead of "NA"):
  mutate(Core = ifelse(DownToDepth_cm <30, "Shallow", 
                                    ifelse(DownToDepth_cm < 60, "Intermediate",
                                    ifelse(DownToDepth_cm < 201, "Deep", DownToDepth_cm)))) %>%
  filter(Core != "NA" ) %>%
  filter(Core !="Shallow") #no shallow cores present in data

#Second dataset for bars must be averages:  
sg_soil_C_Species2 <-select (sg, Species, SOC_MgCha, SoilDensity_gDWml, SOC_Percent,  DownToDepth_cm, SOM_Percent) %>%
  
  mutate(Soil_C_Mgha = ifelse(is.na(SOC_MgCha)=="FALSE", SOC_MgCha, 
                              SoilDensity_gDWml*SOC_Percent*DownToDepth_cm*100)) %>%
  
  #Rank cores by their depths (Rank function in excel turned all blank cells in "Shallow" instead of "NA"):
  mutate(Core = ifelse(DownToDepth_cm <30, "Shallow", 
                                    ifelse(DownToDepth_cm < 60, "Intermediate", 
                                           ifelse(DownToDepth_cm < 201, "Deep", DownToDepth_cm)))) %>%
  filter(Core != "NA" ) %>%
  filter(Core !="Shallow") %>% #no shallow cores present in data

  #Soil organic matter generally contains approximately 56% organic carbon (Brady, 1990). The following #equation was therefore used to estimate percentage soil organic carbon (SOC) from total soil organic matter #(Brady, 1990): SOC% = SOM x 0.56
  mutate(Soil_C_Mgha2 = ifelse(is.na(Soil_C_Mgha)=="FALSE", Soil_C_Mgha, 
                               SoilDensity_gDWml*SOM_Percent*.56*DownToDepth_cm*100))  %>%
  
  filter(Soil_C_Mgha2>0) %>%
  
  group_by(Species) %>%    #delete this line to get overall Soil_C average
  
  summarise(AV= mean(Soil_C_Mgha2, na.rm = T),
            SD= sd(Soil_C_Mgha2, na.rm = T),
            N = length(Soil_C_Mgha2),
            SE= SD / sqrt(N)) %>%
   
  mutate(data = "Species") 

dodge <- position_dodge(width=0.9) 

sg_soil_C_Species2$Species <- factor(sg_soil_C_Species2$Species,
                                     levels = c("C.rotundata","C.serrulata","Cymodocea.spp",
                                                "E.acoroides","T.ciliatum","T.hemprichii","Mixed"))

#PLOT:
plot_seagrass_soil_species_depth <- ggplot(sg_soil_C_Species2, aes(x=Species, y=AV))+
  geom_bar(position="identity", stat="identity", fill="494928")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  #scale_fill_manual(values = c("#494928","#949447"))+
  geom_point(data=sg_raw_soil,aes(Species,Soil_C_Mgha2,color=Core), position=dodge)+
  xlab("")+
  ylab(bquote("Soil carbon stock "  (Mg*~ha^-1)))+
  ggtitle("Seagrasses")+
  #facet_grid(Core~.)+ #, scale = "free", space="free"
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5, size=12, angle=45),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15, angle=45),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        strip.text = element_text(size=15),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))

plot_seagrass_soil_species_depth

ggsave(plot_seagrass_soil_species_depth, dpi=600, width = 9, height = 5,
       filename = "FIG_Seagrass_Soil_OC_by_Species_ByDepthRank.png")

write.csv(sg_soil_C_Species2, row.names = F,
          file = "FIG_Seagrass_Soil_OC_by_Species_ByDepthRank.csv")

--------------------------------------------------------------------------------------------------

#FIG_Seagrass_Soil_OC_by_Country_ByDepthRank (mean +- SE):=========
#First dataset to plot points must be raw:
sg_soil_C_Country_raw <-select (sg, Country, SOC_MgCha, SoilDensity_gDWml, SOC_Percent,  DownToDepth_cm, SOM_Percent) %>%
  mutate(Soil_C_Mgha = ifelse(is.na(SOC_MgCha)=="FALSE", SOC_MgCha,SoilDensity_gDWml*SOC_Percent*DownToDepth_cm)) %>%
  mutate(Soil_C_Mgha2 = ifelse(is.na(Soil_C_Mgha)=="FALSE", Soil_C_Mgha,SoilDensity_gDWml*SOM_Percent * 0.56 * DownToDepth_cm))  %>% # convert to C-stock. #mutate again to include converstion from OM to OC. Soil organic matter generally contains approximately 56% organic carbon (Brady, 1990). The following #equation was therefore used to estimate percentage soil organic carbon (SOC) from total soil organic matter #(Brady, 1990): SOC% = SOM x 0.56
  mutate(Core = ifelse(DownToDepth_cm <30, "Shallow",  ifelse(DownToDepth_cm < 60, "Intermediate",ifelse(DownToDepth_cm < 201, "Deep", DownToDepth_cm)))) %>%  #Rank cores by their depths (Rank function in excel turned all blank cells in "Shallow" instead of "NA"):
  filter(Core != "NA" ) %>%  
  filter(Core !="Shallow") #no shallow cores present in data

#Second dataset for bars must be averages:  
sg_soil_C_Country2 <-select (sg, Country, SOC_MgCha, SoilDensity_gDWml, SOC_Percent,  DownToDepth_cm, SOM_Percent) %>%
  mutate(Soil_C_Mgha = ifelse(is.na(SOC_MgCha)=="FALSE", SOC_MgCha,SoilDensity_gDWml*SOC_Percent*DownToDepth_cm)) %>%
  
  #Rank cores by their depths (Rank function in excel turned all blank cells in "Shallow" instead of "NA"):
  mutate(Core = ifelse(DownToDepth_cm <30, "Shallow", 
                       ifelse(DownToDepth_cm < 60, "Intermediate", 
                              ifelse(DownToDepth_cm < 201, "Deep", DownToDepth_cm)))) %>%
  filter(Core != "NA" ) %>%
  #Soil organic matter generally contains approximately 56% organic carbon (Brady, 1990). The following #equation was therefore used to estimate percentage soil organic carbon (SOC) from total soil organic matter #(Brady, 1990): SOC% = SOM x 0.56
  mutate(Soil_C_Mgha2 = ifelse(Soil_C_Mgha > 0, Soil_C_Mgha, 
                               SoilDensity_gDWml*SOM_Percent* 0.56* DownToDepth_cm))  %>%
  filter(Soil_C_Mgha2>0) %>% #filters out all NA values
  group_by(Country) %>%    #delete this line to get overall Soil_C average
  summarise(AV= mean(Soil_C_Mgha2, na.rm = T),
            SD= sd(Soil_C_Mgha2, na.rm = T),
            N = length(Soil_C_Mgha2),
            SE= SD / sqrt(N)) %>%
  mutate(data = "Country")

#PLOT:
dodge <- position_dodge(width=0.9) 

plot_seagrass_soil_Country_depth <- ggplot(sg_soil_C_Country2, aes(x=Country, y=AV))+
  geom_bar(position="identity", stat="identity", fill="grey90")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  #scale_fill_manual(values = c("#494928","#949447"))+
  geom_point(data=sg_soil_C_Country_raw ,aes(Country,Soil_C_Mgha2,color=Core),position=dodge)+
  xlab("")+
  ylab(bquote("Soil carbon stock "  (Mg*~ha^-1)))+
  ggtitle("Seagrasses")+
  #facet_grid(Core~.)+ #, scale = "free", space="free"
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5, size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15, angle=45),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        strip.text = element_text(size=15),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))


plot_seagrass_soil_Country_depth

ggsave(plot_seagrass_soil_Country_depth, dpi=600, width = 7, height = 5,
       filename = "FIG_Seagrass_Soil_OC_by_Country_ByDepthRank_WithN.png")

write.csv(sg_soil_C_Country2, row.names = F,
          file = "FIG_Seagrass_Soil_OC_by_Country_ByDepthRank.csv")

--------------------------------------------------------------------------------------
  
#Mean Seagrass AGC overall:========
sg_plant_above_mean <- select (sg, Country,  BGC_MgCha, AGC_MgCha, AGB_gDWm2, BGB_gDWm2)%>%
  mutate(Plant_C_Mgha = ifelse(is.na(AGC_MgCha)=="FALSE", AGC_MgCha, AGB_gDWm2*0.35/100 )) %>% #/100 to convert g/m2 to Mg/ha
  filter(Plant_C_Mgha >0) %>%  #I had to do filter here as NA-s were kept and counted against N
  summarise(AV=mean(Plant_C_Mgha, na.rm = T),
            SD=sd(Plant_C_Mgha, na.rm = T),
            N = length(Plant_C_Mgha),
            SE= SD / sqrt(N)) 

sg_plant_above_mean #0.678 0.716   239 0.0463

#Mean seagrass BGC overall:============
sg_plant_below_mean <- select (sg, Country, BGC_MgCha, AGC_MgCha, BGB_gDWm2) %>%
  mutate(Plant_C_Mgha = ifelse(is.na(BGC_MgCha)=="FALSE", BGC_MgCha, BGB_gDWm2*0.35/100)) %>%
  summarise(AV = mean(Plant_C_Mgha, na.rm = T),
            SD = sd(Plant_C_Mgha, na.rm = T),
            N  = length(Plant_C_Mgha),
            SE = SD / sqrt(N))

sg_plant_below_mean # 2.21  2.12   239 0.137


#Mean Seagrass SOC overall==========
SCO PErcent:
sg_soil_perecent_mean <-select (sg, Species,  SOC_Percent) %>%
  summarise(AV=mean(SOC_Percent, na.rm = T),
            SD=sd(SOC_Percent, na.rm = T),
            N = length(SOC_Percent),
            SE= SD / sqrt(N)) 

sg_soil_perecent_mean #0.752  1.27   239 0.0820




sg_soil_mean <-select (sg, Species, SOC_MgCha, SoilDensity_gDWml, SOC_Percent,  DownToDepth_cm, SOM_Percent) %>%

mutate(Soil_C_Mgha = ifelse(is.na(SOC_MgCha)=="FALSE", SOC_MgCha, 
                            SoilDensity_gDWml*SOC_Percent*DownToDepth_cm)) %>%
  
  #mutate againg to include converstion from OM to OC:
  #Soil organic matter generally contains approximately 56% organic carbon (Brady, 1990). The following #equation was therefore used to estimate percentage soil organic carbon (SOC) from total soil organic matter #(Brady, 1990): SOC% = SOM x 0.56
  mutate(Soil_C_Mgha2 = ifelse(is.na(Soil_C_Mgha)=="FALSE", Soil_C_Mgha, 
                               SoilDensity_gDWml*SOM_Percent*.56*DownToDepth_cm))  %>%
  
  #Rank cores by their depths (Rank function in excel turned all blank cells in "Shallow" instead of "NA"):
  mutate(Core = ifelse(DownToDepth_cm <30, "Shallow", 
                       ifelse(DownToDepth_cm < 60, "Intermediate",
                              ifelse(DownToDepth_cm < 201, "Deep", DownToDepth_cm)))) %>%
  filter(Core != "NA" ) %>%
  filter(Core !="Shallow") %>% #no shallow cores present in data
  summarise(AV=mean(Soil_C_Mgha2, na.rm = T),
            SD=sd(Soil_C_Mgha2, na.rm = T),
            N = length(Soil_C_Mgha2),
            SE= SD / sqrt(N)) 
sg_soil_mean

#Mean Mangrove AGC overall:========
mg_plant_above_mean <- select (mg, Country,  BGC_MgCha, AGC_MgCha, AGB_MgDWha) %>%
  mutate(Plant_C_Mgha = ifelse(is.na(AGC_MgCha)=="FALSE", AGC_MgCha, 0.48 * AGB_MgDWha)) %>% #/100 to convert g/m2 to Mg/ha
  filter(Plant_C_Mgha >0)# %>%  #I had to do filter here as NA-s were kept and counted against N
summarise(AV=mean(Plant_C_Mgha, na.rm = T),
          SD=sd(Plant_C_Mgha, na.rm = T),
          N = length(Plant_C_Mgha),
          SE= SD / sqrt(N)) 

mg_plant_above_mean #60.1  71.0   291  4.16

