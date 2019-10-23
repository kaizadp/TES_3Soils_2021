# calculating soil moisture and core weights, etc.
# Kaizad F. Patel
# October 2019

source("0-packages.R")

# I downloaded the files from Google Drive, since I was not able to import the data directly from Google Drive.
# https://docs.google.com/spreadsheets/d/1wsI3tldbhhMDSDoRejmS2jE2c9U-75sht_Dyum3QUBY/edit#gid=0


key = read.csv("moisture/3Soils_CPCRW_SR_DWP_PicarroLog - sampleID_key.csv")
valve = read.csv("moisture/3Soils_CPCRW_SR_DWP_PicarroLog - valve_map.csv")

key %>% 
  dplyr::select(SampleID,Site,CoreNo,Treatment,DryMass_SoilOnly_g,DryMass_NONsoil_ALL_g)->
  key2

valve %>% 
  dplyr::filter(stringr::str_detect(TREATMENT_PHASE,c("DROUGHT_INCUBATION","FIELD_MOIST_INCUBATION","SATURATION_INCUBATION")),
                !stringr::str_detect(Ignore_Data,"Y"))->
  valve2


valve2 %>% 
  filter(!Treatment=="ambient") %>% 
  mutate(Start_Date_Time2 = as.POSIXct(strptime(valve2$Start_Date_Time,format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")),
         Additional_Wt_toRemove = as.numeric(Additional_Wt_toRemove))%>% 
 # group_by(Site, SampleID, Treatment) %>% 
  #dplyr::filter(Start_Date_Time2 == max(Start_Date_Time2)) %>% 
  
  #dplyr::select(1:6) %>% 
  left_join(key2, by = c("SampleID","Site","Treatment")) %>%
  replace_na(0) %>% 
  dplyr::rename(total_g = Total_core_mass_pot_pie_pans_g,
                drysoil_g = DryMass_SoilOnly_g) %>% 
  dplyr::mutate(total_g=as.numeric(as.character(total_g)),
                nonsoil_g = (Additional_Wt_toRemove+DryMass_NONsoil_ALL_g)) %>% 
  #dplyr::select(1:4,total_g,drysoil_g,nonsoil_g) %>% 
  #drop_na %>% 
  dplyr::mutate(moistsoil_g = total_g - nonsoil_g,
                water_g = moistsoil_g - drysoil_g,
                moisture_perc = (water_g/drysoil_g)*100)->
  valve3

names(valve3)


ggplot(valve3, aes(x = (Start_Date_Time2), y = moisture_perc))+
  geom_point()+
  facet_grid(Site~Treatment)
  
  
  
  
  
  
  