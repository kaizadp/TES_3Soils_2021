# calculating soil moisture and core weights, etc.
# Kaizad F. Patel
# October 2019

source("code/0b-packages.R")

# I downloaded the files from Google Drive, since I was not able to import the data directly from Google Drive.
# https://docs.google.com/spreadsheets/d/1wsI3tldbhhMDSDoRejmS2jE2c9U-75sht_Dyum3QUBY/edit#gid=0



# 1. CALCULATE CORE DRY WEIGHTS ----------------------------------------------
## we have (a) gravimetric moisture (after porewater was pulled) and (b) core moist weight

#core_weights_file = read_excel("moisture/3Soils_MoistureContentRevised_20190314.xlsx", sheet = "3Soils_Moisture_Porosity_Weight") 
core_weights_file = read_csv("data/moisture/3Soils_MoistureContentRevised_20190314-3Soils_Moisture_Porosity_Weight.csv") 

core_weights = 
  core_weights_file %>% 
  dplyr::select(1,2,3,4,8,15,21,26,27) %>% 
  dplyr::rename(gmoisture = Soil_θgrav,
                litter_g = WET_litter_weight_g,
                total_g = CoreMassCore_mass_w_liner_w_caps_prior_to_processing_g,
                empty_g = Core_empty_liner_plus_end_caps_g) %>% 
  mutate_at(6:9, funs(as.numeric)) %>% 
  mutate_at(6:9,funs(round(.,2))) %>% 
  mutate(gmoisture = round(gmoisture,4),
         empty_corr = if_else(is.na(empty_g),50,empty_g),
         moist_g = total_g - empty_corr - litter_g,
         dry_g = round(moist_g/(1+gmoisture),2),
         bd = round(dry_g/VolumeSoil_cm3,2)) %>% 
  dplyr::select(Site, Core, SampleID, Treatment, dry_g, bd)

#### OUTPUT
write.csv(core_weights,CORE_WEIGHTS, row.names = FALSE)


# use the dry weights from this file for all calculations.
core_weights_temp = core_weights %>% dplyr::select(SampleID,dry_g, bd)

#
# 2. CALCULATE MOISTURE CONTENT BY TREATMENT ---------------------------------
## calculating moisture content during the incubations

## retrieve mass and valve data for the cores
## key has weights of the setup
## valve has total weights at different time points

key = read.csv("data/moisture/3Soils_CPCRW_SR_DWP_PicarroLog - sampleID_key.csv")
valve = read.csv("data/moisture/3Soils_CPCRW_SR_DWP_PicarroLog - valve_map.csv")

key %>% 
  dplyr::select(SampleID,Site,CoreNo,Treatment,DryMass_NONsoil_ALL_g, VolumeSoil_cm3, DryMass_SoilOnly_g)->
  key2

valve %>% 
  dplyr::filter(stringr::str_detect(TREATMENT_PHASE,c("DROUGHT_INCUBATION","FIELD_MOIST_INCUBATION","SATURATION_INCUBATION")),
                !stringr::str_detect(Ignore_Data,"Y"))->
  valve2


moisture_by_core = 
  valve2 %>% 
  dplyr::select(Site, Core_ID, Treatment, SampleID,
                Total_core_mass_pot_pie_pans_g, Additional_Wt_toRemove, Start_Date_Time, Notes) %>% 
  filter(!Treatment=="ambient") %>% 
  mutate(Start_Date_Time = as.POSIXct(strptime(valve2$Start_Date_Time,
                                                format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")),
         Additional_Wt_toRemove = as.numeric(Additional_Wt_toRemove))%>% 
  left_join(key2, by = c("SampleID","Site","Treatment")) %>%
  left_join(core_weights_temp, by = "SampleID") %>% 
  replace(is.na(.),0) %>% 
  dplyr::rename(total_g = Total_core_mass_pot_pie_pans_g) %>% 
# SR-drought non-soil weight does not include the empty liner (50 g), so adding that here  
  dplyr::mutate(total_g=as.numeric(as.character(total_g)),
                nonsoil_g = if_else(Site=="SR"&Treatment=="drought", 
                                    50+Additional_Wt_toRemove+DryMass_NONsoil_ALL_g,
                                    Additional_Wt_toRemove+DryMass_NONsoil_ALL_g)) %>% 
  dplyr::mutate(moistsoil_g = total_g - nonsoil_g,
                water_g = moistsoil_g - dry_g,
                moisture_perc = (water_g/dry_g)*100,
                bd2 = DryMass_SoilOnly_g/VolumeSoil_cm3,
                moisture_perc_vol = moisture_perc*bd2) %>% 
  group_by(Site, Core_ID, Treatment, SampleID) %>% 
  dplyr::summarize(moisture_grav_perc = mean(moisture_perc, na.rm = T),
                   moisture_vol_perc = mean(moisture_perc_vol, na.rm = T),
                   bd = mean(bd2)) %>% 
  ungroup() %>% 
  mutate_at(5:7,funs(round(.,2)))
  
names(moisture_by_core)

moisture_by_core %>% 
  group_by(Site, Treatment) %>% 
  dplyr::summarise(moisture_grav_perc = mean(moisture_grav_perc, na.rm = T),
                   moisture_vol_perc = mean(moisture_vol_perc, na.rm = T)) %>% 
  ggplot(aes(x = Site, y = moisture_grav_perc, color = Treatment))+
  geom_point(aes(fill = Treatment),
             position = position_dodge(width = 0.5), size = 4, stroke = 1, color = "black", shape = 21)+
  geom_point(data = valve3,
             position = position_dodge(width = 0.5), size = 2, shape = 19)+
  theme_kp()+
  NULL

moisture_by_core %>% 
  #filter(Treatment == "field_moist") %>% 
  group_by(Site, Treatment) %>% 
  dplyr::summarise(moisture_vol_perc = mean(moisture_grav_perc, na.rm = T))
  
# calculate summary
moisture_summary = 
  moisture_by_core %>% 
  #filter(Treatment == "field_moist") %>% 
  group_by(Site, Treatment) %>% 
  dplyr::summarise(moisture_grav_perc = mean(moisture_grav_perc, na.rm = T),
                   moisture_vol_perc = mean(moisture_vol_perc, na.rm = T),
                   bd = mean(bd)) %>% 
  group_by(Site) %>% 
  dplyr::mutate(moisture_grav_perc = if_else(moisture_grav_perc<0, 0, moisture_grav_perc),
                perc_saturation = (moisture_grav_perc/max(moisture_grav_perc))*100,
                perc_saturation = round(perc_saturation,2))

moisture_summary %>% 
  ggplot(aes(x = Site, y = perc_saturation, color = Treatment))+
  geom_point(size = 3) +
  theme_kp()+
  NULL