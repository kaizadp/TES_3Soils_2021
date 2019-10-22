# calculating soil moisture and core weights, etc.
# Kaizad F. Patel
# October 2019

source("0-packages.R")

# I downloaded the files from Google Drive, since I was not able to import the data directly from Google Drive.
# https://docs.google.com/spreadsheets/d/1wsI3tldbhhMDSDoRejmS2jE2c9U-75sht_Dyum3QUBY/edit#gid=0


key = read.csv("moisture/3Soils_CPCRW_SR_DWP_PicarroLog - sampleID_key")
valve = read.csv("moisture/3Soils_CPCRW_SR_DWP_PicarroLog - valve_map")

