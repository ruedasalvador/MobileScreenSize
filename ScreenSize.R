library(readr)
library(dplyr)
library(esquisse)
library(ggplot2)


devices_and_ios_OB <- read_csv("devices_and_ios_OB.csv", col_names = FALSE) # Estos dispositivos nos los ha compativo Alberto tirado a partir de una extracción directa de la BBDD
View(devices_and_ios_OB)

#Asigno nombre a las columnas 
colnames(devices_and_ios_OB) <- c("UserID","OB_ID","OB_Name","Model","deviceId","DeviceType")


#Me quedo con los dispositivos con sistema operativo android y ademas con aquellos dispositivos que con ususados por mas de 100 usuarios en la OB correspondiente. 
OB_devices <- devices_and_ios_OB %>% filter(DeviceType %in% c("sphAndroid","tabAndroid") & OB_Name %in% c("Chile","OBArgentina","OBColombia","OBEcuador","OBPeru","Vivo Play (Brasil)") ) %>% 
  select(OB_Name,Model)  %>% group_by(OB_Name,Model) %>% 
      summarise(TotalUsers = length(Model)) %>% filter(TotalUsers>=100) %>% write.csv2("OB_devices.csv")
  
  
#Creo listado unico de dispositivos
unique(OB_devices[2]) %>% write.csv2("Unique_Devices.csv")



#Importo listado de dispositivo con su corrrepodiente resolución 

Mapping_SizeScreen_Devices <- read_csv("Mapping_SizeScreen_Devices.csv",col_names = FALSE)

colnames(Mapping_SizeScreen_Devices) <- c("Model","SizeScreen")

Mapping_SizeScreen_Devices


# join entre OB_devices  y Mapping_SizeScreen_Devices

OB_devices_raw <- left_join(OB_devices , Mapping_SizeScreen_Devices)


OB_devices_summary <- OB_devices_raw %>% select(OB_Name, SizeScreen, TotalUsers) %>% group_by(OB_Name,SizeScreen ) %>% summarise(TotalUsers=sum(TotalUsers))



ggplot(OB_devices_summary) +
  aes(
    x = SizeScreen,
    y = TotalUsers,
    fill = SizeScreen,
    colour = SizeScreen
  ) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(OB_Name), ncol = 7L)


