library(readr)
library(dplyr)
library(esquisse)
library(ggplot2)

# Descarga el csv https://telefonicacorp-my.sharepoint.com/:x:/g/personal/salvadoralejandro_rueda_telefonica_com/EZQIbMKOs5JIvrzTZrO8Y8kB24OXz2SrPmoUUETqK23ZCQ?e=ZtIYki 
devices_and_ios_OB <- read_csv("devices_and_ios_OB.csv", col_names = FALSE) # Estos dispositivos nos los ha compativo Alberto tirado a partir de una extracción directa de la BBDD


#Asigno nombre a las columnas 
colnames(devices_and_ios_OB) <- c("UserID","OB_ID","OB_Name","Model","deviceId","DeviceType")


#Me quedo con los dispositivos con sistema operativo android y ademas con aquellos dispositivos que con ususados por mas de 100 usuarios en la OB correspondiente. 
OB_devices <- devices_and_ios_OB %>% filter(DeviceType %in% c("sphAndroid","tabAndroid") & OB_Name %in% c("Chile","OBArgentina","OBColombia","OBEcuador","OBPeru","Vivo Play (Brasil)") ) %>% 
  select(OB_Name,Model,DeviceType)  %>% group_by(OB_Name,Model,DeviceType) %>% 
      summarise(TotalUsers = length(Model)) %>% filter(TotalUsers>=100) 

OB_devices %>% write.csv2("OB_devices.csv")
  
  
#Creo listado unico de dispositivos
unique(OB_devices[2]) %>% write.csv2("Unique_Devices.csv")


#https://telefonicacorp-my.sharepoint.com/:x:/g/personal/salvadoralejandro_rueda_telefonica_com/EfgOcypbDa1FnXrniUxGwwkBtbKkJVo-eH2oOjJfi6ovsQ?e=DcmDF0
#Importo listado de dispositivo con su corrrepodiente resolución 

Mapping_SizeScreen_Devices <- read_csv("Mapping_SizeScreen_Devices.csv",col_names = FALSE)

colnames(Mapping_SizeScreen_Devices) <- c("Model","SizeScreen")

Mapping_SizeScreen_Devices


# join entre OB_devices  y Mapping_SizeScreen_Devices

OB_devices_raw <- left_join(OB_devices , Mapping_SizeScreen_Devices)




OB_devices_summary <- OB_devices_raw %>% group_by(OB_Name,DeviceType) %>% mutate(per_TotalUsers= round((TotalUsers/sum(TotalUsers))*100,digits=2)) %>% 
  select(OB_Name,DeviceType, SizeScreen, TotalUsers,per_TotalUsers) %>% group_by(OB_Name,SizeScreen,DeviceType ) %>% 
  summarise(TotalUsers=sum(TotalUsers),per_TotalUsers=sum(per_TotalUsers) )


OB_devices_summary <- OB_devices_raw %>%  
  select(OB_Name,DeviceType, SizeScreen, TotalUsers) %>% group_by(OB_Name,SizeScreen,DeviceType ) %>% 
  summarise(TotalUsers=sum(TotalUsers)) %>% group_by(OB_Name,DeviceType) %>% mutate(per_TotalUsers= round((TotalUsers/sum(TotalUsers))*100,digits=2))





# Plot mobiles
myplot <-OB_devices_summary %>%
  filter(DeviceType %in% "sphAndroid") %>%
  ggplot() +
  aes(
    x = SizeScreen,
    y = per_TotalUsers,
    fill = SizeScreen,
    colour = SizeScreen ) +
  geom_col() +
  geom_text(aes(label = per_TotalUsers),
            #position = "identity" ,
            hjust = -0.05L,
            color = "black",size = 3) +  # Cambiar el color de las etiquetas a negro
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Resolución de Pantalla",
    y = "Total Dispositivos",
    title = "Total Dispositivos x Resolución de Pantalla por OB",
    subtitle = "Dispositivos Smartphone Android",
    fill = " ",
    color = " "
  ) +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(OB_Name), ncol = 7L)+
  theme(legend.position="none", axis.text.x=element_blank())+
  ylim(0, 40)
myplot


# Tablets
myplot <-OB_devices_summary %>%
  filter(DeviceType %in% "tabAndroid") %>%
  ggplot() +
  aes(
    x = SizeScreen,
    y = per_TotalUsers,
    fill = SizeScreen,
    colour = SizeScreen ) +
  geom_col() +
  geom_text(aes(label = per_TotalUsers),
            #position = "identity" ,
            hjust = -0.05L,
            color = "black",size = 3) +  # Cambiar el color de las etiquetas a negro
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Resolución de Pantalla",
    y = "Total Dispositivos",
    title = "Total Dispositivos x Resolución de Pantalla por OB",
    subtitle = "Dispositivos Tablets Android",
    fill = " ",
    color = " "
  ) +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(OB_Name), ncol = 7L)+
  theme(legend.position="none", axis.text.x=element_blank())+
  ylim(0, 85)
myplot

