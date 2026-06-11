#Paquetes necesarios
library(readr)
library(dplyr)

#_______________________________________________________________________________

#DATOS SIG
#Se realizan cambios de unidades para mejorar la interepretabilidad.

Lacuna_Yield_data_SIG_raw <- read_csv("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/base_de_datos_final/dataset_SIG_raw/Lacuna_Yield_data_SIG_raw.csv")
View(Lacuna_Yield_data_SIG_raw)


#Las variables de proximidad están en metros. Se realiza la conversión a km
Lacuna_Yield_data_SIG_raw$prox_towns_km<-Lacuna_Yield_data_SIG_raw$prox_towns/1000
Lacuna_Yield_data_SIG_raw$prox_roads_km<-Lacuna_Yield_data_SIG_raw$prox_roads/1000
Lacuna_Yield_data_SIG_raw$prox_water_lines_km<-Lacuna_Yield_data_SIG_raw$prox_water_lines/1000
Lacuna_Yield_data_SIG_raw$prox_water_areas_km<-Lacuna_Yield_data_SIG_raw$prox_water_areas/1000


#Las variables relativas a las propiedades de SoilGrids vienen convertidas.
#SoilGrids realiza una conversión para evitar que los rásters originales 
#contengan decimales,lo que disminuye el peso de los archivos. En este caso, 
#las variables relativas al suelo contienen decimales por la interpolación 
#(Bilineal) que se realiza al reproyectar las capas de EPSG:4326 a EPSG:6933.#La 
#conversión realizada es la que se recomienda en la documentación de SoilGrids.


# pH
Lacuna_Yield_data_SIG_raw$phh2o_0_30cm   <- Lacuna_Yield_data_SIG_raw$phh2o_0_30cm_raw/10
Lacuna_Yield_data_SIG_raw$phh2o_30_100cm <- Lacuna_Yield_data_SIG_raw$phh2o_30_100cm_raw/10
Lacuna_Yield_data_SIG_raw$phh2o_100_200cm<- Lacuna_Yield_data_SIG_raw$phh2o_100_200cm_raw/10

# SOC
Lacuna_Yield_data_SIG_raw$soc_0_30cm   <- Lacuna_Yield_data_SIG_raw$soc_0_30cm_raw/10
Lacuna_Yield_data_SIG_raw$soc_30_100cm <- Lacuna_Yield_data_SIG_raw$soc_30_100cm_raw/10
Lacuna_Yield_data_SIG_raw$soc_100_200cm<- Lacuna_Yield_data_SIG_raw$soc_100_200cm_raw/10

# Nitrógeno
Lacuna_Yield_data_SIG_raw$N_0_30cm   <- Lacuna_Yield_data_SIG_raw$N_0_30cm_raw/100
Lacuna_Yield_data_SIG_raw$N_30_100cm <- Lacuna_Yield_data_SIG_raw$N_30_100cm_raw/100
Lacuna_Yield_data_SIG_raw$N_100_200cm<- Lacuna_Yield_data_SIG_raw$N_100_200cm_raw/100

# CEC
Lacuna_Yield_data_SIG_raw$cec_0_30cm   <- Lacuna_Yield_data_SIG_raw$cec_0_30cm_raw/10
Lacuna_Yield_data_SIG_raw$cec_30_100cm <- Lacuna_Yield_data_SIG_raw$cec_30_100cm_raw/10
Lacuna_Yield_data_SIG_raw$cec_100_200cm<- Lacuna_Yield_data_SIG_raw$cec_100_200cm_raw/10

# Bulk density
Lacuna_Yield_data_SIG_raw$bdod_0_30cm   <- Lacuna_Yield_data_SIG_raw$bdod_0_30cm_raw/100
Lacuna_Yield_data_SIG_raw$bdod_30_100cm <- Lacuna_Yield_data_SIG_raw$bdod_30_100cm_raw/100
Lacuna_Yield_data_SIG_raw$bdod_100_200cm<- Lacuna_Yield_data_SIG_raw$bdod_100_200cm_raw/100

# Coarse fragments
Lacuna_Yield_data_SIG_raw$cfvo_0_30cm   <- Lacuna_Yield_data_SIG_raw$cfvo_0_30cm_raw/10
Lacuna_Yield_data_SIG_raw$cfvo_30_100cm <- Lacuna_Yield_data_SIG_raw$cfvo_30_100cm_raw/10
Lacuna_Yield_data_SIG_raw$cfvo_100_200cm<- Lacuna_Yield_data_SIG_raw$cfvo_100_200cm_raw/10


# Clay
Lacuna_Yield_data_SIG_raw$clay_0_30cm   <- Lacuna_Yield_data_SIG_raw$clay_0_30cm_raw/10
Lacuna_Yield_data_SIG_raw$clay_30_100cm <- Lacuna_Yield_data_SIG_raw$clay_30_100cm_raw/10
Lacuna_Yield_data_SIG_raw$clay_100_200cm<- Lacuna_Yield_data_SIG_raw$clay_100_200cm_raw/10

# Sand
Lacuna_Yield_data_SIG_raw$sand_0_30cm   <- Lacuna_Yield_data_SIG_raw$sand_0_30cm_raw/10
Lacuna_Yield_data_SIG_raw$sand_30_100cm <- Lacuna_Yield_data_SIG_raw$sand_30_100cm_raw/10
Lacuna_Yield_data_SIG_raw$sand_100_200cm<- Lacuna_Yield_data_SIG_raw$sand_100_200cm_raw/10

# Silt
Lacuna_Yield_data_SIG_raw$silt_0_30cm   <- Lacuna_Yield_data_SIG_raw$silt_0_30cm_raw/10
Lacuna_Yield_data_SIG_raw$silt_30_100cm <- Lacuna_Yield_data_SIG_raw$silt_30_100cm_raw/10
Lacuna_Yield_data_SIG_raw$silt_100_200cm<- Lacuna_Yield_data_SIG_raw$silt_100_200cm_raw/10


# Water content 10 kPa
Lacuna_Yield_data_SIG_raw$wv0010_0_30cm   <- Lacuna_Yield_data_SIG_raw$wv0010_0_30cm_raw/10
Lacuna_Yield_data_SIG_raw$wv0010_30_100cm <- Lacuna_Yield_data_SIG_raw$wv0010_30_100cm_raw/10
Lacuna_Yield_data_SIG_raw$wv0010_100_200cm<- Lacuna_Yield_data_SIG_raw$wv0010_100_200cm_raw/10

# Water content 33 kPa
Lacuna_Yield_data_SIG_raw$wv003_0_30cm   <- Lacuna_Yield_data_SIG_raw$wv003_0_30cm_raw/10
Lacuna_Yield_data_SIG_raw$wv003_30_100cm <- Lacuna_Yield_data_SIG_raw$wv003_30_100cm_raw/10
Lacuna_Yield_data_SIG_raw$wv003_100_200cm<- Lacuna_Yield_data_SIG_raw$wv003_100_200cm_raw/10

# Water content 1500 kPa
Lacuna_Yield_data_SIG_raw$wv1500_0_30cm   <- Lacuna_Yield_data_SIG_raw$wv1500_0_30cm_raw/10
Lacuna_Yield_data_SIG_raw$wv1500_30_100cm <- Lacuna_Yield_data_SIG_raw$wv1500_30_100cm_raw/10
Lacuna_Yield_data_SIG_raw$wv1500_100_200cm<- Lacuna_Yield_data_SIG_raw$wv1500_100_200cm_raw/10



#Normalización a 100 de las variables sand, silt y clay. Este paso obligará a 
#que la suma de estas variables sea exactamente 100%

#Para 0-30 cm 
total_0_30 <- Lacuna_Yield_data_SIG_raw$clay_0_30cm +
  Lacuna_Yield_data_SIG_raw$sand_0_30cm +
  Lacuna_Yield_data_SIG_raw$silt_0_30cm

Lacuna_Yield_data_SIG_raw$clay_0_30cm <- Lacuna_Yield_data_SIG_raw$clay_0_30cm / total_0_30 * 100
Lacuna_Yield_data_SIG_raw$sand_0_30cm <- Lacuna_Yield_data_SIG_raw$sand_0_30cm / total_0_30 * 100
Lacuna_Yield_data_SIG_raw$silt_0_30cm <- Lacuna_Yield_data_SIG_raw$silt_0_30cm / total_0_30 * 100


#Para 30-100 cm
total_30_100 <- Lacuna_Yield_data_SIG_raw$clay_30_100cm +
  Lacuna_Yield_data_SIG_raw$sand_30_100cm +
  Lacuna_Yield_data_SIG_raw$silt_30_100cm

Lacuna_Yield_data_SIG_raw$clay_30_100cm <- Lacuna_Yield_data_SIG_raw$clay_30_100cm / total_30_100 * 100
Lacuna_Yield_data_SIG_raw$sand_30_100cm <- Lacuna_Yield_data_SIG_raw$sand_30_100cm / total_30_100 * 100
Lacuna_Yield_data_SIG_raw$silt_30_100cm <- Lacuna_Yield_data_SIG_raw$silt_30_100cm / total_30_100 * 100


#Para 100-200 cm 
total_100_200 <- Lacuna_Yield_data_SIG_raw$clay_100_200cm +
  Lacuna_Yield_data_SIG_raw$sand_100_200cm +
  Lacuna_Yield_data_SIG_raw$silt_100_200cm

Lacuna_Yield_data_SIG_raw$clay_100_200cm <- Lacuna_Yield_data_SIG_raw$clay_100_200cm / total_100_200 * 100
Lacuna_Yield_data_SIG_raw$sand_100_200cm <- Lacuna_Yield_data_SIG_raw$sand_100_200cm / total_100_200 * 100
Lacuna_Yield_data_SIG_raw$silt_100_200cm <- Lacuna_Yield_data_SIG_raw$silt_100_200cm / total_100_200 * 100



#Eliminación de las columnas con datos 'crudos' con select de dplyr
Lacuna_Yield_data_SIG_raw <- Lacuna_Yield_data_SIG_raw %>%select(
    -prox_towns,
    -prox_roads,
    -prox_water_lines,
    -prox_water_areas,
    -phh2o_0_30cm_raw, -phh2o_30_100cm_raw, -phh2o_100_200cm_raw,
    -soc_0_30cm_raw, -soc_30_100cm_raw, -soc_100_200cm_raw,
    -N_0_30cm_raw, -N_30_100cm_raw, -N_100_200cm_raw,
    -cec_0_30cm_raw, -cec_30_100cm_raw, -cec_100_200cm_raw,
    -bdod_0_30cm_raw, -bdod_30_100cm_raw, -bdod_100_200cm_raw,
    -cfvo_0_30cm_raw, -cfvo_30_100cm_raw, -cfvo_100_200cm_raw,
    -clay_0_30cm_raw, -clay_30_100cm_raw, -clay_100_200cm_raw,
    -sand_0_30cm_raw, -sand_30_100cm_raw, -sand_100_200cm_raw,
    -silt_0_30cm_raw, -silt_30_100cm_raw, -silt_100_200cm_raw,
    -wv0010_0_30cm_raw, -wv0010_30_100cm_raw, -wv0010_100_200cm_raw,
    -wv003_0_30cm_raw, -wv003_30_100cm_raw, -wv003_100_200cm_raw,
    -wv1500_0_30cm_raw, -wv1500_30_100cm_raw, -wv1500_100_200cm_raw)



#_______________________________________________________________________________
#DATOS DE NDVI
library(readr)
Lacuna_Yield_NDVI_data <- read_csv("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/output/Features_NDVI/Resultados_NDVI.csv")
View(Lacuna_Yield_NDVI_data)

#Gestión de NAs
colSums(is.na(Lacuna_Yield_NDVI_data)) 
#4446 datos NA de NDVI


dim(na.omit(Lacuna_Yield_NDVI_data))
#Trabajar solo con datos disponibles supondría perder 1449 observaciones.
#Tampoco es correcto imputar sobre 4446 datos de 1449 observaciones
#Los NA son posiblemente debidos a que el satélite Sentinel-2 no puede tomar la 
#foto como consecuencia de una alta cobertura nubosa, y porque tiene una frecuencia 
#de al menos 5 días.

#Se decide imputar solo los datos de: 
#ndvi_mean_wcomplete   ndvi_max_wcomplete   ndvi_sum_wcomplete 
#Ya que estos datos solo tienen 1 NA cada uno: se imputan 3 datos
#Esto permite incluir información sobre features de NDVI sin imputación agresiva

#Los 3 datos NA de NDVI_wcomplete son de location_id 1584
Lacuna_Yield_NDVI_data[is.na(Lacuna_Yield_NDVI_data$ndvi_mean_wcomplete), ]
Lacuna_Yield_NDVI_data[is.na(Lacuna_Yield_NDVI_data$ndvi_max_wcomplete), ]
Lacuna_Yield_NDVI_data[is.na(Lacuna_Yield_NDVI_data$ndvi_sum_wcomplete), ]


#Imputación simple por media de: ndvi_mean_wcomplete   ndvi_max_wcomplete   ndvi_sum_wcomplete 


Lacuna_Yield_NDVI_data  <- Lacuna_Yield_NDVI_data  %>%
  mutate(
    ndvi_mean_wcomplete = ifelse(
      is.na(ndvi_mean_wcomplete),
      mean(ndvi_mean_wcomplete, na.rm = TRUE),
      ndvi_mean_wcomplete),
    
    ndvi_max_wcomplete = ifelse(
      is.na(ndvi_max_wcomplete),
      mean(ndvi_max_wcomplete, na.rm = TRUE),
      ndvi_max_wcomplete),
    
    ndvi_sum_wcomplete = ifelse(
      is.na(ndvi_sum_wcomplete),
      mean(ndvi_sum_wcomplete, na.rm = TRUE),
      ndvi_sum_wcomplete))


#Estas variables ya no tienen ningún NA
colSums(is.na(Lacuna_Yield_NDVI_data)) 

#Creamos nuevo df para la unión
Lacuna_Yield_NDVI_data <-Lacuna_Yield_NDVI_data%>%
  select("location_id","ndvi_mean_wcomplete","ndvi_max_wcomplete", "ndvi_sum_wcomplete")

any(is.na(Lacuna_Yield_NDVI_data))

#Estas features de NDVI ahora tomarán los siguientes valores
Lacuna_Yield_NDVI_data[Lacuna_Yield_NDVI_data$location_id==1584, ]




#_______________________________________________________________________________

#JOIN DE LAS BASES DE DATOS DE SIG, DE VARIABLES METEREOLÓGICAS Y DE NDVI A TRAVÉS DE ID 
Lacuna_Yield_metereological_data <- read_csv("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/output/Variables_metereológicas_transformadas/resultados_finales_transformaciones_VM.csv")
View(Lacuna_Yield_metereological_data)

#Para que luego en la unión no haya una columna repetida
Lacuna_Yield_metereological_data$Crop_Type
Lacuna_Yield_metereological_data <- Lacuna_Yield_metereological_data %>%select(-Crop_Type)

#_______________________________________________________________________________
#JOIN DE LAS BASES DE DATOS DE SIG, DE VARIABLES METEREOLÓGICAS Y DE NDVI A TRAVÉS DE ID

#La variable de unión para estos dataframes es:
Lacuna_Yield_data_SIG_raw$ID
Lacuna_Yield_metereological_data$location_id
Lacuna_Yield_NDVI_data$location_id


#JOIN

df_completed <- merge(Lacuna_Yield_data_SIG_raw, Lacuna_Yield_metereological_data,
  by.x = "ID", by.y = "location_id", all = TRUE)
df_completed <- merge(df_completed,Lacuna_Yield_NDVI_data, by.x = "ID", by.y = "location_id",
  all = TRUE)

View(df_completed)
str(df_completed)
#Me guardo este dataframe. A partir de ahora, este será el dataset input para 
#posteriores análisis. La línea de código de guardado se ha borrado para evitar 
#reasignaciones del archivo. 















