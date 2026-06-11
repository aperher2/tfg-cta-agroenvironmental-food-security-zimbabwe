#CÁLCULO DE LAS TRANSFORMACIONES METEREOLÓGICAS REALIZADAS PARA CADA VENTANA DE 
#LA CAMPAÑA.

#Al hacer la consulta de Open Meteo. Se ha abierto el archivo para poner los
#identificadores en la segunda hoja y los resultados de la consulta en la primera.
#También, en los resultados de la consulta, se le cambia el nombre a las variables
#quitándole las unidades de medida del nombre, para evitar errores. 

#_______________________________________________________________________________

# VENTANA 1: de Planting a t1

#Cargamos la consulta realizada a Open Meteo para la ventana 1
library(readxl)
OM_Query_w1 <- read_excel("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/input/Variables_metereológicas_Open_Meteo/Consultas_Open_Meteo/Resultados_Consulta_Open_Meteo/Ventana_1/Resultados_Consulta_Ventana_1.xlsx")
View(OM_Query_w1)


summary(OM_Query_w1)


#Para que location_ID generado por Open Meteo coincida con el ID generado para 
#cada parcela de estudio en QGIS
OM_Query_w1$location_id<-OM_Query_w1$input_id + 1


#Transformaciones metereológicas para la ventana 1 
results_window_1 <- do.call(rbind,
                            lapply(split(OM_Query_w1, OM_Query_w1$location_id), function(x) {
                              data.frame(
                                #################################
                                # IDENTIFICADOR
                                #################################
                                location_id = x$location_id[1],
                                
                                #################################
                                # TEMPERATURA
                                #################################
                                temp_mean_w1 = mean(x$temperature_2m_mean, na.rm = TRUE),
                                
                                dtr_mean_w1 = mean(x$temperature_2m_max - x$temperature_2m_min, na.rm = TRUE),
                                
                                n_hot35_w1 = sum(x$temperature_2m_max >= 35, na.rm = TRUE),
                                heat_excess35_w1 = sum(pmax(x$temperature_2m_max - 35, 0), na.rm = TRUE),
                                
                                n_cold10_w1 = sum(x$temperature_2m_min <= 10, na.rm = TRUE),
                                cold_deficit10_w1 = sum(pmax(10 - x$temperature_2m_min, 0), na.rm = TRUE),
                                
                                soil_temp_mean_w1 = mean(x$soil_temperature_0_to_100cm_mean, na.rm = TRUE),
                                
                                dew_point_mean_w1 = mean(x$dew_point_2m_mean, na.rm = TRUE),
                                
                                #################################
                                # PRECIPITACIÓN Y AGUA
                                #################################
                                rain_total_w1 = sum(x$rain_sum, na.rm = TRUE),
                                
                                precipitation_total_w1 = sum(x$precipitation_sum, na.rm = TRUE),
                                
                                precip_hours_w1 = sum(x$precipitation_hours, na.rm = TRUE),
                                
                                n_rain1_w1 = sum(x$rain_sum > 1, na.rm = TRUE),
                                n_rain10_w1 = sum(x$rain_sum > 10, na.rm = TRUE),
                                
                                max_rain1d_w1 = max(x$rain_sum, na.rm = TRUE),
                                
                                dryspell_w1 = {
                                  r <- rle(x$rain_sum < 1)
                                  if (any(r$values)) max(r$lengths[r$values]) else 0
                                },
                                
                                sm_mean_w1 = mean(x$soil_moisture_0_to_100cm_mean, na.rm = TRUE),
                                sm_min_w1  = min(x$soil_moisture_0_to_100cm_mean, na.rm = TRUE),
                                
                                #################################
                                # RADIACIÓN
                                #################################
                                rad_total_w1 = sum(x$shortwave_radiation_sum, na.rm = TRUE),
                                
                                sunshine_hours_w1 = sum(x$sunshine_duration, na.rm = TRUE) / 3600,
                                
                                cloud_mean_w1 = mean(x$cloud_cover_mean, na.rm = TRUE),
                                
                                #################################
                                # HUMEDAD Y EVAPOTRANSPIRACIÓN
                                #################################
                                et0_total_w1 = sum(x$et0_fao_evapotranspiration_sum, na.rm = TRUE),
                                
                                n_et0_high_w1 = sum(x$et0_fao_evapotranspiration_sum >= 5, na.rm = TRUE),
                                
                                rh_mean_w1 = mean(x$relative_humidity_2m_mean, na.rm = TRUE),
                                
                                rh_min_w1 = min(x$relative_humidity_2m_min, na.rm = TRUE),
                                
                                n_rh_low_w1 = sum(x$relative_humidity_2m_min <= 30, na.rm = TRUE),
                                
                                vpd_max_w1 = max(x$vapour_pressure_deficit_max, na.rm = TRUE),
                                
                                water_balance_w1 = sum(x$rain_sum, na.rm = TRUE) -
                                  sum(x$et0_fao_evapotranspiration_sum, na.rm = TRUE),
                                
                                #################################
                                # VIENTO
                                #################################
                                wind_mean_w1 = mean(x$wind_speed_10m_mean, na.rm = TRUE),
                                
                                wind_gust_mean_w1 = mean(x$wind_gusts_10m_mean, na.rm = TRUE),
                                
                                wind_max_w1 = max(x$wind_speed_10m_max, na.rm = TRUE),
                                
                                wind_gust_max_w1 = max(x$wind_gusts_10m_max, na.rm = TRUE),
                                
                                n_wind_gust30_w1 = sum(x$wind_gusts_10m_max >= 30, na.rm = TRUE))}))


View(results_window_1)

any(is.na(results_window_1))


#_______________________________________________________________________________

# VENTANA 2: de t1 a t2

#Cargamos la consulta realizada a Open Meteo para la ventana 2
library(readxl)
library(readxl)
OM_Query_w2 <- read_excel("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/input/Variables_metereológicas_Open_Meteo/Consultas_Open_Meteo/Resultados_Consulta_Open_Meteo/Ventana_2/Resultados_Consulta_Ventana_2.xlsx")
View(OM_Query_w2)


#Para que location_ID generado por Open Meteo coincida con el ID generado para 
#cada parcela de estudio en QGIS
OM_Query_w2$location_id<-OM_Query_w2$input_id + 1


#Transformaciones metereológicas para la ventana 2
results_window_2 <- do.call(rbind,
                            lapply(split(OM_Query_w2, OM_Query_w2$location_id), function(x) {
                              data.frame(
                                location_id = x$location_id[1],
                                
                                temp_mean_w2 = mean(x$temperature_2m_mean, na.rm = TRUE),
                                dtr_mean_w2 = mean(x$temperature_2m_max - x$temperature_2m_min, na.rm = TRUE),
                                
                                n_hot35_w2 = sum(x$temperature_2m_max >= 35, na.rm = TRUE),
                                heat_excess35_w2 = sum(pmax(x$temperature_2m_max - 35, 0), na.rm = TRUE),
                                
                                n_cold10_w2 = sum(x$temperature_2m_min <= 10, na.rm = TRUE),
                                cold_deficit10_w2 = sum(pmax(10 - x$temperature_2m_min, 0), na.rm = TRUE),
                                
                                soil_temp_mean_w2 = mean(x$soil_temperature_0_to_100cm_mean, na.rm = TRUE),
                                dew_point_mean_w2 = mean(x$dew_point_2m_mean, na.rm = TRUE),
                                
                                rain_total_w2 = sum(x$rain_sum, na.rm = TRUE),
                                precipitation_total_w2 = sum(x$precipitation_sum, na.rm = TRUE),
                                precip_hours_w2 = sum(x$precipitation_hours, na.rm = TRUE),
                                
                                n_rain1_w2 = sum(x$rain_sum > 1, na.rm = TRUE),
                                n_rain10_w2 = sum(x$rain_sum > 10, na.rm = TRUE),
                                
                                max_rain1d_w2 = max(x$rain_sum, na.rm = TRUE),
                                
                                dryspell_w2 = {
                                  r <- rle(x$rain_sum < 1)
                                  if (any(r$values)) max(r$lengths[r$values]) else 0
                                },
                                
                                sm_mean_w2 = mean(x$soil_moisture_0_to_100cm_mean, na.rm = TRUE),
                                sm_min_w2  = min(x$soil_moisture_0_to_100cm_mean, na.rm = TRUE),
                                
                                rad_total_w2 = sum(x$shortwave_radiation_sum, na.rm = TRUE),
                                sunshine_hours_w2 = sum(x$sunshine_duration, na.rm = TRUE) / 3600,
                                cloud_mean_w2 = mean(x$cloud_cover_mean, na.rm = TRUE),
                                
                                et0_total_w2 = sum(x$et0_fao_evapotranspiration_sum, na.rm = TRUE),
                                n_et0_high_w2 = sum(x$et0_fao_evapotranspiration_sum >= 5, na.rm = TRUE),
                                
                                rh_mean_w2 = mean(x$relative_humidity_2m_mean, na.rm = TRUE),
                                rh_min_w2 = min(x$relative_humidity_2m_min, na.rm = TRUE),
                                n_rh_low_w2 = sum(x$relative_humidity_2m_min <= 30, na.rm = TRUE),
                                
                                vpd_max_w2 = max(x$vapour_pressure_deficit_max, na.rm = TRUE),
                                
                                water_balance_w2 = sum(x$rain_sum, na.rm = TRUE) -
                                  sum(x$et0_fao_evapotranspiration_sum, na.rm = TRUE),
                                
                                wind_mean_w2 = mean(x$wind_speed_10m_mean, na.rm = TRUE),
                                wind_gust_mean_w2 = mean(x$wind_gusts_10m_mean, na.rm = TRUE),
                                
                                wind_max_w2 = max(x$wind_speed_10m_max, na.rm = TRUE),
                                wind_gust_max_w2 = max(x$wind_gusts_10m_max, na.rm = TRUE),
                                
                                n_wind_gust30_w2 = sum(x$wind_gusts_10m_max >= 30, na.rm = TRUE))}))


View(results_window_2)
any(is.na(results_window_2))

#_______________________________________________________________________________
# VENTANA 3: de t2 a t3

#Cargamos la consulta realizada a Open Meteo para la ventana 3
library(readxl)
OM_Query_w3 <- read_excel("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/input/Variables_metereológicas_Open_Meteo/Consultas_Open_Meteo/Resultados_Consulta_Open_Meteo/Ventana_3/Resultados_Consulta_Ventana_3.xlsx")
View(OM_Query_w3)


#Para que location_ID generado por Open Meteo coincida con el ID generado para 
#cada parcela de estudio en QGIS 
OM_Query_w3$location_id<-OM_Query_w3$input_id + 1


#Transformaciones metereológicas para la ventana 3
results_window_3 <- do.call(rbind,
                            lapply(split(OM_Query_w3, OM_Query_w3$location_id), function(x) {
                              data.frame(
                                location_id = x$location_id[1],
                                
                                temp_mean_w3 = mean(x$temperature_2m_mean, na.rm = TRUE),
                                dtr_mean_w3 = mean(x$temperature_2m_max - x$temperature_2m_min, na.rm = TRUE),
                                
                                n_hot35_w3 = sum(x$temperature_2m_max >= 35, na.rm = TRUE),
                                heat_excess35_w3 = sum(pmax(x$temperature_2m_max - 35, 0), na.rm = TRUE),
                                
                                n_cold10_w3 = sum(x$temperature_2m_min <= 10, na.rm = TRUE),
                                cold_deficit10_w3 = sum(pmax(10 - x$temperature_2m_min, 0), na.rm = TRUE),
                                
                                soil_temp_mean_w3 = mean(x$soil_temperature_0_to_100cm_mean, na.rm = TRUE),
                                dew_point_mean_w3 = mean(x$dew_point_2m_mean, na.rm = TRUE),
                                
                                rain_total_w3 = sum(x$rain_sum, na.rm = TRUE),
                                precipitation_total_w3 = sum(x$precipitation_sum, na.rm = TRUE),
                                precip_hours_w3 = sum(x$precipitation_hours, na.rm = TRUE),
                                
                                n_rain1_w3 = sum(x$rain_sum > 1, na.rm = TRUE),
                                n_rain10_w3 = sum(x$rain_sum > 10, na.rm = TRUE),
                                
                                max_rain1d_w3 = max(x$rain_sum, na.rm = TRUE),
                                
                                dryspell_w3 = {
                                  r <- rle(x$rain_sum < 1)
                                  if (any(r$values)) max(r$lengths[r$values]) else 0
                                },
                                
                                sm_mean_w3 = mean(x$soil_moisture_0_to_100cm_mean, na.rm = TRUE),
                                sm_min_w3  = min(x$soil_moisture_0_to_100cm_mean, na.rm = TRUE),
                                
                                rad_total_w3 = sum(x$shortwave_radiation_sum, na.rm = TRUE),
                                sunshine_hours_w3 = sum(x$sunshine_duration, na.rm = TRUE) / 3600,
                                cloud_mean_w3 = mean(x$cloud_cover_mean, na.rm = TRUE),
                                
                                et0_total_w3 = sum(x$et0_fao_evapotranspiration_sum, na.rm = TRUE),
                                n_et0_high_w3 = sum(x$et0_fao_evapotranspiration_sum >= 5, na.rm = TRUE),
                                
                                rh_mean_w3 = mean(x$relative_humidity_2m_mean, na.rm = TRUE),
                                rh_min_w3 = min(x$relative_humidity_2m_min, na.rm = TRUE),
                                n_rh_low_w3 = sum(x$relative_humidity_2m_min <= 30, na.rm = TRUE),
                                
                                vpd_max_w3 = max(x$vapour_pressure_deficit_max, na.rm = TRUE),
                                
                                water_balance_w3 = sum(x$rain_sum, na.rm = TRUE) -
                                  sum(x$et0_fao_evapotranspiration_sum, na.rm = TRUE),
                                
                                wind_mean_w3 = mean(x$wind_speed_10m_mean, na.rm = TRUE),
                                wind_gust_mean_w3 = mean(x$wind_gusts_10m_mean, na.rm = TRUE),
                                
                                wind_max_w3 = max(x$wind_speed_10m_max, na.rm = TRUE),
                                wind_gust_max_w3 = max(x$wind_gusts_10m_max, na.rm = TRUE),
                                
                                n_wind_gust30_w3 = sum(x$wind_gusts_10m_max >= 30, na.rm = TRUE))}))



View(results_window_3)

any(is.na(results_window_3))

#_______________________________________________________________________________

# VENTANA 4: de t3 a Harvesting

#Cargamos la consulta realizada a Open Meteo para la ventana 4
library(readxl)
OM_Query_w4 <- read_excel("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/input/Variables_metereológicas_Open_Meteo/Consultas_Open_Meteo/Resultados_Consulta_Open_Meteo/Ventana_4/Resultados_Consulta_Ventana_4.xlsx")
View(OM_Query_w4)


#Para que location_ID generado por Open Meteo coincida con el ID generado para 
#cada parcela de estudio en QGIS 
OM_Query_w4$location_id<-OM_Query_w4$input_id + 1


#Transformaciones metereológicas para la ventana 4
results_window_4 <- do.call(rbind,
                            lapply(split(OM_Query_w4, OM_Query_w4$location_id), function(x) {
                              data.frame(
                                location_id = x$location_id[1],
                                
                                temp_mean_w4 = mean(x$temperature_2m_mean, na.rm = TRUE),
                                dtr_mean_w4 = mean(x$temperature_2m_max - x$temperature_2m_min, na.rm = TRUE),
                                
                                n_hot35_w4 = sum(x$temperature_2m_max >= 35, na.rm = TRUE),
                                heat_excess35_w4 = sum(pmax(x$temperature_2m_max - 35, 0), na.rm = TRUE),
                                
                                n_cold10_w4 = sum(x$temperature_2m_min <= 10, na.rm = TRUE),
                                cold_deficit10_w4 = sum(pmax(10 - x$temperature_2m_min, 0), na.rm = TRUE),
                                
                                soil_temp_mean_w4 = mean(x$soil_temperature_0_to_100cm_mean, na.rm = TRUE),
                                dew_point_mean_w4 = mean(x$dew_point_2m_mean, na.rm = TRUE),
                                
                                rain_total_w4 = sum(x$rain_sum, na.rm = TRUE),
                                precipitation_total_w4 = sum(x$precipitation_sum, na.rm = TRUE),
                                precip_hours_w4 = sum(x$precipitation_hours, na.rm = TRUE),
                                
                                n_rain1_w4 = sum(x$rain_sum > 1, na.rm = TRUE),
                                n_rain10_w4 = sum(x$rain_sum > 10, na.rm = TRUE),
                                
                                max_rain1d_w4 = max(x$rain_sum, na.rm = TRUE),
                                
                                dryspell_w4 = {
                                  r <- rle(x$rain_sum < 1)
                                  if (any(r$values)) max(r$lengths[r$values]) else 0
                                },
                                
                                sm_mean_w4 = mean(x$soil_moisture_0_to_100cm_mean, na.rm = TRUE),
                                sm_min_w4  = min(x$soil_moisture_0_to_100cm_mean, na.rm = TRUE),
                                
                                rad_total_w4 = sum(x$shortwave_radiation_sum, na.rm = TRUE),
                                sunshine_hours_w4 = sum(x$sunshine_duration, na.rm = TRUE) / 3600,
                                cloud_mean_w4 = mean(x$cloud_cover_mean, na.rm = TRUE),
                                
                                et0_total_w4 = sum(x$et0_fao_evapotranspiration_sum, na.rm = TRUE),
                                n_et0_high_w4 = sum(x$et0_fao_evapotranspiration_sum >= 5, na.rm = TRUE),
                                
                                rh_mean_w4 = mean(x$relative_humidity_2m_mean, na.rm = TRUE),
                                rh_min_w4 = min(x$relative_humidity_2m_min, na.rm = TRUE),
                                n_rh_low_w4 = sum(x$relative_humidity_2m_min <= 30, na.rm = TRUE),
                                
                                vpd_max_w4 = max(x$vapour_pressure_deficit_max, na.rm = TRUE),
                                
                                water_balance_w4 = sum(x$rain_sum, na.rm = TRUE) -
                                  sum(x$et0_fao_evapotranspiration_sum, na.rm = TRUE),
                                
                                wind_mean_w4 = mean(x$wind_speed_10m_mean, na.rm = TRUE),
                                wind_gust_mean_w4 = mean(x$wind_gusts_10m_mean, na.rm = TRUE),
                                
                                wind_max_w4 = max(x$wind_speed_10m_max, na.rm = TRUE),
                                wind_gust_max_w4 = max(x$wind_gusts_10m_max, na.rm = TRUE),
                                
                                n_wind_gust30_w4 = sum(x$wind_gusts_10m_max >= 30, na.rm = TRUE))}))



View(results_window_4)

any(is.na(results_window_4))

#_______________________________________________________________________________

# VENTANA TERMINAL: de t_terminal a Harvesting
#Cargamos la consulta realizada a Open Meteo para la ventana terminal
library(readxl)
OM_Query_wterminal <- read_excel("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/input/Variables_metereológicas_Open_Meteo/Consultas_Open_Meteo/Resultados_Consulta_Open_Meteo/Ventana_terminal/Resultados_Consulta_Ventana_Terminal.xlsx")
View(OM_Query_wterminal)


#Para que location_ID generado por Open Meteo coincida con el ID generado para 
#cada parcela de estudio en QGIS 
OM_Query_wterminal$location_id<-OM_Query_wterminal$input_id + 1



#Transformaciones metereológicas para la ventana terminal
results_wterminal <- do.call(rbind,
                             lapply(split(OM_Query_wterminal, OM_Query_wterminal$location_id), function(x) {
                               data.frame(
                                 location_id = x$location_id[1],
                                 
                                 n_hot35_wterminal = sum(x$temperature_2m_max >= 35, na.rm = TRUE),
                                 heat_excess35_wterminal = sum(pmax(x$temperature_2m_max - 35, 0), na.rm = TRUE),
                                 
                                 n_cold10_wterminal = sum(x$temperature_2m_min <= 10, na.rm = TRUE),
                                 cold_deficit10_wterminal = sum(pmax(10 - x$temperature_2m_min, 0), na.rm = TRUE),
                                 
                                 sm_min_wterminal = min(x$soil_moisture_0_to_100cm_mean, na.rm = TRUE),
                                 
                                 vpd_max_wterminal = max(x$vapour_pressure_deficit_max, na.rm = TRUE),
                                 
                                 wind_gust_max_wterminal = max(x$wind_gusts_10m_max, na.rm = TRUE),
                                 
                                 n_wind_gust30_wterminal = sum(x$wind_gusts_10m_max >= 30, na.rm = TRUE))}))


View(results_wterminal)

any(is.na(results_wterminal))


#_______________________________________________________________________________

# VENTANA COMPLETA: de Planting a Harvesting
#Cargamos la consulta realizada a Open Meteo para la ventana completa
library(readxl)
OM_Query_wcomplete <- read_excel("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/input/Variables_metereológicas_Open_Meteo/Consultas_Open_Meteo/Resultados_Consulta_Open_Meteo/Ventana_completa/Resultados_Consulta_Ventana_Completa.xlsx")
View(OM_Query_wcomplete_)


#Para que location_ID generado por Open Meteo coincida con el ID generado para 
#cada parcela de estudio en QGIS 
OM_Query_wcomplete$location_id<-OM_Query_wcomplete$input_id + 1


#Join de crop_type a través de ID
#Cargo .csv de Preparación_Consulta_Open_Meteo
file.choose()
Preparación_Consulta_Open_Meteo<-read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\TFG_CTA_Zimbabwe\\Obtención_datos_CTA_Zimbabwe\\input\\Variables_metereológicas_Open_Meteo\\Consultas_Open_Meteo\\Preparación_Consulta_Open_Meteo\\Preparación_Consulta_Open_Meteo.csv", sep=",", dec=".")
View(Preparación_Consulta_Open_Meteo)

any(is.na(Preparación_Consulta_Open_Meteo$Crop_Type))

#Join con la función match
OM_Query_wcomplete$Crop_Type <- Preparación_Consulta_Open_Meteo$Crop_Type[match(OM_Query_wcomplete$location_id, Preparación_Consulta_Open_Meteo$ID)]
View(OM_Query_wcomplete)


#Transformaciones metereológicas para la ventana completa

results_wcomplete <- do.call(rbind,
                             lapply(split(OM_Query_wcomplete, OM_Query_wcomplete$location_id), function(x) {
                               data.frame(
                                 location_id = x$location_id[1],
                                 Crop_Type = x$Crop_Type[1],
                                 
                                 precipitation_total_wcomplete = sum(x$precipitation_sum, na.rm = TRUE),
                                 
                                 et0_total_wcomplete = sum(x$et0_fao_evapotranspiration_sum, na.rm = TRUE),
                                 
                                 rad_total_wcomplete = sum(x$shortwave_radiation_sum, na.rm = TRUE),
                                 
                                 gdd_total_wcomplete = sum(
                                   ifelse(x$Crop_Type[1] == "Maize/Corn",
                                          pmax(0, pmin(x$temperature_2m_mean, 30) - 8),
                                          
                                          ifelse(x$Crop_Type[1] == "Sorghum",
                                                 pmax(0, pmin(x$temperature_2m_mean, 30) - 8),
                                                 
                                                 ifelse(x$Crop_Type[1] == "Soybeans",
                                                        pmax(0, pmin(x$temperature_2m_mean, 30) - 5),
                                                        
                                                        ifelse(x$Crop_Type[1] == "Wheat",
                                                               pmax(0, pmin(x$temperature_2m_mean, 26) - 0),
                                                               NA)))),na.rm = TRUE))}))


View(results_wcomplete)
any(is.na(results_wcomplete))


#_______________________________________________________________________________

#JOIN DE TODOS LOS RESULTADOS a través de location_id
results_all <- merge(results_window_1, results_window_2, by = "location_id", all = TRUE)
results_all <- merge(results_all, results_window_3, by = "location_id", all = TRUE)
results_all <- merge(results_all, results_window_4, by = "location_id", all = TRUE)
results_all <- merge(results_all, results_wterminal, by = "location_id", all = TRUE)
results_all <- merge(results_all, results_wcomplete, by = "location_id", all = TRUE)


dim(results_all)
sum(duplicated(results_all$location_id))


#Descarga en formato .csv
write.csv(results_all,
  "C:/Users/aguia/OneDrive/Escritorio/resultados_finales_transformaciones_VM.csv",
  row.names = FALSE)


















