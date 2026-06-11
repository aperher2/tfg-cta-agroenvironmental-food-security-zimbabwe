#OBTENCIÓN DE LOS DATOS DE NDVI

#Una vez ya se ha realizado la consulta de los datos según las coordenadas, y el
#intervalo de fechas definido (cada ventana) al satélite Sentinel 2 a través de 
#Google Earth Engine, se calculan las features de NDVI para cada ventana

#_______________________________________________________________________________

#Uno de los problemas de GEE es que para algunas localizaciones devuelve NA, y para
#otras ni siquiera devuelve NA. Es decir, el ID no se encuentra el .csv output de
#la consulta.
#Esto es debido a que el Satélite Sentinel ofrece imágenes satelitales con 5 
#días de frecuencia, y a veces, no se puede tomar la imagen por la presencia de 
#nubes que imposibilitan el cálculo del NDVI.

#Antes de calcular las features NDVI para cada ventana, hay que garantizar que 
# estén todos los IDs en el input de la función lapply

#Cargo el archivo que contiene location_id
file.choose()
Preparación_Consultas_GEE<-read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\TFG_CTA_Zimbabwe\\Obtención_datos_CTA_Zimbabwe\\input\\NDVI\\Consultas_Google_Earth_Engine\\Preparación_Consultas_Google_Earth_Engine\\Preparación_Consulta_GEE.csv" , sep=",", dec=".")
View(Preparación_Consultas_GEE)

all_ids<-Preparación_Consultas_GEE$ID



# VENTANA 1: de Planting a t1

#Cargamos la consulta realizada a Open Meteo para la ventana 1
file.choose()
GEE_Query_w1 <- read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\TFG_CTA_Zimbabwe\\Obtención_datos_CTA_Zimbabwe\\input\\NDVI\\Consultas_Google_Earth_Engine\\Resultados_Consultas_Google_Earth_Engine\\Ventana_1\\NDVI_w1.csv", sep=",", dec=".")
View(GEE_Query_w1)

#IDs QUE FALTAN 
missing_ids_w1 <- setdiff(all_ids, unique(GEE_Query_w1$ID))
length(missing_ids_w1)

missing_df_w1 <- data.frame(system.index = NA, ID = missing_ids_w1, date = NA,
                            ndvi_mean = NA, .geo = NA)
dim(missing_df_w1)

#Añado los ids que faltan 
GEE_Query_w1_full <- rbind(GEE_Query_w1, missing_df_w1)

#Ordeno el df según los ids (en orden ascendente)
GEE_Query_w1_full <- GEE_Query_w1_full[order(GEE_Query_w1_full$ID), ]



#Features de NDVI para la ventana 1
results_ndvi_w1 <- do.call(rbind,
                           lapply(split(GEE_Query_w1_full, GEE_Query_w1_full$ID), function(x) {
                             data.frame(
                               location_id = x$ID[1],
                               
                               ndvi_mean_w1 = ifelse(all(is.na(x$ndvi_mean)), NA, mean(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_max_w1 = ifelse(all(is.na(x$ndvi_mean)), NA, max(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_sum_w1 = ifelse(all(is.na(x$ndvi_mean)), NA, sum(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_n_obs_w1 = sum(!is.na(x$ndvi_mean)))}))


View(results_ndvi_w1)

#_______________________________________________________________________________

# VENTANA 2: de t1 a t2

#Cargamos la consulta realizada a Open Meteo para la ventana 2
file.choose()
GEE_Query_w2<-read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\TFG_CTA_Zimbabwe\\Obtención_datos_CTA_Zimbabwe\\input\\NDVI\\Consultas_Google_Earth_Engine\\Resultados_Consultas_Google_Earth_Engine\\Ventana_2\\NDVI_w2.csv", sep=",", dec=".")
View(GEE_Query_w2)


#IDs QUE FALTAN 
missing_ids_w2 <- setdiff(all_ids, unique(GEE_Query_w2$ID))
length(missing_ids_w2)

missing_df_w2 <- data.frame(system.index = NA, ID = missing_ids_w2, date = NA,
                            ndvi_mean = NA, .geo = NA)
dim(missing_df_w2)

#Añado los ids que faltan 
GEE_Query_w2_full <- rbind(GEE_Query_w2, missing_df_w2)

#Ordeno el df según los ids (en orden ascendente)
GEE_Query_w2_full <- GEE_Query_w2_full[order(GEE_Query_w2_full$ID), ]




#Features de NDVI para la ventana 2
results_ndvi_w2 <- do.call(rbind,
                           lapply(split(GEE_Query_w2_full, GEE_Query_w2_full$ID), function(x) {
                             data.frame(
                               location_id = x$ID[1],
                               
                               ndvi_mean_w2 = ifelse(all(is.na(x$ndvi_mean)), NA, mean(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_max_w2  = ifelse(all(is.na(x$ndvi_mean)), NA, max(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_sum_w2  = ifelse(all(is.na(x$ndvi_mean)), NA, sum(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_n_obs_w2 = sum(!is.na(x$ndvi_mean)))}))


View(results_ndvi_w2)


#_______________________________________________________________________________

# VENTANA 3: de t2 a t3

#Cargamos la consulta realizada a Open Meteo para la ventana 3
file.choose()
GEE_Query_w3 <-read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\TFG_CTA_Zimbabwe\\Obtención_datos_CTA_Zimbabwe\\input\\NDVI\\Consultas_Google_Earth_Engine\\Resultados_Consultas_Google_Earth_Engine\\Ventana_3\\NDVI_w3.csv" , sep=",", dec=".")


#IDs QUE FALTAN 
missing_ids_w3 <- setdiff(all_ids, unique(GEE_Query_w3$ID))
length(missing_ids_w3)

missing_df_w3 <- data.frame(system.index = NA, ID = missing_ids_w3, date = NA,
                            ndvi_mean = NA, .geo = NA)
dim(missing_df_w3)

#Añado los ids que faltan 
GEE_Query_w3_full <- rbind(GEE_Query_w3, missing_df_w3)

#Ordeno el df según los ids (en orden ascendente)
GEE_Query_w3_full <- GEE_Query_w3_full[order(GEE_Query_w3_full$ID), ]


#Features de NDVI para la ventana 3
results_ndvi_w3 <- do.call(rbind,
                           lapply(split(GEE_Query_w3_full, GEE_Query_w3_full$ID), function(x) {
                             data.frame(
                               location_id = x$ID[1],
                               
                               ndvi_mean_w3 = ifelse(all(is.na(x$ndvi_mean)), NA, mean(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_max_w3  = ifelse(all(is.na(x$ndvi_mean)), NA, max(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_sum_w3  = ifelse(all(is.na(x$ndvi_mean)), NA, sum(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_n_obs_w3 = sum(!is.na(x$ndvi_mean)))}))

View(results_ndvi_w3)

#_______________________________________________________________________________

# VENTANA 4: de t3 a Harvesting 

#Cargamos la consulta realizada a Open Meteo para la ventana 4
file.choose()
GEE_Query_w4 <-read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\TFG_CTA_Zimbabwe\\Obtención_datos_CTA_Zimbabwe\\input\\NDVI\\Consultas_Google_Earth_Engine\\Resultados_Consultas_Google_Earth_Engine\\Ventana_4\\NDVI_w4.csv", sep=",", dec=".")
View(GEE_Query_w4)


#IDs QUE FALTAN 
missing_ids_w4 <- setdiff(all_ids, unique(GEE_Query_w4$ID))
length(missing_ids_w4)

missing_df_w4 <- data.frame(system.index = NA, ID = missing_ids_w4, date = NA,
                            ndvi_mean = NA, .geo = NA)
dim(missing_df_w4)

#Añado los ids que faltan 
GEE_Query_w4_full <- rbind(GEE_Query_w4, missing_df_w4)

#Ordeno el df según los ids (en orden ascendente)
GEE_Query_w4_full <- GEE_Query_w4_full[order(GEE_Query_w4_full$ID), ]


#Features de NDVI para la ventana 4
results_ndvi_w4 <- do.call(rbind,
                           lapply(split(GEE_Query_w4_full, GEE_Query_w4_full$ID), function(x) {
                             data.frame(
                               location_id = x$ID[1],
                               
                               ndvi_mean_w4 = ifelse(all(is.na(x$ndvi_mean)), NA, mean(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_max_w4  = ifelse(all(is.na(x$ndvi_mean)), NA, max(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_sum_w4  = ifelse(all(is.na(x$ndvi_mean)), NA, sum(x$ndvi_mean, na.rm = TRUE)),
                               
                               ndvi_n_obs_w4 = sum(!is.na(x$ndvi_mean)))}))


View(results_ndvi_w4)

#_______________________________________________________________________________

# VENTANA TERMINAL: de t_terminal a Harvesting
file.choose()
GEE_Query_wterminal<- read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\TFG_CTA_Zimbabwe\\Obtención_datos_CTA_Zimbabwe\\input\\NDVI\\Consultas_Google_Earth_Engine\\Resultados_Consultas_Google_Earth_Engine\\Ventana_terminal\\NDVI_terminal.csv" , sep=",", dec=".")
View(GEE_Query_wterminal)


#IDs QUE FALTAN 
missing_ids_wterminal <- setdiff(all_ids, unique(GEE_Query_wterminal$ID))
length(missing_ids_wterminal)

missing_df_wterminal <- data.frame(system.index = NA, ID = missing_ids_wterminal, date = NA,
                            ndvi_mean = NA, .geo = NA)
dim(missing_df_wterminal)

#Añado los ids que faltan 
GEE_Query_wterminal_full <- rbind(GEE_Query_wterminal, missing_df_wterminal)

#Ordeno el df según los ids (en orden ascendente)
GEE_Query_wterminal_full <- GEE_Query_wterminal_full[order(GEE_Query_wterminal_full$ID), ]

#Features de NDVI para la ventana terminal
results_ndvi_wterminal <- do.call(rbind,
                                  lapply(split(GEE_Query_wterminal_full, GEE_Query_wterminal_full$ID), function(x) {
                                    data.frame(
                                      location_id = x$ID[1],
                                      
                                      ndvi_mean_wterminal = ifelse(all(is.na(x$ndvi_mean)), NA, mean(x$ndvi_mean, na.rm = TRUE)),
                                      
                                      ndvi_max_wterminal  = ifelse(all(is.na(x$ndvi_mean)), NA, max(x$ndvi_mean, na.rm = TRUE)),
                                      
                                      ndvi_sum_wterminal  = ifelse(all(is.na(x$ndvi_mean)), NA, sum(x$ndvi_mean, na.rm = TRUE)),
                                      
                                      ndvi_n_obs_wterminal = sum(!is.na(x$ndvi_mean)))}))


View(results_ndvi_wterminal)


#_______________________________________________________________________________

#VENTANA COMPLETA: de Planting_D a Harvesting

file.choose()
GEE_Query_wcomplete<-read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\TFG_CTA_Zimbabwe\\Obtención_datos_CTA_Zimbabwe\\input\\NDVI\\Consultas_Google_Earth_Engine\\Resultados_Consultas_Google_Earth_Engine\\Ventana_completa\\NDVI_full.csv" , sep=",", dec=".")
View(GEE_Query_wcomplete)


#IDs QUE FALTAN 
missing_ids_wcomplete <- setdiff(all_ids, unique(GEE_Query_wcomplete$ID))
length(missing_ids_wcomplete)

missing_df_wcomplete <- data.frame(system.index = NA, ID = missing_ids_wcomplete, date = NA,
                            ndvi_mean = NA, .geo = NA)
dim(missing_df_wcomplete)

#Añado los ids que faltan 
GEE_Query_wcomplete_full <- rbind(GEE_Query_wcomplete, missing_df_wcomplete)

#Ordeno el df según los ids (en orden ascendente)
GEE_Query_wcomplete_full <- GEE_Query_wcomplete_full[order(GEE_Query_wcomplete_full$ID), ]


#Features de NDVI para la ventana completa
results_ndvi_wcomplete <- do.call(rbind,
                                  lapply(split(GEE_Query_wcomplete_full, GEE_Query_wcomplete_full$ID), function(x) {
                                    data.frame(
                                      location_id = x$ID[1],
                                      
                                      ndvi_mean_wcomplete = ifelse(all(is.na(x$ndvi_mean)), NA, mean(x$ndvi_mean, na.rm = TRUE)),
                                      
                                      ndvi_max_wcomplete  = ifelse(all(is.na(x$ndvi_mean)), NA, max(x$ndvi_mean, na.rm = TRUE)),
                                      
                                      ndvi_sum_wcomplete  = ifelse(all(is.na(x$ndvi_mean)), NA, sum(x$ndvi_mean, na.rm = TRUE)),
                                      
                                      ndvi_n_obs_wcomplete = sum(!is.na(x$ndvi_mean)))}))

View(results_ndvi_wcomplete)



#_______________________________________________________________________________
#JOIN DE LOS RESULTADOS DE FEATURES DE NDVI PARA CADA VENTANA A TRAVÉS DE LA 
#VARIABLE location$id

results_ndvi_all <- merge(results_ndvi_w1, results_ndvi_w2, by = "location_id", all = TRUE)
results_ndvi_all <- merge(results_ndvi_all, results_ndvi_w3, by = "location_id", all = TRUE)
results_ndvi_all <- merge(results_ndvi_all, results_ndvi_w4, by = "location_id", all = TRUE)
results_ndvi_all <- merge(results_ndvi_all, results_ndvi_wterminal, by = "location_id", all = TRUE)
results_ndvi_all <- merge(results_ndvi_all, results_ndvi_wcomplete, by = "location_id", all = TRUE)


#Guardar en .csv
write.csv(results_ndvi_all,
  "C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/output/Features_NDVI/Resultados_NDVI.csv",
  row.names = FALSE)





          


