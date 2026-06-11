

#Paquetes necesarios 

install.packages("readr")
library(readr)

install.packages("glmnet")
library(glmnet)

install.packages("dplyr")
library(dplyr)

install.packages("car")
library(car)

install.packages("rlang")
install.packages("interactions")   
library(interactions)

install.packages("mgcv")
library(mgcv)

install.packages("spdep")
library(spdep)



#1.SELECCIÓN DE VARIABLES CON ELASTIC NET 
# Respuesta: log(Yield_Mt_H + 1)

#Carga de la base de datos
df_final <- read_csv("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/base_de_datos_final/final_dataset/with_all_crops/df_completed.csv")
View(df_final)


#Eliminación de variables que no se van a utilizar de nuevo en el análisis
df_final<-df_final%>%select(-fid,-Adm0,-Admin1, -Crop_Varie,-Wet_Weight,-Dry_weight,
                            -Type,-Other_crop, -f_Planting_D, -f_Harvesting, -f_t1, 
                            -f_t2, -f_t3, -f_t_terminal,-Y_4326, -X_4326, - Crop_Condi, 
                            -ID)




#Convertir variables cualitativas a factores
df_final$administrative_regions <- as.factor(df_final$administrative_regions)
df_final$Field_Irri <- as.factor(df_final$Field_Irri)
df_final$flood_prone_zones <- as.factor(df_final$flood_prone_zones)
df_final$Crop_Type <- as.factor(df_final$Crop_Type)

table(df_final$Crop_Type, df_final$flood_prone_zones)
table(df_final$Crop_Type, df_final$Field_Irri)
table(df_final$Crop_Type, df_final$Intercropp)





#Crear variable respuesta transformada. Respuesta: log(Yield_Mt_H + 1)
df_final$Yield_log <- log(df_final$Yield_Mt_H + 1)




#Definir fórmula del modelo. 
#Usamos todas las variables, excepto Yield_Mt_H original
form_elastic <- Yield_log ~ . - Yield_Mt_H




#Crear model frame y matriz de diseño
#model.matrix convierte automáticamente factores en dummies

mf <- model.frame(form_elastic, data = df_final)

x <- model.matrix(form_elastic, data = mf)[, -1]
y <- model.response(mf)




#Crear folds fijos para validación cruzada reproducible
set.seed(123)
foldid <- sample(rep(1:10, length.out = length(y)))




#Paso 1: Buscar el mejor alpha
#Usamos secuencias de alphas de 0.05
alphas <- seq(0.05, 0.95, by = 0.05)

cv_list <- lapply(alphas, function(a) {
  cv.glmnet(
    x = x,
    y = y,
    alpha = a,
    family = "gaussian",
    foldid = foldid,
    standardize = TRUE)})




#Paso 2: Elegir alpha óptimo
# Para cada alpha, tomamos el menor error CV posible
cv_errors <- sapply(cv_list, function(m) min(m$cvm))

results_alpha <- data.frame(alpha = alphas, cv_error = cv_errors)
print(results_alpha)

best_index <- which.min(cv_errors)
best_alpha <- alphas[best_index]

cat("Mejor alpha:", best_alpha, "\n")
cat("Error CV mínimo:", min(cv_errors), "\n")




#Paso 3: Reentrenar modelo usando el mejor alpha
final_model <- cv.glmnet( x = x, y = y, alpha = best_alpha, family = "gaussian",
                          foldid = foldid, standardize = TRUE)


#Paso 4: Elegir lambda
# lambda.min = menor error CV
# lambda.1se = modelo más simple dentro de 1 error estándar
# Usaremos lambda.1se ya que es un modelo más parsimonioso


cat("Lambda mínimo:", final_model$lambda.min, "\n")
cat("Lambda 1SE:", final_model$lambda.1se, "\n")




#Variables seleccionadas con lambda.1se
# Más conservador, suele seleccionar menos variables
coef_1se <- coef(final_model, s = "lambda.1se")

selected_1se <- rownames(coef_1se)[coef_1se[, 1] != 0]
selected_1se <- selected_1se[selected_1se != "(Intercept)"]

print(selected_1se)

cat("Número de variables seleccionadas con lambda.1se:",
    length(selected_1se), "\n")






#Ver coeficientes distintos de cero
coeficientes_1se <- coef_1se[coef_1se[, 1] != 0, , drop = FALSE]
coeficientes_min <- coef_min[coef_min[, 1] != 0, , drop = FALSE]

coeficientes_1se
coeficientes_min


#Gráficos 
# Error CV según alpha
plot(alphas, cv_errors, type = "b", xlab = "Alpha", 
     ylab = "Error de validación cruzada")

# Curva de validación cruzada para lambda
plot(final_model)


#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

#CORRELACIONES ENTRE VARIABLES SELECCIONADAS
#Metodología: agrupamos variables en bloques.
#De las altamente correlacionadas se eligen variables representativas
#Las no correlacionadas (o "independientes") entran todas


# BLOQUES DE VARIABLES


bloque_suelo <- c("phh2o_0_30cm","soc_30_100cm","N_30_100cm", "N_100_200cm",
                  "cec_30_100cm", "cec_100_200cm","bdod_0_30cm", "bdod_100_200cm",
                  "cfvo_30_100cm", "cfvo_100_200cm","clay_0_30cm", "clay_30_100cm",
                  "sand_30_100cm","silt_30_100cm","wv0010_0_30cm", "wv0010_30_100cm", 
                  "wv0010_100_200cm","wv003_30_100cm", "wv003_100_200cm","wv1500_0_30cm", "wv1500_100_200cm")



bloque_termico <- c("temp_mean_w1", "dtr_mean_w1", "heat_excess35_w1", "soil_temp_mean_w1",
  "temp_mean_w2", "dtr_mean_w2", "n_hot35_w2", "heat_excess35_w2", "soil_temp_mean_w2",
  "temp_mean_w3", "n_hot35_w3", "heat_excess35_w3", "cold_deficit10_w3", "soil_temp_mean_w3",
  "temp_mean_w4", "n_hot35_w4", "heat_excess35_w4", "cold_deficit10_w4",
  "n_cold10_wterminal", "cold_deficit10_wterminal")



bloque_hidrico <- c("rain_total_w1", "precipitation_total_w1", "precip_hours_w1",
                    "n_rain1_w1", "n_rain10_w1", "dryspell_w1",
                    "sm_mean_w1", "cloud_mean_w1", "n_et0_high_w1",
                    "rh_mean_w1", "rh_min_w1", "n_rh_low_w1", "water_balance_w1",
                    
                    "rain_total_w2", "precipitation_total_w2",
                    "n_rain10_w2", "max_rain1d_w2","sm_min_w2", "rad_total_w2", 
                    "n_et0_high_w2","n_rh_low_w2", "vpd_max_w2", "water_balance_w2",
                    
                    "rain_total_w3", "precipitation_total_w3", "precip_hours_w3",
                    "n_rain10_w3", "dryspell_w3","sm_mean_w3", "sm_min_w3", "cloud_mean_w3",
                    "rh_mean_w3", "rh_min_w3", "water_balance_w3",
                                      
                    "rain_total_w4", "precipitation_total_w4",
                    "n_rain10_w4", "max_rain1d_w4", "dryspell_w4",
                    "sm_mean_w4", "sm_min_w4", "rad_total_w4",
                    "cloud_mean_w4", "rh_mean_w4", "rh_min_w4","n_rh_low_w4",
                    "precipitation_total_wcomplete","sm_min_wterminal")



bloque_viento <- c( "wind_mean_w1", "wind_gust_mean_w1", "wind_max_w1", "n_wind_gust30_w1",
  "wind_gust_mean_w2", "wind_max_w2", "wind_gust_max_w2", "n_wind_gust30_w2",
  "wind_mean_w3", "wind_gust_mean_w3", "wind_max_w3", "wind_gust_max_w3", "n_wind_gust30_w3",
  "wind_gust_mean_w4", "wind_max_w4", "wind_gust_max_w4",
  "wind_gust_max_wterminal", "n_wind_gust30_wterminal")

#_______________________________________________________________________________

#VARIBALES INDEPENDIENTES (sin correlación fuerte con otras)

#Función que extrae las no correlacionadas
variables_no_correlacionadas <- function(df, vars, umbral = 0.7) {
  
  vars <- vars[vars %in% names(df)]
  
  cor_mat <- cor(df[, vars], use = "pairwise.complete.obs",method = "pearson")
  
  # Quitamos diagonal
  diag(cor_mat) <- 0
  
  # Máxima correlación absoluta de cada variable
  max_cor <- apply(abs(cor_mat), 1, max)
  
  # Variables sin correlaciones altas
  vars_indep <- names(max_cor[max_cor < umbral])
  
  return(data.frame( variable = names(max_cor), max_correlacion = max_cor,
                     independiente = max_cor < umbral))}



#Aplicamos la función. #Estas variables entran directamente al modelo
indep_suelo <- variables_no_correlacionadas(df_final, bloque_suelo, 0.7)
indep_termico <- variables_no_correlacionadas(df_final, bloque_termico, 0.7)
indep_hidrico <- variables_no_correlacionadas(df_final, bloque_hidrico, 0.7)
indep_viento <- variables_no_correlacionadas(df_final, bloque_viento, 0.7)

indep_suelo
subset(indep_suelo, independiente == TRUE)
#soc_30_100cm, wv1500_0_30cm, wv1500_100_200cm

indep_termico
subset(indep_termico, independiente == TRUE)
#dtr_mean_w1, heat_excess35_w1, dtr_mean_w2, temp_mean_w4

indep_hidrico
subset(indep_hidrico, independiente == TRUE)
#max_rain1d_w2, rad_total_w2, rad_total_w4

indep_viento
subset(indep_viento, independiente == TRUE)
#wind_max_w1, n_wind_gust30_w1, wind_gust_mean_w2, wind_gust_max_wterminal 



#_______________________________________________________________________________

# FUNCIÓN PARA CORRELACIONES ALTAS
correlaciones_altas <- function(df, vars, umbral = 0.7) {
  
  vars <- vars[vars %in% names(df)]
  
  datos <- df[, vars]
  
  cor_mat <- cor(datos, use = "pairwise.complete.obs", method = "pearson")
  
  cor_pairs <- as.data.frame(as.table(cor_mat))
  
  cor_pairs <- cor_pairs[cor_pairs$Var1 != cor_pairs$Var2, ]
  
  cor_pairs <- cor_pairs[!duplicated(t(apply(cor_pairs[, 1:2], 1, sort))),]
  
  cor_pairs_high <- cor_pairs[abs(cor_pairs$Freq) >= umbral, ]
  
  cor_pairs_high <- cor_pairs_high[order(-abs(cor_pairs_high$Freq)),]
  
  return(cor_pairs_high)}




# APLICAR POR BLOQUE
cor_suelo <- correlaciones_altas(df_final, bloque_suelo, 0.7)
cor_termico <- correlaciones_altas(df_final, bloque_termico, 0.7)
cor_hidrico <- correlaciones_altas(df_final, bloque_hidrico, 0.7)
cor_viento <- correlaciones_altas(df_final, bloque_viento, 0.7)

cor_suelo
#Seleccionamos como representantes: cec_30_100cm, bdod_0_30cm, cfvo_30_100cm,clay_0_30cm,
#wv0010_0_30cm, N_30_100cm

cor_termico
#Seleccionamos como representantes: temp_mean_w3,,n_hot35_w3,n_cold10_wterminal

cor_hidrico
#Seleccionamos como representantes: rain_total_w3, sm_mean_w3, rh_mean_w4, dryspell_w3, vpd_max_w2

cor_viento
#Seleccionamos como representantes: wind_gust_mean_w4, n_wind_gust30_wterminal

#_______________________________________________________________________________

#Juntando las variables no correlacionadas con otras ("independientes"), y las 
#variables representativas por bloque nos quedaría esto. Estas son las variables 
#que siguen el proceso de selección.





# Variables finales tras selección por bloques


vars_seleccion <- c(
  # BLOQUE GENERAL
  "Y_6933",
  "X_6933",
  "DEM",
  "Slope",
  "prox_towns_km",
  "prox_roads_km",
  "prox_water_lines_km",
  "prox_water_areas_km",
  "gdd_total_wcomplete",
  "ndvi_mean_wcomplete",
  "ndvi_max_wcomplete",
  "ndvi_sum_wcomplete",
  
  # BLOQUE SUELO
  "soc_30_100cm",
  "wv1500_0_30cm",
  "wv1500_100_200cm",
  "cec_30_100cm",
  "bdod_0_30cm",
  "cfvo_30_100cm",
  "clay_0_30cm",
  "wv0010_0_30cm",
  "N_30_100cm",
  
  # BLOQUE TÉRMICO
  "dtr_mean_w1",
  "heat_excess35_w1",
  "dtr_mean_w2",
  "temp_mean_w4",
  "temp_mean_w3",
  "n_hot35_w3",
  "n_cold10_wterminal",
  
  # BLOQUE HÍDRICO
  "max_rain1d_w2",
  "rad_total_w2",
  "rad_total_w4",
  "rain_total_w3",
  "sm_mean_w3",
  "rh_mean_w4",
  "dryspell_w3",
  "vpd_max_w2",
  
  # BLOQUE VIENTO
  "wind_max_w1",
  "n_wind_gust30_w1",
  "wind_gust_mean_w2",
  "wind_gust_max_wterminal",
  "wind_gust_mean_w4",
  "n_wind_gust30_wterminal")

# Comprobar que todas existen en df_final
vars_seleccion[!vars_seleccion %in% names(df_final)]


# Matriz de correlación global
datos_cor <- df_final[, vars_seleccion]

cor_mat_final <- cor( datos_cor, use = "pairwise.complete.obs",method = "pearson")

# Extraer pares con |r| >= 0.7
cor_pairs_final <- as.data.frame(as.table(cor_mat_final))

# Quitar diagonal
cor_pairs_final <- cor_pairs_final[cor_pairs_final$Var1 != cor_pairs_final$Var2, ]

# Quitar duplicados
cor_pairs_final <- cor_pairs_final[ !duplicated(t(apply(cor_pairs_final[, 1:2], 1, sort))), ]

# Filtrar correlaciones altas
cor_pairs_high_final <- cor_pairs_final[ abs(cor_pairs_final$Freq) >= 0.7, ]

# Ordenar de mayor a menor correlación absoluta
cor_pairs_high_final <- cor_pairs_high_final[order(-abs(cor_pairs_high_final$Freq)), ]
cor_pairs_high_final


#Elegimos: dryspell_w3, temp_mean_w3, wind_gust_mean_w4, rain_total_w3, ndvi_mean_complete

# Variables sin correlación >= 0.7 con ninguna otra
cor_abs <- abs(cor_mat_final)
diag(cor_abs) <- 0

max_cor_por_variable <- apply(cor_abs, 1, max)

vars_sin_cor_alta <- data.frame(variable = names(max_cor_por_variable), 
                                max_correlacion = max_cor_por_variable,
                                sin_correlacion_alta = max_cor_por_variable < 0.7)

vars_sin_cor_alta <- vars_sin_cor_alta[order(-vars_sin_cor_alta$max_correlacion),]
vars_sin_cor_alta

# Solo variables sin correlación alta
subset(vars_sin_cor_alta, sin_correlacion_alta == TRUE)


#_______________________________________________________________________________
#Las variables selccionadas finalmente son: 
# dryspell_w3, temp_mean_w3, wind_gust_mean_w4, rain_total_w3, ndvi_mean_complete
# Y_6933, vpd_max_w2, cec_30_100cm, wind_gust_mean_w2, temp_mean_w4, n_cold10_wterminal
# rad_total_w4, n_wind_gust30_w1, dtr_mean_w1, dtr_mean_w2, rad_total_w2, sm_mean_w3
# max_rain1d_w2, wv1500_0_30cm, clay_0_30cm, N_30_100cm, bdod_0_30cm
# wv0010_0_30cm, prox_water_areas_km, ndvi_sum_wcomplete, wv1500_100_200cm, Slope
# gdd_total_wcomplete, soc_30_100cm, X_6933, cfvo_30_100cm, prox_towns_km
# wind_max_w1, n_hot35_w3, heat_excess35_w1, prox_water_lines_km, prox_roads_km




#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

#2.OBTENCIÓN DE UN MODELO INTERPRETABLE (MODELO ECOLÓGICO)
#X_6933 y Y_6933 no se incluyen porque no representan ningún mecanismo agroecológico 
#directo. Al igual que prox_roads_km y prox_towns_km que son proxys socioespaciales.
#ndvi_sum_wcomplete tampoco se mantiene ya que su interpretación puede ser engañosa

#2.1.MODELO LINEAL SIN INTERACCIONES 

#Carga de la base de datos
df_final <- read_csv("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/base_de_datos_final/final_dataset/with_all_crops/df_completed.csv")
View(df_final)


#Eliminación de variables que no se van a utilizar de nuevo en el análisis
df_final<-df_final%>%select(-fid,-Adm0,-Admin1, -Crop_Varie,-Wet_Weight,-Dry_weight,
                            -Type,-Other_crop, -f_Planting_D, -f_Harvesting, -f_t1, 
                            -f_t2, -f_t3, -f_t_terminal,-Y_4326, -X_4326, - Crop_Condi, 
                            -ID)


#Convertir variables cualitativas a factores
df_final$administrative_regions <- as.factor(df_final$administrative_regions)
df_final$Field_Irri <- as.factor(df_final$Field_Irri)
df_final$flood_prone_zones <- as.factor(df_final$flood_prone_zones)
df_final$Crop_Type <- as.factor(df_final$Crop_Type)


#Crear variable respuesta transformada. Respuesta: log(Yield_Mt_H + 1)
df_final$Yield_log <- log(df_final$Yield_Mt_H + 1)


#Modelo base (solo con el intercepto, sirve para ver mejora al incluir covariables)
modelo_base <- lm(Yield_log ~ 1, data= df_final)
summary(modelo_lm_base)



#modelo_lm_v1. Se hace un modelo tras el proceso de selección de variables

modelo_lm_v1 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                     Slope + prox_water_lines_km + prox_water_areas_km +
                     ndvi_mean_wcomplete +
                     
                     cec_30_100cm + N_30_100cm + soc_30_100cm +
                     
                     clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                     wv1500_100_200cm +
                     
                     temp_mean_w3 + temp_mean_w4 + dtr_mean_w1 + dtr_mean_w2 + 
                     n_hot35_w3 + heat_excess35_w1 +n_cold10_wterminal + gdd_total_wcomplete + 
                     
                     rain_total_w3 + max_rain1d_w2 + dryspell_w3 + sm_mean_w3 +
                     vpd_max_w2 + rad_total_w2 + rad_total_w4 +
                     
                     wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 +n_wind_gust30_w1 
                   
                   ,data = df_final)


vif(modelo_lm_v1)
summary(modelo_lm_v1)
AIC(modelo_lm_v1)

#Lo primero en mirar será el VIF, luego significancia, luego AIC y Radj.


#modelo_lm_v2. Según VIF, quitamos temp_mean_w4
modelo_lm_v2 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                     Slope + prox_water_lines_km + prox_water_areas_km +
                     ndvi_mean_wcomplete +
                     
                     cec_30_100cm + N_30_100cm + soc_30_100cm +
                     
                     clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                     wv1500_100_200cm +
                     
                     temp_mean_w3 + dtr_mean_w1 + dtr_mean_w2 + 
                     n_hot35_w3 + heat_excess35_w1 +n_cold10_wterminal + gdd_total_wcomplete + 
                     
                     rain_total_w3 + max_rain1d_w2 + dryspell_w3 + sm_mean_w3 +
                     vpd_max_w2 + rad_total_w2 + rad_total_w4 +
                     
                     wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 +n_wind_gust30_w1 
                   
                   ,data = df_final)


vif(modelo_lm_v2)
summary(modelo_lm_v2)
AIC(modelo_lm_v2)


#modelo_lm_v3. Según VIF, quitamos dtr_mean_w2
modelo_lm_v3 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                     Slope + prox_water_lines_km + prox_water_areas_km +
                     ndvi_mean_wcomplete +
                     
                     cec_30_100cm + N_30_100cm + soc_30_100cm +
                     
                     clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                     wv1500_100_200cm +
                     
                     temp_mean_w3 + dtr_mean_w1 + 
                     n_hot35_w3 + heat_excess35_w1 +n_cold10_wterminal + gdd_total_wcomplete + 
                     
                     rain_total_w3 + max_rain1d_w2 + dryspell_w3 + sm_mean_w3 +
                     vpd_max_w2 + rad_total_w2 + rad_total_w4 +
                     
                     wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 +n_wind_gust30_w1 
                   
                   ,data = df_final)



vif(modelo_lm_v3)
summary(modelo_lm_v3)
AIC(modelo_lm_v3)

#Ya no hay multicolinealidad, ahora los criterios serán significancia, AIC y estabilidad

#modelo_lm_v4. Quitamos gdd_total_wcomplete 
modelo_lm_v4 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                     Slope + prox_water_lines_km + prox_water_areas_km +
                     ndvi_mean_wcomplete +
                     
                     cec_30_100cm + N_30_100cm + soc_30_100cm +
                     
                     clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                     wv1500_100_200cm +
                     
                     temp_mean_w3 + dtr_mean_w1 + 
                     n_hot35_w3 + heat_excess35_w1 +n_cold10_wterminal + 
                     
                     rain_total_w3 + max_rain1d_w2 + dryspell_w3 + sm_mean_w3 +
                     vpd_max_w2 + rad_total_w2 + rad_total_w4 +
                     
                     wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 +n_wind_gust30_w1 
                   
                   ,data = df_final)

vif(modelo_lm_v4)
summary(modelo_lm_v4)
AIC(modelo_lm_v4)


#modelo_lm_v5. Quitamos heat_excess35_w1 
modelo_lm_v5 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                     Slope + prox_water_lines_km + prox_water_areas_km +
                     ndvi_mean_wcomplete +
                     
                     cec_30_100cm + N_30_100cm + soc_30_100cm +
                     
                     clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                     wv1500_100_200cm +
                     
                     temp_mean_w3 + dtr_mean_w1 + 
                     n_hot35_w3 + n_cold10_wterminal + 
                     
                     rain_total_w3 + max_rain1d_w2 + dryspell_w3 + sm_mean_w3 +
                     vpd_max_w2 + rad_total_w2 + rad_total_w4 +
                     
                     wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 +n_wind_gust30_w1 
                   
                   ,data = df_final)

vif(modelo_lm_v5)
summary(modelo_lm_v5)
AIC(modelo_lm_v5)


#modelo_lm_v6. Quitamos max_rain1d_w2 
modelo_lm_v6 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                     Slope + prox_water_lines_km + prox_water_areas_km +
                     ndvi_mean_wcomplete +
                     
                     cec_30_100cm + N_30_100cm + soc_30_100cm +
                     
                     clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                     wv1500_100_200cm +
                     
                     temp_mean_w3 + dtr_mean_w1 + 
                     n_hot35_w3 + n_cold10_wterminal + 
                     
                     rain_total_w3 + dryspell_w3 + sm_mean_w3 +
                     vpd_max_w2 + rad_total_w2 + rad_total_w4 +
                     
                     wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 +n_wind_gust30_w1 
                   
                   ,data = df_final)


vif(modelo_lm_v6)
summary(modelo_lm_v6)
AIC(modelo_lm_v6)


#modelo_lm_v7. Quitamos n_hot35_w3  
modelo_lm_v7 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                     Slope + prox_water_lines_km + prox_water_areas_km +
                     ndvi_mean_wcomplete +
                     
                     cec_30_100cm + N_30_100cm + soc_30_100cm +
                     
                     clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                     wv1500_100_200cm +
                     
                     temp_mean_w3 + dtr_mean_w1 + 
                     n_cold10_wterminal + 
                     
                     rain_total_w3 + dryspell_w3 + sm_mean_w3 +
                     vpd_max_w2 + rad_total_w2 + rad_total_w4 +
                     
                     wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 +n_wind_gust30_w1 
                   
                   ,data = df_final)


vif(modelo_lm_v7)
summary(modelo_lm_v7)
AIC(modelo_lm_v7)



#modelo_lm_v8. Quitamos dryspell_w3 
modelo_lm_v8 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                     Slope + prox_water_lines_km + prox_water_areas_km +
                     ndvi_mean_wcomplete +
                     
                     cec_30_100cm + N_30_100cm + soc_30_100cm +
                     
                     clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                     wv1500_100_200cm +
                     
                     temp_mean_w3 + dtr_mean_w1 + 
                     n_cold10_wterminal + 
                     
                     rain_total_w3 + sm_mean_w3 +
                     vpd_max_w2 + rad_total_w2 + rad_total_w4 +
                     
                     wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 +n_wind_gust30_w1 
                   
                   ,data = df_final)


vif(modelo_lm_v8)
summary(modelo_lm_v8)
AIC(modelo_lm_v8)


#modelo_lm_v9. Quitamos vpd_max_w2  
modelo_lm_v9 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                     Slope + prox_water_lines_km + prox_water_areas_km +
                     ndvi_mean_wcomplete +
                     
                     cec_30_100cm + N_30_100cm + soc_30_100cm +
                     
                     clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                     wv1500_100_200cm +
                     
                     temp_mean_w3 + dtr_mean_w1 + 
                     n_cold10_wterminal + 
                     
                     rain_total_w3 + sm_mean_w3 +
                     rad_total_w2 + rad_total_w4 +
                     
                     wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 +n_wind_gust30_w1 
                   
                   ,data = df_final)


vif(modelo_lm_v9)
summary(modelo_lm_v9)
AIC(modelo_lm_v9)



#modelo_lm_v10. Quitamos temp_mean_w3 
modelo_lm_v10 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                     Slope + prox_water_lines_km + prox_water_areas_km +
                     ndvi_mean_wcomplete +
                     
                     cec_30_100cm + N_30_100cm + soc_30_100cm +
                     
                     clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                     wv1500_100_200cm +
                     
                     dtr_mean_w1 + 
                     n_cold10_wterminal + 
                     
                     rain_total_w3 + sm_mean_w3 +
                     rad_total_w2 + rad_total_w4 +
                     
                     wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 + n_wind_gust30_w1 
                   
                   ,data = df_final)


vif(modelo_lm_v10)
summary(modelo_lm_v10)
AIC(modelo_lm_v10)


#modelo_lm_v11. Quitamos Slope 
modelo_lm_v11 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                      prox_water_lines_km + prox_water_areas_km +
                      ndvi_mean_wcomplete +
                      
                      cec_30_100cm + N_30_100cm + soc_30_100cm +
                      
                      clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                      wv1500_100_200cm +
                      
                      dtr_mean_w1 + 
                      n_cold10_wterminal + 
                      
                      rain_total_w3 + sm_mean_w3 +
                      rad_total_w2 + rad_total_w4 +
                      
                      wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 + n_wind_gust30_w1 
                    
                    ,data = df_final)


vif(modelo_lm_v11)
summary(modelo_lm_v11)
AIC(modelo_lm_v11)


#modelo_lm_v12. Quitamos soc_30_100cm 
modelo_lm_v12 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                      prox_water_lines_km + prox_water_areas_km +
                      ndvi_mean_wcomplete +
                      
                      cec_30_100cm + N_30_100cm + 
                      
                      clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                      wv1500_100_200cm +
                      
                      dtr_mean_w1 + 
                      n_cold10_wterminal + 
                      
                      rain_total_w3 + sm_mean_w3 +
                      rad_total_w2 + rad_total_w4 +
                      
                      wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 + n_wind_gust30_w1 
                    
                    ,data = df_final)


vif(modelo_lm_v12)
summary(modelo_lm_v12)
AIC(modelo_lm_v12)


#modelo_lm_v13. Quitamos dtr_mean_w1  
modelo_lm_v13 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                      prox_water_lines_km + prox_water_areas_km +
                      ndvi_mean_wcomplete +
                      
                      cec_30_100cm + N_30_100cm + 
                      
                      clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                      wv1500_100_200cm +
                      
                      n_cold10_wterminal +
                     
                      
                      rain_total_w3 + sm_mean_w3 +
                      rad_total_w2 + rad_total_w4 +
                      
                      wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 + n_wind_gust30_w1 
                    
                    ,data = df_final)


vif(modelo_lm_v13)
summary(modelo_lm_v13)
AIC(modelo_lm_v13)


#2.1.MODELO ECOLÓGICO LINEAL CON INTERACCIONES 

#Lista predefinida de interacciones con sentido físico y agronómico.
#rain_total_w3 : clay_0_30cm
table(cut(df_final$rain_total_w3, 5), cut(df_final$clay_0_30cm, 5))

# 2. rain_total_w3 : wv1500_0_30cm
table(cut(df_final$rain_total_w3, 5), cut(df_final$wv1500_0_30cm, 5))

# 3. sm_mean_w3 : rad_total_w4
table(cut(df_final$sm_mean_w3, 5), cut(df_final$rad_total_w4, 5))

# 4. sm_mean_w3 : wind_gust_mean_w4
table(cut(df_final$sm_mean_w3, 5),cut(df_final$wind_gust_mean_w4, 5))

# 5. sm_mean_w3 : n_cold10_wterminal
table(cut(df_final$sm_mean_w3, 5), cut(df_final$n_cold10_wterminal, 5))

# 6. clay_0_30cm : sm_mean_w3
table(cut(df_final$clay_0_30cm, 5),cut(df_final$sm_mean_w3, 5))

# 7. ndvi_mean_wcomplete : sm_mean_w3
table(cut(df_final$ndvi_mean_wcomplete, 5),cut(df_final$sm_mean_w3, 5))

# 8. rad_total_w4 : wind_gust_mean_w4
table(cut(df_final$rad_total_w4, 5),cut(df_final$wind_gust_mean_w4, 5))

# 9. N_30_100cm : sm_mean_w3
table(cut(df_final$N_30_100cm, 5),cut(df_final$sm_mean_w3, 5))

# 10. cec_30_100cm : sm_mean_w3
table(cut(df_final$cec_30_100cm, 5),cut(df_final$sm_mean_w3, 5))

#11. sm_mean_w3:Crop_Type
table(cut(df_final$sm_mean_w3, 5),df_final$Crop_Type)

#12. n_cold10_wterminal:Crop_Type
table(cut(df_final$n_cold10_wterminal, 5),df_final$Crop_Type)

#13. rad_total_w4:Crop_Type
table(cut(df_final$rad_total_w4, 5),df_final$Crop_Type)

#14. wind_gust_mean_w4:Crop_Type
table(cut(df_final$wind_gust_mean_w4, 5),df_final$Crop_Type)

#15.sm_mean_w3:N_30_100cm 
table(cut(df_final$N_30_100cm, 5),cut(df_final$sm_mean_w3, 5))

#16. wind_gust_mean_w4 : n_cold10_wterminal
table(cut(df_final$wind_gust_mean_w4, 5),cut(df_final$n_cold10_wterminal,5))

#17.rain_total_w3 : sm_mean_w3
table(cut(df_final$sm_mean_w3, 5),cut(df_final$rain_total_w3, 5))

#18.ndvi_mean_wcomplete:Crop_Type
table(cut(df_final$ndvi_mean_wcomplete, 5),df_final$Crop_Type)

#Tienen suficiente soporte las siguientes: 
#sm_mean_w3:rad_total_w4-->NO
#rain_total_w3:clay_0_30cm-->SI
#sm_mean_w3 : wind_gust_mean_w4-->NO
#rad_total_w4 : Crop_Type-->SI





#Además de la significancia se mirara la dirección de la interacción para
#verificar que es coherente desde una perspectiva agrónomica.


#modelo_lm_int_v1. A partir de modelo_lm_v13 probaremos interacciones. Probamos rain_total_w3:clay_0_30cm
modelo_lm_int_v1 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                      prox_water_lines_km + prox_water_areas_km +
                      ndvi_mean_wcomplete +
                      
                      cec_30_100cm + N_30_100cm + 
                      
                      clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                      wv1500_100_200cm +
                      
                      
                      n_cold10_wterminal + 
                      
                      rain_total_w3 + sm_mean_w3 +
                      rad_total_w2 + rad_total_w4 +
                      
                      wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 + n_wind_gust30_w1 +
                      rain_total_w3:clay_0_30cm
                    
                    ,data = df_final)


vif(modelo_lm_int_v1, type = "predictor")
summary(modelo_lm_int_v1)
AIC(modelo_lm_int_v1)

#Visualización del efecto marginal
interact_plot(modelo_lm_int_v1, pred = rain_total_w3,modx = clay_0_30cm)


#La interacción se mantiene


#modelo_lm_int_v2.Probamos rad_total_w4 : Crop_Type
modelo_lm_int_v2 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                         prox_water_lines_km + prox_water_areas_km +
                         ndvi_mean_wcomplete +
                         
                         cec_30_100cm + N_30_100cm + 
                         
                         clay_0_30cm + bdod_0_30cm + cfvo_30_100cm + wv0010_0_30cm + wv1500_0_30cm + 
                         wv1500_100_200cm +
                         
                         
                         n_cold10_wterminal + 
                         
                         rain_total_w3 + sm_mean_w3 +
                         rad_total_w2 + rad_total_w4 +
                         
                         wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 + n_wind_gust30_w1 +
                         rain_total_w3:clay_0_30cm +
                         rad_total_w4 : Crop_Type
                       
                       ,data = df_final)


vif(modelo_lm_int_v2, type = "predictor")
summary(modelo_lm_int_v2)
AIC(modelo_lm_int_v2)

#Visualización del efecto marginal
interact_plot(modelo_lm_int_v2, pred = rad_total_w4,modx = Crop_Type)


#La interacción se mantiene. 


#modelo_lm_int_v3.Quitamos cfvo_30_100cm
modelo_lm_int_v3 <- lm(Yield_log ~ Crop_Type + Field_Irri + Intercropp + flood_prone_zones +
                         prox_water_lines_km + prox_water_areas_km +
                         ndvi_mean_wcomplete +
                         
                         cec_30_100cm + N_30_100cm + 
                         
                         clay_0_30cm + bdod_0_30cm + wv0010_0_30cm + wv1500_0_30cm + 
                         wv1500_100_200cm +
                         
                         
                         n_cold10_wterminal + 
                         
                         rain_total_w3 + sm_mean_w3 +
                         rad_total_w2 + rad_total_w4 +
                         
                         wind_max_w1 + wind_gust_mean_w2 + wind_gust_mean_w4 + n_wind_gust30_w1 +
                         rain_total_w3:clay_0_30cm +
                         rad_total_w4 : Crop_Type
                       
                       ,data = df_final)


vif(modelo_lm_int_v3, type = "predictor")
summary(modelo_lm_int_v3)
AIC(modelo_lm_int_v3)



#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

#3.OBTENCIÓN DE UN MODELO PREDICTIVO 
#Se parte del conjunto de variables seleccionado y depurado en el apartado 1. 
#Además, también se incluyen: DEM, X_6933, Y_6933, prox_towns_kmy prox_roads_km
#administrative_regions no se incluye porque haría que 
#el modelo dependiese de categorías discretas no extrapolables


#modelo_bam_pred_v1. Ajustamos con bam() para exploración inicial, bam() está diseñado para Big Data.
#El modelo final se ajustará con gam y se comprobará que no varíe drásticamente.
modelo_bam_pred_v1 <- bam(Yield_log ~ Crop_Type + Field_Irri + Intercropp + 
                            flood_prone_zones + s(prox_towns_km) + s(prox_roads_km) +
                            s(prox_water_lines_km) + s(prox_water_areas_km) + s(Slope) + 
                            s(ndvi_mean_wcomplete) + s(ndvi_sum_wcomplete) +
                            
                            s(cec_30_100cm) + s(N_30_100cm) + s(soc_30_100cm) +
                            
                            s(clay_0_30cm) + s(bdod_0_30cm) + s(cfvo_30_100cm) + 
                            s(wv0010_0_30cm) + s(wv1500_0_30cm) + s(wv1500_100_200cm) +
                            
                            s(temp_mean_w3) + s(temp_mean_w4) + s(dtr_mean_w1) +
                            s(dtr_mean_w2) + n_hot35_w3 + s(heat_excess35_w1) +
                            n_cold10_wterminal + s(gdd_total_wcomplete) + 
                            
                            s(rain_total_w3) + s(max_rain1d_w2) + s(dryspell_w3) +
                            s(sm_mean_w3) + s(vpd_max_w2) + s(rad_total_w2) + 
                            s(rad_total_w4) + 
                            
                            s(wind_max_w1) + s(wind_gust_mean_w2) + s(wind_gust_mean_w4) +
                            s(n_wind_gust30_w1) +
                            
                            te(X_6933, Y_6933, DEM, k = c(10, 10, 5)),
                          
                          data = df_final, method = "fREML", select = TRUE)


summary(modelo_bam_pred_v1)
gam.check(modelo_bam_pred_v1)
AIC(modelo_bam_pred_v1)
concurvity(modelo_bam_pred_v1)


length(unique(df_final$n_hot35_w3))
length(unique(df_final$n_cold10_wterminal))

#modelo_bam_pred_v2. Quitamos los términos edf ≈ 0 Estos son: prox_towns_km, soc_30_100cm
#cfvo_30_100cm, dtr_mean_w1, heat_excess35_w1, gdd_total_wcomplete, rad_total_w4,
#wind_max_w1, wind_gust_mean_w2, y el párametro n_cold10_wterminal.

modelo_bam_pred_v2 <- bam(Yield_log ~ Crop_Type + Field_Irri + Intercropp + 
                            flood_prone_zones + s(prox_roads_km) +
                            s(prox_water_lines_km) + s(prox_water_areas_km) + s(Slope) + 
                            s(ndvi_mean_wcomplete) + s(ndvi_sum_wcomplete) +
                            
                            s(cec_30_100cm) + s(N_30_100cm) + 
                            
                            s(clay_0_30cm) + s(bdod_0_30cm) + 
                            s(wv0010_0_30cm) + s(wv1500_0_30cm) + s(wv1500_100_200cm) +
                            
                            s(temp_mean_w3) + s(temp_mean_w4) + 
                            s(dtr_mean_w2) + n_hot35_w3 + 
                            
                            
                            s(rain_total_w3) + s(max_rain1d_w2) + s(dryspell_w3) +
                            s(sm_mean_w3) + s(vpd_max_w2) + s(rad_total_w2) + 
                            
                            
                            s(wind_gust_mean_w4) +
                            s(n_wind_gust30_w1) +
                            
                            te(X_6933, Y_6933, DEM, k = c(10, 10, 5)),
                          
                          data = df_final, method = "fREML", select = TRUE)


summary(modelo_bam_pred_v2)
gam.check(modelo_bam_pred_v2)
AIC(modelo_bam_pred_v2)
concurvity(modelo_bam_pred_v2)



#modelo_bam_pred_v3. Quitamos los términos edf ≈ 0. Estos son: N_30_100cm, ndvi_mean_wcomplete,
#wind_gust_mean_w4

modelo_bam_pred_v3 <- bam(Yield_log ~ Crop_Type + Field_Irri + Intercropp + 
                            flood_prone_zones + s(prox_roads_km) +
                            s(prox_water_lines_km) + s(prox_water_areas_km) + s(Slope) + 
                            s(ndvi_sum_wcomplete) +
                            
                            s(cec_30_100cm) +  
                            
                            s(clay_0_30cm) + s(bdod_0_30cm) + 
                            s(wv0010_0_30cm) + s(wv1500_0_30cm) + s(wv1500_100_200cm) +
                            
                            s(temp_mean_w3) + s(temp_mean_w4) + 
                            s(dtr_mean_w2) + n_hot35_w3 + 
                            
                            
                            s(rain_total_w3) + s(max_rain1d_w2) + s(dryspell_w3) +
                            s(sm_mean_w3) + s(vpd_max_w2) + s(rad_total_w2) + 
                            
                            
                          
                            s(n_wind_gust30_w1) +
                            
                            te(X_6933, Y_6933, DEM, k = c(10, 10, 5)),
                          
                          data = df_final, method = "fREML", select = TRUE)


summary(modelo_bam_pred_v3)
gam.check(modelo_bam_pred_v3)
AIC(modelo_bam_pred_v3)
concurvity(modelo_bam_pred_v3)



#modelo_bam_pred_v4. Quitamos los términos edf ≈ 0. Estos son: wv0010_0_30cm, wv1500_100_200cm
modelo_bam_pred_v4 <- bam(Yield_log ~ Crop_Type + Field_Irri + Intercropp + 
                            flood_prone_zones + s(prox_roads_km) +
                            s(prox_water_lines_km) + s(prox_water_areas_km) + s(Slope) + 
                            s(ndvi_sum_wcomplete) +
                            
                            s(cec_30_100cm) +  
                            
                            s(clay_0_30cm) + s(bdod_0_30cm) + 
                            s(wv1500_0_30cm) + 
                            
                            s(temp_mean_w3) + s(temp_mean_w4) + 
                            s(dtr_mean_w2) + n_hot35_w3 + 
                            
                            
                            s(rain_total_w3) + s(max_rain1d_w2) + s(dryspell_w3) +
                            s(sm_mean_w3) + s(vpd_max_w2) + s(rad_total_w2) + 
                            
                            
                            
                            s(n_wind_gust30_w1) +
                            
                            te(X_6933, Y_6933, DEM, k = c(10, 10, 5)),
                          
                          data = df_final, method = "fREML", select = TRUE)


summary(modelo_bam_pred_v4)
gam.check(modelo_bam_pred_v4)
AIC(modelo_bam_pred_v4)
concurvity(modelo_bam_pred_v4)


#modelo_bam_pred_v5. Quitamos los términos edf ≈ 0. Estos son: s(wv1500_0_30cm)
modelo_bam_pred_v5 <- bam(Yield_log ~ Crop_Type + Field_Irri + Intercropp + 
                            flood_prone_zones + s(prox_roads_km) +
                            s(prox_water_lines_km) + s(prox_water_areas_km) + s(Slope) + 
                            s(ndvi_sum_wcomplete) +
                            
                            s(cec_30_100cm) +  
                            
                            s(clay_0_30cm) + s(bdod_0_30cm) + 
                           
                            
                            s(temp_mean_w3) + s(temp_mean_w4) + 
                            s(dtr_mean_w2) + n_hot35_w3 + 
                            
                            
                            s(rain_total_w3) + s(max_rain1d_w2) + s(dryspell_w3) +
                            s(sm_mean_w3) + s(vpd_max_w2) + s(rad_total_w2) + 
                            
                            
                            
                            s(n_wind_gust30_w1) +
                            
                            te(X_6933, Y_6933, DEM, k = c(10, 10, 5)),
                          
                          data = df_final, method = "fREML", select = TRUE)

summary(modelo_bam_pred_v5)
gam.check(modelo_bam_pred_v5)
AIC(modelo_bam_pred_v5)
concurvity(modelo_bam_pred_v5)

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

#3.DIAGNOSIS DE LOS MODELOS 


#_______________________________________________________________________________
#3.1.EVALUACIÓN IN-SAMPLE

M1 <- modelo_base
M2 <- modelo_lm_v13
M3 <- modelo_lm_int_v3
M4 <- modelo_bam_pred_v5


# Observado en escala original (ton/ha)
obs <- df_final$Yield_Mt_H


#Predicciones 
#M1
pred_M1_log <- predict(M1, newdata = df_final)
pred_M1 <- exp(pred_M1_log) - 1

#M2 
pred_M2_log <- predict(M2, newdata = df_final)
pred_M2 <- exp(pred_M2_log) - 1

#M3
pred_M3_log <- predict(M3, newdata = df_final)
pred_M3 <- exp(pred_M3_log) - 1

#M4 
pred_M4_log <- predict(M4, newdata = df_final)
pred_M4 <- exp(pred_M4_log) - 1



#AIC
AIC_M1 <- AIC(M1)
AIC_M2 <- AIC(M2)
AIC_M3 <- AIC(M3)
AIC_M4 <- AIC(M4)

#BIC
BIC_M1 <- BIC(M1)
BIC_M2 <- BIC(M2)
BIC_M3 <- BIC(M3)
BIC_M4 <- BIC(M4)


#R-squared adjusted (R² adjusted)
AdjR2_M1 <- summary(M1)$adj.r.squared
AdjR2_M2 <- summary(M2)$adj.r.squared
AdjR2_M3 <- summary(M3)$adj.r.squared
AdjR2_M4 <- summary(M4)$r.sq


#RMSE (Root mean square error)
RMSE_M1 <- sqrt(mean((obs - pred_M1)^2))
RMSE_M2 <- sqrt(mean((obs - pred_M2)^2))
RMSE_M3 <- sqrt(mean((obs - pred_M3)^2))
RMSE_M4 <- sqrt(mean((obs - pred_M4)^2))


#MAE (Mean absolute Error)
MAE_M1 <- mean(abs(obs - pred_M1))
MAE_M2 <- mean(abs(obs - pred_M2))
MAE_M3 <- mean(abs(obs - pred_M3))
MAE_M4 <- mean(abs(obs - pred_M4))



# Resumen de la evaluación in-sample
resultados_insample <- data.frame(Modelo = c("M1_modelo_base", "M2_modelo_lm_v13",
                                             "M3_modelo_lm_int_v3", "M4_modelo_bam_pred_v5"),
  
                                  AIC = c(AIC_M1, AIC_M2, AIC_M3, AIC_M4),
                                  BIC = c(BIC_M1, BIC_M2, BIC_M3, BIC_M4),
                                  Adj_R2 = c(AdjR2_M1, AdjR2_M2, AdjR2_M3, AdjR2_M4),
                                  RMSE = c(RMSE_M1, RMSE_M2, RMSE_M3, RMSE_M4),
                                  MAE = c(MAE_M1, MAE_M2, MAE_M3, MAE_M4))


round(resultados_insample[, 2:6], 3)


#_______________________________________________________________________________
#3.2.DIAGNOSIS DE LOS RESIDUOS 

#_______________________________________________________________________________
#M2

# Diagnósticos gráficos
par(mfrow = c(2, 2))
plot(M2)

# Residuos vs ajustados
plot(fitted(M2), residuals(M2), xlab = "Fitted values", ylab = "Residuals", main = "M2")
abline(h = 0, col = "red")

# Normalidad
qqnorm(rstudent(M2))
qqline(rstudent(M2), col = "red")


#Histograma
hist(residuals(M2), main = "Histograma de residuos - M2", xlab = "Residuos", breaks = 20)

# Distancia de Cook 
plot(cooks.distance(M2), type = "h", ylab = "Cook's distance", main = "M2")
abline(h = 4/nobs(M2), col = "red", lty = 2)

#Hay 3002 observaciones
max(cooks.distance(M2))
summary(cooks.distance(M2))


# Multicolinealidad
vif(M2)

# Observaciones potencialmente problemáticas
which(abs(rstudent(M2)) > 3)
length(which(cooks.distance(M2) > 4/nobs(M2)))

dev.off()
#_______________________________________________________________________________

#M3

# Diagnósticos gráficos
par(mfrow = c(2, 2))
plot(M3)

# Residuos vs ajustados
plot(fitted(M3), residuals(M3), xlab = "Fitted values", ylab = "Residuals", main = "M3")
abline(h = 0, col = "red")

# Normalidad
qqnorm(rstudent(M3))
qqline(rstudent(M3), col = "red")

#Histograma
hist(residuals(M3), main = "Histograma de residuos - M3", xlab = "Residuos", breaks = 20)

# Distancia de Cook
plot(cooks.distance(M3), type = "h", ylab = "Cook's distance", main = "M3")
abline(h = 4/nobs(M3), col = "red", lty = 2)

max(cooks.distance(M3))
summary(cooks.distance(M3))

# Multicolinealidad
vif(modelo_lm_int_v3, type = "predictor")

# Observaciones potencialmente problemáticas
which(abs(rstudent(M3)) > 3)
length(which(cooks.distance(M3) > 4/nobs(M3)))



#_______________________________________________________________________________
#M4
# Diagnóstico general
gam.check(M4)

# Residuos vs ajustados
plot(fitted(M4), residuals(M4, type = "deviance"), xlab = "Valores ajustados", ylab = "Residuos deviance", main = "M4")
abline(h = 0, col = "red")

# QQ-plot
qqnorm(residuals(M4))
qqline(residuals(M4), col = "red")

# Histograma
hist(residuals(M4, type = "deviance"), main = "Histograma de residuos", xlab = "Residuos deviance", breaks = 20)

# Concurvidad (equivalente GAM de la colinealidad)
concurvity(M4)


summary(M1)
summary(M2)
summary(M3)
summary(M4)


#_______________________________________________________________________________
#3.3.TEST DE I MORAN POR KNN (K-Nearest Neighbours)

#Por KNN (K-Nearest Neighbours)
#Matriz de coordenadas
coords <- cbind(df_final$X_6933, df_final$Y_6933)
coords <- as.matrix(coords)



#Creamos una función para realizar de forma automatizada el Test de I Moran 
#mediante KNN (K-Nearest Neighbours)con distintos k para evaluar sensibilidad y consistencia. 

k_values <- c(20, 30, 40, 50, 65, 80, 100, 120, 140)

eval_knn_moran <- function(coords, res, k_values, nsim = 999) {
  
  results <- lapply(k_values, function(k) {
    
    knn <- knearneigh(coords, k = k, longlat = FALSE)
    nb  <- knn2nb(knn, sym = TRUE)
    lw  <- nb2listw(nb, style = "W", zero.policy = TRUE)
    
    moran <- moran.mc(res, listw = lw, nsim = nsim, zero.policy = TRUE)
    
    dists <- nbdists(nb, coords)
    max_dist <- sapply(dists, max)
    
    data.frame(
      k = k,
      components = n.comp.nb(nb)$nc,
      mean_neighbours = mean(card(nb)),
      min_neighbours = min(card(nb)),
      max_neighbours = max(card(nb)),
      median_max_distance_m = median(max_dist),
      mean_max_distance_m = mean(max_dist),
      max_distance_m = max(max_dist),
      moran_I = as.numeric(moran$statistic),
      p_value = moran$p.value)})
  
  do.call(rbind, results)}



# Ejecutamos Moran's I para cada modelo
results_knn_M1 <- eval_knn_moran(coords, residuals(M1), k_values)
results_knn_M2 <- eval_knn_moran(coords, residuals(M2), k_values)
results_knn_M3 <- eval_knn_moran(coords, residuals(M3), k_values)
results_knn_M4 <- eval_knn_moran(coords, residuals(M4), k_values)



#Tabla resumen de conectividad y distancias. Es la misma para todos los modelos, 
#ya que depende de las coordenadas de las parcelas 

tabla_conectividad <- results_knn_M1[, c( "k", "components", "mean_neighbours",
                                          "min_neighbours", "max_neighbours", 
                                          "median_max_distance_m","mean_max_distance_m",
                                          "max_distance_m")]

round(tabla_conectividad,2)


#Tabla con K, Moran's I y p-value por modelo


tabla_moran <- data.frame(k = k_values,
                          
                          Moran_I_M1 = results_knn_M1$moran_I,
                          p_value_M1 = results_knn_M1$p_value,
                          
                          Moran_I_M2 = results_knn_M2$moran_I,
                          p_value_M2 = results_knn_M2$p_value,
                          
                          Moran_I_M3 = results_knn_M3$moran_I,
                          p_value_M3 = results_knn_M3$p_value,
                          
                          Moran_I_M4 = results_knn_M4$moran_I,
                          p_value_M4 = results_knn_M4$p_value)

round(tabla_moran, 3)


#Gráfico Moran's I vs k
#Hago el gráfico general sobre M1 y añado las capas de M2,M3,M4, leyenda y 
#línea que indica Moran's I= 0
plot(results_knn_M1$k, results_knn_M1$moran_I, type = "b", pch = 16, col = "red",
     lwd = 2, ylim = range(c(results_knn_M1$moran_I, results_knn_M2$moran_I,
                             results_knn_M3$moran_I,results_knn_M4$moran_I)),
     xlab = "Number of neighbours (k)", ylab = "Moran's I",main = "Sensitivity of Moran's I to k")

lines(results_knn_M2$k, results_knn_M2$moran_I, type = "b", pch = 17, lty = 2, lwd = 2,
      col = "blue")

lines(results_knn_M3$k, results_knn_M3$moran_I, type = "b", pch = 15, lty = 3, lwd = 2,
      col = "darkgreen")

lines(results_knn_M4$k, results_knn_M4$moran_I, type = "b", pch = 18, lty = 4, lwd = 2,
      col = "orange")

abline(h = 0, lty = 2, col = "grey50")

legend( "topright", inset = c(0.02, 0.02), legend = c("M1", "M2", "M3", "M4"), 
        col = c("red", "blue", "darkgreen", "orange"), pch = c(16, 17, 15, 18),
        lty = c(1, 2, 3, 4), lwd = 2, cex = 0.8, bty = "n")












#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________


#4.SPATIAL BLOCK CROSS VALIDATION BALANCEADA (5 K-FOLD) PARA DISTINTOS TAMAÑOS DEL BLOQUE

# Coordenadas proyectadas en metros
df_final$x <- df_final$X_6933
df_final$y <- df_final$Y_6933

n_folds <- 5
block_sizes <- c(50000, 100000, 150000, 200000)  # 50, 100, 150, 200 km


# Métricas en escala original

calc_metrics <- function(obs, pred) {
  data.frame(
    RMSE = sqrt(mean((pred - obs)^2, na.rm = TRUE)),
    MAE = mean(abs(pred - obs), na.rm = TRUE),
    R2 = cor(obs, pred, use = "complete.obs")^2,
    Bias = mean(pred - obs, na.rm = TRUE))}


# Función para crear folds espaciales balanceados

create_spatial_folds <- function(data, block_size, n_folds = 5, seed = 123) {
  
  set.seed(seed)
  
  data$block_x <- floor(data$x / block_size)
  data$block_y <- floor(data$y / block_size)
  data$block_id <- paste(data$block_x, data$block_y, sep = "_")
  
  block_counts <- as.data.frame(table(data$block_id))
  names(block_counts) <- c("block_id", "n")
  
  block_counts <- block_counts[order(-block_counts$n), ]
  
  fold_load <- rep(0, n_folds)
  block_counts$fold <- NA
  
  for(i in seq_len(nrow(block_counts))) {
    f <- which.min(fold_load)
    block_counts$fold[i] <- f
    fold_load[f] <- fold_load[f] + block_counts$n[i]}
  
  data$fold <- block_counts$fold[match(data$block_id, block_counts$block_id)]
  
  data}


# Función de CV espacial para un modelo

spatial_block_cv <- function(model, data, response_original, n_folds = 5) {
  
  out <- list()
  
  for(f in 1:n_folds) {
    
    train_data <- data[data$fold != f, ]
    test_data  <- data[data$fold == f, ]
    
    model_f <- update(model, data = train_data)
    
    pred_log <- predict(model_f, newdata = test_data)
    pred_original <- exp(pred_log) - 1
    
    obs_original <- test_data[[response_original]]
    
    metrics <- calc_metrics(obs_original, pred_original)
    metrics$fold <- f
    
    out[[f]] <- metrics}
  
  do.call(rbind, out)}


# Función para ejecutar M2, M3 y M4 con un tamaño de bloque

run_cv_for_block_size <- function(block_size) {
  
  data_bs <- create_spatial_folds(
    data = df_final,
    block_size = block_size,
    n_folds = n_folds,
    seed = 123)
  
  cat("\nBlock size:", block_size / 1000, "km\n")
  print(table(data_bs$fold))
  
  cv_M2 <- spatial_block_cv(M2, data_bs, "Yield_Mt_H", n_folds)
  cv_M3 <- spatial_block_cv(M3, data_bs, "Yield_Mt_H", n_folds)
  cv_M4 <- spatial_block_cv(M4, data_bs, "Yield_Mt_H", n_folds)
  
  cv_M2$model <- "M2"
  cv_M3$model <- "M3"
  cv_M4$model <- "M4"
  
  cv_all <- rbind(cv_M2, cv_M3, cv_M4)
  cv_all$block_size_km <- block_size / 1000
  
  cv_all}


# Ejecutar CV para todos los tamaños de bloque

cv_all_blocks <- do.call(rbind, lapply(block_sizes, run_cv_for_block_size))


# Resumen medio por modelo y tamaño de bloque

cv_summary_blocks <- aggregate(cbind(RMSE, MAE, R2, Bias) ~ block_size_km + model,
                               data = cv_all_blocks,
                               FUN = mean)

cv_summary_blocks


# Desviación estándar entre folds

cv_sd_blocks <- aggregate(cbind(RMSE, MAE, R2, Bias) ~ block_size_km + model,
                          data = cv_all_blocks, FUN = sd)

cv_sd_blocks


#_______________________________________________________________________________


# Unir media y desviación típica de R2
plot_data <- merge(cv_summary_blocks[, c("block_size_km", "model", "R2")],
                   cv_sd_blocks[, c("block_size_km", "model", "R2")],
                   by = c("block_size_km", "model"),
                   suffixes = c("_mean", "_sd"))

# Mismos límites del eje Y para los tres gráficos
ylim_range <- range(plot_data$R2_mean - plot_data$R2_sd,
                    plot_data$R2_mean + plot_data$R2_sd,
                    na.rm = TRUE)

# Separar datos por modelo
plot_M2 <- plot_data[plot_data$model == "M2", ]
plot_M3 <- plot_data[plot_data$model == "M3", ]
plot_M4 <- plot_data[plot_data$model == "M4", ]

plot_M2 <- plot_M2[order(plot_M2$block_size_km), ]
plot_M3 <- plot_M3[order(plot_M3$block_size_km), ]
plot_M4 <- plot_M4[order(plot_M4$block_size_km), ]

# Tres gráficos en una fila
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

# M2
plot(plot_M2$block_size_km, plot_M2$R2_mean, type = "b", pch = 16,lwd = 2, col = "blue",
     ylim = ylim_range, xlab = "Block size (km)",ylab = expression(R^2), main = "M2")

arrows(x0 = plot_M2$block_size_km, y0 = plot_M2$R2_mean - plot_M2$R2_sd,
  x1 = plot_M2$block_size_km, y1 = plot_M2$R2_mean + plot_M2$R2_sd,
  angle = 90, code = 3, length = 0.05, col = "blue")
abline(h = 0, lty = 2, col = "grey50")


# M3
plot(plot_M3$block_size_km, plot_M3$R2_mean, type = "b", pch = 17, lwd = 2, 
     col = "darkgreen", ylim = ylim_range, xlab = "Block size (km)", 
     ylab = expression(R^2), main = "M3")

arrows(x0 = plot_M3$block_size_km, y0 = plot_M3$R2_mean - plot_M3$R2_sd,
       x1 = plot_M3$block_size_km, y1 = plot_M3$R2_mean + plot_M3$R2_sd,
       angle = 90, code = 3, length = 0.05, col = "darkgreen")
abline(h = 0, lty = 2, col = "grey50")


# M4
plot(plot_M4$block_size_km, plot_M4$R2_mean, type = "b", pch = 15, lwd = 2,
     col = "red", ylim = ylim_range, xlab = "Block size (km)", ylab = expression(R^2),
     main = "M4")

arrows(x0 = plot_M4$block_size_km, y0 = plot_M4$R2_mean - plot_M4$R2_sd, 
       x1 = plot_M4$block_size_km, y1 = plot_M4$R2_mean + plot_M4$R2_sd, angle = 90,
       code = 3, Length = 0.05, col = "red")

abline(h = 0, lty = 2, col = "grey50")


dev.off()


#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

#5.INTERPRETACIÓN DEL MODELO M3 

summary(M3)

#Recordar que para la modelización se había hecho esta transformación Respuesta: log(Yield_Mt_H + 1)
df_final$Yield_log <- log(df_final$Yield_Mt_H + 1)

#Para la interpretación, a nivel cuantitativo, se debe deshacer la transformación.
#La interpretación será a nivel de variación porcentual ya que e^β es el factor multiplicativo 

# Tabla final
resultados <- data.frame(coeficiente = coef(M3),
                         cambio_porcentual = 100 * (exp(coef(M3)) - 1),
                         IC_inf_pct = 100 * (exp(confint(M3)[, 1]) - 1),
                         IC_sup_pct = 100 * (exp(confint(M3)[, 2]) - 1))

round(resultados,4)


#Gráficos para las interacciones (de la variable transformada***)
interact_plot(M3, pred = rad_total_w4,modx = Crop_Type)
interact_plot(M3, pred = rain_total_w3,modx = clay_0_30cm)

coef(M3)



