
#ANÁLISIS DESCRIPTIVO

install.packages("readr")
library(readr)

install.packages("dplyr")
library(dplyr)

install.packages("e1071")
library(e1071)

install.packages("corrplot")
library(corrplot)

install.packages("ggcorrplot")
library(ggcorrplot)


View(df_completed)

df_EDA <- read_csv("C:/Users/aguia/OneDrive/Escritorio/TFG_CTA_Zimbabwe/Obtención_datos_CTA_Zimbabwe/base_de_datos_final/final_dataset/with_all_crops/df_completed.csv")
View(df_completed)



#Eliminamos variables que no se van a utilizar de nuevo en el análisis
df_EDA<-df_EDA%>%select(-fid,-Adm0,-Admin1, -Crop_Varie,-Wet_Weight,-Dry_weight,
                        -Type,-Other_crop, -f_Planting_D, -f_Harvesting, -f_t1, 
                        -f_t2, -f_t3, -f_t_terminal,-Y_4326, -X_4326, - Crop_Condi, 
                        -ID)


View(df_EDA)

#_______________________________________________________________________________

#1.DESCRIPCIÓN GENERAL DEL CONJUNTO DE DATOS.

#Numero de observaciones total: 3002

#Número de observaciones por cultivo: 
table(df_EDA$Crop_Type)
#En porcentaje: 
table(df_EDA$Crop_Type)*100/3002


#Número de regiones administrativas
df_EDA$administrative_regions<-as.factor(df_EDA$administrative_regions)
levels(df_EDA$administrative_regions)


#Resumen del rango de simebra y cosecha para cada cultivo
#Los datos son de parcelas distintas. Creo que no tiene sentido analizarlas, porque
#las parcelas se analizan en campañas distintas.

df_EDA%>%filter(Crop_Type=="Maize/Corn")%>%summarise(planting_min = min(f_Planting_D, na.rm = TRUE),
                                                     planting_max = max(f_Planting_D, na.rm = TRUE),
                                                     harvest_min = min(f_Harvesting, na.rm = TRUE),
                                                     harvest_max = max(f_Harvesting, na.rm = TRUE))


df_EDA%>%filter(Crop_Type=="Sorghum")%>%summarise(planting_min = min(f_Planting_D, na.rm = TRUE),
                                                     planting_max = max(f_Planting_D, na.rm = TRUE),
                                                     harvest_min = min(f_Harvesting, na.rm = TRUE),
                                                     harvest_max = max(f_Harvesting, na.rm = TRUE))

df_EDA%>%filter(Crop_Type=="Soybeans")%>%summarise(planting_min = min(f_Planting_D, na.rm = TRUE),
                                                  planting_max = max(f_Planting_D, na.rm = TRUE),
                                                  harvest_min = min(f_Harvesting, na.rm = TRUE),
                                                  harvest_max = max(f_Harvesting, na.rm = TRUE))


df_EDA%>%filter(Crop_Type=="Wheat")%>%summarise(planting_min = min(f_Planting_D, na.rm = TRUE),
                                                   planting_max = max(f_Planting_D, na.rm = TRUE),
                                                   harvest_min = min(f_Harvesting, na.rm = TRUE),
                                                   harvest_max = max(f_Harvesting, na.rm = TRUE))





#Hay 3 variables del manejo del cultivo.
#Hay una variable que indica el tipo de cultivo
#Hay 2 variables topográficas: altitud y pendiente.
#Hay 3 variables hidrológicas: distancia a líneas y áreas de agua y la 
#identificación de zonas propensas a inundaciones.
#Hay 2 variables de infraestructura:distancia a ciudades y a carreteras.
#Hay 3 variables espaciales: coordenadas de longitud y latitud en EPSG:6933, y 
#la región administrativa. 
#Hay 36 variables edáficas de propiedades físicas y químicas del suelo.
#Hay 139 variables metereológicas.
#Hay 3 variables de features de NDVI del ciclo completo
#En total, hay 192 covariables 

#_______________________________________________________________________________

#2.VARIABLE RESPUESTA: RENDIMIENTO AGRÍCOLA EN PESO SECO (t/ha)

#Estadísticos principales del rendimiento 
summary_yield<- data.frame(
  Min = min(df_EDA$Yield_Mt_H),
  Q1 = quantile(df_EDA$Yield_Mt_H, probs = 0.25),
  Median = median(df_EDA$Yield_Mt_H),
  Mean = mean(df_EDA$Yield_Mt_H),
  Q3 = quantile(df_EDA$Yield_Mt_H, probs = 0.75),
  Max = max(df_EDA$Yield_Mt_H), 
  sd = sd(df_EDA$Yield_Mt_H),
  CV = sd(df_EDA$Yield_Mt_H)/mean(df_EDA$Yield_Mt_H))

summary_yield


#Histogramas 
#Histograma superficie en escala general (t/ha) 
hist(df_EDA$Yield_Mt_H,
     breaks = 60,
     col ="red" ,
     border = "white",
     main="Histograma de Rendimiento",
     xlab = "Rendimiento (t/ha)", 
     ylab="Frecuencia")


#Histograma log(Rendimiento + 1).
#Nota: le sumo 1 porque min(df_EDA$Yield_Mt_H)=0
hist(log(df_EDA$Yield_Mt_H+1),
     breaks = 60,
     col = "red",
     border = "white",
     main="Histograma de log(Rendimiento + 1)",
     xlab = "log(Rendimiento+1)", 
     ylab="Frecuencia")


#Boxplot del rendimiento global
boxplot(
  df_EDA$Yield_Mt_H,
  col = "red",
  main = "Boxplot del rendimiento agrícola",
  ylab = "t/ha",
  names = "Rendimiento agrícola")


#Asimetría 
skewness(df_EDA$Yield_Mt_H)

#Pruebas de normalidad 
shapiro.test(df_EDA$Yield_Mt_H)
shapiro.test(log(df_EDA$Yield_Mt_H+1))

#¿Cuántos valores con 0 hay?
sum(df_EDA$Yield_Mt_H == 0)
sum(df_EDA$Yield_Mt_H == 0, na.rm = TRUE)* 100 / length(df_EDA$Yield_Mt_H) 




#Comparación entre cultivos 
#Boxplot del rendimiento por variedad
boxplot(Yield_Mt_H ~ Crop_Type , data = df_EDA, 
        names= c("Maíz", "Sorgo", "Soja", "Trigo"), ylab="t/ha",
        col=c("yellow", "lightblue", "green", "orange"))


#Estadísticos descriptivos por cultivo
aggregate(Yield_Mt_H ~ Crop_Type, data = df_EDA,FUN = function(x) c(
  n = length(x),
  mean = mean(x),
  median = median(x),
  sd = sd(x),
  min = min(x),
  max = max(x),
  cv = sd(x) / mean(x)))






#Posibles Outliers. Conclusión: No parecen datos incorrectos
df_EDA_corn_potential_outliers <- df_EDA_corn%>%
  filter(df_EDA_corn$Yield_Mt_H==0 | df_EDA_corn$Yield_Mt_H>8)%>%
  select("Yield_Mt_H","Dry_weight", "Crop_Condi")

View(df_EDA_corn_potential_outliers)


#_______________________________________________________________________________
#3.ANÁLISIS ESPACIAL. Se ha hecho en QGIS.
table(df_EDA$administrative_regions,df_EDA$Crop_Type)

#_______________________________________________________________________________
#4.ANÁLISIS DE LAS VARIABLES CLIMÁTICAS


# Grupos de variables climáticas
vars_temp <- c("temp_mean_w1", "temp_mean_w2", "temp_mean_w3", "temp_mean_w4")

vars_prec <- c("precipitation_total_w1", "precipitation_total_w2",
               "precipitation_total_w3", "precipitation_total_w4")

vars_et0 <- c("et0_total_w1", "et0_total_w2",
              "et0_total_w3", "et0_total_w4")

vars_rh <- c("rh_mean_w1", "rh_mean_w2", "rh_mean_w3", "rh_mean_w4")

vars_rad <- c("rad_total_w1", "rad_total_w2",
              "rad_total_w3", "rad_total_w4")

vars_wind <- c("wind_mean_w1", "wind_mean_w2",
               "wind_mean_w3", "wind_mean_w4")




#Estadísticos descriptivos por grupo
resumen_basico <- function(datos, vars) {
  data.frame(
    variable = vars,
    min = sapply(datos[vars], min, na.rm = TRUE),
    q1 = sapply(datos[vars], quantile, probs = 0.25, na.rm = TRUE),
    median = sapply(datos[vars], median, na.rm = TRUE),
    mean = sapply(datos[vars], mean, na.rm = TRUE),
    q3 = sapply(datos[vars], quantile, probs = 0.75, na.rm = TRUE),
    max = sapply(datos[vars], max, na.rm = TRUE),
    sd = sapply(datos[vars], sd, na.rm = TRUE))}

res_temp <- resumen_basico(df_EDA, vars_temp)
res_prec <- resumen_basico(df_EDA, vars_prec)
res_et0 <- resumen_basico(df_EDA, vars_et0)
res_rh <- resumen_basico(df_EDA, vars_rh)
res_rad <- resumen_basico(df_EDA, vars_rad)
res_wind <- resumen_basico(df_EDA, vars_wind)

res_temp
res_prec
res_et0
res_rh
res_rad
res_wind

rbind(res_temp,
      res_prec,
      res_et0,
      res_rh,
      res_rad,
      res_wind)


#Boxplots por ventana
par(mfrow = c(2, 3))

boxplot(df_EDA[vars_temp],
        names = c("w1", "w2", "w3", "w4"),
        col = "tomato",
        main = "Temperatura",
        ylab = "°C")

boxplot(df_EDA[vars_prec],
        names = c("w1", "w2", "w3", "w4"),
        col = "lightblue",
        main = "Precipitación",
        ylab = "mm")

boxplot(df_EDA[vars_et0],
        names = c("w1", "w2", "w3", "w4"),
        col = "orange",
        main = "ET0",
        ylab = "mm")

boxplot(df_EDA[vars_rh],
        names = c("w1", "w2", "w3", "w4"),
        col = "lightgreen",
        main = "Humedad relativa",
        ylab = "%")

boxplot(df_EDA[vars_rad],
        names = c("w1", "w2", "w3", "w4"),
        col = "yellow",
        main = "Radiación",
        ylab = "MJ/m²")

boxplot(df_EDA[vars_wind],
        names = c("w1", "w2", "w3", "w4"),
        col = "grey",
        main = "Viento",
        ylab = "km/h")


dev.off()


#Matriz de correlación simple
vars_clima_principales <- c(vars_temp, vars_prec, vars_et0, vars_rh, vars_rad, vars_wind)

# Matriz de correlación
cor_clima_simple <- cor(df_EDA[vars_clima_principales],use = "complete.obs")

# Correlograma
corrplot(cor_clima_simple,method = "color",type = "upper",
  order = "hclust",tl.cex = 0.6,addCoef.col = "black",
  number.cex = 0.4,insig = "blank")


#_______________________________________________________________________________

#5.ANÁLISIS DE LAS VARIABLES EDÁFICAS Y TOPOGRÁFICAS

#Grupos de variables 
# Variables topográficas
vars_topo <- c("DEM", "Slope")

# Variables edáficas agrupadas por profundidad
vars_soil_0_30 <- c("phh2o_0_30cm", "soc_0_30cm", "N_0_30cm", "cec_0_30cm",
                    "bdod_0_30cm", "cfvo_0_30cm", "clay_0_30cm", "sand_0_30cm",
                    "silt_0_30cm", "wv0010_0_30cm", "wv003_0_30cm", "wv1500_0_30cm")

vars_soil_30_100 <- c("phh2o_30_100cm", "soc_30_100cm", "N_30_100cm", "cec_30_100cm",
                      "bdod_30_100cm", "cfvo_30_100cm", "clay_30_100cm", "sand_30_100cm",
                      "silt_30_100cm", "wv0010_30_100cm", "wv003_30_100cm", "wv1500_30_100cm")

vars_soil_100_200 <- c("phh2o_100_200cm", "soc_100_200cm", "N_100_200cm", "cec_100_200cm",
                       "bdod_100_200cm", "cfvo_100_200cm", "clay_100_200cm", "sand_100_200cm",
                       "silt_100_200cm", "wv0010_100_200cm", "wv003_100_200cm", "wv1500_100_200cm")



#Estadísticos descriptivos
resumen_basico <- function(datos, vars) {
  data.frame(variable = vars,
    min = sapply(datos[vars], min, na.rm = TRUE),
    q1 = sapply(datos[vars], quantile, probs = 0.25, na.rm = TRUE),
    median = sapply(datos[vars], median, na.rm = TRUE),
    mean = sapply(datos[vars], mean, na.rm = TRUE),
    q3 = sapply(datos[vars], quantile, probs = 0.75, na.rm = TRUE),
    max = sapply(datos[vars], max, na.rm = TRUE),
    sd = sapply(datos[vars], sd, na.rm = TRUE))}

res_topo <- resumen_basico(df_EDA, vars_topo)
res_soil_0_30 <- resumen_basico(df_EDA, vars_soil_0_30)
res_soil_30_100 <- resumen_basico(df_EDA, vars_soil_30_100)
res_soil_100_200 <- resumen_basico(df_EDA, vars_soil_100_200)

res_topo
res_soil_0_30
res_soil_30_100
res_soil_100_200


#Boxplots topográficos
par(mfrow = c(1, 2))

boxplot(df_EDA$DEM, col = "lightblue", main = "Altitud",
        ylab = "Altitud (m)")

boxplot(df_EDA$Slope,col = "lightgreen", main = "Pendiente",
        ylab = "Pendiente (%)")

dev.off()

#Boxplots edáficos 

par(mfrow = c(2, 2))

boxplot(df_EDA[, c("phh2o_0_30cm", "phh2o_30_100cm", "phh2o_100_200cm")],
        names = c("0-30", "30-100", "100-200"),
        col = "lightyellow",
        main = "pH por profundidad",
        ylab = "pH")

boxplot(df_EDA[, c("soc_0_30cm", "soc_30_100cm", "soc_100_200cm")],
        names = c("0-30", "30-100", "100-200"),
        col = "tan",
        main = "Carbono orgánico",
        ylab = "g/kg")

boxplot(df_EDA[, c("N_0_30cm", "N_30_100cm", "N_100_200cm")],
        names = c("0-30", "30-100", "100-200"),
        col = "lightgreen",
        main = "Nitrógeno total",
        ylab = "g/kg")

boxplot(df_EDA[, c("cec_0_30cm", "cec_30_100cm", "cec_100_200cm")],
        names = c("0-30", "30-100", "100-200"),
        col = "lightblue",
        main = "CEC",
        ylab = "cmol(c)/kg")



dev.off()
par(mfrow = c(2, 2))

boxplot(df_EDA[, c("clay_0_30cm", "clay_30_100cm", "clay_100_200cm")],
        names = c("0-30", "30-100", "100-200"),
        col = "orange",
        main = "Arcilla",
        ylab = "%")

boxplot(df_EDA[, c("sand_0_30cm", "sand_30_100cm", "sand_100_200cm")],
        names = c("0-30", "30-100", "100-200"),
        col = "khaki",
        main = "Arena",
        ylab = "%")

boxplot(df_EDA[, c("silt_0_30cm", "silt_30_100cm", "silt_100_200cm")],
        names = c("0-30", "30-100", "100-200"),
        col = "grey",
        main = "Limo",
        ylab = "%")

boxplot(df_EDA[, c("wv0010_0_30cm", "wv0010_30_100cm", "wv0010_100_200cm")],
        names = c("0-30", "30-100", "100-200"),
        col = "lightblue",
        main = "Agua a 10 kPa",
        ylab = "cm³/cm³")


dev.off()
par(mfrow = c(1, 2))


boxplot(df_EDA[, c("wv003_0_30cm", "wv003_30_100cm", "wv003_100_200cm")],
        names = c("0-30", "30-100", "100-200"),
        col = "skyblue",
        main = "Agua a 33 kPa",
        ylab = "cm³/cm³")

boxplot(df_EDA[, c("wv1500_0_30cm", "wv1500_30_100cm", "wv1500_100_200cm")],
        names = c("0-30", "30-100", "100-200"),
        col = "steelblue",
        main = "Agua a 1500 kPa",
        ylab = "cm³/cm³")


dev.off()


#Matriz de correlación simple
vars_topo_edáficas <- c(vars_topo, vars_soil_0_30,vars_soil_30_100, vars_soil_100_200)

# Matriz de correlación
cor_topo_edáficas <- cor(df_EDA[vars_topo_edáficas],use = "complete.obs")

# Correlograma
corrplot(cor_topo_edáficas,method = "color",type = "upper",
         order = "hclust",tl.cex = 0.6,addCoef.col = "black",
         number.cex = 0.4,insig = "blank")


#_______________________________________________________________________________

# 6. VARIABLES HIDROLÓGICAS, INFRAESTRUCTURA Y MANEJO

# Variables continuas
vars_dist <- c("prox_towns_km","prox_roads_km","prox_water_lines_km",
  "prox_water_areas_km")

# Variables categóricas
vars_cat <- c("flood_prone_zones","Field_Irri","Intercropp","Crop_Type")


#Estadísticos descriptivos de las variables de distancia 

resumen_basico <- function(datos, vars) {
  data.frame(
    variable = vars,
    min = sapply(datos[vars], min, na.rm = TRUE),
    q1 = sapply(datos[vars], quantile, probs = 0.25, na.rm = TRUE),
    median = sapply(datos[vars], median, na.rm = TRUE),
    mean = sapply(datos[vars], mean, na.rm = TRUE),
    q3 = sapply(datos[vars], quantile, probs = 0.75, na.rm = TRUE),
    max = sapply(datos[vars], max, na.rm = TRUE),
    sd = sapply(datos[vars], sd, na.rm = TRUE))}

res_dist <- resumen_basico(df_EDA, vars_dist)
res_dist


#Boxplots de distancias 
par(mfrow = c(2, 2))

boxplot(df_EDA$prox_towns_km,
        col = "lightblue",
        main = "Distancia a ciudadades",
        ylab = "km")

boxplot(df_EDA$prox_roads_km,
        col = "lightgreen",
        main = "Distancia a carreteras",
        ylab = "km")

boxplot(df_EDA$prox_water_lines_km,
        col = "lightcyan",
        main = "Distancia a red hidrográfica",
        ylab = "km")

boxplot(df_EDA$prox_water_areas_km,
        col = "lightyellow",
        main = "Distancia a cuerpos de agua",
        ylab = "km")

dev.off()




#Frecuencia de las variables categóricas
# Susceptibilidad a inundación
table(df_EDA$flood_prone_zones)
prop.table(table(df_EDA$flood_prone_zones)) * 100

# Riego
table(df_EDA$Field_Irri)
prop.table(table(df_EDA$Field_Irri)) * 100

# Cultivo intercalado
table(df_EDA$Intercropp)
prop.table(table(df_EDA$Intercropp)) * 100

# Tipo de cultivo
table(df_EDA$Crop_Type)
prop.table(table(df_EDA$Crop_Type)) * 100


#Tablas de contigencia 
# Inundación por cultivo
table(df_EDA$Crop_Type, df_EDA$flood_prone_zones)


# Riego por cultivo
table(df_EDA$Crop_Type, df_EDA$Field_Irri)


# Intercropping por cultivo
table(df_EDA$Crop_Type, df_EDA$Intercropp)







#Boxplots de rendimiento según variables categóricas
par(mfrow = c(1, 3))

boxplot(Yield_Mt_H ~ Field_Irri,
        data = df_EDA,
        col = c("lightgrey", "lightblue"),
        main = "Rendimiento según riego",
        xlab = "Riego",
        ylab = "Rendimiento (t/ha)")

boxplot(Yield_Mt_H ~ Intercropp,
        data = df_EDA,
        col = c("lightgrey", "lightgreen"),
        main = "Rendimiento según intercropping",
        xlab = "Intercropping",
        ylab = "Rendimiento (t/ha)")

boxplot(Yield_Mt_H ~ flood_prone_zones,
        data = df_EDA,
        col = c("lightgrey", "lightcyan"),
        main = "Rendimiento según inundación",
        xlab = "Zona inundable",
        ylab = "Rendimiento (t/ha)")

dev.off()

#_______________________________________________________________________________
#7.VARIABLES ESPECTRALES 

#Variables espectrales
vars_ndvi <- c("ndvi_mean_wcomplete","ndvi_max_wcomplete","ndvi_sum_wcomplete")

#Estadísticos descriptivos
resumen_basico <- function(datos, vars) {
  data.frame(
    variable = vars,
    min = sapply(datos[vars], min, na.rm = TRUE),
    q1 = sapply(datos[vars], quantile, probs = 0.25, na.rm = TRUE),
    median = sapply(datos[vars], median, na.rm = TRUE),
    mean = sapply(datos[vars], mean, na.rm = TRUE),
    q3 = sapply(datos[vars], quantile, probs = 0.75, na.rm = TRUE),
    max = sapply(datos[vars], max, na.rm = TRUE),
    sd = sapply(datos[vars], sd, na.rm = TRUE))}

res_ndvi <- resumen_basico(df_EDA, vars_ndvi)

res_ndvi

#Boxplots 
par(mfrow=c(1,3))

boxplot(df_EDA$ndvi_mean_wcomplete,
        col = "lightgreen",
        main = "NDVI medio por parcela",
        ylab = "NDVI")

boxplot(df_EDA$ndvi_max_wcomplete,
        col = "green",
        main = "NDVI máximo por parcela",
        ylab = "NDVI")

boxplot(df_EDA$ndvi_sum_wcomplete,
        col = "darkgreen",
        main = "NDVI acumulado por parcela",
        ylab = "NDVI")

#Boxplots de NDVI por cultivo 

par(mfrow = c(1, 3))

boxplot(ndvi_mean_wcomplete ~ Crop_Type,
        data = df_EDA,
        col = "lightgreen",
        main = "NDVI medio por cultivo",
        ylab = "NDVI")

boxplot(ndvi_max_wcomplete ~ Crop_Type,
        data = df_EDA,
        col = "green",
        main = "NDVI máximo por cultivo",
        ylab = "NDVI")

boxplot(ndvi_sum_wcomplete ~ Crop_Type,
        data = df_EDA,
        col = "darkgreen",
        main = "NDVI acumulado por cultivo",
        ylab = "NDVI")

#_______________________________________________________________________________

#8.RELACIONES ENTRE VARIABLES Y ESTRUCTURA MULTIVARIANTE 

#Nuevo dataframe solo con rendimiento y variables númericas
#(de las metereológicas solo las principales: una por grupo)

vars_num_eda <- c("Yield_Mt_H",vars_clima_principales,vars_topo_edáficas,
                  vars_dist,vars_ndvi)

df_num_eda <- df_EDA[, vars_num_eda]
View(df_num_eda)


#Matriz de correlación
cor_general <- cor(df_num_eda, use = "pairwise.complete.obs")

corrplot(cor_general, method = "color",type = "upper",order = "hclust",
         tl.cex = 0.5,tl.col = "black",
         col = colorRampPalette(c("red", "white", "blue"))(200))
         


#Pares altamente correlacionados
# Correlaciones absolutas
cor_abs <- abs(cor_general)

# Quitar diagonal
diag(cor_abs) <- NA

# Extraer pares con |r| > 0.80
high_cor <- which(cor_abs > 0.80, arr.ind = TRUE)

high_cor_pairs <- data.frame(
  var1 = rownames(cor_abs)[high_cor[, 1]],
  var2 = colnames(cor_abs)[high_cor[, 2]],
  cor = cor_general[high_cor])

# Eliminar duplicados
high_cor_pairs <- high_cor_pairs[high_cor_pairs$var1 < high_cor_pairs$var2, ]

# Ordenar de mayor a menor correlación absoluta
high_cor_pairs <- high_cor_pairs[order(abs(high_cor_pairs$cor), decreasing = TRUE), ]

high_cor_pairs

# Número de pares altamente correlacionados
nrow(high_cor_pairs)




# Correlación de todas las variables con Yield
cor_yield <- cor(df_num_eda, df_num_eda$Yield_Mt_H,
                 use = "pairwise.complete.obs")

cor_yield_df <- data.frame(variable = rownames(cor_yield),
                           cor_yield = as.numeric(cor_yield))

# Quitar la correlación de Yield consigo misma
cor_yield_df <- cor_yield_df[cor_yield_df$variable != "Yield_Mt_H", ]

#Diagrama de barras de la correlación con Yield
barplot(cor_yield_df$cor_yield,
        names.arg = cor_yield_df$variable,
        las = 2,
        cex.names = 0.5,
        line = -0.5,   # acerca los nombres al eje
        col = "lightblue",
        main = "Correlación de Pearson con el rendimiento",
        ylim = c(min(cor_yield_df$cor_yield) - 0.05,
                 max(cor_yield_df$cor_yield) + 0.05))


#_______________________________________________________________________________



#PRUEBAS
df_EDA$administrative_regions<- as.factor(df_EDA$administrative_regions)
df_EDA$Field_Irri <- as.factor(df_EDA$Field_Irri)
df_EDA$flood_prone_zones <-as.factor(df_EDA$flood_prone_zones)
df_EDA$Crop_Type <- as.factor(df_EDA$Crop_Type)


modelo <- lm(Yield_Mt_H ~ . , data = df_EDA)

summary(modelo)







#Añadir Crop_Condi es data leakage:
#No es una variable disponible en el momento de la predicción.
#




