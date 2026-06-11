# TFG - Agro-Environmental Food Security Analysis in Zimbabwe

## Overview

This repository contains the code and data developed for the Bachelor's Thesis:

**"Influence of Agro-Environmental Variables on the Potential Availability of Food Raw Materials in Zimbabwe: Implications for Food Security"**

The objective of this work is to analyse how agroenvironmental factors influence the potential availability of food raw materials in Zimbabwe through the integration of geospatial, meteorological and remote sensing information.

## Data Sources

The analysis is based on three main sources of information:

1. **Agricultural plots dataset (Lacuna)**

   * Plot identifiers
   * Geographic coordinates
   * Planting dates
   * Harvest dates

2. **Meteorological data**

   * Historical weather information obtained through the Open-Meteo API.

3. **Remote sensing data**

   * Sentinel-2 imagery processed in Google Earth Engine.
   * NDVI indicators extracted for different crop development periods.

## Repository Structure

```text
power-query/
    GetHistoricalWeather.pq

gee/
    Get_NDVI.js

R/
    Metereological_variables_calculations.R
    NDVI_features_calculations.R
    join_of_different_datasets_through_ID.R
    exploratory_data_analysis.R
    modelization_CTA.R

data/
    df_completed.csv
```

## Workflow

### 1. Meteorological variables

Historical weather data were retrieved from Open-Meteo using Power Query and processed through:

* `GetHistoricalWeather.pq`
* `Metereological_variables_calculations.R`

### 2. Vegetation variables

NDVI indicators were extracted from Sentinel-2 imagery using Google Earth Engine and subsequently processed through:

* `Get_NDVI.js`
* `NDVI_features_calculations.R`

### 3. Dataset integration

All datasets were merged using unique plot identifiers through:

* `join_of_different_datasets_through_ID.R`

### 4. Data analysis

The final dataset was used for:

* Exploratory Data Analysis (EDA)
* Modelling

Scripts:

* `exploratory_data_analysis.R`
* `modelization_CTA.R`

## Software

* R
* Google Earth Engine
* Power Query
* QGIS & GDAL 

## Author

Álvaro Pérez Hernando

Bachelor's Thesis (CTA)
