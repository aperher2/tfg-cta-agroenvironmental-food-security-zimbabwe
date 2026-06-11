(input_id as number, lat as number, lon as number, elev as number, tz as text, pStart as date, pEnd as date) =>
let
    apiKey = "koN3g8PzAfZQbBYT",

    dailyVars =
        "temperature_2m_mean,temperature_2m_max,temperature_2m_min," &
        "soil_temperature_0_to_100cm_mean,dew_point_2m_mean," &
        "precipitation_sum,rain_sum,precipitation_hours," &
        "soil_moisture_0_to_100cm_mean,shortwave_radiation_sum," &
        "sunshine_duration,cloud_cover_mean,et0_fao_evapotranspiration_sum," &
        "relative_humidity_2m_mean,relative_humidity_2m_min," &
        "vapour_pressure_deficit_max,wind_speed_10m_mean,wind_speed_10m_max," &
        "wind_gusts_10m_mean,wind_gusts_10m_max",

    Source = Json.Document(
        Web.Contents(
            "https://customer-archive-api.open-meteo.com/v1/archive",
            [
                Headers = [
                    #"Accept-Encoding" = "identity"
                ],
                Query = [
                    latitude = Number.ToText(lat, null, "en-US"),
                    longitude = Number.ToText(lon, null, "en-US"),
                    start_date = Date.ToText(pStart, "yyyy-MM-dd"),
                    end_date = Date.ToText(pEnd, "yyyy-MM-dd"),
                    daily = dailyVars,
                    timezone = tz,
                    elevation = Number.ToText(elev, null, "en-US"),
                    apikey = apiKey
                ]
            ]
        )
    ),

    Daily = Source[daily],

    DailyTable = Table.FromColumns(
        Record.FieldValues(Daily),
        Record.FieldNames(Daily)
    ),

    // Añadimos info del input
    AddInputID = Table.AddColumn(DailyTable, "input_id", each input_id, Int64.Type),
    AddLat = Table.AddColumn(AddInputID, "input_lat", each lat, type number),
    AddLon = Table.AddColumn(AddLat, "input_lon", each lon, type number),

    // Añadimos coordenadas reales que devuelve Open-Meteo
    AddOMLat = Table.AddColumn(AddLon, "openmeteo_lat", each Source[latitude], type number),
    AddOMLon = Table.AddColumn(AddOMLat, "openmeteo_lon", each Source[longitude], type number)

in
    AddOMLon