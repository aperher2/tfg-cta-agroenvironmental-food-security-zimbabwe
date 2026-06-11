
/*
TFG - NDVI extraction template

Google Earth Engine script for extracting mean NDVI values
from Sentinel-2 imagery over agricultural plots.

Modify the parameters below to reproduce different temporal windows.
*/

// ======================================
// PARAMETERS
// ======================================

var featureCollection = "users/<username>/Consulta_ventana_1";

var startField = "Planting_D";
var endField = "t1";

var exportName = "NDVI_w1";

// ======================================
// LOAD PLOTS
// ======================================

var parcelas = ee.FeatureCollection(featureCollection);

// ======================================
// SENTINEL-2
// ======================================

var s2 = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
  .filterBounds(parcelas)
  .filter(ee.Filter.lt("CLOUDY_PIXEL_PERCENTAGE", 40));

// ======================================
// CLOUD MASK
// ======================================

var maskS2 = function(image) {

  var scl = image.select("SCL");

  var mask = scl.neq(3)
    .and(scl.neq(8))
    .and(scl.neq(9))
    .and(scl.neq(10))
    .and(scl.neq(11));

  return image.updateMask(mask);
};

// ======================================
// NDVI
// ======================================

var addNDVI = function(img) {

  var ndvi = img
    .normalizedDifference(["B8", "B4"])
    .rename("NDVI");

  return img.addBands(ndvi)
            .copyProperties(img, ["system:time_start"]);
};

var s2_ndvi = s2.map(maskS2).map(addNDVI);

// ======================================
// EXTRACTION
// ======================================

var extractNDVI = function(feature) {

  var start = ee.Date.parse(
    "YYYY-MM-dd",
    feature.get(startField)
  );

  var end = ee.Date.parse(
    "YYYY-MM-dd",
    feature.get(endField)
  );

  var parcelaBuffer = feature.geometry().buffer(4);

  var filtered = s2_ndvi
    .filterBounds(parcelaBuffer)
    .filterDate(start, end);

  return filtered.map(function(img) {

    var date = ee.Date(
      img.get("system:time_start")
    ).format("YYYY-MM-dd");

    var values = img.select("NDVI")
      .reduceRegion({
        reducer: ee.Reducer.mean(),
        geometry: parcelaBuffer,
        scale: 10,
        maxPixels: 1e9
      });

    return ee.Feature(null, {
      ID: feature.get("ID"),
      date: date,
      ndvi_mean: values.get("NDVI")
    });

  });

};

var result = parcelas
  .map(extractNDVI)
  .flatten();

// ======================================
// EXPORT
// ======================================

Export.table.toDrive({
  collection: result,
  description: exportName,
  fileFormat: "CSV"
});