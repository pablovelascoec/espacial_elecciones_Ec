import requests
import json
import geopandas as gpd
import os

#estableciendo ruta
working_dir = r"I:\Mi unidad\electoral_obs\Py_minado"
os.chdir(working_dir)

# URL base del servicio en ArcGIS REST API
BASE_URL = "https://services6.arcgis.com/SGiLSdsy6CJtpjEM/arcgis/rest/services/Organizacion_Territorial/FeatureServer/0/query"

# Parámetros base para la consulta
params = {
    "where": "1=1",
    "outFields": "*",
    "f": "geojson",
    "resultOffset": 0,
    "resultRecordCount": 2000,
    "orderByFields": "OBJECTID ASC"
}

# Lista para almacenar los datos
all_features = []

print("Descargando datos desde ArcGIS...")
while True:
    response = requests.get(BASE_URL, params=params)
    data = response.json()

    features = data.get("features", [])
    if not features:
        break

    all_features.extend(features)
    params["resultOffset"] += params["resultRecordCount"]

# Guardar como GeoJSON temporal
geojson_path = "CNE_recintoselectorales_2025.geojson"
with open(geojson_path, "w", encoding="utf-8") as file:
    json.dump({"type": "FeatureCollection", "features": all_features}, file, ensure_ascii=False, indent=4)

print(f"Datos descargados: {len(all_features)} entidades.")
print("Convirtiendo a Shapefile...")

# Leer el GeoJSON y guardar como Shapefile
gdf = gpd.read_file(geojson_path)
gdf.to_file("CNE_recintoselectorales_2025.shp", driver="ESRI Shapefile")

print("Conversión completada: 'CNE_recintoselectorales_2025.shp'")

##Para descargar parroquias, zonas u otras, buscar la capa en la página de ArcGIS y actualizar en el documento