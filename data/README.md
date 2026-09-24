# Data replacement required before production

The app now detects the supplied GRID3 shapefiles automatically. The bundled GeoJSON files are only fallback demo fixtures. Replace or remove them before deployment if desired.

- `GRID_COD_v8_0_province_dissolve_HZ.shp` plus its companion files: province polygons. The app reads the `province` field.
- `GRID3_COD_health_zones_v8_0.shp` plus its companion files: health-zone polygons. The app reads the `zonesante` field.
- A separate DRC border file is optional. If absent, the app derives the DRC outline by dissolving the province layer.
- `cities.csv`: curated city/town points with unique `id`, `name`, `longitude`, and `latitude` fields.

The app derives province context for cities and health zones during preprocessing so prompts can identify locations such as `Goma, North Kivu`.

All geospatial files must contain a CRS and will be transformed to EPSG:4326 during preprocessing. The deployed app should use `game_data.rds`; it loads the preprocessed cache instead of reading and repairing the shapefiles for each startup.
