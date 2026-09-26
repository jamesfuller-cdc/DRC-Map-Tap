# DRC Daily Map

An anonymous daily geography game built with R Shiny and Leaflet for deployment to Posit Connect.

## Run locally

Install the packages used by the app:

```r
install.packages(c("shiny", "leaflet", "sf"))
shiny::runApp()
```

## Preprocess data for faster startup

The first launch can be slow because the health-zone shapefile is large and must be transformed and validated. Run this once after adding or replacing boundary data:

```r
source("scripts/preprocess_data.R")
```

This creates `data/game_data.rds`, containing validated WGS84 geometries, precomputed area values, the dissolved DRC outline, and lightweight runtime geometries. The original shapefiles remain the authoritative source, but are not needed by the running app when the cache is present. Re-run the script whenever the shapefiles or city table change.

The cache contains a simplified health-zone display layer indexed by province and simplified scoring geometries. This keeps Posit Connect startup fast without doing geometry processing for each user session.

## Posit Connect Cloud deployment

Run `scripts/preprocess_data.R` and regenerate `manifest.json` before publishing:

```r
rsconnect::writeManifest(appFiles = c("app.R", "R/data.R", "R/game.R"))
```

The production repository should include `app.R`, `R/`, `www/`, `data/game_data.rds`, and `manifest.json`. The app can run with the cache alone; it does not need to rebuild geometry on Connect. The source shapefiles may remain local or be included for reproducibility.

In Connect Cloud, choose **Publish → Shiny**, select this GitHub repository and branch, choose `app.R` as the primary file, and enable automatic republishing on push. Connect Cloud requires `manifest.json` in the same directory as `app.R` and supports automatic republishing from the connected GitHub branch.

No scheduled job is required. Each session derives the puzzle from the current calendar date in `America/New_York`, with the daily boundary at 12:01 AM Eastern, and checks for a date change once per minute. An open session resets to the new puzzle after the boundary. The same date always produces the same five targets for every user.

If boundary or city data changes, rebuild the cache locally and republish the app. Set `DRC_REBUILD_DATA=1` only for a deliberate local rebuild; do not use it in the deployed app.

## Imagery configuration

The app uses EOX Sentinel-2 cloudless imagery by default for local development. It also supports a configured XYZ tile URL:

```r
Sys.setenv(DRC_MAP_TILES_URL = "tiles/{z}/{x}/{y}.png")
Sys.setenv(DRC_MAP_ATTRIBUTION = "© imagery provider")
```

For restricted Posit Connect environments, place a locally cached tile pyramid under `www/tiles/`, or configure an approved internal tile service. Do not deploy external imagery without confirming its license, attribution requirements, and Connect network policy. The default EOX service requires attribution and should be reviewed against your deployment policy.

## Production data

Replace the demo files described in `data/README.md` with authoritative boundary, health-zone, and city data before production use.
