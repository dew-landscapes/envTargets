
<!-- README.md is generated from README.Rmd. Please edit that file -->

# envTargets

<!-- badges: start -->

<!-- badges: end -->

The goal of envTargets is to assist targets based workflows in `env`
projects.

## Installation

You can install the development version of envTargets from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("dew-landscapes/envTargets")
```

Load `envTargets`

``` r
library("envTargets")
```

## Example workflows

### Bookdown

Need to create `_bookdown.yaml`…

``` r

tar_target(bookdown_yaml
           , prepare_bookdown_yaml() # an envTargets function
           , format = "file"
           )
```

Then a generic `0X0_report.R` script will knit, respecting target
dependencies.

``` r

library(targets)
library(tarchetypes)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar options --------
tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages)

# from elsewhere -----
tar_load(extent, store = tars$report_prep$store)

# targets --------
list(
  ## dependencies -------
  tar_files(file_deps
            , c(fs::path(tars$clean$store, "objects", "bio_clean")
                # put any other target dependencies in here
                , "report/_bookdown.yaml"
                , get_deps()[[1]]
                )
            )
  ## render --------
  , tar_target(report
               , render_with_deps(input_directory = "report"
                                  , deps = file_deps |> unlist() |> unname()
                                  )
               )
  )
```

## Raster preparation

Need a `settings/env.yaml` (or similar):

``` r

env:
  extent:
    vector: "sa_br_dissolve"
    filt_col: NULL
    filt_level: NULL
    buffer: 0
  grain:
    temp: "P10Y"
    res: 90
```

To make a tibble of the raster stack:

``` r

tar_target(name = env_df
               , command = prepare_env(settings_env
                                       , base_year = 2015
                                       )
               )
```

To make the actual raster stack (never a target, just feed this in as an
argument if/where needed). For example see the raster split, apply,
combine section below where `make_env_stack()` is used within
`predict_terra_tiles()`.

``` r

make_env_stack(env_df, aoi = aoi_sf)
```

## Raster split, apply, combine

Also see the `geotargets` package.

`terra_memfrac` and `use_cores` are global objects specified at the top
of the targets workflow. They attempt to balance memory use over the
cores used (with each tile being sent to a core, where it is processed
sequentially). While this seems to work well, it causes target
invalidation if the workflow is run with different cores/RAM.

``` r

# cores --------
use_cores <- floor(parallel::detectCores() * 3 / 4)

# RAM -------
total_terra_ram_prop <- 0.6 # across all cores
terra_memfrac <- total_terra_ram_prop / use_cores # prop of available memory allowed per core (or per tile)
```

The code below assumes that the rasters are all in place, usually
created by `envRas`. `aoi_sf` is an, optional, simple feature to set the
raster window (via `terra::window()`).

``` r
### base grid --------
  , tar_target(base_grid
             , command = name_env_out(base_dir = "I:"
                                      , set_list = settings_import$env
                                      ) |>
               dplyr::mutate(path = fs::path(path, "base.tif")) |>
               dplyr::pull(path)
             , format = "file"
             )
#### split --------
, tar_target(name = tile_extents
             , make_tile_extents(base_grid_path = base_grid
                                 , aoi = aoi_sf
                                 )
             )
#### apply -------
, tar_target(use_memfrac
             , if(nrow(tile_extents >= use_cores)) {terra_memfrac} else
               {(total_ram * total_terra_ram_prop / nrow(tile_extents)) / total_ram}
             )
, tar_target(predicted_tiles
             , command = predict_terra_tiles(extent = tile_extents
                                             , predict_stack = make_env_stack(env_df, aoi_sf)
                                             , model = rf_good$rf
                                             , terra_options = list(memfrac = use_memfrac)
                                             , out_dir = fs::path(tars$predict$store, "tiles")
                                             , load_packages = c("randomForest")
                                             # via dots to terra::predict
                                             , na.rm = TRUE
                                             , overwrite = TRUE
                                             , wopt = list(datatype = "INT1U")
                                             )
             , pattern = map(tile_extents)
             , retrieval = "worker"
             , format = "file"
             )
#### combine -------
, tar_target(prediction_raw
             , combine_tiles(tiles = predicted_tiles
                             , out_file = fs::path(tars$predict$store, "ecosystems_raw.tif")
                             , overwrite = TRUE
                             , wopt = list(datatype = "INT1U")
                             )
             , deployment = "main"
             , format = "file"
             )
```

## What else is in `envTargets`?

| object | class | description |
|:---|:---|:---|
| `envTargets::collect_values()` | function | Create a vector of values from a targets store object |
| `envTargets::combine_tiles()` | function | Combine tiles into a single raster |
| `envTargets::extract_min_date()` | function | Extract a minimum date to use in the project, based on the settings |
| `envTargets::extract_temporal_grain()` | function | Get the temporal grain from settings |
| `envTargets::get_deps()` | function | Get dependencies |
| `envTargets::make_env_stack()` | function | Make a spatRaster from env_df |
| `envTargets::make_targets()` | function | Make targets |
| `envTargets::make_tars()` | function | Make tars |
| `envTargets::make_tar_id()` | function | Make a suitable targets ‘id’ |
| `envTargets::make_tile_extents()` | function | Make tile (extents) for raster split, apply, combine |
| `envTargets::predict_terra_tiles()` | function | Predict a model across tiles |
| `envTargets::prepare_bookdown_yaml()` | function | Prepare \_bookdown.yaml within a targets workflow |
| `envTargets::prepare_env()` | function | Prepare a tibble of environmental layers from settings |
| `envTargets::render_with_deps()` | function | Render bookdown and force Rmd file dependencies |
| `envTargets::store_dir()` | function | Generate a path to a storage directory |
| `envTargets::summarise_store_data()` | function | Summarise a cleaning workflow from a targets store |
| `envTargets::write_tars()` | function | Write tars as yaml |
