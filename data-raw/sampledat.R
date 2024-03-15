## code to prepare `sampledat` dataset goes here

# # wetland polygons
# mu = sf::read_sf('C:/Users/kdybala/OneDrive - Point Blue/Documents/A_Projects/2163_WaterSmart/water_smart_GIS/derived_data/wetland_units_update_20240126.shp')
#
# wetland class data
pred = readr::read_csv('C:/Users/kdybala/OneDrive - Point Blue/Documents/A_Projects/2163_WaterSmart/data/wetlands_predicted.csv') |>
  select(MU, pred.class) |>
  mutate(CLASS = recode(pred.class, seas = 'managed')) |>
  select(-pred.class)

dat = readr::read_csv('C:/Users/kdybala/OneDrive - Point Blue/Documents/A_Projects/2163_WaterSmart/data/combined_units-wetland_vers-20240126_method-zonal.csv') |>
  left_join(pred, by = 'MU')
#dat = readr::read_csv('C:/Users/kdybala/OneDrive - Point Blue/Documents/A_Projects/2163_WaterSmart/data/combined_units-wetland_version-20240126_method-zonal.csv')
#dat = readr::read_csv('C:/Users/kdybala/OneDrive - Point Blue/Documents/A_Projects_inactive/DRWP/data/flood_curves/cropscape2021_rice.csv')


sampledat = bind_rows(
  dat |>
    dplyr::select(WETLAND, FIELD, MU, CLASS, AREA_HA, AREA_AC, Mosaic:dplyr::last_col()) |>
    filter(grepl('GWMA', MU) & FIELD == 'N-22') |>
    dplyr::mutate(WETLAND = 'SampleWetland1',
                  MU = as.numeric(as.factor(MU)),
                  MU = paste0('SampleWetland1_Unit', MU)),
  dat |>
    dplyr::select(WETLAND, FIELD, MU, CLASS, AREA_HA, AREA_AC, Mosaic:dplyr::last_col()) |>
    filter(grepl('SAC', MU) & FIELD %in% c('T11', 'T13')) |>
    dplyr::mutate(WETLAND = 'SampleWetland2',
                  MU = as.numeric(as.factor(MU)),
                  MU = paste0('SampleWetland2_Unit', MU))
  ) |>
  select(-FIELD, -EstimatedAreaWaterHa) |>
  arrange(WETLAND, MU)

# summary:
sampledat |> select(WETLAND, MU, CLASS) |> distinct()

usethis::use_data(sampledat, overwrite = TRUE)
