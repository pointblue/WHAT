## code to prepare `sampledat` dataset goes here

dat = readr::read_csv('C:/Users/kdybala/OneDrive - Point Blue/Documents/A_Projects_inactive/DRWP/data/flood_curves/cropscape2021_rice.csv')

sampledat = dat |> dplyr::select(Name = BasinName, Mosaic:Threshold) |>
  dplyr::mutate(Name = dplyr::recode(Name,
                                     American = 'unit1',
                                     Butte = 'unit2',
                                     Colusa = 'unit3',
                                     Delta = 'unit4',
                                     `San Joaquin` = 'unit5',
                                     Suisun = 'unit6',
                                     Sutter = 'unit7',
                                     Tulare = 'unit8',
                                     Yolo = 'unit9'))

usethis::use_data(sampledat, overwrite = TRUE)
