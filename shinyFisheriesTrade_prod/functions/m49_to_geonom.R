m49_to_geonom <- function(m49reporter) {
  fal_2_m49 <- faosws::ReadDatatable("fal_2_m49")
  
  stopifnot(nrow(fal_2_m49) > 0)
  
  faocode <- as.integer(fal_2_m49[m49 == m49reporter]$fal)
  
  stopifnot(length(faocode) == 1)
  
  geonom2fao <- faosws::ReadDatatable("geonom2fao")
  
  stopifnot(nrow(geonom2fao) > 0)
  
  # The [1] below is because of Germany:
  #  > geonom2fao[code %in% c(4, 58)]
  #   code faostat active         name
  #1:    4      79     79   Fr Germany
  #2:   58      79     79 German Dem.R
  
  geonomcode <- geonom2fao[faostat == faocode]$code[1]
  
  geonomcode <- stringr::str_pad(geonomcode, 3, "left", "0")
  
  return(geonomcode)
}
