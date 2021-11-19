library(tidyverse)
library(raster)
library(sf)

#------ Mareograph data
baseurl <- "https://opendata.fmi.fi/wfs?request=getFeature&storedquery_id=fmi::observations::mareograph::timevaluepair"

get_data <- function(start, end, id) {
  wfs_request <- paste0(baseurl, "&starttime=", start, "&endtime=", end, "&fmisid=", id, "&parameters=watlev")
  res <- sf::st_read(wfs_request, quiet = TRUE, stringsAsFactors = FALSE)
}

hki_mareograph <- fmi2::fmi_stations() %>% 
  filter(type == "Mareografiasema" & str_detect(name, "Helsinki"))

start = "2021-10-20T09:00:00"
end = "2021-10-25T09:00:00"

hki_data <- get_data(start, end, hki_mareograph$fmisid)

hki_data_unnest <- hki_data %>% 
  unnest(cols = c("time", "value")) %>% 
  dplyr::select(time, value) %>% 
  mutate(cm = value * 0.1) # mm to cm

maxrise <- hki_data_unnest %>% 
  filter(cm == max(cm)) %>%
  dplyr::select(time, cm)

theme_set(theme_minimal())

theme_update(
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  panel.grid.major = element_blank(),
  plot.title = element_text(size = 28, 
                            face = "bold",
                            hjust = 0.5,
                            margin = margin(t = 18, b = 0)),
  plot.subtitle = element_text(size = 15, 
                               face = "plain",
                               hjust = 0.5,
                               lineheight = 1.2,
                               margin = margin(t = 6, b = 0)),
  plot.caption = element_text(size = 8, 
                              hjust = 0.5,
                              margin = margin(t = 56, b = 18))
)

#------- Helsinki coastline grid DEM 10m, https://tiedostopalvelu.maanmittauslaitos.fi/tp/kartta?lang=en
r_10 <- raster::raster("L4133.tif")

r_10.df <- raster::as.data.frame(r_10, xy = TRUE)

r_10_levels <- (raster::extract(r_10, raster::extent(r_10)) %>% 
             range(finite = TRUE))[2] %>% 
  seq(0, ., by = 0.5)

ggplot(data = r_10.df, aes(x = x, y = y)) +
  geom_raster(aes(fill = L4133)) +
  geom_contour(aes(z = L4133, color = "Now"), breaks = r_10_levels[2], size = 0.5) +
  geom_contour(aes(z = L4133, color = "3 m rise"), breaks = r_10_levels[7], size = 0.5) +
  scale_fill_gradientn(colours = hcl.colors(15, palette = "Turku", rev = TRUE), na.value = "transparent") +
  labs(title = "Helsinki coastline now, and when sea level rises", 
       subtitle = paste0("Recent max mean water was ", as.numeric(maxrise$cm)*0.01, " m on ", str_extract(maxrise$time, "^[^T]+")),
       caption = "@ttso | Data: National Land Survey of Finland, Finnish Meteorological Institute",
       fill = "Elevation", color = "Sea level") 

ggsave("sealevel_hki.pdf", width = 18, height = 12.2, device = cairo_pdf)
ggsave("sealevel_hki.png", width = 35, height = 25, dpi = 72, units = "cm", device = 'png')

rm(r_10)
rm(r_10.df)
gc()

#----------- My home grid DEM 2m
r_2 <- raster::raster("L4133D.tif")

# Focus on Kalasatama and Arabia
#
# sf::st_bbox(r_2)
# new_extent <- extent(386000, 389500, 6673000, 6676000)
# r_2_cropped <- crop(x = r_2, y = new_extent)

r_2.df <- raster::as.data.frame(r_2, xy = TRUE)

r_2_levels <- (raster::extract(r_2, raster::extent(r_2)) %>%
             range(finite = TRUE))[2] %>%
  seq(0, ., by = 0.5)

ggplot(data = r_2.df, aes(x = x, y = y)) +
  geom_raster(aes(fill = L4133D)) +
  geom_contour(aes(z = L4133D, color = "Now"), breaks = r_2_levels[2], size = 0.5) +
  geom_contour(aes(z = L4133D, color = "3 m rise"), breaks = r_2_levels[7], size = 0.5) +
  scale_fill_gradientn(colours = hcl.colors(15, palette = "Turku", rev = TRUE), na.value = "transparent") +
  labs(title = "Helsinki coastline now, and when sea level rises",
       subtitle = "Focus on Kalasatama, Arabia, Kulosaari, Herttoniemi",
       caption = "@ttso | Data: National Land Survey of Finland, Finnish Meteorological Institute",
       fill = "Elevation", color = "Sea level")

ggsave("sealevelhki_east.pdf", width = 18, height = 12.2, device = cairo_pdf)
ggsave("sealevelhki_east.png", width = 35, height = 25, dpi = 72, units = "cm", device = 'png')
