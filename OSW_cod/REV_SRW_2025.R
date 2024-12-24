##### Cod 2025 Final Report

##### packages
require(sf)
require(tidyverse)
require(ggplot2)
require(readxl)
require(lubridate)

##### import tag data
# deployment 1 
# https://marine.rutgers.edu/~dkaragon/glider_profile_plotting/ru34-20241102T1737/ru34-20241102T1737_vemco-detections.kmz
tag_layers = st_layers('~/Downloads/ru34-20241102T1737_vemco-detections.kml')
tag_ids = st_layers('~/Downloads/ru34-20241102T1737_vemco-detections.kml')[,1]
tag_sum = tibble(tag_layers) %>% dplyr::select(name, features)
write.csv(tag_sum, file="tags_Nov_Dec_2024_summary.csv")

# read each tag id layer
layers_sf = list()
for(i in 1:length(tag_ids)) {
  layers_sf[[tag_ids[i]]] = read_sf(dsn = '~/Downloads/ru34-20241102T1737_vemco-detections.kml', 
                                    layer = tag_ids[i])
}

tag_df = sf::st_as_sf(data.table::rbindlist(layers_sf))
tag_df$date = as.Date(
  tolower(
  sapply(strsplit(sapply(strsplit(tag_df$Description,"<br>"),head,1),"DATE: "),tail,1)),
  format = "%d-%b-%Y %h:%M:%s")

# a plot f the first to test the list object `layers_sf`:
#plot(layers_sf[[1]])

# path<-"~/Downloads/ru34-20241102T1737_vemco-detections.kml"
# gdb<-st_layers(path)
# list_of_features<-purrr::map(gdb$name,~st_read(dsn=path,layer=.))

# receiver sync tags
sync_tag_id = c("61099","61140","61141","61142","61143","61146","61148","61149",
                "61150","61153","61154","61155","61156","61157","61158","61159",
                "61160","61161","61162","61163","61164","61165","61171","61188",
                "61196","61197","61212","61215","61219","61221","61243","61245",
                "65006","65008")
#sync_tags = tag_layers[lapply(tag_layers, layer_name %in% sync_tag_id)] 

# Ali's tags
FreyTags <- read_excel("Downloads/FreyAllSMASTTags.xlsx")
FreyTags$id = sapply(strsplit(FreyTags$'Tag ID', "-"), tail, 1)
matches = FreyTags[FreyTags$id %in% tag_ids,]
Frey_tags = dplyr::filter(tag_sum, name %in% matches$id)

