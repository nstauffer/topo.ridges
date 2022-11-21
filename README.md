# topo.ridges
## Create Topographic Maps With Density Ridgelines In R
This package is absolutely not on CRAN so you can install it using `devtools::install_github("nstauffer/topo.ridges")`.

If you're looking for freely-available elevation data in North America, check out the [USDA NRCS Geospatial Data Gateway](https://datagateway.nrcs.usda.gov/).

### Example
```r
#### Read in data ####
# Read in the raster
elev_raster <- raster::raster("path/to/raster.tif")

#### Crop the raster (optional) ####
# Grab the current extent of the raster
frame_extent <- raster::extent(elev_raster)

# Manually change the extent here to include only what should be mapped
# Make sure you're using the correct units for your raster!
frame_extent@xmax <- -106.46
frame_extent@xmin <- -106.62
frame_extent@ymax <- 32.42
frame_extent@ymin <- 32.18

# Crop the raster to the new extent
elev_raster_crop <- raster::crop(x = elev_raster,
                                 y = frame_extent)

# Check to see that looks right
# Adjust your new frame extent and recrop if it doesn't
raster::plot(elev_raster_crop)

#### Raster aggregation (optional) ####
# Reduce the resolution of the raster
# This isn't necessary but can make a huge difference for processing time
# This example uses fact = 2 and so the result has 1/2 the cells of the original
elev_raster_lowres <- raster::aggregate(elev_raster_crop,
                                        fact = 2)

#### Create and save the map ####
# This will reformat the data as necessary and create the ggplot object to save
# See the documentation for explanations of the arguments
map <- topo.ridges::ridgemap(elev_data = elev_raster_lowres,
                             line_color = "white",
                             background_color = "gray10",
                             line_count = 300,
                             y_scalar = 75,
                             scale_factor = 1.25,
                             line_weight = 0.5,
                             min_height = 0)

# This saves a square version of the map as an SVG
# If you change the device argument to something like "png"" don't forget to
# also change the extension in the filename
ggsave(filename = "map.svg",
       path = "output/path/here",
       plot = map,
       device = "svg",
       width = 10,
       height = 10)
```
