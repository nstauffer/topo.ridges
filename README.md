# topo.ridges
## Create Topographic Maps With Density Ridgelines In R


### Example
```r
library(topo.ridges)

## Create a data frame of coordinates from a raster and a polygon shapefile to crop it
# The only arguments you MUST provide values for are raster/raster.path, if there's no cropping to be done
# This is for a state park within a raster of a mountain range
elev.df <- elev.density.df(raster.path = "rasters/mountain_range.tif",
                           frame.path = "shapefiles/state_park.shp",
                           # I'm not taking just a proportion of the latitude lines
                           y.resolution = 1,
                           # I am however only taking an equally-spaced 50 latitude lines to plot
                           n.y = 50,
                           # I'm not dropping any longitudinal lines
                           x.resolution = 1,
                           n.x = NULL,
                           # Because I can amp up the scaling when plotting, I'm halving the relative elevation values
                           # This will also reduce the output data frame length by half
                           elev.scale = 0.5,
                           # Wherever there are NA values in my raster, replace them with 0
                           na.sub = 0)
                           
## Convert a data frame of coordinates into a ridgeline plot
# Again, the only REQUIRED argument is the data
plot <- topo.ridges.plot(data = elev.df,
                         # The x and y variables in my data frame are called "x" and "y"
                         x.variable = "x",
                         y.variable = "y",
                         # Exaggerate the elevation by a factor of 10 to make it pop
                         scale = 10,
                         # Use a line weight of 1
                         size = 1,
                         # Make the backdrop black
                         fill = "black",
                         # Color the lines with a gradient, getting slightly darker in back
                         line.gradient.start = "white",
                         line.gradient.end = "gray90",
                         # For some reason, sometimes the y values from elev.density.raster() are inverted? This will let you correct that
                         invert.y = TRUE)

## Write out the plot
# I usually write out plots as .SVG files and convert to .PNG for smoother lines
# But! Direct to .PNG works fine
ggplot2::ggsave("figures/example_topo_plot.png",
                device = "png")
```
