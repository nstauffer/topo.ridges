#' Convert a matrix to a data frame
#' @description Convert a matrix of elevation data where columns represent the x component and rows represent the y to a data frame.
#' @param matrix The matrix of elevation values to be converted.
#' @return A data frame with the variables "x", "y", and "elev".
#' @export
matrix_to_dataframe <- function(matrix){
  # Convert the matrix to a data frame
  df <- as.data.frame(matrix)

  # The y values run the wrong way, so flip those
  df[["y"]] <- nrow(df):1

  # Convert to a long/tall format
  df_tall <- tidyr::pivot_longer(data = df,
                                 cols = tidyr::matches("[^y]"),
                                 names_to = "x",
                                 values_to = "elev")

  # Clean up the x values
  df_tall[["x"]] <- as.numeric(gsub(df_tall[["x"]],
                                    pattern = "[A-z]",
                                    replacement = ""))

  df_tall
}

#' Convert an elevation raster to a data frame
#' @description Convert a raster where cell values correspond to elevation to a data frame where each cell is an observation with the variables "x", "y", and "elev".
#' @param elev_raster The elevation raster to convert.
#' @return A data frame with the variables "x", "y", and "elev".
#' @export
raster_to_dataframe <- function(elev_raster){
  if (!("RasterLayer" %in% class(elev_raster))) {
    stop("elev_raster must be a raster.")
  }

  # First step is a matrix
  elev_matrix <- raster::as.matrix(elev_raster)

  # Zero out NA values
  elev_matrix[is.na(elev_matrix)] <- 0

  # Normalize the values
  elev_min <- min(elev_matrix)
  elev_matrix <- elev_matrix - elev_min
  elev_matrix[elev_matrix < 0] <- 0

  # Convert from matrix to data frame
  elev_dataframe <- matrix_to_dataframe(elev_matrix)

  elev_dataframe
}

# #' Return values from a kernel applied to a matrix
# #' @description Specify the dimensions of a rectangular kernel to scan the given matrix with
# # Note that the kernel is rectangular
# kernel_scan <- function(matrix,
#                         search_distance_x = 50,
#                         search_distance_y = 20,
#                         search_value_min = 10,
#                         search_value_max = Inf,
#                         return_value = 1.5) {
#   n_row <- nrow(matrix)
#   n_col <- ncol(matrix)
#
#   min_y_vector <- sapply(X = 1:n_row,
#                          search_distance = search_distance_y,
#                          FUN = function(X, search_distance){
#                            max(1, X - search_distance)
#                          })
#   max_y_vector <- sapply(X = 1:n_row,
#                          max = n_row,
#                          search_distance = search_distance_y,
#                          FUN = function(X, max, search_distance){
#                            min(max, X + search_distance)
#                          })
#
#   min_x_vector <- sapply(X = 1:n_col,
#                          search_distance = search_distance_x,
#                          FUN = function(X, search_distance){
#                            max(1, X - search_distance)
#                          })
#   max_x_vector <- sapply(X = 1:n_col,
#                          max = n_col,
#                          search_distance = search_distance_x,
#                          FUN = function(X, max, search_distance){
#                            min(max, X + search_distance)
#                          })
#
#   df <- data.frame(x = unlist(lapply(X = 1:n_col, times = n_row, FUN = rep)),
#                    y = rep(1:n_row, times = n_col),
#                    min_x = unlist(lapply(X = min_x_vector, times = n_row, FUN = rep)),
#                    max_x = unlist(lapply(X = max_x_vector, times = n_row, FUN = rep)),
#                    min_y = rep(min_y_vector, times = n_col),
#                    max_y = rep(max_y_vector, times = n_col))
#
#
#   check_vector <- sapply(X = 1:nrow(df),
#                          df = df,
#                          matrix = matrix,
#                          search_value_max = search_value_max,
#                          search_value_min = search_value_min,
#                          FUN = function(X, df, matrix, search_value_max, search_value_min){
#                            current_value <- matrix[df[X, "y"], df[X, "x"]]
#                            if (current_value >= search_value_min & current_value <= search_value_max) {
#                              result <- FALSE
#                            } else {
#                              x_range <- df[X, "min_x"]:df[X, "max_x"]
#                              y_range <- df[X, "min_y"]:df[X, "max_y"]
#                              current_matrix <- matrix[y_range, x_range]
#                              result <- any(current_matrix >= search_value_min & current_matrix <= search_value_max)
#                            }
#                            return(result)
#                          })
#
#   output_vector <- rep(0, times = length(check_vector))
#   output_vector[check_vector] <- return_value
#
#   output_matrix <- matrix(output_vector, ncol = n_col)
#
#   return(output_matrix)
# }

#' Create a topographic ridgeline map from elevation data
#' @description Using elevation data in the form of a raster, matrix, or data frame, create a map using ridgeline plotting.
#' @param elev_data Raster, matrix, or data frame. The elevation data. If a raster, the value in each cell must represent the elevation. If a matrix, the values must be the elevations and the rows and columns must correspond to the x and y coordinates, respectively. If a data frame, there must be the variables \code{"x"}, \code{"y"}, and \code{"elev"} containing the x coordinates, y coordinates, and elevations.
#' @param line_color Character string. The color for the ridgelines. Must be recognizable by \code{ggplot}, e.g. \code{"white"} or \code{"#ffffff"}. Defaults to \code{"white"}.
#' @param background_color Character string. The color for the ridgelines. Must be recognizable by \code{ggplot}, e.g. \code{"black"} or \code{"#000000"}. Defaults to \code{"gray10"}.
#' @param line_count Optional numeric. The number of ridgelines to draw. If this is \code{NULL} or greater than or equal to the number of available latitude lines in the data, all possible lines will be drawn. If it is fewer than the available latitudes, only this many lines will be drawn, using an evenly-spaced subset of the data. Defaults to \code{NULL}.
#' @param scale_factor Numeric. The vertical scaling factor to apply to the elevations in the plot. Larger values result in more exaggerated topography. Defaults to \code{1}.
#' @param line_weight Numeric. The thickness of the ridgelines. Used as the \code{size} argument in the \code{geom_ridges()} call. Defaults to \code{0.5}.
#' @param min_height. Numeric. The minimum height to display. Used by \code{geom_ridges()}. Defaults to \code{0}.
#' @return A ridgeline elevation map as a ggplot object
#' @export
ridgemap <- function(elev_data,
                     line_color = "white",
                     background_color = "gray10",
                     line_count = NULL,
                     y_scalar = 100,
                     scale_factor = 1,
                     line_weight = 0.5,
                     min_height = 0){
  if (!any(class(elev_data) %in% c("RasterLayer", "data.frame", "matrix"))) {
    stop("elev_data must be one of the following: raster, data frame, or matrix")
  }

  if ("RasterLayer" %in% class(elev_data)) {
    elev_df <- raster_to_dataframe(elev_data)
  } else if ("matrix" %in% class(elev_elev_data)) {
    elev_df <- matrix_to_dataframe(elev_matrix)
  } else {
    elev_df <- elev_data
  }

  current_lines <- length(unique(elev_df[["y"]]))

  if (is.null(line_count)) {
    plotting_data <- elev_df
    line_count <- current_lines
  } else {
    if (line_count > current_lines) {
      plotting_data <- elev_df
      line_count <- current_lines
      warning("Only ", current_lines, " lines of latitude available.")
    } else {
      y_values <- round((1:line_count) * (current_lines / line_count))
      plotting_data <- elev_df[elev_df[["y"]] %in% y_values, ]
      # I don't know whether it'll be runs of x values or runs of y values
      # So, if the first two y values are the same, assume all y values are in runs
      if (elev_df[["y"]][1] == elev_df[["y"]][2]) {
        plotting_data[["y"]] <- as.vector(sapply(X = length(unique(plotting_data[["y"]])):1,
                                                 times = length(unique(plotting_data[["x"]])),
                                                 FUN = function(X, times){
                                                   rep(X, times = times)
                                                 }))
      } else {
        plotting_data[["y"]] <- rep(c(length(unique(plotting_data[["y"]])):1),
                                    times = length(unique(plotting_data[["x"]])))
      }
    }
  }

  ridge_map <- ggplot2::ggplot(plotting_data) +
    ggridges::geom_ridgeline(ggplot2::aes(x = x,
                                          y = y * y_scalar,
                                          group = y,
                                          height = elev),
                             min_height = min_height,
                             scale = scale_factor,
                             color = line_color,
                             size = line_weight,
                             fill = background_color) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = background_color),
                   panel.background = ggplot2::element_rect(fill = background_color,
                                                            color = background_color),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(c(0,0,0,0),"mm"),
                   axis.line = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(0,0,0,0),"mm"),
                   legend.position = "none") +
    ggplot2::labs(x = NULL,
                  y = NULL)

  ridge_map
}

ridgemap_water <- function(elev_matrix,
                           water_matrix = NULL,
                           land_color = "gray20",
                           water_color = "gray70",
                           background_color = "white",
                           line_count = NULL,
                           scale_factor = 1,
                           size = 0.5,
                           min_height = 0,
                           search_distance_x = 10,
                           search_distance_y = 3,
                           search_value_min = NULL,
                           search_value_max = NULL,
                           return_value = NULL){

  if (is.null(water_matrix)) {
    if (is.null(search_value_min)) {
      search_value_min <- min_height
    }
    if (is.null(search_value_max)) {
      search_value_max <- Inf
    }
    if (is.null(return_value)) {
      return_value <- min_height + 0.01
    }
    water_matrix <- kernel_scan(matrix = elev_matrix,
                                search_distance_x = search_distance_x,
                                search_distance_y = search_distance_y,
                                search_value_min = search_value_min,
                                search_value_max = search_value_max,
                                return_value = return_value)
  }

  water_df <- matrix_to_dataframe(water_matrix)

  elev_df <- matrix_to_dataframe(elev_matrix)

  if (is.null(line_count)) {
    plotting_data_water <- water_df
    line_count <- length(unique(water_df[["y"]]))
  } else {
    current_lines <- length(unique(water_df[["y"]]))
    y_values <- round((1:line_count) * (current_lines / line_count))
    plotting_data_water <- water_df[water_df[["y"]] %in% y_values, ]
    plotting_data_water[["y"]] <- rep(c(length(unique(plotting_data_water[["y"]])):1), times = length(unique(plotting_data_water[["x"]])))
  }

  ridgemap <- ggplot2::ggplot(plotting_data_water) +
    ggridges::geom_ridgeline(ggplot2::aes(x = x,
                                          y = y,
                                          group = y,
                                          height = elev),
                             min_height = min_height,
                             scale = scale_factor,
                             color = water_color,
                             size = size,
                             fill = background_color) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = background_color),
                   panel.background = ggplot2::element_rect(fill = background_color,
                                                            color = background_color),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(c(0,0,0,0),"mm"),
                   axis.line = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(0,0,0,0),"mm"),
                   legend.position = "none") +
    ggplot2::labs(x = NULL,
                  y = NULL)


  if (is.null(line_count)) {
    plotting_data_land <- elev_df
    line_count <- length(unique(elev_df[["y"]]))
  } else {
    current_lines <- length(unique(elev_df[["y"]]))
    y_values <- round((1:line_count) * (current_lines / line_count))
    plotting_data_land <- elev_df[elev_df[["y"]] %in% y_values, ]
    plotting_data_land[["y"]] <- rep(c(length(unique(plotting_data_land[["y"]])):1), times = length(unique(plotting_data_land[["x"]])))
  }

  ridgemap <- ridgemap + ggridges::geom_ridgeline(data = plotting_data_land,
                                                  ggplot2::aes(x = x,
                                                               y = y,
                                                               group = y,
                                                               height = elev),
                                                  min_height = min_height,
                                                  scale = scale_factor,
                                                  color = land_color,
                                                  size = size,
                                                  fill = background_color)

  ridgemap
}