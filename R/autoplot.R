#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Plot a collection of spectra in their current processing state.
#'
#' @param object A `collection` object.
#' @param type A single character string for the type of plot to
#'   produce with values "raw", "average" or "label_average". See details.
#' @param limits A numeric vector of size two, used to crop the data.
#' @param enlarge A numeric vector of size two, used to extract 
#'   a portion of the data to display enlarged in a window overlayed on
#'   the main plot.
#' @param offset_x x axis offset value (in the axis unit) for the spectra
#' @param offset_y y axis offset value (in the axis unit) for the spectra
#' @param show_bins A boolean controlling whether to show bin limits.
#' @param ... Not currently used.
#' @return a `ggplot` object
#' @details
#' A `raw` plot displays individual spectra, combine it with offset values
#'   to separate individual spectra along the x and/or y axes.
#'
#' An `average` plot will represent only the average spectra and the range of 
#'   values within each bin. Offset values will be ignored.
#'
#' A `label_average` plot will display the label-average spectrum and range of values 
#'   within each label. Use offset values to separate the spectra.
#' 
#' Further plot customization is possible (although limited) by passing ggplot 
#'   methods to the returned object
#'
#' @importFrom ggplot2 ggplotGrob annotation_custom geom_vline theme geom_rect element_blank
#' @export
autoplot.collection <- function(object,
                            type="raw", 
                            limits = c(-Inf, Inf),
                            enlarge = NULL,
                            offset_x = 0,
                            offset_y = 0,
                            show_bins = FALSE,
                            ...) {
    if (type == "label_average" & length(levels(object$labels$label))==0){
        rlang::abort("You choose a grouping by labels but no labels are defined")
    }
    
    # crop dataset
    dat <- object %>% extract(from = limits[1], to = limits[2])
    
    # Produce main plot
    if (type == "average"){
        main <- average_plot(dat, ...)
    } else if (type == "label_average"){
        main <- average_plot_by_labels(dat, offset_x, offset_y, ...)
    } else {
        main <- all_plot(dat, offset_x, offset_y, ...)
    }
    
    # Add bins
    if (show_bins){
        breaks <- pull_breaks(dat)
        
        main <- main + 
                geom_vline(xintercept = breaks,
                           size = 0.1,
                           color = 'grey80',
                           alpha = 0.7)
    }
    
    # Produce box
    if (!is.null(enlarge)){
        subdat <- object %>% extract(from = enlarge[1], to = enlarge[2])
        
        if (type == "average"){
            zoom <- average_plot(subdat, ...)
        } else if (type == "label_average"){
            zoom <- average_plot_by_labels(subdat, offset_x, offset_y, ...)
        } else {
            zoom <- all_plot(subdat, offset_x, offset_y, ...)
        }
        
        # Remove axis titles on zoombox
        zoom <- zoom + 
                theme(legend.position = "none",
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank()) 
        zoombox <- ggplotGrob(zoom)
        
        # Add frame to main
        main <- main +
                geom_rect(data = NULL, 
                          aes(xmin = min(enlarge), 
                              xmax = max(enlarge), 
                              ymin= min(dat$data$values) * 1.1,
                              ymax = max(subdat$data$values) * 1.2),
                          color = "black", 
                          alpha=0, size= 0.2)
        
        x_max <- max(dat$data$bins)
        y_max <- max(dat$data$values)
        
        box_x_min <- x_max - (0.01 * x_max) - x_max/4
        box_x_max <- x_max - (0.01 * x_max)
        box_y_min <- y_max - (0.01 * y_max) - y_max/4
        box_y_max <- y_max - (0.01 * y_max)
        
        main <- main +
                annotation_custom(grob = zoombox,
                                  xmin = box_x_min,
                                  xmax = box_x_max,
                                  ymin = box_y_min,
                                  ymax = box_y_max)
    }
    
    return(main)
}

#' @importFrom ggplot2 ggplot aes geom_line labs
all_plot <- function(dat, offset_x, offset_y, ...){
    # Offset
    dat <- dat$data %>%
           mutate(x_offset = (as.numeric(id) -1 ) * offset_x,
                  y_offset = (as.numeric(id) - 1 ) * offset_y)
    
    # Plotting
    p <- ggplot(dat, aes (x = bins + x_offset,
                          y = values + y_offset)) +
         geom_line(aes(color = id),
                   size = 0.1,
                   alpha = 0.5)+
         labs(x = "bins", y = "intensity")
}

#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs
#' @importFrom dplyr group_by summarise n
average_plot <- function(dat, ...){
    dat <- dat$data %>% 
        group_by(bins) %>%
        summarise(count = n(),
                  average = mean(values),
                  min_val = min(values),
                  max_val = max(values),
                  .groups= 'drop'
                  )
    
    # Plotting
    p <- ggplot(dat) +
            geom_line(aes(x = bins, y = average), size = 0.2)+
            geom_ribbon(aes(x = bins, 
                            ymin = min_val, 
                            ymax = max_val),
                        alpha = 0.6, linetype = 0) +
            labs(x = "bins", y= "mean intenssity and range")

    return(p)
}

#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs
#' @importFrom dplyr group_by summarise n
average_plot_by_labels <- function(dat, offset_x, offset_y, ...){
    #bind labels
    dat <- inner_join(dat$data, dat$labels, by = 'id') %>%
           group_by(label, bins) %>%
           summarise(count = n(),
                  average = mean(values),
                  min_val = min(values),
                  max_val = max(values),
                  .groups = 'drop'
                  )
    
    # Offsetting data 
    dat <- dat %>%
           mutate(x_offset = (as.numeric(label) -1 ) * offset_x,
                  y_offset = (as.numeric(label) - 1 ) * offset_y)
           
    
    # Plotting
    p <- ggplot(dat) +
            geom_line(aes(x = bins + x_offset, 
                          y = average + y_offset, 
                          color = label), 
                      size = 0.2) +
            geom_ribbon(aes(x = bins + x_offset, 
                            ymin = min_val + y_offset,
                            ymax = max_val + y_offset,
                            color = label, 
                            fill = label),
                        alpha = 0.6, linetype = 0) +
            labs(x = "bins", y= "Mean intensity and range")

    return(p)
}