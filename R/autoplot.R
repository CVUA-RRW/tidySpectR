#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Plot a collection of spectra in their current processing state.
#'
#' @param object A `collection` object.
#' @param type A single character string for the type of plot to
#'   produce with values "raw", "average" or "label_average". See details.
#' @param offset_x x axis offset value (in the axis unit) for the spectra
#' @param offset_y y axis offset value (in the axis unit) for the spectra
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
#' @importFrom ggplot2 theme_set theme_bw
#' @export
autoplot.collection <- function(object,
                            type="raw", 
                            offset_x = 0,
                            offset_y = 0,
                            ...) {
    
    if (type == "label_average" & length(levels(object$labels$label))==0){
        rlang::abort("You choose a grouping by labels but no labels are defined")
    }
    
    # Produce main plot
    theme_set(theme_bw())
    
    if (type == "average"){
        main <- average_plot(dat, ...)
    } else if (type == "label_average"){
        main <- average_plot_by_labels(dat, offset_x, offset_y, ...)
    } else {
        main <- all_plot(dat, offset_x, offset_y, ...)
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