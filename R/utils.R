DESC_SEP_GROUPED_BAR <- 90
DESC_SEP_STACKED_BAR <- 100


groupedSingle = function(data, var, options, ggtheme) {
    group <- options$group
    grouped <- !is.null(group)
    freq <- options$freq
    labels <- options$labels
    
    description <-
        stringr::str_wrap(attr(data, "jmv-desc"), DESC_SEP_GROUPED_BAR)
    barWidth <- 0.65
    pd <- ggplot2::position_dodge(0.85)
    
    if (grouped) {
        fillGroup <- "group"
    } else {
        fillGroup <- "is_missing"
    }
    
    yLabel <- ifelse(freq == "count", "count", "freq")
    
    if (freq == "count")
        yTitle <- "Frequency (N)"
    else
        yTitle <- "Frequency (%)"
    
    barPlot <- data %>%
        ggplot2::ggplot(ggplot2::aes(
            x = forcats::fct_rev(var),
            y = get(yLabel),
            fill = forcats::fct_rev(get(fillGroup))
        )) +
        ggplot2::geom_bar(stat = "identity",
                          position = pd,
                          width = barWidth) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, .1))) +
        ggtheme +
        ggplot2::labs(
            title = var,
            subtitle = description,
            fill = group,
            y = yTitle
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, byrow = TRUE)) +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_text(size = 14),
            axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_text(hjust = 0),
            axis.text.x = ggplot2::element_text(),
            plot.title = ggplot2::element_text(hjust = 0),
            plot.subtitle =  ggplot2::element_blank(),
            plot.title.position = "plot",
            legend.position = "none",
            legend.text = ggplot2::element_text(size = 11),
            legend.title = ggplot2::element_text(size = 14),
            axis.ticks = ggplot2::element_blank()
        )
    
    if (options$desc && length(description) > 0)
        barPlot = barPlot +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(
                hjust = 0,
                size = 13,
                face = "italic",
                lineheight = 1.15,
                margin = ggplot2::margin(0, 0, 15, 0)
            )
        )
    
    if (grouped)
        barPlot = barPlot + ggplot2::theme(legend.position = "bottom")
    
    aesFreq <- function (freq) {
        gen_offset <- 0.015
        if (freq == "count") {
            offset <- max(data$count) * gen_offset
            return(ggplot2::aes(y = get(yLabel) + offset, label = count))
        } else {
            offset <- max(data$freq) * gen_offset
            return(ggplot2::aes(
                y = get(yLabel) + offset,
                label = sprintf("%1.0f%%", 100 * freq)
            ))
        }
    }
    
    if (labels == "inPlot") {
        barPlot = barPlot + ggplot2::geom_text(
            aesFreq(freq),
            position = pd,
            hjust = 0,
            size = 4
        ) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.line.x = ggplot2::element_blank()
            )
    } else if (freq == "perc") {
        barPlot = barPlot + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    }
    
    return(barPlot)
    
}


stackedSingle = function(data, var, options, ggtheme) {
    group <- options$group
    grouped <- !is.null(group)
    freq <- options$freq
    labels <- options$labels
    
    description <-
        stringr::str_wrap(attr(data, "jmv-desc"), DESC_SEP_STACKED_BAR)
    yLabel <- ifelse(freq == "count", "count", "freq")
    
    if (freq == "count")
        yTitle <- "Frequency (N)"
    else
        yTitle <- "Frequency (%)"
    
    if (grouped) {
        barPlot <-
            ggplot2::ggplot(data,
                            ggplot2::aes(
                                x = forcats::fct_rev(group),
                                y = get(yLabel),
                                fill = forcats::fct_rev(var)
                            ))
    } else {
        barPlot <-
            ggplot2::ggplot(data,
                            ggplot2::aes(
                                x = "a",
                                y = get(yLabel),
                                fill = forcats::fct_rev(var)
                            ))
    }
    
    barPlot <-
        barPlot + ggplot2::geom_bar(stat = "identity", width = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, .1))) +
        ggtheme +
        ggplot2::labs(title = var,
                      subtitle = description,
                      y = yTitle) +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, byrow = TRUE)) +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_text(size = 14),
            axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(),
            plot.title = ggplot2::element_text(hjust = 0),
            plot.subtitle =  ggplot2::element_blank(),
            plot.title.position = "plot",
            legend.position = "bottom",
            legend.text = ggplot2::element_text(size = 11),
            legend.title = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()
        )
    
    if (grouped)
        barPlot = barPlot + ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = 0))
    
    if (options$desc && length(description) > 0)
        barPlot = barPlot +
        ggplot2::theme(
            plot.subtitle = ggplot2::element_text(
                hjust = 0,
                size = 13,
                face = "italic",
                lineheight = 1.15,
                margin = ggplot2::margin(0, 0, 15, 0)
            )
        )
    
    aesFreq <- function (freq) {
        if (freq == "count") {
            return(ggplot2::aes(label = count))
        } else {
            return(ggplot2::aes(label = sprintf("%1.0f%%", 100 * freq)))
        }
    }
    
    if (labels == "inPlot") {
        barPlot = barPlot +
            ggplot2::geom_text(
                data = dplyr::filter(data, count != 0),
                aesFreq(freq),
                position = ggplot2::position_stack(vjust = 0.5),
                size = 3
            )
        
        barPlot = barPlot +
            ggplot2::theme(
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank(),
                axis.line.x = ggplot2::element_blank()
            )
        
    } else if (freq == "perc") {
        barPlot = barPlot + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    }
    
    return(barPlot)
}

rainCloud = function(data, var, options, ggtheme) {
    
    group <- options$group
    description <- stringr::str_wrap(attr(data, "jmv-desc"), DESC_SEP_STACKED_BAR)
    
    raincloudPlot <- 
        ggplot2::ggplot(data = data, ggplot2::aes(y = var, x = group, fill = group)) +
        geom_flat_violin(position = ggplot2::position_nudge(x = .2, y = 0), alpha = .8) +
        ggplot2::geom_point(
            ggplot2::aes(y = var, color = group),
            position = ggplot2::position_jitter(width = .15),
            size = .5,
            alpha = 0.8
        ) +
        ggplot2::geom_boxplot(
            width = .15,
            guides = FALSE,
            outlier.shape = NA,
            alpha = 0.5
        ) +
        # expand_limits(x = 3.5) +
        ggplot2::guides(fill = FALSE, color = FALSE) +
        ggplot2::coord_flip() + ggtheme
    
    return(raincloudPlot)
}

plotSizeGroupedSingle = function(data, var, options) {
    group <- options$group
    grouped <- !is.null(group)
    bar_labels <- options$labels
    hideNA <- options$hideNA
    sub_title <- options$desc
    
    desc_lines <- attr(data[[var]], "jmv-desc") %>%
        stringr::str_wrap(DESC_SEP_GROUPED_BAR) %>%
        stringr::str_count("\n") %>% magrittr::add(1)
    
    if (grouped) {
        levelsGroup <- levels(data[[group]])
        nLevelsGroup <- length(levelsGroup)
        sizeLegend <- 50 + 25 * ceiling(nLevelsGroup / 4)
    } else {
        levelsGroup <- 1
        nLevelsGroup <- 1
        sizeLegend <- 0
    }
    
    if (bar_labels == "onAxis") {
        sizeXAxis <- 50
    } else {
        sizeXAxis <- 25
    }
    
    levelsVar <- levels(data[[var]])
    nLevelsVar <- length(levelsVar)
    
    if (!hideNA)
        nLevelsVar <- nLevelsVar + 1
    
    sizeGroup <- 25
    sizeBars <- sizeGroup * nLevelsGroup * nLevelsVar
    
    sizeTitleMain <- 75
    
    if (sub_title && length(desc_lines) > 0) {
        sizeTitleSub <- 35 + 25 * desc_lines
    } else {
        sizeTitleSub <- 0
    }
    
    sizeTitle <- sizeTitleMain + sizeTitleSub
    
    width <- 550
    height <- sizeTitle + sizeBars + sizeXAxis + sizeLegend
    
    return(list(width = width, height = height))
}

plotSizeStackedSingle = function(data, var, options) {
    group <- options$group
    grouped <- !is.null(group)
    bar_labels <- options$labels
    hideNA <- options$hideNA
    sub_title <- options$desc
    desc_lines <- attr(data[[var]], "jmv-desc") %>%
        stringr::str_wrap(DESC_SEP_STACKED_BAR) %>%
        stringr::str_count("\n") %>% magrittr::add(1)
    
    if (grouped) {
        levelsGroup <- levels(data[[group]])
        nLevelsGroup <- length(levelsGroup)
        if (!hideNA)
            nLevelsGroup <- nLevelsGroup + 1
    } else {
        levelsGroup <- 1
        nLevelsGroup <- 1
    }
    
    if (bar_labels == "onAxis") {
        sizeXAxis <- 50
    } else {
        sizeXAxis <- 35
    }
    
    levelsVar <- levels(data[[var]])
    nLevelsVar <- length(levelsVar)
    sizeLegend <- 50 + 25 * ceiling(nLevelsVar / 4)
    
    sizeGroup <- 35
    sizeBars <- sizeGroup * nLevelsGroup
    
    sizeTitleMain <- 40
    
    if (sub_title  && length(desc_lines) > 0) {
        sizeTitleSub <- 55 + 20 * desc_lines
    } else {
        sizeTitleSub <- 0
    }
    
    sizeTitle <- sizeTitleMain + sizeTitleSub
    
    width <- 600
    height <- sizeTitle + sizeBars + sizeXAxis + sizeLegend
    
    return(list(width = width, height = height))
}

plotSizeRainCloud = function(data, var, options) {
    return(list(width = 550, height = 600))
}