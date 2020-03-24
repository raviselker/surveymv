#' @importFrom magrittr %>%
barPlotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "barPlotClass",
    inherit = barPlotBase,
    private = list(
        .init = function() {
            
            vars <- self$options$vars
            plotsAll <- self$results$plots
            
            for (var in vars) {
                image <- plotsAll$get(key=var)$barplot
                size <- private$.plotSize(var)
                image$setSize(size$width, size$height)
            }
        },
        .run = function() {
            
            unloadNamespace('jmv')
            unloadNamespace('ggplot2')
            requireNamespace('ggplot2')
            
            data <- private$.prepareData()
            private$.prepareBarPlot(data)

        },
        #### Plot functions ----
        .prepareBarPlot = function(data) {
            
            vars <- self$options$vars
            plotsAll <- self$results$plots
            
            for (var in vars) {
                image <- plotsAll$get(key=var)$barplot
                image$setState(list(data=data[[var]], var=var))
            }
        },
        .barplot = function(image, ggtheme, theme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            data <- image$state$data
            var <- image$state$var
            group <- self$options$group
            grouped <- ! is.null(group)
            bar_labels <- self$options$bar_labels
            
            desc <- "This question was about this and that and such and so and this and that this question was about this and that and such and so and this and that?"
            description <- stringr::str_wrap(desc, 96)
            barWidth <- 0.65
            pd <- ggplot2::position_dodge(0.85)
            
            if (grouped) {
                fill_group <- "group"
            } else {
                fill_group <- "is_missing"
            }
            
            subTitle <- FALSE
            yLabel <- ifelse(bar_labels == "count", "count", "freq")
            
            barPlot <- data %>%
                ggplot2::ggplot(ggplot2::aes(x=forcats::fct_rev(var),
                                             y=get(yLabel),
                                             fill=forcats::fct_rev(get(fill_group)))) + 
                ggplot2::geom_bar(stat="identity", position=pd, width = barWidth) + 
                ggplot2::coord_flip() +
                ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, .1))) +
                ggtheme +
                ggplot2::labs(title = var, subtitle = description, fill = group) +
                ggplot2::guides(fill=ggplot2::guide_legend(reverse=TRUE, byrow = TRUE)) +
                ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                               panel.grid.minor = ggplot2::element_blank(),
                               axis.title.x=ggplot2::element_blank(), 
                               axis.title.y=ggplot2::element_blank(),
                               axis.text.y = ggplot2::element_text(hjust = 0),
                               axis.text.x = ggplot2::element_blank(),
                               plot.title = ggplot2::element_text(hjust = 0),
                               plot.subtitle =  ggplot2::element_blank(),
                               plot.title.position="plot",
                               legend.position="none",
                               legend.text = ggplot2::element_text(size=11),
                               legend.title = ggplot2::element_text(size=14),
                               axis.ticks = ggplot2::element_blank(),
                               axis.line.x = ggplot2::element_blank())
            
            if (subTitle)
                barPlot = barPlot + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size=12, 
                                                                                         face = "italic", 
                                                                                         lineheight = 1.15,
                                                                                         margin = ggplot2::margin(0,0,15,0)))
            
            if (grouped)
                barPlot = barPlot + ggplot2::theme(legend.position="bottom")
            
            aes_bar_labels <- function (bar_labels) {
                gen_offset <- 0.015
                if (bar_labels == "count") {
                    offset <- max(data$count) * gen_offset
                    return(ggplot2::aes(y=get(yLabel) + offset, label = count))
                } else if (bar_labels == "freq") {
                    offset <- max(data$freq) * gen_offset
                    return(ggplot2::aes(y=get(yLabel) + offset, label = round(freq, 2)))
                } else {
                    offset <- max(data$freq) * gen_offset
                    return(ggplot2::aes(y=get(yLabel) + offset, label = sprintf("%1.0f%%", 100*freq)))
                }
            }
            
            barPlot = barPlot + ggplot2::geom_text(aes_bar_labels(bar_labels), position=pd, hjust=0, size = 4)
            
            # if (bar_labels == "count") {
            #     barPlot = barPlot + ggplot2::geom_text(ggplot2::aes(y=get(yLabel) + 0.2, label = count), 
            #                                            position=pd, hjust=0, size = 4)
            # } else if (bar_labels == "freq") {
            #     barPlot = barPlot + ggplot2::geom_text(ggplot2::aes(y=get(yLabel) + 0.2, label = round(freq, 2)), 
            #                                            position=pd, hjust=0, size = 4)
            # } else {
            #     barPlot = barPlot + ggplot2::geom_text(ggplot2::aes(y=get(yLabel) + 0.2, 
            #                                                         label = sprintf("%1.0f%%", 100*freq)), 
            #                                            position=pd, hjust=0, size = 4)
            # }
            
            return(barPlot)
            
        },
        #### Helper functions ----
        .prepareData = function() {
            
            vars <- self$options$vars
            group <- self$options$group
            show_na <- self$options$show_na
            
            dataList <- list()
            for (var in vars) {
                dataList[[var]] <- self$data %>%
                    dplyr::select("group" = group, "var" = var) %>%
                    { if (show_na) . else tidyr::drop_na(.) } %>%
                    dplyr::mutate_if(is.factor, forcats::fct_explicit_na, na_level = "(Missing)") %>%
                    dplyr::group_by_all(.drop = FALSE) %>%
                    dplyr::tally(name = "count") %>% 
                    dplyr::mutate(freq = count/sum(count)) %>%
                    dplyr::mutate(is_missing = factor(var=="(Missing)", levels=c(TRUE, FALSE)))
            }

            return(dataList)
        },
        .plotSize = function(var) {
            
            data <- self$data
            group <- self$options$group
            grouped <- ! is.null(group)
            show_na <- self$options$show_na
            sub_title <- FALSE
            
            if (grouped) {
                levelsGroup <- levels(data[[group]])
                sizeLegend <- 75
            } else {
                levelsGroup <- 1
                sizeLegend <- 0
            }
            
            levelsVar <- levels(data[[var]])
            
            nLevelsGroup <- length(levelsGroup)
            nLevelsVar <- length(levelsVar)
            
            if (show_na)
                nLevelsVar <- nLevelsVar + 1
            
            sizeGroup <- 25
            sizeBars <- sizeGroup * nLevelsGroup * nLevelsVar
            
            sizeTitleMain <- 75
            sizeTitleSub <- ifelse(sub_title, 75, 0)
            sizeTitle <- sizeTitleMain + sizeTitleSub

            width <- 550
            height <- sizeTitle + sizeBars + sizeLegend
            
            return(list(width=width, height=height))
            
        })
)
