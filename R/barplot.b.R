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

            if (self$options$bar_type == "stacked") {
                barPlot <- stackedBarPlotSingle(data, var=var, options=self$options, ggtheme=ggtheme)
            } else {
                barPlot <- barPlotSingle(data, var=var, options=self$options, ggtheme=ggtheme)
            }
            
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
                    { if (show_na) dplyr::mutate_if(., is.factor, addNA) else tidyr::drop_na(.) } %>%
                    dplyr::mutate_if(is.factor, forcats::fct_explicit_na, na_level = "(Missing)") %>%
                    dplyr::group_by_all(.drop = FALSE) %>%
                    dplyr::tally(name = "count") %>% 
                    dplyr::mutate(freq = count/sum(count)) %>%
                    dplyr::mutate_at(dplyr::vars(freq), ~replace(., is.nan(.), 0)) %>%
                    dplyr::mutate(is_missing = factor(var=="(Missing)", levels=c(TRUE, FALSE))) %>%
                    setter::copy_attributes(self$data[[var]], "jmv-desc")
            }

            return(dataList)
        },
        .plotSize = function(var) {
            
            if (self$options$bar_type == "grouped")
                size <- plotSizeBarPlotSingle(self$data, var, self$options)
            else
                size <- plotSizeStackedBarPlotSingle(self$data, var, self$options)
            
            return(size)
        })
)
