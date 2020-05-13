#' @importFrom magrittr %>%
surveyPlotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "surveyPlotClass",
    inherit = surveyPlotBase,
    private = list(
        #### Init + run functions ----
        .init = function() {
            
            vars <- self$options$vars
            plotsAll <- self$results$plots
            
            if (length(vars) == 0)
                plotsAll$addItem(key='')
            
            for (var in vars) {
                image <- plotsAll$get(key=var)
                size <- private$.plotSize(var)
                image$setSize(size$width, size$height)
            }
        },
        .run = function() {
            
            data <- private$.prepareData()
            private$.prepareSurveyPlot(data)
            
        },
        
        #### Plot functions ----
        .prepareSurveyPlot = function(data) {
            
            vars <- self$options$vars
            plotsAll <- self$results$plots
            
            for (var in vars) {
                image <- plotsAll$get(key=var)
                image$setState(list(data=data[[var]], var=var))
            }
        },
        .surveyplot = function(image, ggtheme, theme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            data <- image$state$data
            var <- image$state$var
            
            if (self$options$type == "stacked") {
                barPlot <- stackedSingle(data, var=var, options=self$options, ggtheme=ggtheme)
            } else {
                barPlot <- groupedSingle(data, var=var, options=self$options, ggtheme=ggtheme)
            }
            
            return(barPlot)
            
        },
        
        #### Helper functions ----
        .prepareData = function() {
            
            vars <- self$options$vars
            group <- self$options$group
            hideNA <- self$options$hideNA
            
            dataList <- list()
            for (var in vars) {
                print(attr(self$data[[var]], "jmv-missings"))
                dataList[[var]] <- self$data %>%
                    dplyr::select("group" = group, "var" = var) %>%
                    { if (hideNA) tidyr::drop_na(.) else dplyr::mutate_if(., is.factor, addNA) } %>%
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
            
            if (self$options$type == "grouped")
                size <- plotSizeGroupedSingle(self$data, var, self$options)
            else
                size <- plotSizeStackedSingle(self$data, var, self$options)
            
            return(size)
        })
)
