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
                image$setState(list(data=data[[1]][[var]], var=var, factor=data[[2]][[var]]))
            }
        },
        .surveyplot = function(image, ggtheme, theme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            data <- image$state$data
            var <- image$state$var
            factor <- image$state$factor
            
            # return empty plot if variable does not contain values
            if (nrow(data) == 0 || (nrow(data) == 1 && !self$options$hideNA)) {
                barPlot <- ggplot2::ggplot() + ggplot2::theme_void()
            } else if ( ! factor) {
                barPlot <- rainCloud(data, var=var, options=self$options, ggtheme=ggtheme)
            } else if (self$options$type == "stacked") {
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
            varTypes <- list()
            for (var in vars) {
                varTypes[[var]] <- is.factor( self$data[[var]] )
                
                if (varTypes[[var]]) {
                    dataList[[var]] <- self$data %>%
                        dplyr::select("group" = group, "var" = var) %>%
                        { if (hideNA) tidyr::drop_na(.) else dplyr::mutate_if(., is.factor, addNA) } %>%
                        dplyr::mutate_if(is.factor, forcats::fct_explicit_na, na_level = "(Missing)") %>%
                        dplyr::mutate_if(is.factor, private$.ellipsify) %>%
                        dplyr::group_by_all(.drop = FALSE) %>%
                        dplyr::tally(name = "count") %>% 
                        dplyr::mutate(freq = count/sum(count)) %>%
                        dplyr::mutate_at(dplyr::vars(freq), ~replace(., is.nan(.), 0)) %>%
                        dplyr::mutate(is_missing = factor(var=="(Missing)", levels=c(TRUE, FALSE))) %>%
                        setter::copy_attributes(self$data[[var]], "jmv-desc")
                } else {
                    cols <- c("group"="A", "var"="var")
                    
                    dataList[[var]] <- self$data %>%
                        dplyr::select("group" = group, "var" = var) %>% 
                        tibble::add_column(!!!cols[setdiff(names(cols), names(.))]) %>%
                        dplyr::mutate_if(is.factor, forcats::fct_rev) %>%
                        dplyr::mutate_if(is.factor, private$.ellipsify) %>%
                        jmvcore::naOmit() %>%
                        setter::copy_attributes(self$data[[var]], "jmv-desc")
                }
            }
            
            return(list(dataList, varTypes))
        },
        .plotSize = function(var) {
            
            if (! is.factor(self$data[[var]])) {
                size <- plotSizeRainCloud(self$data, var, self$options)
            } else if (self$options$type == "grouped")
                size <- plotSizeGroupedSingle(self$data, var, self$options)
            else
                size <- plotSizeStackedSingle(self$data, var, self$options)
            
            return(size)
        },
        .ellipsify = function(column, width = 25) {
            levels(column) <- stringr::str_trunc(levels(column), width)
            return(column)
        })
)
