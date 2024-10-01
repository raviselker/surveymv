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
                    df <- self$data %>%
                        dplyr::select(group=!!group, var=!!var) %>%
                        { if (hideNA) tidyr::drop_na(.) else dplyr::mutate_if(., is.factor, addNA) } %>%
                        { if (hideNA) . else  dplyr::mutate_if(., is.factor, forcats::fct_na_value_to_level, level = "(Missing)") } %>%
                        dplyr::mutate_if(is.factor, private$.ellipsifyLevels) %>%
                        dplyr::group_by_all(.drop = FALSE) %>%
                        dplyr::tally(name = "count") %>% 
                        dplyr::mutate(freq = count/sum(count)) %>%
                        dplyr::mutate_at(dplyr::vars(freq), ~replace(., is.nan(.), 0)) %>%
                        dplyr::mutate(is_missing = factor(var=="(Missing)", levels=c(TRUE, FALSE)))
                    
                    attr(df$var, "jmv-desc") <- attr(self$data[[var]], "jmv-desc")
                    
                    if (! is.null(group))
                        attr(df$group, "jmv-desc") <- attr(self$data[[group]], "jmv-desc")
                    
                    dataList[[var]] <- df
                } else {
                    cols <- c("group"="A", "var"="var")
                    
                    dataList[[var]] <- self$data %>%
                        dplyr::select(group=!!group, var=!!var) %>% 
                        tibble::add_column(!!!cols[setdiff(names(cols), names(.))]) %>%
                        dplyr::mutate_if(is.factor, private$.ellipsifyLevels) %>%
                        dplyr::mutate_if(is.factor, forcats::fct_rev) %>%
                        jmvcore::naOmit()
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
        #' Truncates levels of a vector when they exceed a certain width.
        #' 
        #' @description 
        #' Adds an ellipsis in the middle of the level if the the level exceeds the provided width. 
        #'
        #' @param column a factor for which the levels need to be shortened
        #' @param width the maximum number of characters of the levels
        #'
        #' @return a factor with (potentially) shortened levels
        .ellipsifyLevels = function(column, width = 25) {
            levels(column) <- make.unique(
                stringr::str_trunc(levels(column), width, side="center"), 
                sep=" - "
            )
            return(column)
        })
)
