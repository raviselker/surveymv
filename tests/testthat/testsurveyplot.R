testthat::context('surveyplot')

testthat::test_that('surveyPlot doesn\'t return an error for variables with no values', {

    data <- data.frame('A' = factor(),
                       'B' = factor(),
                       'C' = factor())

    sink("/dev/null")
    
    # grouped plot
    plot <- surveymv::surveyPlot(data, vars(A), type = 'grouped')
    testthat::expect_error(print(plot[[1]]), NA)
    
    # stacked plot (show missing)
    plot <- surveymv::surveyPlot(data, vars(A), type = 'stacked')
    testthat::expect_error(print(plot[[1]]), NA)
    
    # stacked plot (hide missings)
    plot <- surveymv::surveyPlot(data, vars(A), type = 'stacked', hideNA = TRUE)
    testthat::expect_error(print(plot[[1]]), NA)
    
    sink()
})
