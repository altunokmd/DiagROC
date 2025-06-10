
# This file is a generated template, your changes will not be overwritten
source("./R/utils.R")
source("./R/tables.R")
manualClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "manualClass",
    inherit = manualBase,
    private = list(
        .validateInputs = function() {
            values <- c(self$options$tp, self$options$tn, self$options$fp, self$options$fn)
            if (any(sapply(values, is.null)) || any(values < 0)) return(FALSE)
            if (self$options$fp + self$options$tn <= 0) return(FALSE)
            if (self$options$tp + self$options$fp <= 0) return(FALSE)
            if (self$options$tp + self$options$fn <= 0) return(FALSE)
            if (self$options$fn + self$options$tn <= 0) return(FALSE)
            TRUE
        },
        .init = function() {
            cont <- self$results$contingency
            cont$setRow(rowNo = 1, list(ptext = .("New Test /<br>Exposure"), stext = .("Positive")))
            cont$setRow(rowNo = 2, list(ptext = .("New Test /<br>Exposure"), stext = .("Negative")))
            cont$setRow(rowNo = 3, list(ptext = .("New Test /<br>Exposure"), stext = .("Total")))
            cont$addFormat(rowNo = 3, 1, jmvcore::Cell.BEGIN_END_GROUP)

            init_diagnostic_table(self, self$results$diagnostics)
            init_relative_table(self, self$results$relative)
            init_posttest_table(self, self$results$posttest)
        },
        .run = function() {
            if (!private$.validateInputs()) return()
            tp <- self$options$tp
            fp <- self$options$fp
            tn <- self$options$tn
            fn <- self$options$fn

            cont <- self$results$contingency
            cont$setRow(rowNo = 1, list(refPos = tp, refNeg = fp, total = tp + fp))
            cont$setRow(rowNo = 2, list(refPos = fn, refNeg = tn, total = fn + tn))
            cont$setRow(rowNo = 3, list(refPos = tp + fn, refNeg = fp + tn, total = tp + fp + fn + tn))

            if (self$options$diagnostic)
                populate_diagnostic_table(
                    table = self$results$diagnostics,
                    tp = tp, fp = fp, tn = tn, fn = fn
                )

            if (self$options$relative)
                populate_relative_table(
                    self,
                    table = self$results$relative,
                    tp = tp, fp = fp, tn = tn, fn = fn
                )

            if (self$options$posttest) {
                p <- populate_posttest_table(
                    table = self$results$posttest,
                    tp = tp, fp = fp, tn = tn, fn = fn,
                    pretest = self$options$pretest
                )
                if (!is.null(p$fagan) && self$options$fagan)
                    self$results$fagan$setState(p$fagan)
            }
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) {
                return()
            }
            draw_fagan_nomogram(self, image$state, ggtheme, theme)
        })
)
