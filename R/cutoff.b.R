
# This file is a generated template, your changes will not be overwritten
source("R/utils.R")
source("R/tables.R")
cutoffClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "cutoffClass",
    inherit = cutoffBase,
    active = list(
        isBinary = function() {
            if (is.null(private$.isBinary)) {
                if (!is.null(self$options$dep)) {
                    data <- self$data
                    dep <- self$options$dep
                    private$.testLevels <- levels(data[[dep]])
                    private$.isBinary <- is.factor(data[[dep]]) &&
                                (!is.null(private$.testLevels) && length(private$.testLevels) == 2 || !jmvcore::canBeNumeric(data[[dep]]))
                } else {
                    private$.isBinary <- NULL
                }
            }
            private$.isBinary
        },
        testLevels = function() {
            if (!is.null(self$isBinary) && self$isBinary == FALSE && is.null(private$.testLevels)) {
                private$.testLevels <- levels(self$data[[self$options$dep]])
            }
            private$.testLevels
        },
        refLevels = function() {
            if (is.null(private$.refLevels)) {
                if (!is.null(self$options$classVar)) {
                    private$.refLevels <- levels(self$data[[self$options$classVar]])
                }
            }
            private$.refLevels
        },
        contingencyHeader = function() {
            if (is.null(private$.contingencyHeader)) {
                private$.contingencyHeader <- private$.makeContingencyHeader()
            }
            private$.contingencyHeader
        },
        cutoffs = function() {
            if (is.null(private$.cutoffs)) {
                private$.cutoffs <- private$.makeCutoffs()
            }
            private$.cutoffs
        }
    ),
    private = list(
        .isBinary = NULL,
        .testLevels = NULL,
        .cutoffs = NULL,
        .refLevels = NULL,
        .contingencyHeader = NULL,
        .init = function() {
            data <- self$data
            dep <- self$options$dep
            classVar <- self$options$classVar
            isBinaryFactor <- FALSE
            testlevels <- NULL
            sumtable <- self$results$summary

            self$results$procedure$setContent(paste0(
                .("<h4>Procedure Notes</h4>"),
                .("<p>Please begin by selecting your test/exposure variable and reference test/disease variable.</p>"),
                .("<p>The test/exposure variable can be of any measurement type. The level selector and cutoff editor will be enabled or disabled automatically based on the variable type.</p>"),
                .("<p>Ordinal variables with more than two levels will be treated as continuous, and require at least one cutoff value to be specified.</p>"),
                .("<p>You may define multiple cutoff values for continuous or ordinal test variables.</p>"),
                .("<p>Ordinal variables with exactly two levels will be treated as dichotomous.</p>")
            ))

            validCutoff <- FALSE
            for (c in seq_along(self$cutoffs)) {
                cutoff <- self$cutoffs[[c]]
                if (is.null(cutoff$val)) next
                validCutoff <- TRUE
                gkey <- paste(cutoff$var, c, sep = "-")
                groups <- self$results$cutoffs
                groups$addItem(key = gkey)
                group <- groups$get(key = gkey)
                crow <- private$.makeContingencyRow(cutoff)
                init_contingency_table(self, group$contingency,
                    ptitle = crow$varTitle,
                    superTitle = self$contingencyHeader$superTitle,
                    refpos = self$contingencyHeader$refpos,
                    refneg = self$contingencyHeader$refneg,
                    testpos = crow$testpos,
                    testneg = crow$testneg
                )
                init_diagnostic_table(self, group$diagnostics)
                init_relative_table(self, group$relative)
                init_posttest_table(self, group$posttest)
            }
            self$results$procedure$setVisible(!validCutoff)
        },
        .run = function() {
            if(is.null(self$options$dep) || is.null(self$options$classVar)) return()
            classVar <- self$options$classVar
            refpos <- self$options$event
            dep <- self$options$dep
            data <- self$data[!is.na(self$data[[dep]]), ]
            self$results$summary$setRow(rowKey = dep, values = list(
                type = self$cutoffs[[1]]$methodTitle,
                n = length(data[[dep]]),
                miss = length(self$data[[dep]]) - length(data[[dep]])
            ))
            for (c in seq_along(self$cutoffs)) {
                cutoff <- self$cutoffs[[c]]
                if (is.null(cutoff$val)) next
                gkey <- paste(cutoff$var, c, sep = "-")
                group <- self$results$cutoffs$get(key = gkey)

                tp <- 0
                fp <- 0
                tn <- 0
                fn <- 0

                dirOpps <- list(">=" = "<", ">" = "<=", "<" = ">=", "<=", ">")
                if (cutoff$method == "factor") {
                    tp <- sum(data[[cutoff$var]] == cutoff$val & data[[classVar]] == refpos)
                    fp <- sum(data[[cutoff$var]] == cutoff$val & data[[classVar]] != refpos)
                    tn <- sum(data[[cutoff$var]] != cutoff$val & data[[classVar]] != refpos)
                    fn <- sum(data[[cutoff$var]] != cutoff$val & data[[classVar]] == refpos)
                } else {
                    tp <- sum(eval(parse(text = paste("data[[cutoff$var]]", cutoff$dir, cutoff$val))) & data[[classVar]] == refpos)
                    fp <- sum(eval(parse(text = paste("data[[cutoff$var]]", cutoff$dir, cutoff$val))) & data[[classVar]] != refpos)
                    tn <- sum(eval(parse(text = paste("data[[cutoff$var]]", dirOpps[[cutoff$dir]], cutoff$val))) & data[[classVar]] != refpos)
                    fn <- sum(eval(parse(text = paste("data[[cutoff$var]]", dirOpps[[cutoff$dir]], cutoff$val))) & data[[classVar]] == refpos)
                }
                cont <- group$contingency
                cont$setRow(rowNo = 1, list(refpos = tp, refneg = fp, total = tp + fp))
                cont$setRow(rowNo = 2, list(refpos = fn, refneg = tn, total = fn + tn))
                cont$setRow(rowNo = 3, list(refpos = tp + fn, refneg = fp + tn, total = tp + fp + fn + tn))
                if (self$options$diagnostic)
                    populate_diagnostic_table(
                        table = group$diagnostics,
                        tp = tp, fp = fp, tn = tn, fn = fn
                    )

                if (self$options$relative)
                    populate_relative_table(
                        self,
                        table = group$relative,
                        tp = tp, fp = fp, tn = tn, fn = fn
                    )

                if (self$options$posttest) {
                    p <- populate_posttest_table(
                        table = group$posttest,
                        tp = tp, fp = fp, tn = tn, fn = fn,
                        pretest = self$options$pretest
                    )
                    if (!is.null(p$fagan) && self$options$fagan)
                        group$fagan$setState(p$fagan)
                }
            }
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) {
                return()
            }
            draw_fagan_nomogram(self, image$state, ggtheme, theme)
        },
        .makeCutoffs = function() {
            c <- list()
            if (!is.null(self$isBinary)) {
                if (self$isBinary == TRUE) {
                    c[[1]] <- list(
                        var = self$options$dep,
                        method = "factor",
                        methodTitle = .("Factor"),
                        dir = NULL,
                        val = self$options$testpos
                    )
                } else {
                    for (i in seq_along(self$options$cutoffs)) {
                        c[[i]] <- list(
                            var = self$options$dep,
                            method = "cutoff",
                            methodTitle = .("Numeric"),
                            dir = if (is.null(self$options$cutoffs[[i]]$direction)) ">=" else self$options$cutoffs[[i]]$direction,
                            val = self$options$cutoffs[[i]]$cutoff
                        )
                    }
                }
            }
            c
        },
        .makeContingencyHeader = function() {
            header <- list(
                refpos = .("Positive"),
                refneg = .("Negative"),
                superTitle = .("Referance Test / Disease")
            )
            if (!is.null(self$refLevels)) {
                refpos <- self$options$event
                refneg <- if (refpos == self$refLevels[2]) self$refLevels[1] else self$refLevels[2]
                header <- list(
                    refpos = refpos,
                    refneg = refneg,
                    superTitle = self$options$classVar
                )
            }
            header
        },
        .makeContingencyRow = function(cutoff) {
            rows <- list(
                varTitle = .("New Test /<br>Exposure"),
                testpos = .("Positive"),
                testneg = .("Negative")
            )
            dirOpps <- list(">=" = c("≥", "<"), ">" = c(">", "≤"), "<" = c("<", "≥"), "<=" = c("≤", ">"))
            if (!is.null(self$isBinary)) {
                if (self$isBinary == TRUE) {
                    rows <- list(
                        testpos = cutoff$val,
                        testneg = if (cutoff$val == self$testLevels[2]) self$testLevels[1] else self$testLevels[2],
                        varTitle = cutoff$var
                    )
                } else {
                    rows <- list(
                        testpos = paste(dirOpps[[cutoff$dir]][1], cutoff$val),
                        testneg = paste(dirOpps[[cutoff$dir]][2], cutoff$val),
                        varTitle = cutoff$var
                    )
                }
            }
            rows
        })
)
