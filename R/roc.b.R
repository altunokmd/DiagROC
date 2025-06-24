# This file is a generated template, your changes will not be overwritten
source("R/utils.R")
source("R/tables.R")
rocClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "rocClass",
        inherit = rocBase,
        private = list(
            roc_results = list(),
            levels = NULL,
            delong = list(),
            .validateInputs = function() {
                data <- self$data
                opts <- self$options
                if (is.null(data) || is.null(opts$deps) || is.null(opts$classVar) || is.null(opts$eventLevel)) {
                    return(FALSE)
                }
                classLevels <- base::levels(as.factor(data[[opts$classVar]]))
                if (length(classLevels) != 2) {
                    stop(.("Class variable must have exactly 2 levels."))
                }
                if (!(opts$eventLevel %in% classLevels)) {
                    stop(.("Selected event level is not valid."))
                }
                # for(col in opts$deps) { # nolint: indentation_linter.
                #     n_levels <- length(unique(na.omit(data[[opts$classVar]][!is.na(data[[col]])])))
                #     if(n_levels != 2)
                #         stop(jmvcore::format("The class variable doesn't have exactly 2 levels in rows where '{col}' is not missing.", col = col))
                # }
                if (self$options$eventLevel == classLevels[1]) {
                    classLevels <- rev(classLevels)
                }
                private$levels <- classLevels
                TRUE
            },
            .init = function() {
                if (!private$.validateInputs()) {
                    return()
                }

                data <- self$data
                cols <- self$options$deps
                classVar <- self$options$classVar
                vars <- self$results$varStats
                levels <- base::levels(as.factor(data[[classVar]]))
                positiveClass <- self$options$eventLevel
                negativeClass <- if (positiveClass == levels[1]) levels[2] else levels[1]

                for (col in cols) {
                    var <- vars$get(key = col)
                    c <- var$contingency
                    t <- var$coords
                    init_contingency_table(self, c, ptitle = col, superTitle = classVar, refpos = positiveClass, refneg = negativeClass)
                    var$curve$setSize(self$options$pWidth, self$options$pHeight)
                }

                if (self$options$compareRoc == TRUE && length(cols) > 1) {
                    self$results$delong$setVisible(visible = TRUE)
                } else {
                    self$results$delong$setVisible(visible = FALSE)
                }

                self$results$combinedCurve$setSize(self$options$pWidth, self$options$pHeight)
            },
            .run = function() {
                if (!private$.validateInputs()) {
                    return()
                }

                private$.compute_roc_all()
                if (self$options$compareRoc == TRUE) {
                    private$.compare_rocs()
                    delongT <- self$results$delong
                    for (i in names(private$delong)) {
                        delongT$addRow(rowKey = i, values = as.list(private$delong[[i]]))
                    }
                }

                cmbCurve <- self$results$combinedCurve
                aucT <- self$results$auc

                displayCurves <- self$options$displayCurves
                combineCurves <- self$options$combineCurves
                if (displayCurves && combineCurves) {
                    if (is.null(cmbCurve$state)) {
                        roc_data <- do.call(rbind, lapply(private$roc_results, function(df) df$plot))
                        cmbCurve$setState(roc_data)
                    }
                }

                for (col in names(private$roc_results)) {
                    r <- private$roc_results[[col]]
                    aucT$setRow(rowKey = col, values = r$auc)
                    vars <- self$results$varStats$get(key = col)
                    if (displayCurves && !combineCurves) {
                        image <- vars$curve
                        if (is.null(image$state)) {
                            image$setTitle(paste(.("ROC Curve:"), col))
                            image$setState(r$plot)
                        }
                    }

                    if (self$options$showCoords == TRUE) {
                        t <- vars$coords
                        for (i in seq_len(nrow(r$coords))) {
                            row <- r$coords[i, ]
                            t$addRow(rowKey = row$threshold, values = as.list(row))
                        }
                        t$setNote("d", sprintf(.("Positive if test %s cut-off (direction chosen to maximize AUC)."), ifelse(r$roc$direction == "<", "≥", "≤")))
                    }

                    if (self$options$showDiagnostics == TRUE) {
                        c <- vars$contingency
                        for (i in seq_len(nrow(r$cont))) {
                            c$setRow(rowNo = i, as.list(r$cont[i, ]))
                        }

                        populate_diagnostic_table(self, table = vars$diagnostics, tp = r$best$tp, fp = r$best$fp, tn = r$best$tn, fn = r$best$fn)
                    }
                }
            },
            .compare_rocs = function() {
                cols <- names(private$roc_results)
                nc <- length(cols)
                delong <- list()
                for (r1 in 1:nc) {
                    if (nc < r1 + 1) break
                    for (r2 in (r1 + 1):nc) {
                        roc1 <- private$roc_results[[cols[r1]]]$roc
                        roc2 <- private$roc_results[[cols[r2]]]$roc
                        test <- pROC::roc.test(roc1, roc2, method = "d", reuse.auc = TRUE, conf.level = 0.95)
                        delong[[paste0(r1, "-", r2)]] <- list(
                            pair = paste(cols[r1], "vs.", cols[r2]),
                            diff = roc1$auc - roc2$auc,
                            ci.l = test$conf.int[1],
                            ci.u = test$conf.int[2],
                            stat = test$statistic[1],
                            p = test$p.value
                        )
                    }
                }
                private$delong <- delong
            },
            .compute_roc_all = function() {
                private$roc_results <- lapply(self$options$deps, function(predictor) {
                    compute_single_roc(
                        self,
                        data = self$data,
                        classVar = self$options$classVar,
                        predictor = predictor,
                        levels = private$levels,
                        options = self$options
                    )
                }) |> setNames(self$options$deps)
            },
            .plot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return()
                }

                roc_data <- image$state
                gridLine <- element_blank()
                if (self$options$cGridLine) gridLine <- element_line(color = "#eeeeee", linewidth = 0.5)
                plot <- ggplot(roc_data, aes(x = fpr, y = tpr, color = Model)) +
                    geom_line(linewidth = 1.2) +
                    labs(x = .("1 - Specificity"), y = .("Sensitivity")) +
                    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
                    ggtheme +
                    theme(
                        panel.grid.major.y = gridLine,
                        panel.grid.major.x = gridLine,
                        panel.grid.minor.y = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        axis.line = element_line(color = "black"),
                        axis.ticks = element_line(linewidth = 2)
                    )
                if (self$options$cPoints) plot <- plot + geom_point(size = 1.3)
                if (self$options$cABLine) plot <- plot + geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "gray")
                plot
            }
        )
    )
}
