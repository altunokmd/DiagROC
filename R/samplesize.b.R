
# This file is a generated template, your changes will not be overwritten

samplesizeClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "samplesizeClass",
    inherit = samplesizeBase,
    private = list(
        .init = function() {
            snewt <- self$results$snewresult # nolint: indentation_linter.
            snewt$setRow(rowNo = 1, values = list(text = .("N(sens)")))
            snewt$setRow(rowNo = 2, values = list(text = .("N(spec)")))

            snullt <- self$results$snullresult
            snullt$setRow(rowNo = 1, values = list(text = .("N(sens)")))
            snullt$setRow(rowNo = 2, values = list(text = .("N(spec)")))
            snullt$setRow(rowNo = 3, values = list(text = .("N(sens, adjusted)")))
            snullt$setRow(rowNo = 4, values = list(text = .("N(spec, adjusted)")))
            snullt$setRow(rowNo = 5, values = list(text = .("N(max)")))
            snullt$addFormat(rowNo = 5, 1, jmvcore::Cell.BEGIN_END_GROUP)

            compt <- self$results$compresult
            compt$setRow(rowNo = 1, values = list(text1 = .("Unpaired Design"), text2 = .("N")))
            compt$setRow(rowNo = 2, values = list(text1 = .("Unpaired Design"), text2 = .("N (Total)")))
            compt$setRow(rowNo = 3, values = list(text1 = .("Paired Design"), text2 = .("N (Ψ_min)")))
            compt$setRow(rowNo = 4, values = list(text1 = .("Paired Design"), text2 = .("N (Ψ_max)")))
            compt$setRow(rowNo = 5, values = list(text1 = .("Paired Design"), text2 = .("Mean")))
            compt$addFormat(rowNo = 1, 1, jmvcore::Cell.BEGIN_GROUP)
            compt$addFormat(rowNo = 2, 1, jmvcore::Cell.END_GROUP)
            compt$addFormat(rowNo = 3, 1, jmvcore::Cell.BEGIN_GROUP)
            compt$addFormat(rowNo = 5, 1, jmvcore::Cell.END_GROUP)

            w <- self$results$whentouse
            c <- self$results$comments
            m <- self$options$mode
            wtitle <- .("<h3>When to use?</h3>")
            ctitle <- .("<h3>Comments</h3>")
            if (m == "singlenew") {
                w$setContent(paste(.("<h2>Single Test Design, new diagnostic test</h2>"), wtitle, .("<p>This formula is used to estimate the sample size when the new diagnostic test is compared with the reference standard in a cohort where the true disease status and prevalence is known.</p>")))
                c$setContent(paste(ctitle, .("<p><strong>Sens or Spec</strong>: pre-determined value ascertained by previous published data or clinician experience/judgment</p><p><strong>Marginal error</strong>: maximum error of the estimate with a confidence level of 95%.</p><p><strong>Prevalence</strong>: disease prevalence in the study population.</p><p>Estimated sample sizes will be different for the same sensitivity and specificity if the disease prevalence is not 50%, or when the number subjects with and without the disease are not equal.</p>")))
            } else if (m == "singlenull") {
                w$setContent(paste(.("<h2>Single Test Design, comparing the accuracy of a single test to a null value</h2>"), wtitle, .("<p>This formula is used to estimate the sample size when the diagnostic test is compared with the reference standard in a cohort where the true disease status and prevalence is <strong>unknown</strong>.</p><p>After the using the Equations, calculated values should be adjusted according to disease prevelance.</p>")))
                c$setContent(paste(ctitle, .("<p><strong>Sens or Spec</strong>: pre-determined value ascertained by previous published data or clinician experience/judgment</p><p><strong>Marginal error</strong>: maximum error of the estimate with a confidence level of 95%.</p><p><strong>Prevalence</strong>: disease prevalence in the study population.</p><p>Estimated sample sizes will be different for the same sensitivity and specificity if the disease prevalence is not 50%, or when the number subjects with and without the disease are not equal.</p>")))
            } else if (m == "comparetwo") {
                w$setContent(paste(.("<h2>Studies comparing two diagnostic tests</h2>"), wtitle, .("<h5>Unpaired (between-subjects) design</h5><p>Participants are randomly assigned to either the index or comparator test.</p><h5>Paired (within-subjects) design</h5><p>Two comparator tests are applied to all subjects along with the reference standard</p>")))
                c$setContent(paste(ctitle, .("<p>One-sided N is preferred since we want to test if one of the paths are different than the other</p><p><strong>Ψ (min)</strong> where the disagreement is minimum (P2-P1)</p><p><strong>Ψ (max)</strong> where the agreement is by chance, or disagreement is maximum</p><p>Cont.Correction are the values where the Yates' continuity correction was applied</p>")))
            }
        },
        .run = function() {
            mode <- self$options$mode
            if (mode == "singlenew") {
                alpha <- self$options$alpha_snew
                ss <- self$options$ss_snew
                prev <- self$options$prev_snew
                me <- self$options$me_snew
                if (is.null(alpha) || is.null(ss) || is.null(prev) || is.null(me)) return()

                alpha <- as.numeric(alpha)
                ss <- as.numeric(ss) / 100
                prev <- as.numeric(prev) / 100
                me <- as.numeric(me) / 100

                z <- qnorm(1 - alpha / 2)
                n <- (z * z * ss * (1 - ss)) / (me * me)

                snewt <- self$results$snewresult
                snewt$setRow(rowNo = 1, values = list(n = round(n / prev, digits = 0)))
                snewt$setRow(rowNo = 2, values = list(n = round(n / (1 - prev), digits = 0)))
            } else if (mode == "singlenull") {
                power <- self$options$power_snull
                alpha <- self$options$alpha_snull
                sens1 <- self$options$sens1_snull
                sens0 <- self$options$sens0_snull
                spec1 <- self$options$spec1_snull
                spec0 <- self$options$spec0_snull
                prev <- self$options$prev_snull
                if (is.null(power) || is.null(alpha) || is.null(sens1) || is.null(sens0) || is.null(spec1) || is.null(spec0) || is.null(prev)) return()

                power <- as.numeric(power)
                alpha <- as.numeric(alpha)
                sens1 <- as.numeric(sens1) / 100
                sens0 <- as.numeric(sens0) / 100
                spec1 <- as.numeric(spec1) / 100
                spec0 <- as.numeric(spec0) / 100
                prev <- as.numeric(prev) / 100

                zp <- abs(qnorm(1 - power))
                za <- abs(qnorm(alpha / 2))
                nsens <- ((za * sqrt(sens0 * (1 - sens0)) + zp * sqrt(sens1 * (1 - sens1)))^2) / ((sens1 - sens0)^2)
                nsens_cont <- (nsens / 4) * (1 + sqrt(1 + 4 / (nsens * abs(sens1 - sens0))))^2

                nspec <- ((za * sqrt(spec0 * (1 - spec0)) + zp * sqrt(spec1 * (1 - spec1)))^2) / ((spec1 - spec0)^2)
                nspec_cont <- (nspec / 4) * (1 + sqrt(1 + 4 / (nspec * abs(spec1 - spec0))))^2

                snullt <- self$results$snullresult
                snullt$setRow(rowNo = 1, values = list(n = round(nsens, digits = 0), cap = .("diseased"), ncont = round(nsens_cont, digits = 0)))
                snullt$setRow(rowNo = 2, values = list(n = round(nspec, digits = 0), cap = .("not-diseased"), ncont = round(nspec_cont, digits = 0)))
                snullt$setRow(rowNo = 3, values = list(n = round(nsens / prev, digits = 0), cap = .("total subjects"), ncont = round(nsens_cont / prev, digits = 0)))
                snullt$setRow(rowNo = 4, values = list(n = round(nspec / (1 - prev), digits = 0), cap = .("total subjects"), ncont = round(nspec_cont / (1 - prev), digits = 0)))
                snullt$setRow(rowNo = 5, values = list(
                    n = round(max(nsens / prev, nspec / (1 - prev)), digits = 0),
                    cap = .("total subjects"),
                    ncont = round(max(nsens_cont / prev, nspec_cont / (1 - prev)), digits = 0)
                ))
            } else if (mode == "comparetwo") {
                power <- self$options$power_comp
                alpha <- self$options$alpha_comp
                prop1 <- self$options$prop1_comp
                prop2 <- self$options$prop2_comp
                dismin <- self$options$dismin_comp
                dismax <- self$options$dismax_comp
                if (is.null(power) || is.null(alpha) || is.null(prop1) || is.null(prop2) || is.null(dismin) || is.null(dismax)) return()

                power <- as.numeric(power)
                alpha <- as.numeric(alpha)
                prop1 <- as.numeric(prop1) / 100
                prop2 <- as.numeric(prop2) / 100
                dismin <- as.numeric(dismin) / 100
                dismax <- as.numeric(dismax) / 100

                zp <- abs(qnorm(power))
                za <- abs(qnorm(alpha))

                avg_prop <- (prop1 + prop2) / 2
                n_unpaired <- (za * sqrt(2 * avg_prop * (1 - avg_prop)) + zp * sqrt(prop1 * (1 - prop1) + prop2 * (1 - prop2)))^2 / (prop1 - prop2)^2
                n_unpaired_cont <- (n_unpaired / 4) * (1 + sqrt(1 + 4 / (n_unpaired * abs(prop1 - prop2))))^2
                n_paired_min <- (za * sqrt(dismin) + zp * sqrt(dismin - (prop2 - prop1)^2))^2 / (prop1 - prop2)^2
                n_paired_min_cont <- (n_paired_min / 4) * (1 + sqrt(1 + 4 / (n_paired_min * abs(prop1 - prop2))))^2
                n_paired_max <- (za * sqrt(dismax) + zp * sqrt(dismax - (prop2 - prop1)^2))^2 / (prop1 - prop2)^2
                n_paired_max_cont <- (n_paired_max / 4) * (1 + sqrt(1 + 4 / (n_paired_max * abs(prop1 - prop2))))^2

                compt <- self$results$compresult
                compt$setRow(rowNo = 1, values = list(cap = .("for each group"), n = round(n_unpaired, digits = 0), ncont = round(n_unpaired_cont, digits = 0)))
                compt$setRow(rowNo = 2, values = list(cap = .("for the study"), n = round(n_unpaired * 2, digits = 0), ncont = round(n_unpaired_cont * 2, digits = 0)))
                compt$setRow(rowNo = 3, values = list(cap = .("for each study"), n = round(n_paired_min, digits = 0), ncont = round(n_paired_min_cont, digits = 0)))
                compt$setRow(rowNo = 4, values = list(cap = .("for each study"), n = round(n_paired_max, digits = 0), ncont = round(n_paired_max_cont, digits = 0)))
                compt$setRow(rowNo = 5, values = list(cap = .("for each study"), n = round((n_paired_min + n_paired_max) / 2, digits = 0), ncont = round((n_paired_min_cont + n_paired_max_cont) / 2, digits = 0)))
            }
        })
)
