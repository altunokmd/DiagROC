source("R/utils.R")
init_diagnostic_table <- function(self, table) {
    dNames <- c(
        .("Sensitivity"), .("Specificity"), .("Positive Likelihood Ratio"), .("Negative Likelihood Ratio"),
        .("Prevalence"), .("Positive Predictive Value"), .("Negative Predictive Value"), .("Accuracy")
    )
    for (n in seq_along(dNames)) table$setRow(rowNo = n, list(text = dNames[n]))
}

populate_diagnostic_table <- function(table, ...) {
    ret <- c("sens", "spec", "plr", "nlr", "prev", "ppv", "npv", "acc")
    values <- roc_diagnostic(ret = ret, ...)
    for (n in seq_along(ret)) {
        table$setRow(rowNo = n, list(
            result = values[[ret[n]]]["estimate"],
            ci.l = values[[ret[n]]]["lower"],
            ci.u = values[[ret[n]]]["upper"]
        ))
    }
}

init_contingency_table <- function(self, table, ptitle = "", superTitle = "", refpos = "", refneg = "", testpos = "", testneg = "") {
    table$addColumn(name = "ptitle", title = "", type = "text", combineBelow = TRUE)
    table$addColumn(name = "stitle", title = "", type = "text")
    table$addColumn(name = "refpos", title = refpos, type = "integer", superTitle = superTitle)
    table$addColumn(name = "refneg", title = refneg, type = "integer", superTitle = superTitle)
    table$addColumn(name = "total", title = .("Total"), type = "integer")
    table$addFormat(rowNo = 3, 1, jmvcore::Cell.BEGIN_END_GROUP)
    table$setRow(rowNo = 1, list(stitle = testpos, ptitle = ptitle))
    table$setRow(rowNo = 2, list(stitle = testneg, ptitle = ptitle))
    table$setRow(rowNo = 3, list(stitle = .("Total"), ptitle = ptitle))
}

init_relative_table <- function(self, table) {
    rNames <- c(
        .("Relative Risk"), .("Absolute Risk Reduction (ARR)"), .("Relative Risk Reduction (RRR)"), .("Number Needed To Treat (NNT)"), .("Odds Ratio")
    )
    for (n in seq_along(rNames)) table$setRow(rowNo = n, list(text = rNames[n]))
    table$addFormat(rowNo = 2, 1, jmvcore::Cell.BEGIN_GROUP)
    table$addFormat(rowNo = 5, 1, jmvcore::Cell.BEGIN_END_GROUP)
}
populate_relative_table <- function(self, table, ...) {
    rr <- relative_risk(...)
    table$setRow(rowNo = 1, list(
        result = sprintf("%.2f", rr$rr),
        ci = paste(sprintf("%.2f", rr$ci_rr), collapse = " - "),
        z = rr$z_rr,
        p = rr$p_rr
    ))
    table$setRow(rowNo = 2, list(
        result = sprintf("%.2f", rr$arr),
        ci = paste(sprintf("%.2f", rr$ci_arr), collapse = " - ")
    ))
    table$setRow(rowNo = 3, list(result = sprintf("%.2f %%", abs(rr$rrr) * 100)))
    if (!is.null(rr$nnt)) {
        table$setRow(rowNo = 4, list(
            result = paste0(sprintf("%.2f", abs(rr$nnt)), ifelse(rr$nnt < 0, .(" (Harm)"), .(" (Benefit)"))),
            ci = paste0(
                sprintf("%.2f", abs(rr$ci_nnt[1])),
                ifelse(rr$ci_nnt[1] < 0, .(" (Harm)"), .(" (Benefit)")),
                " -",
                ifelse(rr$ci_nnt[1] < 0 && rr$ci_nnt[2] > 0, " ∞ -", ""),
                sprintf(" %.2f", abs(rr$ci_nnt[2])),
                ifelse(rr$ci_nnt[2] < 0, .(" (Harm)"), .(" (Benefit)"))
            )
        ))
    }
    table$setRow(rowNo = 5, list(
        result = sprintf("%.2f", rr$odds),
        ci = paste(sprintf("%.2f", rr$ci_odds), collapse = " - "),
        z = rr$z_odds,
        p = rr$p_odds
    ))
}

init_posttest_table <- function(self, table) {
    table$setRow(rowNo = 1, list(text = .("Diagnostic Test Positive")))
    table$setRow(rowNo = 2, list(text = .("Diagnostic Test Negative")))
    table$addFormat(rowNo = 1, 1, jmvcore::Cell.BEGIN_END_GROUP)
    table$addFormat(rowNo = 2, 1, jmvcore::Cell.BEGIN_END_GROUP)
}

populate_posttest_table <- function(table, ...) {
    pt <- posttest_probability(...)
    table$setRow(rowNo = 1, list(result = pt$positive))
    table$setRow(rowNo = 2, list(result = pt$negative))
    pt
}

draw_fagan_nomogram <- function(self, data, ggtheme, theme) {
    library("ggplot2")
    log_odds <- function(o) log10(o / (1 - o))
    ticks_pc <- c(0.1, 0.2, 0.3, 0.5, 0.7, 1, 2, 3, 5, 7, 10, 20, 30, 40, 50,
                60, 70, 80, 90, 93, 95, 97, 98, 99, 99.3, 99.5, 99.7, 99.8, 99.9)
    ticks <- ticks_pc / 100

    ticks_logodds <- log_odds(ticks)
    ticks_middle <- sort(c(10^(-3:3), 2 * (10^(-3:2)), 5 * (10^(-3:2))))
    ticks_logmiddle <- log10(ticks_middle)

    plot <- ggplot(data) +
        geom_line(aes(x = x, y = lo_y, color = line), size = 1) +
        geom_segment(aes(x = 0.5, xend = 0.5, y = 1.75, yend = -1.75), color = "#666666") +
        annotate(geom = "text",
                x = rep(0.6, length(ticks_logmiddle)),
                y = ticks_logmiddle / 2,
                label = ticks_middle) +
        annotate(geom = "text", x = 0.5, y = 2, label = .("Likelihood Ratio")) +
        annotate(geom = "point",
                x = rep(0.5, length(ticks_logmiddle)),
                y = ticks_logmiddle / 2,
                size = 1,
                color = "#666666") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0),
                    limits = range(ticks_logodds),
                    breaks = -ticks_logodds,
                    labels = ticks_pc,
                    name = .("Pretest Probability (%)"),
                    sec.axis = dup_axis(name = .("Posttest Probability (%)"),
                        labels = ticks_pc,
                        breaks = ticks_logodds)) +
        geom_line(aes(x = x, y = lo_y),
            data = data.frame(x = c(0, 1), lo_y = c(data$lo_y[1], -data$lo_y[1])),
            color = "#999999",
            inherit.aes = FALSE,
            lty = 2) +
        annotate(geom = "text",
            x = 0.25,
            y = -2.25,
            label = paste(
                paste0(.("Pretest Prob."), " = ", signif(data$y[1] * 100, digits = 3), "%"),
                paste(.("PLR"), "=", signif(data$detail[1], digits = 4)),
                paste(.("NLR"), "=", signif(data$detail[2], digits = 4)),
                paste0(.("Posttest Positive"), " = ", signif(data$y[2] * 100, digits = 3), "%"),
                paste0(.("Posttest Negative"), " = ", signif(data$y[4] * 100, digits = 3), "%"),
            sep = "\n")) +
        ggtheme +
        scale_color_manual(values = c(
            "positive" = "#24c68e",
            "negative" = "#de342e"
        )) +
        theme(
            legend.position = "none",
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank()
        )
    plot
}