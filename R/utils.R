roc_diagnostic <- function(
    tp, fn, fp, tn,
    conf.level = 0.95,
    ret = c("sens", "spec", "plr", "nlr", "ppv", "npv", "prev", "acc"),
    percent = TRUE,
    dec = 2) {
  ret <- match.arg(ret, several.ok = TRUE)

  pr <- tp + fp
  nr <- tn + fn
  pc <- tp + fn
  nc <- tn + fp
  total <- tp + tn + fp + fn
  z <- qnorm(1 - (1 - conf.level) / 2)
  dlist <- list()

  format_ci <- function(est, lower, upper) {
    if (percent) {
      est <- sprintf("%.*f %%", dec, est * 100)
      lower <- sprintf("%.*f %%", dec, lower * 100)
      upper <- sprintf("%.*f %%", dec, upper * 100)
    }
    c(estimate = est, lower = lower, upper = upper)
  }

  binom_ci_percent <- function(x, n) {
    b <- binom.test(x, n)
    format_ci(as.numeric(b$estimate), b$conf.int[1], b$conf.int[2])
  }

  if ("sens" %in% ret) dlist$sens <- binom_ci_percent(tp, pc)
  if ("spec" %in% ret) dlist$spec <- binom_ci_percent(tn, nc)
  if ("prev" %in% ret) dlist$prev <- binom_ci_percent(pc, total)
  if ("acc" %in% ret) dlist$acc <- binom_ci_percent(tp + tn, total)

  if ("plr" %in% ret) {
    plr <- (tp / pc) / (1 - (tn / nc))
    se <- sqrt((1 / tp) - (1 / pc) + (1 / fp) - (1 / nc))
    ci <- exp(log(plr) + c(-1, 1) * z * se)
    dlist$plr <- c(estimate = plr, lower = ci[1], upper = ci[2])
  }

  if ("nlr" %in% ret) {
    nlr <- (1 - (tp / pc)) / (tn / nc)
    se <- sqrt((1 / fn) - (1 / pc) + (1 / tn) - (1 / nc))
    ci <- exp(log(nlr) + c(-1, 1) * z * se)
    dlist$nlr <- c(estimate = nlr, lower = ci[1], upper = ci[2])
  }

  if ("ppv" %in% ret) {
    ppv <- tp / pr
    logit <- log(ppv / (1 - ppv))
    se <- sqrt((1 - (tp / pc)) / (pc * tp / pc) + (tn / nc) / (nc * (1 - (tn / nc))))
    ci_logit <- logit + c(-1, 1) * z * se
    ci <- plogis(ci_logit)
    dlist$ppv <- format_ci(ppv, ci[1], ci[2])
  }

  if ("npv" %in% ret) {
    npv <- tn / nr
    logit <- log(npv / (1 - npv))
    se <- sqrt((1 - (tn / nc)) / (nc * tn / nc) + (tp / pc) / (pc * (1 - (tp / pc))))
    ci_logit <- logit + c(-1, 1) * z * se
    ci <- plogis(ci_logit)
    dlist$npv <- format_ci(npv, ci[1], ci[2])
  }

  return(dlist)
}

relative_risk <- function(tp, fp, tn, fn, conf.level = 0.95) {
  added <- FALSE
  if (tp == 0 || fn == 0) {
    tp <- tp + 0.5
    tn <- tn + 0.5
    fp <- fp + 0.5
    fn <- fn + 0.5
    added <- TRUE
  }
  e1 <- tp + fp
  e0 <- fn + tn
  z <- qnorm(1 - (1 - conf.level) / 2)

  r1 <- tp / e1
  r0 <- fn / e0

  rr <- r1 / r0
  se_rr <- sqrt(1 / tp + 1 / fn - 1 / e1 - 1 / e0)
  log_rr <- log(rr)
  ci_rr <- exp(log_rr + c(-1, 1) * z * se_rr)

  z_rr <- log_rr / se_rr
  p_rr <- 2 * (1 - pnorm(abs(z_rr)))

  arr <- r0 - r1
  se_arr <- sqrt(((r1 * (1 - r1)) / e1) + ((r0 * (1 - r0)) / e0))
  ci_arr <- arr + c(-1, 1) * z * se_arr
  if (arr != 0) {
    nnt <- 1 / arr
    ci_nnt <- 1 / ci_arr
    ci_nnt <- sort(ci_nnt)
  } else {
    nnt <- NULL
    ci_nnt <- NULL
  }
  rrr <- arr / r0

  if (added == FALSE && (tn == 0 || fp == 0)) {
    tp <- tp + 0.5
    tn <- tn + 0.5
    fp <- fp + 0.5
    fn <- fn + 0.5
  }

  o1 <- tp / fn
  o0 <- fp / tn
  odds <- o1 / o0
  se_odds <- sqrt(1 / tp + 1 / fp + 1 / tn + 1 / fn)
  log_odds <- log(odds)
  ci_odds <- exp(log_odds + c(-1, 1) * z * se_odds)
  z_odds <- log_odds / se_odds
  p_odds <- 2 * (1 - pnorm(abs(z_odds)))

  list(
    rr = rr,
    ci_rr = ci_rr,
    z_rr = z_rr,
    p_rr = p_rr,
    arr = arr,
    ci_arr = ci_arr,
    rrr = rrr,
    nnt = nnt,
    ci_nnt = ci_nnt,
    odds = odds,
    ci_odds = ci_odds,
    z_odds = z_odds,
    p_odds = p_odds
  )
}

posttest_probability <- function(tp, fp, tn, fn, pretest) {
  if (is.null(pretest) || pretest <= 0) return(list(positive = NULL, negative = NULL))

  plr <- (tp / (tp + fn)) / (1 - (tn / (tn + fp)))
  nlr <- (1 - (tp / (tp + fn))) / (tn / (tn + fp))
  pt <- pretest / 100
  odds_pt <- pt / (1 - pt)
  plh_odds_pt <- odds_pt * plr
  nlh_odds_pt <- odds_pt * nlr
  positive <- plh_odds_pt / (plh_odds_pt + 1)
  negative <- nlh_odds_pt / (nlh_odds_pt + 1)

  list(
    positive = positive,
    negative = negative,
    fagan = fagan_data_frame(pt, positive, negative, plr, nlr)
  )
}

fagan_data_frame <- function(pretest, post_pos, post_neg, plr, nlr) {
  logpre <- log10((1 - pretest) / pretest)
  logppos <- log10(post_pos / (1 - post_pos))
  logpneg <- log10(post_neg / (1 - post_neg))
  df <- data.frame(
    x = c(0, 1, 0, 1),
    y = c(pretest, post_pos, pretest, post_neg),
    line = c("positive", "positive", "negative", "negative"),
    lo_y = c(logpre, logppos, logpre, logpneg),
    detail = c(plr, nlr, plr, nlr)
  )
  df
}

find_local_maxima <- function(x) {
  n <- length(x)
  locs <- logical(n)
  if (n == 1) {
    locs[1] <- TRUE
  } else if (n == 2) {
    locs <- c(x[1] > x[2], x[2] > x[1])
  } else {
    locs[1] <- x[1] > x[2]
    for (i in 2:(n - 1)) {
      if (!is.na(x[i - 1]) && !is.na(x[i + 1]) && !is.na(x[i])) {
        locs[i] <- (x[i] > x[i - 1] && x[i] > x[i + 1])
      }
    }
  }
  locs
}

compute_single_roc <- function(self, data, classVar, predictor, levels, options) {
  ret <- list()

  roc_obj <- pROC::roc(
    response = data[[classVar]],
    predictor = jmvcore::toNumeric(data[[predictor]]),
    levels = levels,
    ci = TRUE,
    direction = "auto",
  )
  ret$roc <- roc_obj

  if (options$displayCurves) {
    ret$plotData <- data.frame(
      tpr = rev(roc_obj$sensitivities),
      fpr = rev(1 - roc_obj$specificities),
      Model = predictor
    )
  }

  n1 <- length(roc_obj$cases)
  n0 <- length(roc_obj$controls)
  prev <- n1 / (n1 + n0)
  youden <- roc_obj$sensitivities + roc_obj$specificities - 1
  if (options$showCoords == TRUE) {
    coords <- data.frame(
      threshold = roc_obj$thresholds,
      sensitivity = roc_obj$sensitivities,
      specificity = roc_obj$specificities,
      youden = youden,
      local_max = find_local_maxima(youden)
    )
    if (options$showAllCoords == FALSE) {
      coords <- coords[coords$local_max, ]
    }
    coords <- coords[is.finite(coords$threshold), ]

    if (options$cTP) {
      coords$tp <- coords$sensitivity * n1
    }
    if (options$cFP) {
      coords$fp <- (1 - coords$specificity) * n0
    }
    if (options$cTN) {
      coords$tn <- coords$specificity * n0
    }
    if (options$cFN) {
      coords$fn <- (1 - coords$sensitivity) * n1
    }
    if (options$cPPV) {
      coords$ppv <- coords$sensitivity * prev / (coords$sensitivity * prev + (1 - coords$specificity) * (1 - prev))
    }
    if (options$cNPV) {
      coords$npv <- coords$specificity * (1 - prev) / ((1 - coords$sensitivity) * prev + coords$specificity * (1 - prev))
    }
    if (options$cPLR) {
      coords$plr <- coords$sensitivity / (1 - coords$specificity)
    }
    if (options$cNLR) {
      coords$nlr <- (1 - coords$sensitivity) / coords$specificity
    }
    if (options$cAcc) {
      coords$acc <- (coords$sensitivity * n1 + coords$specificity * n0) / (n1 + n0)
    }
    if (options$cPrev) {
      coords$prev <- prev
    }
    if (options$cF1) {
      coords$f1 <- (2 * coords$sensitivity * n1) / (n1 + coords$sensitivity * n1 + (1 - coords$specificity) * n0)
    }
    if (options$cTL) {
      coords$tl <- sqrt((1 - coords$sensitivity)^2 + (1 - coords$specificity)^2)
    }

    ret$coords <- coords[order(coords$threshold), ]
  }

  if (options$showDiagnostics == TRUE) {
    my <- which.max(youden)
    best <- data.frame(
      threshold = roc_obj$thresholds[my],
      tp = roc_obj$sensitivities[my] * n1,
      fp = (1 - roc_obj$specificities[my]) * n0,
      tn = roc_obj$specificities[my] * n0,
      fn = (1 - roc_obj$sensitivities[my]) * n1
    )
    cont <- data.frame(matrix(c(
      predictor,
      paste0(ifelse(roc_obj$direction == "<", "≥", "≤"), best$threshold),
      best$tp,
      best$fp,
      best$tp + best$fp,
      predictor,
      paste0(ifelse(roc_obj$direction == "<", "<", ">"), best$threshold),
      best$fn,
      best$tn,
      best$tn + best$fn,
      predictor,
      .("Total"),
      best$tp + best$fn,
      best$fp + best$tn,
      n1 + n0
    ), nrow = 3, ncol = 5, byrow = TRUE))
    colnames(cont) <- c("ptitle", "stitle", "refpos", "refneg", "total")
    ret$best <- best
    ret$cont <- cont
  }

  se <- sqrt(pROC::var(roc_obj))
  zs <- (roc_obj$auc - 0.5) / se
  p <- 2 * (1 - pnorm(abs(zs)))

  ret$auc <- list(
    area = roc_obj$auc,
    se = se,
    p = p,
    lowerCi = roc_obj$ci[1],
    upperCi = roc_obj$ci[3]
  )

  ret
}
