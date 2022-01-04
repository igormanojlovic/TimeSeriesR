#' Z: Z value.
#' @param p: Probability level.
#' @return Z value.
Z = function(p) return(abs(qnorm((1-p)/2)))

#' CI: Confidence Interval.
#' @param m: Means.
#' @param sd: Standard deviations.
#' @param n: Sample size.
#' @param p: Probability level.
#' @return A list with the following elements:
#' 1) "lower": Lower confidence interval values.
#' 2) "upper": Upper confidence interval values.
CI = function(m, sd, n, p) {
    m = unlist(m)
    e = Z(p) * unlist(sd) * sqrt(1/n)
    return(list(lower = m-e, upper = m+e))
}

#' PI: Prediction Interval.
#' @param m: Means.
#' @param sd: Standard deviations.
#' @param n: Sample size.
#' @param p: Probability level.
#' @return A list with the following elements:
#' 1) "lower": Lower prediction interval values.
#' 2) "upper": Upper prediction interval values.
PI = function(m, sd, n, p) {
    m = unlist(m)
    e = Z(p) * unlist(sd) * sqrt(1+1/n)
    return(list(lower = m-e, upper = m+e))
}

#' MAE: Mean Absolute Error.
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @return MAE.
MAE = function(y_true, y_pred) {
    y_true = unlist(y_true)
    y_pred = unlist(y_pred)
    return(mean(abs(y_pred - y_true), na.rm=TRUE))
}

#' MAPE: Mean Absolute Percentage Error.
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @return MAPE[%].
MAPE = function(y_true, y_pred) {
    y_true = unlist(y_true)
    y_pred = unlist(y_pred)
    return(100 * mean(abs((y_pred - y_true) / y_true), na.rm=TRUE))
}

#' RMSE: Root Mean Square Error.
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @return RMSE.
RMSE = function(y_true, y_pred) {
    y_true = unlist(y_true)
    y_pred = unlist(y_pred)
    return(sqrt(mean((y_pred - y_true) ^ 2, na.rm=TRUE)))
}

#' CVRMSE: Coefficient of Variation (CV) of the Root Mean Square Error (RMSE).
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @return CV(RMSE)[%].
CVRMSE = function(y_true, y_pred) return(100 * RMSE(y_true, y_pred) / mean(y_true, na.rm=TRUE))

#' ACE: Average Coverage Error.
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @param sd: Expected standard deviations.
#' @param n: Sample size.
#' @param p: Probability level.
#' @return ACE[%].
ACE = function(y_true, y_pred, sd, n, p) {
    y_true = unlist(y_true)
    i = PI(y_pred, sd, n, p)
    covered = as.numeric(i$lower <= y_true & y_true <= i$upper)
    return(100 * (mean(covered, na.rm=TRUE)-p))
}

#' Winkler: Winkler loss
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @param sd: Expected standard deviations.
#' @param n: Sample size.
#' @param p: Probability level.
#' @return One winkler loss for each forecast step.
Winkler = function(y_true, y_pred, sd, n, p) {
    alpha = 1-p
    i = PI(y_pred, sd, n, p)
    d = data.frame(y = unlist(y_true)
                  ,l = i$lower
                  ,u = i$upper)
    d = d %>% mutate(e = (u-l) + ifelse(l <= y & y <= u, 0,
                                           ifelse(y < l, 2*(l-y)/alpha
                                                       , 2*(y-u)/alpha)))
    return(unlist(d[,"e"]))
}

#' WS: Winkler Score.
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @param sd: Expected standard deviations.
#' @param n: Sample size.
#' @param p: Probability level.
#' @return WS.
WS = function(y_true, y_pred, sd, n, p) return(Winkler(y_true, y_pred, sd, n, p) %>% mean(na.rm=TRUE))

#' Pinball: Pinball loss.
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @param sd: Expected standard deviations.
#' @param n: Sample size.
#' @param q: Number of quantiles.
#' @return Table of pinball losses for each forecast step (rows) at each quantile (columns).
Pinball = function(y_true, y_pred, sd, n, q = 100) {
    pinball_i = function(i) {
        d = data.frame( y = unlist(y_true)
                      ,yq = unlist(y_pred+sd*qnorm(i/q))
                      , q = rep(i/q, Count(y_true)))
        d = d %>% mutate(e = ifelse(y<yq, (1-q) * (yq-y)
                                        , q     * (y-yq)))
        return(d[,"e"])
    }

    p = data.frame()
    for (i in 1:(q-1)) {
        p = Join(p, pinball_i(i), colnames2=paste("Q", i, sep = ""))
    }

    return(p)
}

#' QS: Quantile Score.
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @param sd: Expected standard deviations.
#' @param n: Sample size.
#' @param q: Number of quantiles.
#' @return QS
QS = function(y_true, y_pred, sd, n, q = 100) return(Pinball(y_true, y_pred, sd, n, q) %>% unlist() %>% mean(na.rm=TRUE))

#' CRPS: Continuous Ranked Probability Score.
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @param sd: Expected standard deviations.
#' @return CRPS.
CRPS = function(y_true, y_pred, sd) {
    actual = unlist(y_true)
    expect = Join(unlist(y_pred), unlist(sd))
    return(crps(actual, expect)$CRPS)
}

#' DM: Diebold-Mariano (DM) test.
#' Performs DM test for each step in forecast horizon.
#' @param e1: Errors for model 1 for each step in one or more forecast horizons.
#' @param e2: Errors for model 2 for each step in one or more forecast horizons.
#' @param horizon: Forecast horizon.
#' @param p: Probability level.
#' @param power: The power used calculating errors (e.g. 1 for MAE, 2 for RMSE).
#' @return One value for each step in forecast horizon:
#' [1] if model 1 can be considered more accurate than model 2.
#' [-1] if model 2 can be considered more accurate than model 1.
#' [0] if there is no significant difference between the models.
DM = function(e1, e2, horizon, p = 0.95, power = 1) {
    Test = function(better, worse) {
        dm = dm.test(worse, better, alternative = 'g', h = horizon, power = power)
        return(dm$p.value < 1-p)
    }

    e1 = unlist(e1)
    e2 = unlist(e2)
    count = min(Count(e1), Count(e2))
    start = horizon*0:(count/horizon-1)+1

    v = c()
    for(i in 1:horizon) {
        range = start+i-1
        if (Test(e1[range], e2[range])) {
            v = c(v, 1)
        } else if (Test(e2[range], e1[range])) {
            v = c(v, -1)
        } else {
            v = c(v, 0)
        }
    }

    return(v)
}

#' Knee: Knee (elbow) method.
#' Finds the knee (elbow) of a curve.
#' @param curve: Ordered curve values.
#' @return Values before the knee.
Knee = function(curve) {
    diff = curve-c(curve[2:Count(curve)], 0)
    flag = diff==max(diff)

    selected = flag != flag
    for(i in 1:Count(flag)) {
        selected[i] = TRUE
        if (flag[i]) break
    }

    return(curve[selected])
}

#' CumSumIndex: Returns index of value when given percentage of cumulative sum is reached.
#' @param values: Ordered values.
#' @param percentage: Percentage of cumulative sum.
#' @return value index.
CumSumIndex = function(values, percentage = 95) {
    proportion = cumsum(values / sum(values))
    explained = proportion[proportion <= (percentage / 100)]
    return(max(Count(explained)+1, 2) %>% min(Count(values)))
}

#' JMIMScore: Joint Mutual Information Maximisation (JMIM) score.
#' @param x: Table of inputs (one column per variable and one row per example).
#' @param y: Table of outputs (one column for one variable and one row per example).
#' @param focus: Rows to focus on while filtering.
#' @return Vector of selected inputs.
JMIMScore = function(x, y, focus = 1:Count(x)) {
    xy = Join(x, y, rownames=row.names(x)) %>% Subset(rows = focus) %>% na.omit()
    fx = Subset(xy, cols = 1:(ncol(xy)-1))
    fy = Subset(xy, cols = ncol(xy))
    return(JMIM(fx, unlist(fy), ncol(fx))$score)
}

#' Autocorrelation
#' @param x: Table of value (one column per variable and one row per example).
#' @param lag: Max lag to examine.
#' @param verbose: Shows whether or not to plot results.
#' @return List with two tables:
#' 1) acf: Autocorrelation Function
#' 2) pacf: Partial Autocorrelation Function
Autocorrelation = function(x, lag, verbose = FALSE) {
    a = data.frame()
    p = data.frame()
    for(i in 1:ncol(x)) {
        a = a %>% Join(acf(x[,i], lag.max = lag, na.action = na.contiguous, plot = verbose)$acf %>% as.data.frame())
        p = p %>% Join(pacf(x[,i], lag.max = lag, na.action = na.contiguous, plot = verbose)$acf %>% as.data.frame())
    }
    
    colnames(a) = colnames(x)
    colnames(p) = colnames(x)
    return(list(acf = a, pacf = p))
}

#' Mix2Q: Converts Gaussian mixture to quantile cut points.
#' @param mix: Table of Gaussian mixtures.
#' @param m: Number of mixture compontents.
#' @param alphas: Alpha columns.
#' @param means: Mean columns.
#' @param sigmas: Sigma columns.
#' @param q: Number of quantiles.
#' @return Table of cut points.
Mix2Q = function(mix, m = ncol(mix)/3, alphas = 1:m, means = (m+1):(2*m), sigmas = (2*m+1):(3*m), q = 100) {
    GetQR = function(i) {
        if (NA %in% mix[i,]) return(rep(NA, q-1))
        return(qmixnorm((1:(q-1))/100, mix[i,means], mix[i,sigmas], mix[i,alphas]))
    }
    
    qr = data.frame()
    for(i in 1:Count(mix)) qr = qr %>% Union(GetQR(i) %>% t())
    return(qr)
}

#' QR2QS: Quantile Score (QS) for Quantile Regression (QR).
#' @param y_true: Actual values.
#' @param y_qr: QR values.
#' @return QS
QR2QS = function(y_true, y_qr) {
    qs = data.frame()
    for (i in 1:ncol(y_qr)) {
        q = i/(ncol(y_qr)+1)
        d = data.frame( y = unlist(y_true)
                      ,yq = unlist(y_qr[,i])
                      , q = rep(q, Count(y_true)))
        d = d %>% mutate(e = ifelse(y<yq, (1-q) * (yq-y)
                                        , q     * (y-yq)))
        qs = Join(qs, d[,"e"], colnames2 = i)
    }
    
    return(qs)
}

#' QR2WS: Winkler Score (WS) for Quantile Regression (QR).
#' @param y_true: Actual values.
#' @param y_qr: QR values.
#' @return WS
QR2WS = function(y_true, y_qr) {
    width = data.frame()
    penal = data.frame()
    for (l in ceiling(ncol(y_qr)/2):1) {
        u = ncol(y_qr)-l+1
        alpha = 1-(u-l)/(ncol(y_qr)+1)
        lower = y_qr[,l]
        upper = y_qr[,u]
        
        ws = data.frame(y = unlist(y_true)
                        ,l = lower
                        ,u = upper)
        ws = ws %>% mutate(e = (u-l) + ifelse(l <= y & y <= u, 0,
                                                               ifelse(y < l, 2*(l-y)/alpha
                                                                           , 2*(y-u)/alpha)))
        ws = unlist(ws[,"e"])
        w = unlist(upper - lower)
        penal = Join(penal, rev(ws - w))
        width = Join(width, rev(w))
    }
    
    return(list(penal=penal,width=width))
}
