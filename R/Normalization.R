#' NormParam: Prepares normalization parameters.
#' @param data: Table or collection of representative values.
#' @param type: One of available normalization types:
#' 1) "NA": Not Assigned (none).
#' 2) "AM": Abs-Max normalization
#' 3) "MM": Min-Max normalization
#' 4) "AVG": Divide-by-average normalization
#' 5) "Z": Z-normalization
#' @param by: One of available ways to separate data:
#' 1) "row": Seperate data by rows.
#' 2) "col": Seperate data by columns.
#' 3) "": No separation.
#' @return A list containing:
#' 1) type: Value of the "type" parameter.
#' 2) by: Value of the "by" parameter.
#' 3) param: Table of normalization parameters.
NormParam = function(data, type = "Z", by = "") {
    AbsMax = function(values) return(c(absmax = abs(values) %>% max(na.rm=TRUE)))
    MinMax = function(values) return(c(min = values %>% min(na.rm=TRUE), max = values %>% max(na.rm=TRUE)))
    Avg = function(values) return(c(avg = values %>% mean(na.rm=TRUE)))
    Z = function(values) return(c(avg = values %>% mean(na.rm=TRUE), stdev = values %>% sd(na.rm=TRUE)))
    Apply = function(values, type) {
        if (type == "NA") return(NA)
        if (type == "AM") return(AbsMax(values))
        if (type == "MM") return(MinMax(values))
        if (type == "AVG") return(Avg(values))
        return(Z(values))
    }

    data = as.data.frame(data)
    if (Count(data) == 0) return(c())

    param = c()
    if (by == "row") {
        for (i in 1:nrow(data)) {
            values = unlist(na.omit(data[i,]))
            param = Union(param, Apply(values, type))
        }

        rownames(param) = rownames(data)
    }
    else if (by == "col") {
        for (i in 1:ncol(data)) {
            values = unlist(na.omit(data[, i]))
            param = Join(param, Apply(values, type))
        }

        colnames(param) = colnames(data)
    } else {
        values = unlist(na.omit(data))
        param = Apply(values, type)
    }

    param = as.data.frame(param)
    return(list(type = type, by = by, param = param))
}

#' Norm: Applies (de)normalization.
#' @param data: Table or collection of values to (de)normalize.
#' @param param: Normalization parameters.
#' @param normalize: Normalize (TRUE) or denormalize (FALSE).
#' @return Table of normalized values.
Norm = function(data, param, normalize = TRUE) {
    AbsMaxNorm = function(values, absmax) return(values / absmax)
    AbsMaxDenorm = function(values, absmax) return(values * absmax)
    AbsMax = function(values, absmax) {
        if (absmax == 0) return(values)
        if (normalize) return(AbsMaxNorm(values, absmax))
        return(AbsMaxDenorm(values, absmax))
    }
    MinMaxNorm = function(values, min, max) return((values - min) / (max - min))
    MinMaxDenorm = function(values, min, max) return(values * (max - min) + min)
    MinMax = function(values, min, max) {
        if (min == max) return(values)
        if (normalize) return(MinMaxNorm(values, min, max))
        return(MinMaxDenorm(values, min, max))
    }
    AvgNorm = function(values, avg) return(values / avg)
    AvgDenorm = function(values, avg) return(values * avg)
    Avg = function(values, avg) {
        if (avg == 0) return(values)
        if (normalize) return(AvgNorm(values, avg))
        return(AvgDenorm(values, avg))
    }
    ZNorm = function(values, avg, stdev) return((values - avg) / stdev)
    ZDenorm = function(values, avg, stdev) return(values * stdev + avg)
    Z = function(values, avg, stdev) {
        if (stdev == 0) return(values)
        if (normalize) return(ZNorm(values, avg, stdev))
        return(ZDenorm(values, avg, stdev))
    }
    ApplyAbsMax = function(values, p) return(AbsMax(values, p[1]))
    ApplyMinMax = function(values, p) return(MinMax(values, p[1], p[2]))
    ApplyAvg = function(values, p) return(Avg(values, p[1]))
    ApplyZ = function(values, p) return(Z(values, p[1], p[2]))
    Apply = function(values, type, p) {
        if (type == "AM") return(ApplyAbsMax(values, p))
        if (type == "MM") return(ApplyMinMax(values, p))
        if (type == "AVG") return(ApplyAvg(values, p))
        return(ApplyZ(values, p))
    }

    data = as.data.frame(data)
    if (Count(data) == 0) return(c())
    if (param$type == "NA") return(data)

    result = c()
    if (param$by == "row") {
        for (i in 1:nrow(data)) {
            values = unlist(data[i,])
            p = unlist(param$param[i,])
            result = Union(result, Apply(values, param$type, p))
        }

        rownames(result) = rownames(data)
    }
    else if (param$by == "col") {
        for (i in 1:ncol(data)) {
            values = unlist(data[, i])
            p = unlist(param$param[, i])
            result = Join(result, Apply(values, param$type, p))
        }

        colnames(result) = colnames(data)
    } else {
        values = unlist(data)
        p = unlist(param$param)
        result = Apply(values, param$type, p)
        result = matrix(result, nrow(data), ncol(data))
    }

    result = as.data.frame(result)
    return(result)
}
