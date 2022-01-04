#' EncodeTime: DateTime encoding.
#' @param timestamps: DateTime values.
#' @param type: One of available encoding types:
#' 1) "polar": Polar coordinate system.
#' 2) "onehot": One-Hot encoding.
#' @return Table of encoded values.
EncodeTime = function(timestamps, type = "polar") {
    OneHot = function(values, min, max, name) {
        Default = function() {
            nrow = Count(values)
            ncol = max - min + 1
            data = matrix(rep(0, nrow * ncol), nrow = nrow, ncol = ncol)
            colnames(data) = paste(name, min:max, sep = "")
            return(as.data.frame(data))
        }
        Factors = function(x) {
            f = as.matrix(as.factor(as.vector(x)))
            colnames(f) = name
            return(f)
        }
        EncodeOne = function() {
            nrow = Count(values)
            encoded = as.matrix(rep(1, nrow), nrow = nrow, ncol = 1)
            colnames(encoded) = paste(name, values[1], sep = "")
            return(encoded)
        }
        EncodeMany = function() {
            selection = paste("~", name, sep = "")
            encoder = dummyVars(selection, data = Factors(min:max))
            encoded = data.frame(predict(encoder, newdata = Factors(values)))
            return(encoded)
        }
        Encode = function() {
            count = Count(unique(values))
            if (count == 0) return(c())
            if (count == 1) return(EncodeOne())
            return(EncodeMany())
        }

        result = Default()
        encoded = Encode()
        result[,colnames(encoded)] = encoded
        return(result)
    }
    Polar = function(values, min, max, name) {
        degree = 2*pi*(values-min)/max
        x = (sin(degree)+1)/2
        y = (cos(degree)+1)/2
        name.x = paste("sin", name, sep = "")
        name.y = paste("cos", name, sep = "")
        return(Join(x, y, name.x, name.y))
    }
    Transform = function(values, min, max, name) {
        if (type == "onehot") return(OneHot(values, min, max, name))
        return(Polar(values, min, max, name))
    }

    months.of.year = Transform(DatePart(timestamps, "%m"), 1, 12, "M")
    days.of.week = Transform(DatePart(timestamps, "%w") + 1, 1, 7, "D")
    hours.of.day = Transform(DatePart(timestamps, "%H") + 1, 1, 24, "H")
    joined = Join(Join(months.of.year, days.of.week), hours.of.day)
    return(joined)
}

#' ExtractCPCD: Extracts characteristic periods and days from timestamps
#' @param timestamps: DateTime values.
#' @param holidays: Holiday DateTime values.
#' @return Table with characteristic periods and days.
ExtractCPCD = function(timestamps, holidays = c()) {
    timestamps = DateTime(timestamps)
    cp = DatePart(timestamps, "%m")
    cd = as.numeric(DatePart(timestamps, "%u") > 5)
    if (length(holidays) > 0) {
        dates = DateTime(timestamps, "%F")
        hdays = DateTime(holidays, "%F") %>% unique()

        i = 1
        for(d in dates) {
            if (d %in% hdays) {
                cd[i] = 2
            }
            
            i = i+1
        }
    }

    res = Join(cp, cd)
    colnames(res) = c("CP", "CD")
    return(res)
}

#' Decompose: Time Series Decomposition
#' Decomposes given profile into selected number of components.
#' @param profile: One row from a table of profiles.
#' @param count: Preferred but not necessarily resulting number of components (depends on algorithm).
#' @param algorithm: One of available decomposition algorithms:
#' 1) "VMD": Variational Mode Decomposition
#' 2) "EMD": Empirical Mode Decomposition
#' 3) "SSA": Singular Spectrum Analysis
#' @return Table of components ordered by noise (one row per component).
Decompose = function(profile, count = 7, algorithm = "VMD") {
    SSA = function(signal, count) {
        ssa.result = reconstruct(ssa(signal))
        components = as.data.frame(ssa.result[])
        return(components)
    }
    EMD = function(signal, count) {
        emd.result = emd(signal, max.imf = count - 1)
        components = Join(emd.result$residue, emd.result$imf)
        return(components)
    }
    VMD = function(signal, count) {
        vmd.result = vmd(signal, K = count)
        # plot(vmd.result, facet = 'bymode', scales = 'free')
        vmd.result = as.data.frame(vmd.result)
        components = vmd.result[, 3:(ncol(vmd.result) - 1)]
        return(components)
    }

    signal = as.numeric(as.vector(profile))
    if (Count(signal) == 0) {
        return(c())
    }
    if (count < 2) {
        count = 2
    }

    components = NULL
    if (algorithm == "SSA") {
        components = SSA(signal, count)
    } else if (algorithm == "EMD") {
        components = EMD(signal, count)
    } else {
        components = VMD(signal, count)
    }

    components = as.data.frame(t(components))
    colnames(components) = colnames(profile)
    row.names(components) = c()
    return(components)
}

#' Denoise: Time Series Decomposition-Based Noise Removal
#' Decomposes given profiles into selected number of components and removes the last component from each one.
#' @param profiles: Table of profiles to denoise (one row per profile).
#' @param count: Preferred but not necessarily resulting number of components (depends on algorithm).
#' @param algorithm: One of available decomposition algorithms:
#' 1) "VMD": Variational Mode Decomposition
#' 2) "EMD": Empirical Mode Decomposition
#' 3) "SSA": Singular Spectrum Analysis
#' @return Denoised profiles.
Denoise = function(profiles, count = 7, algorithm = "VMD") {
    Sum = function(components) {
        count = Count(components)
        if (count < 2) return(components)
        return(t(colSums(components[1:(count - 1),])))
    }

    result = c()
    for (row in 1:Count(profiles)) {
        profile = profiles[row,]
        components = Decompose(profile, count, algorithm)
        result = Union(result, Sum(components))
    }

    colnames(result) = colnames(profiles)
    row.names(result) = row.names(profiles)
    return(result)
}

#' ExtractFeatures: Feature extraction based on Principal Component Analysis (PCA).
#' @param profiles: Table of profiles to denoise (one row per profile).
#' @param percentage: Percentage of variance explained with reduced dataset.
#' @return Table of transformed profiles with reduced dimensionality.
ExtractFeatures = function(profiles, percentage = 95) {
    Log("Extracting features with PCA...")
    original = ncol(profiles)

    columns = c()
    for (column in 1:original) {
        columns = c(columns, Count(unique(profiles[, column])) > 1)
    }

    result = prcomp(profiles[, columns], center = TRUE, scale = TRUE)
    variance = result$sdev ^ 2
    reduced = CumSumIndex(variance, percentage)
    reduction = (1 - reduced / original) * 100
    Log(c("Dimensionality reduced from ", original, " to ", reduced, " (", round(reduction, 2), " %)."))
    return(as.data.frame(result$x[, 1:reduced]))
}

#' SelectFeatures: Filter-based Feature Selection
#' Filtering is based on Joint Mutual Information Maximisation (JMIM).
#' @param x: Table of inputs (one column per variable and one row per example).
#' @param y: Table of outputs (one column for one variable and one row per example).
#' @param focus: Rows to focus on while filtering.
#' @param percentage: Percentage of information explained with selected features.
#' @return Table of inputs with selected features.
SelectFeatures = function(x, y, focus = 1:Count(x), percentage = 95) {
    score = JMIMScore(x, y, focus) %>% sort(decreasing = TRUE)
    selection = names(score)[1:CumSumIndex(score, percentage)]
    return(Subset(x, cols = selection))
}
