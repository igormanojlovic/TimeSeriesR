# PACKAGES ----
write("Loading required R packages...", stdout())

options(warn = -1)
options(java.parameters = "-Xmx16g") # Changing JVM memory limit before loading any java package (256 MB by default).
suppressWarnings(suppressMessages({
    for (package in c("RODBC", "Hmisc", "clusterSim", "fastcluster", "ClusterR", "kohonen", "RWeka", "dtwclust", "speccalt", "doParallel")) {
        if (!require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
            install.packages(package)
            library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
        }
    }
}))

write("Loading TimeSeriesR functions...", stdout())

# COMMON FUNCTIONS ----

#' Now: Returns current time as string.
#' @return A string representing current time.
Now = function() {
    return(paste(Sys.time(), sep = ""))
}

#' Log: Logs current time and given message.
#' @param message: A single object, vector or a list of objects to be concatenated into a log message.
Log = function(message) {
    log.message = paste(message, collapse = "")
    write(paste(paste("[", Now(), "]", sep = ""), log.message), stdout())
}

#' StopwatchStartNew: Creates and starts new stopwatch.
#' @return A number representing stopwatch.
StopwatchStartNew = function() {
    return(proc.time())
}

#' StopwatchElapsedSeconds: Extracts elapsed number of seconds from given stopwatch.
#' @param stopwatch: Stopwatch created with the StopwatchStartNew function.
#' @return Elapsed number of seconds.
StopwatchElapsedSeconds = function(stopwatch) {
    return(as.integer((proc.time() - stopwatch)[3]))
}

#' OpenSqlConnection: Opens trusted connection to SQL Server database.
#' @param instance: SQL Server instance.
#' @param database: SQL Server database.
#' @return An open connection to SQL Server database.
OpenSqlConnection = function(instance, database) {
    string = paste("driver={SQL Server};server=.\\", instance, ";database=", database, ";trusted_connection=true", sep = "")
    Log(c("Connecting to SQL Server: ", string))

    connection = RODBC::odbcDriverConnect(string)
    if (connection == -1) {
        stop("Failed to connect to SQL Server.")
    } else {
        Log("Connected to SQL Server.")
    }

    return(connection)
}

#' GetPath: Composes allowed file path.
#' @param folder: Folder name.
#' @param file: File name.
#' @param extension: File extension.
#' @return Composed file path.
GetPath = function(folder, file, extension) {
    file = gsub("[[:punct:]]", "-", paste(file, collapse = ""))
    path = paste(paste(folder, collapse = ""), "\\", file, ".", extension, sep = "")
    return(path)
}

# TABLE FUNCTIONS ----

#' GetRowCount: Returns the number of tabular data rows.
#' @param table: Vector, matrix, data frame or list of objects.
#' @return Number of rows in given table.
GetRowCount = function(table) {
    if (is.null(table)) {
        return(0)
    }
    if (!is.vector(table) && !is.matrix(table) && !is.data.frame(table) && !is.list(table)) {
        return(0)
    }

    return(nrow(as.data.frame(table)))
}

#' Join: Joins columns.
#' @param columns1: Data frame, matrix or verctor with first set of columns.
#' @param columns2: Data frame, matrix or verctor with second set of columns.
#' @param colnames1: String or vector with names of columns in the first set.
#' @param colnames2: String or vector with names of columns in the second set.
#' @param rownames: String or vector with names of rows in the resulting table.
#' @return New table with given columns.
Join = function(columns1, columns2, colnames1 = NULL, colnames2 = NULL, rownames = NULL) {
    table1 = as.data.frame(columns1)
    table2 = as.data.frame(columns2)

    table = as.data.frame(cbind(table1, table2))

    if (!is.null(colnames1)) {
        colnames(table)[1:ncol(table1)] = colnames1
    }
    if (!is.null(colnames2)) {
        colnames(table)[(ncol(table1) + 1):ncol(table)] = colnames2
    }
    if (!is.null(rownames)) {
        row.names(table) = rownames
    }

    return(table)
}

#' Union: Creates a union of rows.
#' @param rows1: Data frame, matrix or verctor with first set of rows.
#' @param rows2: Data frame, matrix or verctor with second set of rows.
#' @param rownames1: String or vector with names of rows in the first set.
#' @param rownames2: String or vector with names of rows in the second set.
#' @param colnames: String or vector with names of columns in the resulting table.
#' @return New table with given rows.
Union = function(rows1, rows2, colnames = NULL, rownames = NULL) {
    table = as.data.frame(rbind(rows1, rows2))

    if (!is.null(colnames)) {
        colnames(table) = colnames
    }
    if (!is.null(rownames)) {
        row.names(table) = rownames
    }

    return(table)
}

#' ImportCSV: Imports CSV file to a table.
#' @param folder: Export folder name.
#' @param file: Export file name.
#' @param rows: Shows whether or not to export row names.
#' @return Imported csv file as a table.
ImportCSV = function(folder, file, rows = FALSE) {
    row.names = NULL
    if (rows) { row.names = 1 }
    path = GetPath(folder, file, "csv")
    Log(c("Importing ", path))
    return(read.table(file = path, sep = "|", header = TRUE, row.names = row.names))
}

#' ExportCSV: Exports table to a CSV file.
#' @param table: Table to be exported.
#' @param folder: Export folder name.
#' @param file: Export file name.
#' @param rows: Shows whether or not to export row names.
ExportCSV = function(table, folder, file, rows = FALSE) {
    path = GetPath(folder, file, "csv")
    Log(c("Exporting ", path))
    dir.create(paste(folder, collapse = ""), recursive = TRUE, showWarnings = FALSE)
    write.table(table, file = path, sep = "|", col.names = TRUE, row.names = rows, quote = FALSE)
}

# GROUPING FUNCTIONS ----

#' Cluster: Time Series Clustering
#' Clusters given profiles into an optimal number of clusters.
#' @param profiles: Table of profiles to cluster (one row per profile).
#' @param k.min: Minimum number of clusters.
#' @param k.max: Maximum number of clusters.
#' @param algorithm: One of available clustering algorithms:
#' 1) "TSGA": Time Series Grouping Algorithm
#' 2) "HBKM": Hybrid Bisect K-Means
#' 3) "FSC": Fast Spectral Clustering
#' 4) "ECCF": Electricity Consumer Characterization Framework
#' 5) "k-Shape": Principal Component Analysis + k-Shape clustering
#' 6) "PKM++": Principal Component Analysis + K-Means++
#' @param cvi: One of available cluster validity indices:
#' 1) "DBI": Davies-Bouldin Index
#' 2) "CHI": Calinski-Harabasz Index
#' @param cores: Number of cores to use in the search for an optimal number of clusters (NULL == all cores).
#' @return A list, containing:
#' 1) clusters: Vector of clusters assigned to each profile.
#' 2) centroids: Table of cluster centroids (one row per centroid).
#' 3) validity: Cluster validity according to CVI.
Cluster = function(profiles, k.min, k.max, algorithm = "TSGA", cvi = "DBI", cores = NULL) {
    PCA = function(profiles) {
        Log("Performing dimensionality reduction with PCA...")

        variable.columns = c()
        for (column in 1:ncol(profiles)) {
            variable.columns = c(variable.columns, GetRowCount(unique(profiles[, column])) > 1)
        }

        PCA = prcomp(profiles[, variable.columns], center = TRUE, scale = TRUE)
        variance = PCA$sdev ^ 2
        variance.proportion = cumsum(variance / sum(variance))

        reduced.dim = GetRowCount(variance.proportion[variance.proportion <= 0.95])
        if (reduced.dim < 2) {
            reduced.dim = 2
        }

        original.dim = ncol(profiles)
        reduction = (1 - reduced.dim / original.dim) * 100
        Log(c("Dimensionality reduced from ", original.dim, " to ", reduced.dim, " (", reduction, " %)."))
        return(as.data.frame(PCA$x[, 1:reduced.dim]))
    }

    BKM = function(profiles, k.max) {
        Log(c("Performing BKM until k = ", k.max, "..."))
        clusters = rep(1, GetRowCount(profiles))
        errors = rep(.Machine$double.xmax, GetRowCount(profiles))
        for (k in 2:k.max) {
            selected = clusters == min(clusters[errors == max(errors)])
            result = kmeans(profiles[selected,], centers = 2, nstart = 1, algorithm = "Lloyd")
            clusters.max = max(clusters)
            clusters[selected] = clusters.max + result$cluster
            errors[clusters == clusters.max + 1] = result$withinss[1]
            errors[clusters == clusters.max + 2] = result$withinss[2]
        }

        k = 1
        clusters = -clusters
        for (i in 1:k.max) {
            clusters[clusters == min(clusters)] = k
            k = k + 1
        }

        return(clusters)
    }

    SOM = function(profiles, k.max) {
        profiles.rowcount = GetRowCount(profiles)
        som.dim.x = as.numeric(ceiling(sqrt(k.max)))
        som.dim.y = som.dim.x
        if (som.dim.x * som.dim.y > profiles.rowcount) {
            som.dim.x = as.numeric(as.integer(profiles.rowcount / som.dim.y))
        }

        Log(c("Creating SOM with ", som.dim.x, "x", som.dim.y, " neurons in the output layer... "))
        som = kohonen::som(as.matrix(profiles), somgrid(som.dim.x, som.dim.y, "rectangular"))
        som.map = as.factor(as.vector(som$unit.classif))
        neurons = as.data.frame(som$codes)
        return(list(clusters = som.map, centroids = neurons))
    }

    TakeBetter = function(c1, c2) {
        c1.isnone = is.null(c1$validity) || is.na(c1$validity)
        c2.isnone = is.null(c2$validity) || is.na(c2$validity)
        if (c1$cvi == "CHI") {
            if (c2.isnone || (!c1.isnone && c1$validity > c2$validity)) {
                return(list(clusters = c1$clusters, validity = c1$validity, cvi = cvi))
            } else {
                return(list(clusters = c2$clusters, validity = c2$validity, cvi = cvi))
            }
        } else {
            if (c2.isnone || (!c1.isnone && c1$validity < c2$validity)) {
                return(list(clusters = c1$clusters, validity = c1$validity, cvi = cvi))
            } else {
                return(list(clusters = c2$clusters, validity = c2$validity, cvi = cvi))
            }
        }
    }

    Centroids = function(profiles, clusters) {
        if (GetRowCount(profiles) == 0) {
            return(c())
        }

        group.by.cluster = as.formula(paste(". ~", "cluster"))
        values.clustered = Join(profiles, clusters, colnames2 = "cluster")
        centroids = aggregate(group.by.cluster, values.clustered, mean)[, 2:(ncol(profiles) + 1)]
        return(as.data.frame(centroids))
    }

    TSGA = function(profiles, k.min, k.max, cvi) {
        Log("Applying TSGA...")

        profiles.reduced = PCA(profiles);
        Log(c("Performing HWKM for k = ", k.max, "..."))
        HWKM.result = kmeans(profiles.reduced, centers = k.max, nstart = 1, algorithm = "Hartigan-Wong")
        HWKM.clusters = HWKM.result$cluster
        HWKM.validity = ifelse(cvi == "CHI"
                      , clusterSim::index.G1(profiles, as.vector(HWKM.clusters))
                      , clusterSim::index.DB(profiles, as.vector(HWKM.clusters))$DB)
        if (k.min == k.max) {
            return(list(clusters = HWKM.clusters, validity = HWKM.validity))
        }

        Log("Creating distance matrix from the obtained centroids...")
        centroid.distances = dist(HWKM.result$centers, method = "euclidean")

        Log("Creating tree from the distance matrix using AHC with UPGMA...")
        centroid.tree = fastcluster::hclust(centroid.distances, method = "average")

        k.max = k.max - 1
        Log(c("Cutting the AHC tree for each k between ", k.min, " and ", k.max, "..."))
        result = foreach(k = k.min:k.max, .combine = TakeBetter, .packages = "clusterSim") %dopar% {
            centroid.clusters = cutree(centroid.tree, k = k)
            clusters = centroid.clusters[HWKM.clusters]
            validity = NULL
            if (cvi == "CHI") {
                validity = clusterSim::index.G1(profiles, as.vector(clusters))
                if (HWKM.validity > validity) {
                    clusters = HWKM.clusters
                    validity = HWKM.validity
                }
            } else {
                validity = clusterSim::index.DB(profiles, as.vector(clusters))$DB
                if (HWKM.validity < validity) {
                    clusters = HWKM.clusters
                    validity = HWKM.validity
                }
            }

            list(clusters = clusters, validity = validity, cvi = cvi)
        }

        return(result)
    }

    HBKM = function(profiles, k.min, k.max, cvi) {
        Log("Applying HBKM...")

        BKM.clusters = BKM(profiles, k.max)
        BKM.validity = ifelse(cvi == "CHI"
                     , clusterSim::index.G1(profiles, as.vector(BKM.clusters))
                     , clusterSim::index.DB(profiles, as.vector(BKM.clusters))$DB)
        if (k.min == k.max) {
            return(list(clusters = BKM.clusters, validity = BKM.validity))
        }

        Log("Creating distance matrix from the obtained centroids...")
        centroid.distances = dist(Centroids(profiles, BKM.clusters), method = "euclidean")

        Log("Creating tree from the distance matrix using AHC with UPGMA...")
        centroid.tree = fastcluster::hclust(centroid.distances, method = "average")

        k.max = k.max - 1
        Log(c("Cutting the AHC tree for each k between ", k.min, " and ", k.max, "..."))
        result = foreach(k = k.min:k.max, .combine = TakeBetter, .packages = "clusterSim") %dopar% {
            centroid.clusters = cutree(centroid.tree, k = k)
            clusters = centroid.clusters[BKM.clusters]
            validity = NULL
            if (cvi == "CHI") {
                validity = clusterSim::index.G1(profiles, as.vector(clusters))
                if (BKM.validity > validity) {
                    clusters = BKM.clusters
                    validity = BKM.validity
                }
            } else {
                validity = clusterSim::index.DB(profiles, as.vector(clusters))$DB
                if (BKM.validity < validity) {
                    clusters = BKM.clusters
                    validity = BKM.validity
                }
            }

            list(clusters = clusters, validity = validity, cvi = cvi)
        }

        return(result)
    }

    FSC = function(profiles, k.min, k.max, cvi) {
        Log("Applying FSC...")

        BKM.clusters = BKM(profiles, k.max)
        BKM.validity = ifelse(cvi == "CHI"
                     , clusterSim::index.G1(profiles, as.vector(BKM.clusters))
                     , clusterSim::index.DB(profiles, as.vector(BKM.clusters))$DB)
        if (k.min == k.max) {
            return(list(clusters = BKM.clusters, validity = BKM.validity))
        }

        Log("Creating RBF kernel from the obtained centroids...")
        RBF = speccalt::local.rbfdot(Centroids(profiles, BKM.clusters))

        k.max = k.max - 1
        Log(c("Performing the SC for each k between ", k.min, " and ", k.max, "..."))
        result = foreach(k = k.min:k.max, .combine = TakeBetter, .packages = c("clusterSim", "speccalt")) %dopar% {
            centroid.clusters = speccalt::speccalt(RBF, k)
            clusters = centroid.clusters[BKM.clusters]
            validity = NULL
            if (cvi == "CHI") {
                validity = clusterSim::index.G1(profiles, as.vector(clusters))
                if (BKM.validity > validity) {
                    clusters = BKM.clusters
                    validity = BKM.validity
                }
            } else {
                validity = clusterSim::index.DB(profiles, as.vector(clusters))$DB
                if (BKM.validity < validity) {
                    clusters = BKM.clusters
                    validity = BKM.validity
                }
            }

            list(clusters = clusters, validity = validity, cvi = cvi)
        }

        return(result)
    }

    ECCF = function(profiles, k.min, k.max, cvi) {
        Log("Applying ECCF...")

        SOM.result = SOM(profiles, k.max)
        SOM.clusters = SOM.result$clusters
        SOM.centroids = SOM.result$centroids
        centroids.unique.rowcount = GetRowCount(unique(SOM.centroids))
        if (k.max > centroids.unique.rowcount - 2) {
            k.max = centroids.unique.rowcount - 2
        }

        Log(c("Performing KM for each k between ", k.min, " and ", k.max, "..."))
        result = foreach(k = k.min:k.max, .combine = TakeBetter, .packages = c("clusterSim")) %dopar% {
            centroid.clusters = kmeans(SOM.centroids, centers = k, nstart = 1, algorithm = "Lloyd")$cluster
            clusters = centroid.clusters[SOM.clusters]
            validity = NULL
            if (cvi == "CHI") {
                validity = clusterSim::index.G1(profiles, as.vector(clusters))
            } else {
                validity = clusterSim::index.DB(profiles, as.vector(clusters))$DB
            }

            list(clusters = clusters, validity = validity, cvi = cvi)
        }

        return(result)
    }

    KShape = function(profiles, k.min, k.max, cvi) {
        Log("Applying k-Shape...")

        profiles.reduced = PCA(profiles);
        Log(c("Performing k-Shape for each k between ", k.min, " and ", k.max, "..."))
        result = foreach(k = k.min:k.max, .combine = TakeBetter, .packages = c("clusterSim", "dtwclust")) %dopar% {
            clusters = dtwclust::dtwclust(as.matrix(profiles.reduced), type = "partitional", k = k, preproc = "zscore", distance = "sbd", centroid = "shape")@cluster
            validity = NULL
            if (cvi == "CHI") {
                validity = clusterSim::index.G1(profiles, as.vector(clusters))
            } else {
                validity = clusterSim::index.DB(profiles, as.vector(clusters))$DB
            }

            list(clusters = clusters, validity = validity, cvi = cvi)
        }

        return(result)
    }

    PKMPP = function(profiles, k.min, k.max, cvi) {
        Log("Applying PKM+...")

        profiles.reduced = PCA(profiles);
        Log(c("Performing KM++ for each k between ", k.min, " and ", k.max, "..."))
        result = foreach(k = k.min:k.max, .combine = TakeBetter, .packages = c("clusterSim", "ClusterR")) %dopar% {
            clusters = ClusterR::KMeans_rcpp(profiles.reduced, k, initializer = "kmeans++")$clusters
            validity = NULL
            if (cvi == "CHI") {
                validity = clusterSim::index.G1(profiles, as.vector(clusters))
            } else {
                validity = clusterSim::index.DB(profiles, as.vector(clusters))$DB
            }

            list(clusters = clusters, validity = validity, cvi = cvi)
        }

        return(result)
    }

    profiles.rowcount = GetRowCount(profiles)
    if (profiles.rowcount == 0) {
        Log("Clustering skipped. There are no profiles to cluster.")
        return(list(clusters = c(), centroids = c(), validity = NA))
    }
    if (k.min > k.max) {
        k.min = k.max
    }

    Log(c("Clustering started (", profiles.rowcount, " profiles, dimensionality = ", ncol(profiles), ", k.min = ", k.min, ", k.max = ", k.max, ")..."))
    stopwatch = StopwatchStartNew()
    registerDoParallel(cores = cores)

    clusters = NULL
    centroids = NULL
    validity = NULL

    Log("Checking for profile duplicates...")
    profiles.unique = unique(profiles)
    profiles.unique.rowcount = GetRowCount(profiles.unique)
    if (profiles.unique.rowcount <= k.min) {
        Log(c("Too many duplicates. Using unique rows (", profiles.unique.rowcount, ") as clusters..."))
        clusters = merge(profiles, Join(profiles.unique, 1:profiles.unique.rowcount, colnames2 = "cluster"))[, "cluster"]
        validity = NA
    } else {
        if (k.max > profiles.unique.rowcount - 2) {
            k.max = profiles.unique.rowcount - 2
            if (k.min > k.max) {
                k.min = k.max
            }
        }

        clust = NULL
        if (algorithm == "HBKM") {
            clust = HBKM(profiles, k.min, k.max, cvi)
        } else if (algorithm == "FSC") {
            clust = FSC(profiles, k.min, k.max, cvi)
        } else if (algorithm == "ECCF") {
            clust = ECCF(profiles, k.min, k.max, cvi)
        } else if (algorithm == "k-Shape") {
            clust = KShape(profiles, k.min, k.max, cvi)
        } else if (algorithm == "PKM++") {
            clust = PKMPP(profiles, k.min, k.max, cvi)
        } else {
            clust = TSGA(profiles, k.min, k.max, cvi)
        }

        clusters = clust$clusters
        validity = clust$validity
    }

    Log("Preparing centroids...")
    centroids = Centroids(profiles, clusters)

    Log(c("Clustering finished (", GetRowCount(centroids), " cluster(s), validity = ", validity, ", duration = ", StopwatchElapsedSeconds(stopwatch), " second(s))."))
    return(list(clusters = clusters, centroids = centroids, validity = validity))
}

#' Classify: Time Series Classification
#' Classifies given profiles into clusters with the most similar centroids.
#' @param profiles: Table of profiles to classify (one row per profile).
#' @param centroids: Table of cluster centroids (one row per centroid).
#' @param algorithm: One of available classfication algorithms
#' "SVM": Support Vector Machines
#' "kNN": k-Nearest Neighbors
#' "C45": C4.5 classifier
#' @param validate: if TRUE then 10-fold cross validation (10-FCV) is performed.
#' @return A list, containing:
#' 1) clusters: Vector of predicted clusters assgined to each profile.
#' 2) validity: Classification validity according to 10-FCV if validate = TRUE; Otherwise, NA.
Classify = function(profiles, centroids, algorithm = "SVM", validate = FALSE) {
    profiles.rowcount = GetRowCount(profiles)
    if (profiles.rowcount == 0) {
        Log("Classification skipped. There are no profiles to classify.")
        return(list(clusters = c(), validity = NA))
    }

    centroids.rowcount = GetRowCount(centroids)
    Log(c("Classification started (", profiles.rowcount, " profiles, ", centroids.rowcount, " centroids)..."))
    stopwatch = StopwatchStartNew()

    model = NULL
    if (centroids.rowcount < 2) {
        Log("Training and prediction skipped. Number of centroids for training the model is less than 2.")
        clusters = rep(1, times = profiles.rowcount)
    } else {
        formula = as.formula(paste("cluster", " ~ ."))
        training.set = Join(centroids, as.factor(row.names(centroids)), colnames2 = "cluster")

        if (algorithm == "kNN") {
            Log("Training k-NN...")
            model = RWeka::IBk(formula, data = training.set)
        } else if (algorithm == "C45") {
            Log("Training C4.5...")
            model = RWeka::J48(formula, data = training.set)
        } else {
            Log("Training SVM using SMO...")
            model = RWeka::SMO(formula, data = training.set)
        }

        Log("Predicting clusters...")
        clusters = predict(model, profiles)
    }

    Log(c("Classification finished (duration = ", StopwatchElapsedSeconds(stopwatch), " second(s))."))

    validity = NA
    if (!is.null(model) && validate) {
        Log("Performing 10-FCV folds...")
        validity = as.list(RWeka::evaluate_Weka_classifier(model, numFolds = 10)$details)$pctCorrect
    }

    return(list(clusters = clusters, validity = validity))
}

#' Group: Time Series Grouping
#' Groups given profiles into an optimal number of clusters.
#' @param profiles: Table of profiles to group (one row per profile).
#' @param k.min: Minimum number of clusters.
#' @param k.max: Maximum number of clusters.
#' @param algorithm.clustering: One of available clustering algorithms:
#' 1) "TSGA": Time Series Grouping Algorithm
#' 2) "HBKM": Hybrid Bisect K-Means
#' 3) "FSC": Fast Spectral Clustering
#' 4) "ECCF": Electricity Consumer Characterization Framework
#' 5) "k-Shape": Principal Component Analysis + k-Shape clustering
#' 6) "PKM++": Principal Component Analysis + K-Means++
#' @param cvi: One of available cluster validity indices: 
#' 1) "DBI": Davies-Bouldin Index
#' 2) "CHI": Calinski-Harabasz Index
#' @param cores: Number of cores to use in the search for an optimal number of clusters (NULL == all cores).
#' @param algorithm.classification: One of available classfication algorithms
#' "SVM": Support Vector Machines
#' "kNN": k-Nearest Neighbors
#' "C45": C4.5 classifier
#' @param validate.classification: if TRUE then 10-fold cross validation (10-FCV) is performed.
#' @return A list, containing:
#' 1) groups: Vector of groups assigned to each profile (map between profiles and groups).
#' 2) k: Optimal number of groups.
#' 3) validity.clustering: Cluster validity according to CVI.
#' 4) validity.classification: Classification validity according to 10-FCV if validate = TRUE; Otherwise, NA.
#' 5) duration: Grouping duration in seconds.
Group = function(profiles, k.min, k.max, algorithm.clustering = "TSGA", cvi = "DBI", cores = NULL,
    algorithm.classification = "SVM", validate.classification = FALSE) {
    if (GetRowCount(profiles) == 0) {
        Log("Grouping skipped. There are no profiles to group.")
        return(list(groups = c(), k = 0, cluster.validity = NA, classification.validity = NA, duration = 0))
    }

    profilesToCluster = profiles
    profilesToClassify = NULL # TODO: Detect profiles with damaged or missing data (future work).
    stopwatch = StopwatchStartNew()
    clust = Cluster(profilesToCluster, k.min, k.max, algorithm.clustering, cvi, cores)
    class = Classify(profilesToClassify, clust$centroids, algorithm.classification, validate.classification)
    groups.profilesToCluster = Join(row.names(profilesToCluster), clust$clusters)
    groups.profilesToClassify = Join(row.names(profilesToClassify), class$clusters)
    groups.rownames = 1:(GetRowCount(groups.profilesToCluster) + GetRowCount(groups.profilesToClassify))
    groups = Union(groups.profilesToCluster, groups.profilesToClassify, colnames = c("Profile", "Group"), rownames = groups.rownames)
    k = GetRowCount(clust$centroids)
    if (k == 0) { k = GetRowCount(class$centroids) }
    duration = StopwatchElapsedSeconds(stopwatch)
    return(list(groups = groups, k = k, cluster.validity = clust$validity, classification.validity = class$validity, duration = duration))
}

# LPR FUNCTIONS ----

#' PrepareANDLPs: Prepares Concatenated Average Normalized Load Profiles.
#' Reads ANDLPs from SQL Server database, concatenates them and then
#' exports the resulting profiles into CSV files in specified folder.
#' Database is expected to have 2 stored procedures defined as follows:
#' CREATE PROCEDURE [dbo].[GetDataSets] AS
#' BEGIN
#'     SELECT [DataSetID](INT), [CharacteristicLoadType](INT), [NumberOfMeasurementsPerDay](INT) FROM <source>;
#' END;
#' CREATE PROCEDURE [dbo].[GetANDLPs] @DataSetID INT, @CharacteristicLoadType INT, @NumberOfMeasurementsPerDay INT AS
#' BEGIN
#'     SELECT [ConsumerID](INT), [CharacteristicPeriod](INT), [CharacteristicDay](INT), [PorQ](BIT), [Value_1](FLOAT),..., [Value_N](FLOAT) FROM <source>;
#' END;
#' @param connection: Open connection to SQL Server database.
#' @param folder: Name of a folder into which to export profiles.
PrepareANDLPs = function(connection, folder) {
    GetDataSets = function(connection) {
        return(RODBC::sqlQuery(connection, "EXEC GetDataSets"))
    }

    GetANDLPs = function(connection, dataset) {
        ds = dataset$DataSetID
        CLT = dataset$CharacteristicLoadType
        dim = dataset$NumberOfMeasurementsPerDay
        query = paste("EXEC GetANDLPs ", ds, ", ", CLT, ", ", dim)
        return(RODBC::sqlQuery(connection, query))
    }

    Concatenate = function(ANDLPs) {
        if (GetRowCount(ANDLPs) == 0) {
            return(c())
        }

        ANDLPs$timevar = with(ANDLPs, paste("CP_", CharacteristicPeriod, ".CD_", CharacteristicDay, ".PQ_", PorQ, sep = ""))
        columns = setdiff(colnames(ANDLPs), c("CharacteristicPeriod", "CharacteristicDay", "PorQ"))
        reshaped = reshape(ANDLPs[, columns], direction = "wide", idvar = "ConsumerID", timevar = "timevar")
        row.names(reshaped) = reshaped[, "ConsumerID"]
        profiles = as.matrix(Hmisc::impute(as.data.frame(reshaped[, colnames(reshaped) != "ConsumerID"]), mean))
        return(as.data.frame(profiles))
    }

    datasets = GetDataSets(connection)
    for (row in c(1:GetRowCount(datasets))) {
        dataset = datasets[row,]
        ANDLPs = GetANDLPs(connection, dataset)
        profiles = Concatenate(ANDLPs)
        DS = dataset$DataSetID
        CLT = dataset$CharacteristicLoadType
        ExportCSV(profiles, folder, c("DS", DS, "CLT", CLT), TRUE)
    }
}

#' LPR: Load Pattern Recognition
#' Reads concatenated ANDLPs from CSV files in specified folder, obtains load groups
#' and export the groups and grouping statistics into CSV files in parent folder.
#' @param folder: Folder with exported ANDLPs.
#' @param k.min: Minimum number of load groups.
#' @param k.max: One or more maximum numbers of load groups.
#' @param algorithm: One of available clustering algorithms:
#' 1) "TSGA": Time Series Grouping Algorithm
#' 2) "HBKM": Hybrid Bisect K-Means
#' 3) "FSC": Fast Spectral Clustering
#' 4) "ECCF": Electricity Consumer Characterization Framework
#' 5) "k-Shape": Principal Component Analysis + k-Shape clustering
#' 6) "PKM++": Principal Component Analysis + K-Means++
#' @param cvi: One of available cluster validity indices: 
#' 1) "DBI": Davies-Bouldin Index
#' 2) "CHI": Calinski-Harabasz Index
#' @param cores: Number of cores to use in the search for an optimal number of clusters (NULL == all cores).
LPR = function(folder, k.min, k.max, algorithm, cvi, cores = NULL) {
    all.files = list.files(folder, pattern = NULL, all.files = FALSE, full.names = FALSE)
    csv.files = gsub(".csv", "", all.files[endsWith(all.files, ".csv")])
    Log(c("Processing ", GetRowCount(csv.files), " datasets..."))

    time = Now()
    statistics = c()
    for (file in csv.files) {
        profiles = ImportCSV(folder, file, TRUE)
        for (current.k.max in k.max) {
            description = c(algorithm, "-", cvi, " k", k.min, "-", current.k.max)
            Log(c("LPR started for ", description, "..."))

            g = Group(profiles, k.min, current.k.max, algorithm, cvi, cores)
            s = t(c(file, ncol(profiles), algorithm, cvi, k.min, current.k.max, g$k, g$cluster.validity, g$duration))
            statistics = Union(statistics, s)

            statistics.named = statistics
            colnames(statistics.named) = c("File", "Dimensionality", "ClusteringAlgorithm", "ClusterValidityIndex", "kMin", "kMax", "k", "ClusterValidity", "ClusteringDurationInSeconds")
            ExportCSV(statistics.named, c(dirname(folder), "\\Statistics"), c("Statistics ", time))
            ExportCSV(g$groups, c(dirname(folder), "\\Groups"), c(file, " ", description, " ", Now()))

            Log(c("LPR finished for ", description, "."))
        }
    }

    Log(c("Processing finished."))
}

#' Execute: Executes CMD.
#' @param args: CMD arguments.
Execute = function(args) {
    if (GetRowCount(args) == 0) {
        write("Parameters not found. Available parameters are:
1) PrepareANDLPs <instance> <database> <folder>: Calls PrepareANDLPs function.
2) LPR <folder> <algorithm> <cvi> <cores (optional param)> <k.max (one or more optional params)>: Calls LPR function.", stdout())
        return()
    }

    action = args[1]
    if (action == "PrepareANDLPs") {
        instance = args[2]
        database = args[3]
        folder = args[4]

        connection = OpenSqlConnection(instance, database)
        PrepareANDLPs(connection, folder)
    } else if (action == "LPR") {
        folder = args[2]
        algorithm = args[3]
        cvi = args[4]

        cores = NULL
        if (GetRowCount(args) > 4) {
            cores = as.numeric(args[5])
        }

        k.max = c(10, 50, 100, 500)
        if (GetRowCount(args) > 5) {
            k.max = as.numeric(args[6:GetRowCount(args)])
        }

        LPR(folder, 2, k.max, algorithm, cvi, cores)
    } else {
        stop("Unsupported action. Available actions are:
1) PrepareANDLPs <instance> <database> <folder>: Calls PrepareANDLPs function.
2) LPR <folder> <algorithm> <cvi> <cores (optional param)> <k.max (one or more optional params)>: Calls LPR function.")
    }
}

# CMD EXECUTION ----

write("Loading CMD parameters...", stdout())
args = commandArgs(trailingOnly = TRUE)
Execute(args)
