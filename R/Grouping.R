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
#' 1) "CHI": Calinski-Harabasz Index
#' 2) "DBI": Davies-Bouldin Index
#' @param cores: Number of cores to use in the search for an optimal number of clusters (NULL == all cores).
#' @return A list, containing:
#' 1) clusters: Vector of clusters assigned to each profile.
#' 2) centroids: Table of cluster centroids (one row per centroid).
#' 3) validity: Cluster validity according to CVI.
Cluster = function(profiles, k.min, k.max, algorithm = "TSGA", cvi = "CHI", cores = NULL) {
    BKM = function(profiles, k.max) {
        Log(c("Performing BKM until k = ", k.max, "..."))
        clusters = rep(1, Count(profiles))
        errors = rep(.Machine$double.xmax, Count(profiles))
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
        profiles.rowcount = Count(profiles)
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
        if (Count(profiles) == 0) {
            return(c())
        }

        group.by.cluster = as.formula(paste(". ~", "cluster"))
        values.clustered = Join(profiles, clusters, colnames2 = "cluster")
        centroids = aggregate(group.by.cluster, values.clustered, mean)[, 2:(ncol(profiles) + 1)]
        return(as.data.frame(centroids))
    }

    TSGA = function(profiles, k.min, k.max, cvi) {
        Log("Applying TSGA...")

        profiles.reduced = ExtractFeatures(profiles);
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
        centroids.unique.rowcount = Count(unique(SOM.centroids))
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

        profiles.reduced = ExtractFeatures(profiles);
        Log(c("Performing k-Shape for each k between ", k.min, " and ", k.max, "..."))
        result = foreach(k = k.min:k.max, .combine = TakeBetter, .packages = c("clusterSim", "dtwclust")) %dopar% {
            clusters = dtwclust::tsclust(as.matrix(profiles.reduced), type = "partitional", k = k, preproc = "zscore", distance = "sbd", centroid = "shape")@cluster
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

        profiles.reduced = ExtractFeatures(profiles);
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

    profiles.rowcount = Count(profiles)
    if (profiles.rowcount == 0) {
        Log("Clustering skipped. There are no profiles to cluster.")
        return(list(clusters = c(), centroids = c(), validity = NA))
    }

    k.min = Limit(k.min, max = k.max)
    watch = Log(c("Clustering started (", profiles.rowcount, " profiles, dimensionality = ", ncol(profiles), ", k.min = ", k.min, ", k.max = ", k.max, ")..."))
    registerDoParallel(cores = cores)

    clusters = NULL
    centroids = NULL
    validity = NULL

    Log("Checking for profile duplicates...")
    profiles.unique = unique(profiles)
    profiles.unique.rowcount = Count(profiles.unique)
    if (profiles.unique.rowcount <= k.min) {
        Log(c("Too many duplicates. Using unique rows (", profiles.unique.rowcount, ") as clusters..."))
        clusters = merge(profiles, Join(profiles.unique, 1:profiles.unique.rowcount, colnames2 = "cluster"))[, "cluster"]
        validity = NA
    } else {
        k.max = Limit(k.max, max = profiles.unique.rowcount - 2)
        k.min = Limit(k.min, max = k.max)

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

    Log(c("Clustering finished (", Count(centroids), " cluster(s), validity = ", round(validity, 3), ", duration = ", Elapsed(watch), " sec)."))
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
    profiles.rowcount = Count(profiles)
    if (profiles.rowcount == 0) {
        Log("Classification skipped. There are no profiles to classify.")
        return(list(clusters = c(), validity = NA))
    }

    centroids.rowcount = Count(centroids)
    watch = Log(c("Classification started (", profiles.rowcount, " profiles, ", centroids.rowcount, " centroids)..."))

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

    Log(c("Classification finished (duration = ", Elapsed(watch), " sec)."))

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
#' 1) "CHI": Calinski-Harabasz Index
#' 2) "DBI": Davies-Bouldin Index
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
Group = function(profiles, k.min, k.max, algorithm.clustering = "TSGA", cvi = "CHI", cores = NULL,
    algorithm.classification = "SVM", validate.classification = FALSE) {
    if (Count(profiles) == 0) {
        Log("Grouping skipped. There are no profiles to group.")
        return(list(groups = c(), k = 0, cluster.validity = NA, classification.validity = NA, duration = 0))
    }

    profilesToCluster = profiles
    profilesToClassify = NULL # TODO: Detect profiles with damaged or missing data (future work).
    watch = Stopwatch()
    clust = Cluster(profilesToCluster, k.min, k.max, algorithm.clustering, cvi, cores)
    class = Classify(profilesToClassify, clust$centroids, algorithm.classification, validate.classification)
    groups.profilesToCluster = Join(row.names(profilesToCluster), clust$clusters)
    groups.profilesToClassify = Join(row.names(profilesToClassify), class$clusters)
    groups.rownames = 1:(Count(groups.profilesToCluster) + Count(groups.profilesToClassify))
    groups = Union(groups.profilesToCluster, groups.profilesToClassify, colnames = c("Profile", "Group"), rownames = groups.rownames)
    k = Count(clust$centroids)
    if (k == 0) { k = Count(class$centroids) }
    duration = Elapsed(watch)
    return(list(groups = groups, k = k, cluster.validity = clust$validity, classification.validity = class$validity, duration = duration))
}
