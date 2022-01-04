#' PrepareANDLPs: Prepares concatenated Average Normalized Load Profiles (ANDLPs).
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
    GetDataSets = function(connection) return(RODBC::sqlQuery(connection, "EXEC GetDataSets"))

    GetANDLPs = function(connection, dataset) {
        ds = dataset$DataSetID
        CLT = dataset$CharacteristicLoadType
        dim = dataset$NumberOfMeasurementsPerDay
        query = paste("EXEC GetANDLPs ", ds, ", ", CLT, ", ", dim)
        return(RODBC::sqlQuery(connection, query))
    }

    datasets = GetDataSets(connection)
    for (row in c(1:Count(datasets))) {
        dataset = datasets[row,]
        ANDLPs = GetANDLPs(connection, dataset)
        profiles = ANDLPs %>% Pivot("ConsumerID", c("CharacteristicPeriod", "CharacteristicDay", "PorQ"))
        DS = dataset$DataSetID
        CLT = dataset$CharacteristicLoadType
        ExportCSV(profiles, folder, c("DS", DS, "CLT", CLT), rows = TRUE)
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
#' 1) "CHI": Calinski-Harabasz Index
#' 2) "DBI": Davies-Bouldin Index
#' @param cores: Number of cores to use in the search for an optimal number of clusters (NULL == all cores).
LPR = function(folder, k.min, k.max, algorithm, cvi, cores = NULL) {
    all.files = list.files(folder, pattern = NULL, all.files = FALSE, full.names = FALSE)
    csv.files = gsub(".csv", "", all.files[endsWith(all.files, ".csv")])
    Log(c("Processing ", Count(csv.files), " datasets..."))

    time = Now()
    statistics = c()
    for (file in csv.files) {
        profiles = ImportCSV(folder, file, rows = TRUE)
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

#' Execute: Executes R functions from CMD.
#' @param args: One of available R functions:
#' 1) PrepareANDLPs
#' 2) LPR
Execute = function(args) {
    if (Count(args) > 0) {
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
            if (Count(args) > 4) {
                cores = as.numeric(args[5])
            }

            k.max = c(10, 50, 100, 500)
            if (Count(args) > 5) {
                k.max = as.numeric(args[6:Count(args)])
            }

            LPR(folder, 2, k.max, algorithm, cvi, cores)
        } else {
            write("Available functions (more details in comments):
1) PrepareANDLPs <instance> <database> <folder>
2) LPR <folder> <algorithm> <cvi> <cores (optional param)> <k.max (one or more optional params)>", stdout())
        }
    }
}

# CMD EXECUTION ----
Execute(commandArgs(trailingOnly = TRUE))
