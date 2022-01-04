#' Now: Returns current time as string.
#' @return A string representing current time.
Now = function() return(paste(Sys.time(), sep = ""))

#' Log: Logs current time and given message.
#' @param message: A single object, vector or a list of objects to be concatenated into a log message.
#' @return Stopwatch that can be used with Elapsed function.
Log = function(message) {
    log.prefix = paste("[", Now(), "]", sep = "")
    log.message = paste(message, collapse = "")
    write(paste(log.prefix, log.message), stdout())
    return(Stopwatch())
}

#' Stopwatch: Creates and starts new stopwatch.
#' @return Stopwatch that can be used with Elapsed function.
Stopwatch = function() return(proc.time())

#' Elapsed: Extracts elapsed number of seconds from stopwatch.
#' @param stopwatch: Stopwatch created with the Stopwatch function.
#' @return Elapsed number of seconds.
Elapsed = function(stopwatch) return(as.integer((proc.time() - stopwatch)[3]))

#' Limit: Limits value to [min, max] range.
#' @param value: Numeric value.
#' @param min: Min value.
#' @param max: Max value.
#' @return Limited value.
Limit = function(value, min = NA, max = NA) {
    if (!is.na(min) && value < min) value = min
    if (!is.na(max) && value > max) value = max
    return(value)
}

#' DateTime: Creates DateTime values.
#' @param strings: Strings in DateTime format.
#' @param format: DateTime format (check ?strptime for more details).
#' @return DateTime values.
DateTime = function(strings, format = '%F %T') {
    s = gsub(" ", "-", strings)
    f = gsub(" ", "-", format)
    return(as_datetime(s, format = f))
}

#' DatePart: Extracts DateTime parts.
#' @param timestamps: DateTime values.
#' @param part: DateTime part in DateTime format (check ?strptime for more details).
#' @return DateTime parts.
DatePart = function(timestamps, part) return(as.numeric(format(timestamps, part)))

#' Combine: Cartesian product of strings.
#' @param a: Collection of strings.
#' @param b: Collection of strings.
#' @return Each string from a concatenated with each string from b.
Combine = function(a, b) {
    c = as.data.frame(expand.grid(unlist(b), unlist(a)))
    return(paste(c[,2],c[,1]))
}

#' Count: Counts the table rows or collection items.
#' @param data: Table (matrix or data frame) or collection (vector or list).
#' @return The number of table rows or collection length.
Count = function(data) {
    if (is.null(data)) return(0)
    if (is.matrix(data) || is.data.frame(data)) return(nrow(data))
    if (is.vector(data) || is.list(data)) return(length(data))
    return(0)
}

#' Join: Joins columns.
#' @param columns1: Data frame, matrix or verctor with first set of columns.
#' @param columns2: Data frame, matrix or verctor with second set of columns.
#' @param colnames1: String or vector with names of columns in the first set.
#' @param colnames2: String or vector with names of columns in the second set.
#' @param rownames: String or vector with names of rows in the resulting table.
#' @return New table with given columns.
Join = function(columns1, columns2, colnames1 = NULL, colnames2 = NULL, rownames = NULL) {
    Bind = function(table1, table2) {
        if (Count(table1) == 0) return(table2)
        if (Count(table2) == 0) return(table1)
        return(as.data.frame(cbind(table1, table2)))
    }

    table1 = as.data.frame(columns1)
    table2 = as.data.frame(columns2)
    table = as.data.frame(Bind(table1, table2))

    if (Count(colnames1) > 0) colnames(table)[1:ncol(table1)] = colnames1
    if (Count(colnames2) > 0) colnames(table)[(ncol(table1) + 1):ncol(table)] = colnames2
    if (Count(rownames) > 0) row.names(table) = rownames

    return(as.data.frame(table))
}

#' Union: Creates a union of rows.
#' @param rows1: Data frame, matrix or verctor with first set of rows.
#' @param rows2: Data frame, matrix or verctor with second set of rows.
#' @param rownames1: String or vector with names of rows in the first set.
#' @param rownames2: String or vector with names of rows in the second set.
#' @param colnames: String or vector with names of columns in the resulting table.
#' @return New table with given rows.
Union = function(rows1, rows2, colnames = NULL, rownames = NULL) {
    Bind = function(table1, table2) {
        if (Count(table1) == 0) return(table2)
        if (Count(table2) == 0) return(table1)
        if (is.data.frame(table1) && Count(colnames(table1)) == 0) colnames(table1) = 1:ncol(table1)
        if (is.data.frame(table2) && Count(colnames(table2)) == 0) colnames(table2) = 1:ncol(table2)
        return(as.data.frame(rbind(table1, table2)))
    }

    table = as.data.frame(Bind(rows1, rows2))

    if (Count(colnames) > 0) colnames(table) = colnames
    if (Count(rownames) > 0) row.names(table) = rownames

    return(table)
}

#' Subset: Creates subset while preserving row and column names.
#' @param table: Table (data frame or matrix).
#' @param rows: Selected rows.
#' @param cols: Selected columns
#' @return Subset of the table.
Subset = function(table, rows = 1:nrow(table), cols = 1:ncol(table)) {
    GetNames = function(names, indexes) {
        selected = na.omit(names[indexes])
        if (Count(selected) == 0) return(indexes)
        return(selected)
    }

    s = as.data.frame(table[rows,cols])
    row.names(s) = GetNames(row.names(table), rows)
    colnames(s) = GetNames(colnames(table), cols)
    return(s)
}

#' Pivot: Pivots the table.
#' @param table: Data frame or matrix.
#' @param rowvars: Variables to be united into row discriminators.
#' @param colvars: Variables to be united into column discriminators.
#' @param impute: Shows whether or not to impute missing values using mean.
#' @return Reshaped table.
Pivot = function(table, rowvars, colvars, impute = TRUE) {
    if (Count(table) == 0) return(c())

    data = table %>% unite(rowvar, rowvars, sep = ".") %>% unite(colvar, colvars, sep = ".")
    reshaped = reshape(data, direction = "wide", idvar = "rowvar", timevar = "colvar")
    row.names(reshaped) = reshaped[, "rowvar"]
    reshaped = reshaped[, colnames(reshaped) != "rowvar"]
    reshaped = as.data.frame(reshaped)
    if (impute) reshaped = as.data.frame(as.matrix(Hmisc::impute(reshaped, mean)))
    return(reshaped)
}

#' Slide: Slides across table rows.
#' @param table: Data frame or matrix.
#' @param slider: Vector of relative row indexes.
#' @param trim: Shows whether or not to remove rows with non-existing values.
#' @return Array with 3 dimensions: examples (table rows), steps (slider) and variables (table columns).
Slide = function(table, slider, trim = TRUE) {
    GetNames = function(prefix, indexes) return(paste(prefix, indexes, sep = ""))
    GetExampleNames = function(table) return(GetNames("E", 1:Count(table)))
    GetVariableNames = function(table) return(GetNames("V", 1:ncol(table)))
    GetStepNames = function(steps) return(GetNames("S", steps))
    Missing = function(count) return(rep(NA, count))
    Backward = function(values, count) {
        selected = 1:(Count(values) - count)
        return(c(Missing(count), values[selected]))
    }
    Forward = function(values, count) {
        selected = (count + 1):Count(values)
        return(c(values[selected], Missing(count)))
    }
    Move = function(values, step) {
        if (step < 0) return(Backward(values, - step))
        if (step > 0) return(Forward(values, step))
        return(values)
    }
    SlideOne = function(values, steps) {
        windows = c()
        for (i in 1:Count(steps)) {
            window = Move(values, steps[i])
            windows = Join(windows, window)
        }
        if (trim) {
            windows = na.omit(windows)
        }

        return(as.matrix(windows))
    }
    SlideAll = function(table, slider) {
        example.count = NULL # Defined below.
        example.names = NULL # Defined below.
        variable.count = ncol(table)
        variable.names = GetVariableNames(table)
        step.count = Count(slider)
        step.names = GetStepNames(slider)

        result = c()
        for (v in 1:variable.count) {
            windows = SlideOne(table[, v], slider)
            result = c(result, windows)
            example.count = Count(windows)
            example.names = GetExampleNames(windows)
        }

        if (example.count == 0) return(array())

        dim.count = list(example.count, step.count, variable.count)
        dim.names = list(example.names, step.names, variable.names)
        return(array(result, dim = dim.count, dimnames = dim.names))
    }

    if (Count(table) == 0 || Count(slider) == 0) return(array())
    return(SlideAll(as.data.frame(table), unlist(slider)))
}

#' Tensor: Creates n-dimensional dataset with training/testing inputs and outputs.
#' IMPORTANT: The number of X and Y rows must the same (use NA to populate unknown values).
#' @param x: Table of X values (one column per variable and one row per example).
#' @param y: Table of Y values (one column per variable and one row per example).
#' @param horizon: The length of forecast horizon.
#' @param lookback: The length of Y past to take as input.
#' @param type: One of available tensor types:
#' 1) "MIMO": Tensor for Multiple-Input Multiple-Output forecast.
#' 2) "rec": Tensor for recursive (iterative) forecast.
#' @return Two arrays in 3D (examples, steps and variables):
#' 1) input: Past Y values combined with X values.
#' 2) output: Y values.
Tensor = function(x, y, horizon, lookback, type = "MIMO") {
    x = as.data.frame(x)
    y = as.data.frame(y)
    count = min(Count(x), Count(y))
    future = (lookback + 1):count
    backward = (1-lookback):0
    if (type == "rec") {
        past = 1:(count - 1)
        forward = 0
    } else {
        past = 1:(count - horizon)
        forward = 0:(horizon-1)
    }

    input.past = Slide(y[past,], backward)
    input.future = Slide(x[future,], forward)
    input = list(input.past, input.future)
    output = Slide(y[future,], forward)
    return(list(input = input, output = output))
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

#' ImportCSV: Imports CSV file to a table.
#' @param folder: Export folder name.
#' @param file: Export file name.
#' @param ext: File extension.
#' @param separator: Column separator.
#' @param rows: Shows whether or not to export row names.
#' @param verbose: Shows whether or not to log file path.
#' @return Imported csv file as a table.
ImportCSV = function(folder, file, ext = "csv", separator = "|", rows = FALSE, verbose = FALSE) {
    path = GetPath(folder, file, ext)
    if (verbose) Log(c("Importing ", path))
    if (rows) return(read.table(file = path, sep = separator, header = TRUE, row.names = 1))
    return(read.table(file = path, sep = separator, header = TRUE))
}

#' ExportCSV: Exports table to a CSV file.
#' @param table: Table to be exported.
#' @param folder: Export folder name.
#' @param file: Export file name.
#' @param ext: File extension.
#' @param separator: Column separator.
#' @param rows: Shows whether or not to export row names.
#' @param verbose: Shows whether or not to log file path.
ExportCSV = function(table, folder, file, ext = "csv", separator = "|", rows = FALSE, verbose = FALSE) {
    dir.create(folder)
    path = GetPath(folder, file, ext)
    if (verbose) Log(c("Exporting ", path))
    dir.create(paste(folder, collapse = ""), recursive = TRUE, showWarnings = FALSE)
    write.table(table, file = path, sep = separator, col.names = TRUE, row.names = rows, quote = FALSE)
}

#' OpenSqlConnection: Opens trusted connection to SQL Server database.
#' @param instance: SQL Server instance.
#' @param database: SQL Server database.
#' @param verbose: Shows whether or not to log connection details.
#' @return An open connection to SQL Server database.
OpenSqlConnection = function(instance, database, verbose = FALSE) {
    string = paste("driver={SQL Server};server=.\\", instance, ";database=", database, ";trusted_connection=true", sep = "")
    if (verbose) Log(c("Connecting to SQL Server: ", string))

    connection = RODBC::odbcDriverConnect(string)
    if (connection == -1) {
        stop("Failed to connect to SQL Server.")
    } else {
        if (verbose) Log("Connected to SQL Server.")
    }

    return(connection)
}

#' Num2Str: Converts numeric vector to string.
#' @param v: Named vector with numeric values.
#' @param sep1: Separator of names and values.
#' @param sep2: Separator of vector elements.
#' @param dec: Number of decimals to use.
#' @return Numeric vector as one string.
Num2Str = function(v, sep1 = "=", sep2 = ", ", dec = 3) {
    words = c()
    for (i in 1:Count(v)) {
        name = names(v)[i]
        value = round(v[i], dec)
        words = c(words, paste(name, value, sep = sep1))
    }

    return(paste(words, collapse = sep2))
}