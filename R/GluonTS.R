#' g_Dataset: GluonTS dataset
#' @param ids: Variable IDs (e.g. column names).
#' @param timestamps: Vector of timestamps (strings).
#' @param values: Table of values (one column per variable).
#' @return Dataset for training or testing GluonTS models.
g_Dataset = function(ids, timestamps, values = NULL) {
    id = factor(rep(ids, each=Count(timestamps)))
    time = as_datetime(rep(unlist(timestamps), times=Count(ids)))
    if (is.null(values)) return(tibble(id=id, time=time))
    return(tibble(id=id, time=time, value=unlist(values)))
}

#' g_Train: Trains GluonTS model.
#' @param model: GluonTS model.
#' @param ids: Variable IDs (e.g. column names).
#' @param timestamps: Vector of timestamps (strings).
#' @param values: Table of values (one column per variable).
#' @param engine: GluonTS engine.
#' @return Trained GluonTS model.
g_Train = function(model, ids, timestamps, values, engine = "gluonts_deepar") {
    ShouldFit = function() return("model_spec" %in% class(model))
    FitData = function(set) return(model %>% parsnip::set_engine(engine) %>% fit(value ~ time + id, set))
    RefitData = function(set) {
        refit = model %>% modeltime_table() %>% modeltime_calibrate(new_data=set) %>% modeltime_refit(set)
        return(refit$.model[[1]])
    }

    set = g_Dataset(ids, timestamps, values)
    if (ShouldFit()) {
        return(FitData(set))
    } else {
        return(RefitData(set))
    }
}

#' g_Test: Tests GluonTS model.
#' @param model: Trained GluonTS model.
#' @param timestamps: Vector of timestamps (strings).
#' @param horizon: The length of forecast horizon.
#' @param rep: The number of times to repeat tests.
#' @return Value forecast.
g_Test = function(model, ids, timestamps, horizon, rep = 25) {
    Forecast = function() {
        data = data.frame()
        set = g_Dataset(ids, timestamps)
        for(i in 1:rep) data = Join(data, model %>% predict(set))
        return(as.matrix(data))
    }
    Transform = function(forecast, fun, name) {
        data = forecast %>%
            apply(1, fun) %>%
            matrix(ncol = Count(ids), nrow = horizon) %>%
            as.data.frame()
        colnames(data) = paste(ids, name, sep="_")
        return(data)
    }
    Prepare = function(forecast) {
        avg = forecast %>% Transform(mean, "AVG")
        sd = forecast %>% Transform(sd, "SD")

        data = data.frame()
        for(i in 1:Count(ids)) {
            avg_sd = Join(avg[,i], sd[,i], colnames(avg)[i], colnames(sd)[i])
            data = Join(data, avg_sd)
        }

        return(data)
    }

    return(Forecast() %>% Prepare())
}

#' g_Save: Saves GluonTS model to disk.
#' @param model: Trained GluonTS model.
#' @param folder: Destination folder.
g_Save = function(model, folder) {
    tryCatch({
        dir.create(folder)
        model %>% save_gluonts_model(folder, overwrite = TRUE)
    }, error = function(e) { Log(c('Failed to save GluonTS model: ', e)) })
}

#' g_Load: Loads GluonTS model from disk.
#' @param folder: Destination folder.
#' @return Saved GluonTS model.
g_Load = function(folder) {
    tryCatch({ load_gluonts_model(folder) }, error = function(e){ return(NULL) })
}