#' DCL: Regression model for Deep Centroid Learning (DCL).
#' @param x.count: The number of X variables.
#' @param y.count: The number of Y variables.
#' @param horizon: The length of forecast horizon.
#' @param lookback: The length of Y past to take as input.
#' @param cnn.depth: The number if CNN layers.
#' @param cnn.filters: The number of CNN filters.
#' @param cnn.kernel: The size of CNN kernels.
#' @param cnn.pool: The size of CNN pools.
#' @param cnn.dropout: Fraction of neurons to drop from CNN layers.
#' @param cnn.type: The type of CNN pools: "max" or "avg".
#' @param s2s.units: The number of S2S encoder units in one direction.
#' @param s2s.dropout: Fraction of encoded S2S context to drop.
#' @param sae.depth: The number of SAE layers.
#' @param sae.middle: The number of neurons in the middle of SAE.
#' @param sae.dropout: Fraction of neurons to drop from SAE.
#' @return Compiled keras-based DCL regression model.
DCL = function(x.count,
               y.count,
               horizon,
               lookback = 7*horizon,
               cnn.depth = 5,
               cnn.filters = 512,
               cnn.kernel = 2,
               cnn.pool = 2,
               cnn.dropout = 0.5,
               cnn.type = "avg",
               s2s.units = 64,
               s2s.dropout = 0,
               sae.depth = 11,
               sae.middle = 16,
               sae.dropout = 0.5) {
    y.past = k_Input(y.count, lookback, "PastY")
    x = k_Input(x.count, horizon, "X")
    y = y.past %>%
        k_CNN(1, cnn.depth, cnn.filters, cnn.kernel, cnn.pool, cnn.dropout, cnn.type) %>%
        k_S2S(x, "LSTM", s2s.units, s2s.dropout, sae.depth, sae.middle, sae.dropout) %>%
        layer_dense(y.count, name = "Y")
    return(k_Model(y.past, x, y, 'mse', k_CVRMSE))
}

#' DMN: Deep Mixture Network (DMN) model.
#' @param x.count: The number of X variables.
#' @param y.count: The number of Y variables.
#' @param horizon: The length of forecast horizon.
#' @param lookback: The length of Y past to take as input.
#' @param cnn.depth: The number if CNN layers.
#' @param cnn.filters: The number of CNN filters.
#' @param cnn.kernel: The size of CNN kernels.
#' @param cnn.pool: The size of CNN pools.
#' @param cnn.dropout: Fraction of neurons to drop from CNN layers.
#' @param cnn.type: The type of CNN pools: "max" or "avg".
#' @param s2s.units: The number of S2S encoder units in one direction.
#' @param s2s.dropout: Fraction of encoded S2S context to drop.
#' @param m: The number of mixture components.
#' @param l1: L1 (lasso) regularization factor.
#' @param l2: L2 (ridge) regularization factor.
#' @return Compiled keras-based DMN model.
DMN = function(x.count,
               y.count,
               horizon,
               lookback = 7*horizon,
               cnn.depth = 1,
               cnn.filters = 50,
               cnn.kernel = 2,
               cnn.pool = 2,
               cnn.dropout = 0.2,
               cnn.type = "max",
               s2s.units = 128,
               s2s.dropout = 0.5,
               m = 25,
               l1 = NULL,
               l2 = NULL) {
    y.past = k_Input(y.count, lookback, "PastY")
    x = k_Input(x.count, horizon, "X")
    y = y.past %>%
        k_CNN(1, cnn.depth, cnn.filters, cnn.kernel, cnn.pool, cnn.dropout, cnn.type) %>%
        k_S2S(x, "GRU", s2s.units, s2s.dropout) %>%
        k_Drop(0.5, name = "S2S_drop") %>%
        layer_dense(500, name = "FCN_feed1") %>%
        k_Drop(0.25, name = "FCN_drop2") %>%
        layer_dense(500, name = "FCN_feed2") %>%
        k_Drop(0.25, name = "FCN_drop3") %>%
        layer_dense(500, name = "FCN_feed3") %>%
        layer_dense(500, name = "FCN_feed4", activation="relu") %>%
        k_MDN(m, name = "Y")
    return(k_Model(y.past, x, y, k_GME, k_GME))
}

#' DEL: Percentile-based regression model for Deep Ensemble Learning (DEL).
#' @param x.count: The number of X variables.
#' @param y.count: The number of Y variables.
#' @param horizon: The length of forecast horizon.
#' @param lookback: The length of Y past to take as input.
#' @param units: The number of encoded units in S2S architecture.
#' @param dropout: Fraction of neurons to drop (randomly sampled from given vector).
#' @param sd: Standard deviation for Gaussian noise (randomly sampled from given vector).
#' @param l1: L1 (lasso) regularization factor.
#' @param l2: L2 (ridge) regularization factor.
#' @return Compiled keras-based DEL regression model.
DEL = function(x.count,
               y.count,
               horizon,
               lookback = 7*horizon,
               units = 128,
               dropout = c(0.15, 0.2, 0.25, 0.3),
               sd = c(0.0, 0.01, 0.02, 0.03),
               l1 = NULL,
               l2 = NULL) {
    y.past = k_Input(y.count, lookback, "PastY")
    x = k_Input(x.count, horizon, "X")
    y = y.past %>%
        k_S2S(x, "GRU", units) %>%
        layer_dropout(sample(dropout, 1), name = "Dropout") %>%
        layer_gaussian_noise(sample(sd, 1), name = "Noise") %>%
        layer_dense(y.count*Count(k_Percentiles()), activity_regularizer=k_L1L2(l1,l2), name = "Y")
    return(k_Model(y.past, x, y, k_QS, k_QS))
}

#' DeepAR: Deep Auto-Regression (DeepAR) model.
#' @param horizon: The length of forecast horizon.
#' @param lookback: The length of Y past to take as input.
#' @param iterations: Maximum number of training iterations.
#' @param patience: Number of iterations for early stopping.
#' @param batch: Batch size for each training iteration.
#' @param frequency: Value frequency (pandas offset alias).
#' @param dropout: Fraction of neurons to drop.
#' @return GluonTS-based DeepAR model.
DeepAR = function(horizon,
                  lookback,
                  iterations = 200,
                  patience = 20,
                  batch = 128,
                  frequency = 'H',
                  dropout = 0.5) {
    model = deep_ar(id = "id",
                    freq = frequency,
                    prediction_length = horizon,
                    lookback_length = lookback,
                    dropout = dropout,
                    epochs = iterations,
                    batch_size = batch,
                    patience = patience)
    return(model)
}

#' TrainDNN: Trains Deep Neural Network (DNN) model.
#' @param model: DNN model (e.g. result of DCL function).
#' @param x: Table of X values (one column per variable and one row per example).
#' @param y: Table of Y values (one column per variable and one row per example).
#' @param horizon: The length of forecast horizon.
#' @param lookback: The length of Y past to take as input.
#' @param type: One of available types of forecast models:
#' 1) "DCL": regression model for Deep Centroid Learning (DCL)
#' 2) "DEL": regression model for Deep Ensemble Learning (DEL)
#' 3) "DMN": Deep Mixture Network (DMN)
#' 4) "DeepAR": Deep Auto-Regression (DeepAR)
#' @param validation: Fraction of examples to use for validation (keras-specific).
#' @param iterations: Maximum number of training iterations (keras-specific).
#' @param patience: Number of iterations for early stopping (keras-specific).
#' @param batch: Batch size for each training iteration (keras-specific).
#' @param verbose: Shows whether or not to log details (keras-specific).
#' @return Trained DNN model.
TrainDNN = function(model,
                    x,
                    y,
                    horizon,
                    lookback,
                    type = "DCL",
                    validation = 0,
                    iterations = 200,
                    patience = 20,
                    batch = 128,
                    verbose = FALSE) {
    if (type == "DeepAR") return(model %>% g_Train(colnames(y), x, y))

    t = Tensor(x, y, horizon, lookback, "MIMO")
    model %>% k_Train(t$input, t$output, validation, iterations, patience, batch, verbose)
    return(model)
}

#' TestDNN: Tests Deep Neural Network (DNN) model.
#' IMPORTANT: The number of X and Y rows must the same (use NA to populate unknown values).
#' @param model: Trained DNN model (result of TrainDNN function).
#' @param x: Table of X values (one column per variable and one row per example).
#' @param y: Table of Y values (one column per variable and one row per example).
#' @param horizon: The length of forecast horizon.
#' @param lookback: The length of Y past to take as input.
#' @param type: One of available types of forecast models:
#' 1) "DCL": regression model for Deep Centroid Learning (DCL)
#' 2) "DEL": regression model for Deep Ensemble Learning (DEL)
#' 3) "DMN": Deep Mixture Network (DMN)
#' 4) "DeepAR": Deep Auto-Regression (DeepAR)
#' @param rep: The number of times to repeat tests (DeepAR-specific).
#' @return Y forecast.
TestDNN = function(model, x, y, horizon, lookback, type = "DCL", rep = 200) {
    if (type == "DeepAR") return(model %>% g_Test(colnames(y), tail(x, horizon), horizon, rep))

    t = Tensor(x, y, horizon, lookback, "MIMO")
    return(model %>% k_Test(t$input))
}

#' SaveDNN: Saves DNN model to file.
#' @param model: Trained DNN model.
#' @param folder: Destination folder.
#' @param file: Destination file name.
#' @param type: One of supported DNN types:
#' 1) "DCL": regression model for Deep Centroid Learning (DCL)
#' 2) "DEL": regression model for Deep Ensemble Learning (DEL)
#' 3) "DMN": Deep Mixture Network (DMN)
SaveDNN = function(model, folder, file, type = "DCL") {
    if (type == "DeepAR") {
        model %>% g_Save(paste(folder, file, sep="\\"))
    } else {
        model %>% k_Save(GetPath(folder, file, type))
    }
}

#' LoadDNN: Loads DNN model from file.
#' @param folder: Source folder.
#' @param file: Source file name.
#' @param type: One of supported DNN types:
#' 1) "DCL": regression model for Deep Centroid Learning (DCL)
#' 2) "DEL": regression model for Deep Ensemble Learning (DEL)
#' 3) "DMN": Deep Mixture Network (DMN)
#' @return Saved DNN model.
LoadDNN = function(folder, file, type = "DCL") {
    GetCustom = function () {
        if (type == "DCL") return(list('python_function' = k_CVRMSE))
        if (type == "DMN") return(list('python_function' = k_GME))
        if (type == "DEL") return(list('python_function' = k_QS))
        return(NULL)
    }

    if (type == "DeepAR") return(g_Load(paste(folder, file, sep="\\")))
    return(k_Load(GetPath(folder, file, type), GetCustom()))
}