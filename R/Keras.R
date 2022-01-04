#' k_Name: keras layer name.
#' @param base: Composition name.
#' @param part: Composition part (layer) name.
#' @param order: The order of the layer in the composition.
k_Name = function(base, part, order="") return(paste(base, "_", part, order, sep = ""))

#' k_Shape: keras-based access to tensor shape.
#' @param t: Tensor data.21
#' @return Size of each dimension in a vector.
k_Shape = function(t) {
    shape = c()
    dims = k_int_shape(t)
    for(i in 1:Count(dims)) {
        shape = c(shape, Limit(sum(dims[[i]]), min=1))
    }
    
    return (shape)
}

#' k_Select: keras-based access to tensor data.
#' @param t: Tensor data.
#' @param d1: Dimension 1.
#' @param d2: Dimension 2.
#' @param d3: Dimension 3.
#' @return Tensor data.
k_Select = function(t, d1, d2, d3) return(k_gather(k_gather(k_gather(t, d1), d2), d3))

#' k_NNELU: keras-based Non-Negative Exponential Linear Unit (NNELU).
#' @param input: Input tensor.
#' @return NNELU.
k_NNELU = function(input) return(k_constant(1)+activation_elu(input))

#' k_RMSE: keras-based Root Mean Square Error (RMSE).
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @return RMSE.
k_RMSE = function(y_true, y_pred) return(k_sqrt(metric_mean_squared_error(y_true, y_pred)))

#' k_CVRMSE: keras-based Coefficient of Variation (CV) of the Root Mean Square Error (RMSE).
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @return CV(RMSE)[%].
k_CVRMSE = function(y_true, y_pred) return(100 * k_RMSE(y_true, y_pred) / k_mean(y_true))

#' k_GME: keras-based Gaussian Mixture Error (GME).
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @return GME.
k_GME = function(y_true, y_pred) {
    GME = function(x, alpha, means, sigma, dim=1) {
        a = k_log(alpha)
        b = (dim/2) * k_log(2*pi*sigma)
        c = (x-means)^2 / (2*sigma^2)
        return(-k_log(k_sum(k_exp(a-b-c))))
    }

    shape = k_Shape(y_pred)
    samples = 1:shape[1]
    steps = 1:shape[2]
    m = shape[3]/3

    sum = 0
    count = 0
    for(sample in samples) {
        for(step in steps) {
            y = y_true %>% k_Select(sample,step,1)
            alpha = y_pred %>% k_Select(sample,step,1:m)
            means = y_pred %>% k_Select(sample,step,(m+1):(2*m))
            sigma = y_pred %>% k_Select(sample,step,(2*m+1):(3*m))
            sum = sum + GME(y,alpha,means,sigma)
            count = count + 1
        }
    }

    return(sum/count)
}

#' k_Percentiles: Percentiles for Quantile Score (QS).
#' Change the function to minimize execution time for quantile regression.
#' For example, use c(1, seq(5, 95, by=5), 99) instead of 1:99.
#' @return Percentiles for k_QS.
k_Percentiles = function() return(1:99)

#' k_QS: keras-based Quantile Score (QS).
#' @param y_true: Actual values.
#' @param y_pred: Expected values.
#' @return QS for k_Percentiles.
k_QS = function(y_true, y_pred) {
    GetY = function(percentiles) return(k_repeat_elements(y_true, Count(percentiles), 3))
    GetQ = function(percentiles) {
        shape = k_Shape(y_true)

        q = list()
        for(i in 1:Count(percentiles)) {
            q[[i]] = k_constant(percentiles[i]/100, shape = shape)
        }
        
        return(k_concatenate(q))
    }
    GetQS = function(y, q, yq) return(return(k_mean(k_maximum((1-q)*(yq-y), q*(y-yq)))))

    p = k_Percentiles()
    return(GetQS(GetY(p), GetQ(p), y_pred))
}

#' k_L1L2: keras L1 and/or L2 regularizer.
#' @param l1: L1 (lasso) regularization factor.
#' @param l2: L2 (ridge) regularization factor.
#' @return L1 and/or L2 regularizer.
k_L1L2 = function(l1 = NULL, l2 = NULL) {
    Has = function(l) return(!is.null(l) && !is.na(l) && l > 0)
    if (Has(l1) && Has(l2)) return(regularizer_l1_l2(l1 = l1, l2 = l2))
    if (Has(l1)) return(regularizer_l1(l1))
    if (Has(l2)) return(regularizer_l2(l2))
    return(NULL)
}

#' k_Input: keras input layer for time series.
#' @param variables: The number of time series variables.
#' @param steps: The number of time steps per time series.
#' @param name: Layer name.
#' @return Input layer.
k_Input = function(variables, steps, name = "Input")
    return(layer_input(shape = list(steps, variables), name = name))

#' k_Drop: keras dropout layer.
#' @param input: Previous layer.
#' @param rate: Fraction of neurons to drop.
#' @param name: Layer name.
#' @return Dropout layer (or previous layer if the rate is 0).
k_Drop = function(input, rate, name = "drop") {
    if (rate <= 0) return(input)
    return(input %>% layer_dropout(rate, name = name))
}

#' k_CNN: keras-based Convolution Neural Network (CNN).
#' @param input: Previous layer.
#' @param dim: Dimensionality: 1, 2 or 3.
#' @param depth: The number of CNN layers.
#' @param filters: The number of filters in each convolutional layer.
#' @param kernel: The size of filters in each convolutional layer.
#' @param pool: The size of pools in each pooling layer.
#' @param dropout: Fraction of neurons to drop.
#' @param type: Pool type: "max" or "avg".
#' @param name: Layer name.
#' @return CNN layers.
k_CNN = function(input, dim = 1, depth = 1, filters = 256, kernel = 3, pool = 3, dropout = 0.5, type = "avg", name = "CNN") {
    Convolution = function(input, name) {
        if (dim == 3) return(input %>% layer_conv_3d(filters = filters,
                                                     kernel_size = kernel,
                                                     activation = "relu",
                                                     padding = "same",
                                                     name = name))

        if (dim == 2) return(input %>% layer_conv_2d(filters = filters,
                                                     kernel_size = kernel,
                                                     activation = "relu",
                                                     padding = "same",
                                                     name = name))

        return(input %>% layer_conv_1d(filters = filters,
                                       kernel_size = kernel,
                                       activation = "relu",
                                       padding = "same",
                                       name = name))
    }
    Pooling = function(input, name) {
        if (dim == 3) {
            if (type == "max") return(input %>% layer_max_pooling_3d(pool_size = pool, name = name))
            return(input %>% layer_average_pooling_3d(pool_size = pool, name = name))
        }
        if (dim == 2) {
            if (type == "max") return(input %>% layer_max_pooling_2d(pool_size = pool, name = name))
            return(input %>% layer_average_pooling_2d(pool_size = pool, name = name))
        }

        if (type == "max") return(input %>% layer_max_pooling_1d(pool_size = pool, name = name))
        return(input %>% layer_average_pooling_1d(pool_size = pool, name = name))
    }
    CNN = function(input, i)
        return(input %>%
               Convolution(k_Name(name, "conv", i)) %>%
               Pooling(k_Name(name, "pool", i)) %>%
               k_Drop(dropout, name = k_Name(name, "drop", i)))

    if (depth == 0) return(input)
    for (i in 1:depth) input = input %>% CNN(i)
    return(input)
}

#' k_SAE: keras-based fully-connected Stacked Autoencoder (SAE).
#' @param input: Previous layer.
#' @param depth: The number of feed-forward layers.
#' @param edge: The number of neurons on the edges.
#' @param middle: The number of neurons in the middle.
#' @param dropout: Fraction of neurons to drop.
#' @param name: Layer name.
#' @return SAE layers.
k_SAE = function(input, depth, edge, middle = edge/3, dropout = 0.5, name = "SAE") {
    GetUnits = function(min, max, count) {
        if (count <= 0) return(c())
        if (count == 1) return(c(max))
        if (count == 2) return(c(max, max))

        units = c(max)
        half = (count - 1) / 2
        step = floor((max - min) / floor(half))
        for (i in 1:floor(half)) {
            units = c(units, Limit(units[i] - step, min, max))
        }

        units = c(units, rev(units[1:ceiling(half)]))
        return(units)
    }
    FNN = function(input, units, i)
        return(input %>%
               layer_dense(units = units, activation = "tanh", name = k_Name(name, "feed", i)) %>%
               k_Drop(dropout, name = k_Name(name, "drop", i)))

    units = GetUnits(middle, edge, depth)
    if (Count(units) == 0) return(input)
    for (i in 1:Count(units)) input = input %>% FNN(units[i], i)
    return(input)
}

#' k_S2S: keras-based bidirectional Sequence-to-Sequence (S2S) Recurrent Neural Network (RNN),
#'        with attention mechanism and Stacked Autoencoder (SAE) applied to context vector.
#' @param past: Input layer with past Y states.
#' @param future: Input layer with future X states.
#' @param type: One of available recurrent cell types:
#' 1) "LSTM": Long Short-Term Memory (LSTM)
#' 2) "GRU": Gated Recurrent Unit (GRU)
#' @param units: The number of encoded units in one direction.
#' @param dropout: Fraction of encoded units to drop.
#' @param sae.depth: The number of SAE layers.
#' @param sae.middle: The number of neurons in the middle of SAE.
#' @param sae.dropout: Fraction of neurons to drop from SAE.
#' @param name: Layer name.
#' @return S2S layers.
k_S2S = function(past, future, type = "LSTM", units, dropout = 0, sae.depth = 0, sae.middle = units/3, sae.dropout = 0.5, name = "S2S") {
    RNN = function(units, states = FALSE, sequences = FALSE, name = NULL) {
        LSTM = function()
            return(layer_lstm(units = units,
                              return_state = states,
                              return_sequences = sequences,
                              name = name,
                              # GPU requirement:
                              activation = "tanh",
                              recurrent_activation = "sigmoid",
                              recurrent_dropout = 0,
                              use_bias = TRUE,
                              unroll = FALSE))
        GRU = function()
            return(layer_gru(units = units,
                             return_state = states,
                             return_sequences = sequences,
                             name = name,
                             # GPU requirement:
                             reset_after = TRUE,
                             activation = "tanh",
                             recurrent_activation = "sigmoid",
                             recurrent_dropout = 0,
                             use_bias = TRUE,
                             unroll = FALSE))

        if (type == "GRU") return(GRU())
        return(LSTM())
    }
    Encode = function(encoder.input, units) {
        encoder = RNN(units, states = TRUE)
        encoded = encoder.input %>% bidirectional(encoder, name = k_Name(name, "encoder"))
        return(encoded)
    }
    Decode = function(encoded, units, decoder.input) {
        EncodedOutput = function() return(encoded[1])
        EncodedStates = function() {
            GRUStates = function() {
                h = layer_concatenate(encoded[2:3], name = "hidden") %>%
                    k_Drop(dropout, name = k_Name(name, "hidden_drop")) %>%
                    k_SAE(sae.depth, units, sae.middle, sae.dropout, name = k_Name(name, "hidden_SAE"))
                return(h)
            }
            LSTMStates = function() {
                h = layer_concatenate(encoded[c(2, 4)], name = k_Name(name, "hidden")) %>%
                    k_Drop(dropout, name = k_Name(name, "hidden_drop")) %>%
                    k_SAE(sae.depth, units, sae.middle, sae.dropout, name = k_Name(name, "hidden_SAE"))
                c = layer_concatenate(encoded[c(3, 5)], name = k_Name(name, "cell")) %>%
                    k_Drop(dropout, name = k_Name(name, "cell_drop")) %>%
                    k_SAE(sae.depth, units, sae.middle, sae.dropout, name = k_Name(name, "cell_SAE"))
                return(list(h, c))
            }

            if (type == "GRU") return(GRUStates())
            return(LSTMStates())
        }
        Attention = function(decoded) {
            attention = layer_attention(c(decoded, EncodedOutput()), name = k_Name(name, "attention"))
            output = layer_concatenate(c(decoded, attention), name = k_Name(name, "output"))
            return(output)
        }

        decoder = RNN(units, sequences = TRUE, name = k_Name(name, "decoder"))
        decoded = decoder.input %>% decoder(initial_state = EncodedStates())
        return(Attention(decoded))
    }

    return(Encode(past, units) %>% Decode(2*units, future))
}

#' k_MDN: keras-based Mixed Density Network (MDN).
#' @param input: Previous layer.
#' @param m: The number of mixture components.
#' @param l1: L1 (lasso) regularization factor.
#' @param l2: L2 (ridge) regularization factor.
#' @param name: Layer name.
#' @return MDN layers.
k_MDN = function(input, m = 1, l1 = NULL, l2 = NULL, name = "MDN") {
    reg = k_L1L2(l1,l2)
    alpha = layer_dense(input, m, activation="softmax", activity_regularizer=reg, name=k_Name(name, "alpha"))
    means = layer_dense(input, m, activation=k_NNELU, activity_regularizer=reg, name=k_Name(name, "means"))
    sigma = layer_dense(input, m, activation=k_NNELU, activity_regularizer=reg, name=k_Name(name, "sigma"))
    mdn = layer_concatenate(list(alpha,means,sigma), name = name)
    return(mdn)
}

#' k_Model: Builds and compiles keras model.
#' @param y.past: Input layer with past Y states.
#' @param x: Input layer with future X states.
#' @param y: Input layer with future Y states.
#' @param loss: Loss function.
#' @param metric: Error metric.
#' @return Compiled keras model.
k_Model = function(y.past, x, y, loss = 'mse', metric = 'mse') {
    model = keras_model(inputs = list(y.past, x), outputs = y)
    model %>% compile(loss = loss, optimizer = "adam", metrics = metric)
    return(model)
}

#' k_Train: Trains keras model.
#' @param model: Compiled keras model.
#' @param x: Tensor with X examples.
#' @param y: Tensor with Y examples.
#' @param validation: Fraction of examples to use for validation.
#' @param iterations: Maximum number of training iterations.
#' @param patience: Number of iterations for early stopping.
#' @param batch: Batch size for each training iteration.
#' @param verbose: Shows whether or not to log details.
k_Train = function(model, x, y, validation = 0, iterations = 200, patience = 20, batch = 128, verbose = FALSE) {
    ValidationSplit = function(count) {
        validation = Limit(validation, 0, 0.5)
        split = ifelse(count < 10, 0, validation)
        return(split)
    }
    EarlyStopping = function() {
        patience = Limit(patience, min = 1)
        callback = callback_early_stopping(monitor = "python_function", mode = "min", patience = patience)
        return(callback)
    }

    if (verbose) model %>% summary()

    model %>% fit(x, y,
                  epochs = max(1, iterations),
                  batch_size = max(1, batch),
                  callbacks = list(EarlyStopping()),
                  validation_split = ValidationSplit(dim(y)[1]),
                  shuffle = FALSE,
                  verbose = as.numeric(verbose))
}

#' k_Test: Tests keras model.
#' @param model: Trained keras model.
#' @param x: Tensor with X examples.
#' @return Table with Y forecast.
k_Test = function(model, x) {
    GetOutput = function(tensor) {
        steps = dim(tensor)[2]
        variables = dim(tensor)[3]
        data = matrix(tensor, ncol = variables, nrow = steps)
        output = as.data.frame(data)
        colnames(output) = c()
        rownames(output) = c()
        return(output)
    }

    return(model %>% predict(x) %>% GetOutput())
}

#' k_Save: Saves keras model to file.
#' @param model: Trained keras model.
#' @param path: Destination file path.
k_Save = function(model, path) {
    tryCatch({
        dir.create(dirname(path))
        model %>% save_model_hdf5(path)
    }, error = function(e) { Log(c('Failed to save keras model: ', e)) })
}

#' k_Load: Loads keras model from file.
#' @param path: Destination file path.
#' @param custom: Custom objects.
#' @return Saved keras model.
k_Load = function(path, custom = NULL) {
    tryCatch({ load_model_hdf5(path, custom_objects=custom) }, error = function(e){ return(NULL) })
}