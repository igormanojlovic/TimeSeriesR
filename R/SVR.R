#' TrainSVR: Trains Support Vector Regression (SVR) model.
#' Creates recursive nu-based SVR model with Radial Basis Function (RBF) kernel.
#' Uses 10-Fold Cross Validation with Mean Square Error (MSE) to optimize the model.
#' IMPORTANT: The number of X and Y rows must the same (use NA to populate unknown values).
#' @param x: Table of X values (one column per variable and one row per example).
#' @param y: Table of Y values (one column per variable and one row per example).
#' @param horizon: The length of forecast horizon.
#' @param lookback: The length of Y past to take as input.
#' @param nu (0, 1]: Parameter nu controls the number of support vectors and training errors for nu-SVR as follows:
#' 1) small nu leads to great sensitivity to margins around hyperplanes and may cause overfitting,
#' 2) large nu leads to small sensitivity to margins around hyperplanes and may cause underfitting.
#' In general, nu-SVR and epsilon-SVR are equivalent but nu parameter is more intuitive that epsilon.
#' For example, if nu = 0.1, then at most 10% of the training examples will be considered as outliers
#  and at least 10% of training examples will act as support vectors (points on the decision boundary).
#' @param gamma [0, infinity]: Parameter for RBF kernel controls the shape of the separating hyperplanes as follows:
#' 1) Small gamma leads to a Gaussian function with a large variance (margins) and may cause underfitting,
#' 2) Large gamma leads to a Gaussian function with a small variance (margins) and may cause overfitting.
#' @param cost (0, infinity]: Parameter C for SVR penalizes errors and controls the complexity of SVR as follows:
#' 1) Small cost leads to large margins around hyperplanes and may cause underfitting,
#' 2) Large cost leads to small margins around hyperplanes and may cause overfitting.
#' @param tolerance (0, 1): Convergence tolerance controls the search for an optimal solution (hyperplanes) as follows:
#' 1) Small tolerance increases difficulty in finding an optimal solution and may cause overfitting,
#' 2) Large tolerance decreases difficulty in finding an optimal solution and may cause underfitting.
#' @return Trained SVR model.
TrainSVR = function(x, y, horizon, lookback = 2*horizon, nu = 0.05, gamma = 0.01, cost = 0.75, tolerance = 0.05) {
    LimitParams = function() {
        nu <<- Limit(nu, 0.001, 1)
        gamma <<- Limit(gamma, min = 0)
        cost <<- Limit(cost, min = 0.001)
        tolerance <<- Limit(tolerance, 0.001, 0.999)
    }
    Train = function() {
        train = Tensor(x, y, horizon, lookback, "rec")
        input = as.matrix(as.data.frame(train$input))
        output = as.matrix(as.data.frame(train$output))
        model = svm(x = input, y = output, type = "nu-regression", nu = nu, gamma = gamma, cost = cost, tolerance = tolerance, cross = 10)
        return(model)
    }

    LimitParams()
    return(Train())
}

#' TestSVR: Tests Support Vector Regression (SVR) model.
#' IMPORTANT: The number of X and Y rows must the same (use NA to populate unknown values).
#' @param model: Trained SVR model (result of TrainSVR function).
#' @param x: Table of X values (one column per variable and one row per example).
#' @param y: Table of Y values (one column per variable and one row per example).
#' @param horizon: The length of forecast horizon.
#' @param lookback: The length of Y past to take as input.
#' @return Y forecast.
TestSVR = function(model, x, y, horizon, lookback) {
    test = Tensor(x, y, horizon, lookback, "rec")
    input = as.matrix(as.data.frame(test$input))
    input.column = ncol(input) - ncol(as.data.frame(x))

    forecast = c()
    for (i in 1:horizon) {
        if (i > 1) input[i, input.column] = forecast[i - 1]
        output = predict(model, matrix(input[i,], 1, ncol(input)))
        forecast = c(forecast, output)
    }

    forecast = as.data.frame(forecast)
    colnames(forecast) = colnames(y)
    rownames(forecast) = c()
    return(forecast)
}

#' SaveSVR: Saves Support Vector Regression (SVR) model to '.svr' file.
#' @param model: Trained SVR model (result of TrainSVR function).
#' @param folder: Destination folder.
#' @param file: Destination file name.
SaveSVR = function(model, folder, file) {
    Save = function() {
        path = GetPath(folder, file, 'svr')
        list(model = model) %>% saveRDS(path)
    }

    tryCatch({ Save() }, error = function(e){ Log(c('Failed to save SVR model: ', e)) })
}

#' LoadSVR: Loads Support Vector Regression (SVR) model from '.svr' file.
#' @param folder: Source folder.
#' @param file: Source file name.
#' @return Trained SVR model or NULL if the model cannot be loaded.
LoadSVR = function(folder, file) {
    Load = function() {
        path = GetPath(folder, file, 'svr')
        dir.create(dirname(path))
        return(readRDS(path)$model)
    }

    tryCatch({ return(Load()) }, error = function(e){ return(NULL) })
}
