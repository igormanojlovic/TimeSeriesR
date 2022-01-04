#' TSF: Time Series Forecasting
#' Executes test for specified steps in the forecasting procedure
#' @param folder: Folder with test data (ANDLP.csv, TSGA.csv, HMLPSA.csv and Compact.csv).
#' @param steps: One or more steps in the forecasting procedure:
#' 1) TSGA: Time Series Grouping Algorithm
#' 2) JMIM: Joint Mutial Information Maximisation
#' 3) AC: Autocorrelation
#' 3) Forecast: probabilistic forecast
#' 4) Accuracy: forecast accuracy
#' 5) DM: Diebold-Mariano test
#' @param procs: One or more forecasting procedures:
#' 1) DCL: Deep Centroid Learning
#' 2) SCL: SVR Centroid Learning
#' 3) Deep Ensemble Learning
#' 4) DMN: Deep Mixture Network
#' 6) DeepAR: Deep AutoRegression
#' 7) Naive: Last week's load is the next week's forecast.
#' @param splitter: Row that splits training from test set (last training set row).
#' @param horizon: The length of forecast horizon.
#' @param lookback: The length of Y past to take as input.
#' @param normalization: One of available normalization types for X values:
#' 1) "NA": Not Assigned (none).
#' 2) "AM": Abs-Max normalization
#' 3) "MM": Min-Max normalization
#' 4) "AVG": Divide-by-average normalization
#' 5) "Z": Z-normalization
#' @param encoding: One of available time encoding types:
#' 1) "polar": Polar coordinate system.
#' 2) "onehot": One-Hot encoding.
#' @param retrain: The number of tests to perform before updating forecast model.
#' @param naivelag: Lag for naive forecast.
#' @param plot: Shows whether or not to plot DCL forecast.
#' @param ...: Step- or procedure-specific parameters.
TSF = function(folder, 
               steps = c("TSGA", "JMIM", "AC", "Forecast", "Accuracy", "DM"), 
               procs = c("DCL", "SCL", "DEL", "DMN", "DeepAR", "Naive"),
               splitter = 365*24,
               horizon = 24, 
               lookback = 7*horizon,
               normalization = 'MM',
               encoding = 'polar',
               retrain = 30,
               naivelag = horizon,
               plot = FALSE,
               ...) {
    lazy = list()
    GetCSV = function(folder, file) {
        id = GetPath(folder, file, "CSV")
        if (is.null(lazy[[id]])) lazy[[id]]<<-ImportCSV(folder, file, verbose = TRUE)
        return(lazy[[id]])
    }

    GetTestFolder = function(proc) return(paste(folder, 'Test', proc, sep = '\\'))
    GetHMLPSA = function() return(GetCSV(folder, "HMLPSA"))
    GetANDLP = function() return(GetCSV(folder, "ANDLP"))
    GetTSGA = function() return(GetCSV(folder, "TSGA"))
    GetCompact = function() return(GetCSV(folder, "Compact"))
    GetTime = function() return(GetCompact() %>% Subset(cols=1))
    GetCentroid = function(group) return(GetCompact() %>% Subset(cols=2*group+0:1))
    GetForecast = function(proc, output) return(GetCSV(GetTestFolder(proc), output))
    GetQS = function(proc) return(GetCSV(paste(GetTestFolder(proc), "Accuracy", sep="\\"), "QS-meter-time"))
    GetK = function() return(GetTSGA()[,2] %>% unique() %>% Count())
    GetMembers = function(group, tsga = GetTSGA()) return(Join(1:Count(tsga), tsga, colnames1 = "ID")[tsga[,2]==group, c(1,4)])
    GetAVG = function(psa, i) return(Subset(psa, cols = 2*i-1))
    GetSD = function(psa, i) return(Subset(psa, cols = 2*i))
    GetQR = function(forecast, n, i) {
        cols = seq(1, ncol(forecast), by=n)+i-1
        return(apply(forecast[,cols], 1, sort) %>% t())
    }
    GetPDH = function() {
        compact = GetCompact()
        timestamps = DateTime(compact[,1])
        holidays = compact[compact[,ncol(compact)]==1,1]
        return(ExtractCPCD(timestamps, holidays) %>% Join(DatePart(timestamps, "%H")))
    }
    
    GetGroupName = function(group) return(paste("Group", group, sep = ""))
    GetGroupForecast = function(proc, group) return(GetForecast(proc, GetGroupName(group)))
    
    TestTSGA = function() {
        hmlpsa = GetHMLPSA()
        #TODO: Implement HMLPSA2ANDLP in R

        andlp = GetANDLP() %>% Pivot("Meter", c("CharacteristicDay", "CharacteristicPeriod"))
        tsga = andlp %>% Group(2, floor(sqrt(Count(andlp))), ...)
        factors = hmlpsa[,-1] %>% GetAVG(1:Count(tsga$groups)) %>% colMeans(na.rm = TRUE)
        groups = Join(tsga$groups, factors)
        colnames(groups) = c("Meter", "Group", "Factor")

        test = GetTestFolder(paste("TSGA", tsga$k, sep = "-"))
        groups %>% ExportCSV(test, "TSGA")
    }
    
    TestAC = function() {
        data = GetCompact()
        a = Autocorrelation(data[,(1:(2*GetK()))+1], lookback, ...)
        test = GetTestFolder("AC")
        a$acf %>% ExportCSV(test, "ACF")
        a$pacf %>% ExportCSV(test, "PACF")
    }

    TestJMIM = function() {
        test = GetTestFolder("JMIM")

        k = GetK()
        compact = GetCompact()[1:splitter,]
        optional = (2+2*k):(ncol(compact)-1)   
        
        score = data.frame()
        selected = c()
        for (i in 2:(2*k+1)) {
            name = colnames(compact)[i]
            
            data = na.omit(compact[,c(i, optional)])
            if (Count(data) == 0) next
            x = data[,1]
            y = data[,-1]
            
            score = Join(score, JMIMScore(x, y), colnames2 = name)
            s = SelectFeatures(x, y, encoding) %>% colnames() %>% paste(collapse = ", ")
            selected = c(selected, paste(name, s, sep = " = "))
        }
        
        score %>% ExportCSV(test, "Score", rows = TRUE)
        selected %>% ExportCSV(test, "Selected")
    }
    
    TestForecast = function() {
        Gauss2Plot = function(y_true,
                              y_pred,
                              sd = c(),
                              n = 1,
                              q = 100,
                              p = c(0.95),
                              x_name = 'time',
                              y_name = 'value',
                              x_min = 1,
                              x_max = Count(unlist(y_true)),
                              y_min = min(y_pred-sd, na.rm=TRUE),
                              y_max = max(y_pred+sd, na.rm=TRUE),
                              width = 1) {
            Plot = function(lines, areas, title, colors) {
                X = function() return(x_min:x_max)
                Y = function() return(c(y_min, y_max))
                MakePlot = function() {
                    plot(x = X(), ylim = Y(), type = "l", main = title, xlab = x_name, ylab = y_name)
                }
                AddLegend = function() {
                    legend("top", horiz = TRUE, legend = names(colors), col = colors, lty = 1, lwd = 3*width, cex = 0.9)
                }
                AddLines = function() {
                    if (Count(lines) == 0) return(c())
                    for(i in 1:ncol(lines)) {
                        color = colors[names(lines)[i]]
                        lines(X(), lines[,i], type = "l", col = color, lwd = width)
                    }
                }
                AddAreas = function() {
                    if (Count(areas) == 0) return(c())
                    x_values = c(X(), rev(X()))
                    for(i in 1:ncol(areas)) {
                        color = colors[names(areas)[i]]
                        polygon(x_values, areas[,i], col = color, border = NA)
                    }
                }
                
                MakePlot()
                AddAreas()
                AddLines()
                AddLegend()
            }
            
            y_true = as.vector(unlist(y_true))
            y_pred = as.vector(unlist(y_pred))
            sd = as.vector(unlist(sd))
            
            errors = c("MAE"  = MAE(y_true, y_pred),
                       "MAPE" = MAPE(y_true, y_pred),
                       "RMSE" = RMSE(y_true, y_pred),
                       "CV"   = CVRMSE(y_true, y_pred))
            if (Count(sd) > 0) {
                e = c("CRPS" = CRPS(y_true, y_pred, sd),
                      "QS"   = QS(y_true, y_pred, sd, n, q))
                errors = c(errors, e)
                for (prob in p) {
                    e = c("ACE" = ACE(y_true, y_pred, sd, n, prob),
                          "WS"  =  WS(y_true, y_pred, sd, n, prob))
                    errors = c(errors, e)
                    perc = paste(c("(", prob * 100, "%", ")"), collapse = "")
                    last = Count(errors)-1:Count(e)+1
                    names(errors)[last] = paste(names(errors)[last], perc, sep = "")
                }
            }
            
            lines = data.frame()
            areas = data.frame()
            
            colors = c("actual" = "black", "expected" = "darkgreen")
            lines = Join(lines, y_pred, colnames2 = names(colors)[2])
            lines = Join(lines, y_true, colnames2 = names(colors)[1])
            if (Count(sd) > 0) {
                colors = c(colors, "green")
                names(colors)[Count(colors)] = "SD"
                areas = Join(areas, c(y_pred-sd, rev(y_pred+sd)), colnames2 = "SD")
            }
            
            Plot(lines, areas, Num2Str(errors), colors)
        }
        
        RunForecast = function(proc, group, test_folder = GetTestFolder(proc), name = GetGroupName(group)) {
            GetX = function() {
                SelectX = function(x, y, focus = 1:splitter) {
                    selection = c()
                    for(i in 1:ncol(y)) selection = c(selection, colnames(SelectFeatures(x, y[,i], focus)))
                    return(Subset(x, cols = unique(selection)))
                }
                
                compact = GetCompact()
                t = GetTime() %>% unlist() %>% DateTime() %>% EncodeTime(encoding)
                r = Subset(compact, cols = ncol(compact))
                o = Subset(compact, cols = (2+2*GetK()):(ncol(compact)-1)) %>% SelectX(GetCentroid(group)) 
                n = NormParam(o, normalization, 'col')
                x = Join(t, r) %>% Join(Norm(o, n))
                return(x)
            }
            Train = function(t, x, y, model=NULL) {
                LoadModel = function() {
                    if (proc == 'SCL') return(LoadSVR(test_folder, name))
                    return(LoadDNN(test_folder, name, proc))
                }
                CreateDNN = function(x.count = ncol(x), y.count = ncol(y)) {
                    if (proc == "DeepAR") return(DeepAR(horizon, lookback, ...))
                    if (proc == "DMN") return(DMN(x.count, y.count, horizon, lookback, ...))
                    if (proc == "DEL") return(DEL(x.count, y.count, horizon, lookback, ...))
                    return(DCL(x.count, y.count, horizon, lookback, ...))
                }
                TrainModel = function(model=NULL) {
                    clean = Join(t,x) %>% Join(y) %>% na.omit()
                    t = clean %>% Subset(cols=1:ncol(t))
                    x = clean %>% Subset(cols=1:ncol(x)+ncol(t))
                    y = clean %>% Subset(cols=1:ncol(y)+ncol(x)+ncol(t))
                    if (proc == 'SCL') {
                        svr = TrainSVR(x, y, horizon, lookback, ...)
                        SaveStructure(svr)
                        return(svr)
                    } else {
                        dnn = model
                        if (is.null(dnn)) {
                            dnn = CreateDNN()
                            SaveStructure(dnn)
                        }
                        
                        if (proc == "DeepAR") return(dnn %>% TrainDNN(t, y, horizon, lookback, proc, ...))
                        return(dnn %>% TrainDNN(x, y, horizon, lookback, proc, ...))
                    }
                }
                SaveStructure = function(model) {
                    tryCatch({
                        struct = model %>% summary() %>% capture.output() %>% as.data.frame()
                        colnames(struct) = name
                        ExportCSV(struct, paste(test_folder, "Structure", sep = "\\"), name)
                    }, error = function(e) {
                        Log(c('Failed to export ', proc, " model structure. Error: ", e))
                    })
                }
                SaveModel = function(model) {
                    if (proc == 'SCL') {
                        #model %>% SaveSVR(test_folder, name)
                    } else {
                        model %>% SaveDNN(test_folder, name, proc)
                    }
                }
                
                if (is.null(model)) {
                    model = LoadModel()
                    if (!is.null(model)) {
                        Log(c(proc, " model loaded."))
                        return(model)
                    }
                }
                
                save = is.null(model)
                model = TrainModel(model)
                if (save) model %>% SaveModel()
                return(model)
            }
            Test = function(model, t, x, y) {
                if (proc == 'SCL') return(model %>% TestSVR(x, y, horizon, lookback, ...))
                if (proc == 'DeepAR') return(model %>% TestDNN(t, y, horizon, lookback, proc, ...))
                return(model %>% TestDNN(x, y, horizon, lookback, proc, ...))
            }
            
            # y row numbers are passed to train and test instead of y
            RunOne = function(train, test) {
                preload = GetHMLPSA()
                preload = GetTSGA()
                preload = GetCompact()
                et = list(training=0,testing=0,updating=0,tcount=0,ucount=0)
                watch = Log(c(proc, " training started for group ", group, "."))

                t = GetTime()
                x = GetX()

                rows = 1:splitter
                trainT = Subset(t, rows=rows)
                trainX = Subset(x, rows=rows)
                model = train(trainT, trainX, rows)
                et$training = Elapsed(watch)
                
                Log(c(proc, " training finished (duration = ", et$training, " sec)."))
                Log(c(proc, " testing (-) and updating (|) started for group ", group, "."))
                
                train_last = splitter
                countdown = retrain
                forecast = data.frame()
                for (test_start in seq(splitter+1, Count(x), by=horizon)) {
                    test_end = test_start+horizon-1
                    if (test_end > Count(x)) break

                    cat("-", ifelse(test_end < Count(x), "", "\n"))
                    watch = Stopwatch()

                    rows = (test_start-lookback):test_end
                    testT = Subset(t, rows=rows)
                    testX = Subset(x, rows=rows)
                    predY = model %>% test(testT, testX, rows)
                    forecast = forecast %>% Union(predY)
                    
                    et$testing = et$testing + Elapsed(watch)
                    et$tcount = et$tcount + 1
                    
                    countdown = countdown - 1;
                    if (countdown <= 0) {
                        cat("|", sep="\n")
                        watch = Stopwatch()
                        
                        rows = ifelse(proc=="SCL", 1, (train_last-lookback+1)):test_end
                        updateT = Subset(t, rows=rows)
                        updateX = Subset(x, rows=rows)
                        model = train(updateT, updateX, rows, model)
                        
                        train_last = test_end
                        countdown = retrain
                        
                        et$updating = et$updating + Elapsed(watch)
                        et$ucount = et$ucount + 1
                    }
                }
                
                et$testing = round(et$testing/et$tcount)
                et$updating = round(et$updating/et$ucount)
                Log(c(proc, " testing and updating finished (duration = ", et$testing, " and ", et$updating, " sec on average)."))
                
                forecast %>% ExportCSV(test_folder, name)
                et %>% t() %>% ExportCSV(test_folder, paste(name, "ET", sep=""))
            }
            
            RunDCL = function() {
                TrainDCL = function(trainT, trainX, rowsY, model=NULL) {
                    trainY = GetCentroid(group) %>% Subset(rows=rowsY)
                    return(Train(trainT, trainX, trainY, model))
                }
                TestDCL = function(model, testT, testX, rowsY) {
                    testY = GetCentroid(group) %>% Subset(rows=rowsY)
                    return(model %>% Test(testT, testX, testY))
                }
                
                RunOne(TrainDCL, TestDCL)
            }
            
            RunSCL = function() {
                TrainSCL = function(trainT, trainX, rowsY, model=NULL) {
                    if (is.null(model)) model = list(m=NULL, s=NULL)
                    trainY = GetCentroid(group) %>% Subset(rows=rowsY)
                    model$m = Train(trainT, trainX, trainY %>% Subset(cols=1), model$m)
                    model$s = Train(trainT, trainX, trainY %>% Subset(cols=2), model$s)
                    return(model)
                }
                TestSCL = function(model, testT, testX, rowsY) {
                    testY = GetCentroid(group) %>% Subset(rows=rowsY)
                    m = model$m %>% Test(testT, testX, testY %>% Subset(cols=1))
                    s = model$s %>% Test(testT, testX, testY %>% Subset(cols=2))
                    return(Join(m, s))
                }
                
                RunOne(TrainSCL, TestSCL)
            }

            RunAVG = function() {
                TrainAVG = function(trainT, trainX, rowsY, model=NULL) {
                    trainY = GetCentroid(group) %>% Subset(rows=rowsY, cols=1)
                    return(Train(trainT, trainX, trainY, model))
                }
                TestAVG = function(model, testT, testX, rowsY) {
                    testY = GetCentroid(group) %>% Subset(rows=rowsY, cols=1)
                    return(model %>% Test(testT, testX, testY))
                }
                
                RunOne(TrainAVG, TestAVG)
            }
            
            RunALL = function() {
                TrainALL = function(trainT, trainX, rowsY, model=NULL) {
                    trainY = GetHMLPSA()[,-1] %>% GetAVG(GetMembers(group)[,1]) %>% Subset(rows=rowsY)
                    return(Train(trainT, trainX, trainY, model))
                }
                TestALL = function(model, testT, testX, rowsY) {
                    testY = GetHMLPSA()[,-1] %>% GetAVG(GetMembers(group)[,1]) %>% Subset(rows=rowsY)
                    return(model %>% Test(testT, testX, testY))
                }
                
                RunOne(TrainALL, TestALL)
            }

            if (proc == "DCL") {
                RunDCL()
            } else if (proc == "SCL") {
                RunSCL()
            } else if (proc == "DMN" || proc == "DeepAR") {
                RunAVG()
            } else if (proc == "DEL") {
                RunALL()
            } else {
                next
            } 
        }

        ExtractSamples = function(proc="DCL", test_folder = GetTestFolder(proc)) {
            hmlpsa = GetHMLPSA()
            rows = (splitter+1):Count(hmlpsa)
            actual = hmlpsa[rows,-1]
            pdh = GetPDH()[rows,]
            
            for (group in 1:GetK()) {
                f = paste(test_folder, "Sample", GetGroupName(group), sep = "\\")
                
                members = GetMembers(group)
                y_true = GetAVG(actual, members[,1])
                for (i in 1:Count(members)) y_true[,i] = y_true[,i] / members[i,2]
                y_pred = GetGroupForecast(proc, group)
                
                
                avg_true = data.frame()
                avg_pred = data.frame()
                for (p in sort(unique(pdh[,1]))) {
                    period = pdh[,1]==p
                    sample = 1:(10*horizon)
                    y_true[period,][sample,] %>% ExportCSV(f, c("Meters_P", p))
                    y_pred[period,][sample,] %>% ExportCSV(f, c("Centroid_P", p))
                }
            }
        }

        for (proc in procs) {
            for (group in 1:GetK()) {
                RunForecast(proc, group)
            }
            
            if (proc == "DCL") ExtractSamples()
        }
    }
    
    TestAccuracy = function(quantiles = k_Percentiles()/100) {
        
        SummarizeScores = function(y_true, y_pred, qs, ws) {
            qs.meter.time = qs %>% rowMeans(na.rm=TRUE) %>% as.data.frame()
            qs.meter.perc = qs %>% colMeans(na.rm=TRUE) %>% as.data.frame()
            ws.width = ws$width%>%colMeans(na.rm=TRUE)
            ws.penal = ws$penal%>%colMeans(na.rm=TRUE)
            return(list(y_pred=y_pred, qs.meter.time=qs.meter.time,qs.meter.perc=qs.meter.perc,ws.width=ws.width,ws.penal=ws.penal))
        }
        
        Gauss2Scores = function(y_true, y_pred, sd, n=1) {
            qs = Pinball(y_true, y_pred, sd, n=n)
            
            width = data.frame()
            penal = data.frame()
            for (i in ceiling(Count(quantiles)/2):1) {
                q = quantiles[i]
                pi = PI(y_pred, sd, n, q)
                w = pi$upper - pi$lower
                colname = paste("PI", i, sep="")
                penal = Join(penal, Winkler(y_true, y_pred, sd, n, q) - w, colnames2 = colname)
                width = Join(width, w, colnames2 = colname)
            }

            ws = list(width=width, penal=penal)
            return(SummarizeScores(y_true, y_pred, qs, ws))
        }
        
        QR2Scores= function(y_true, y_qr) {
            rows = 1:min(Count(y_true), Count(y_qr))
            y_true = y_true[rows,]
            y_qr = y_qr[rows,]
            y_pred = y_qr %>% rowMeans(na.rm = TRUE)

            qs = QR2QS(y_true, y_qr)
            ws = QR2WS(y_true, y_qr)
            return(SummarizeScores(y_true, y_pred, qs, ws))
        }
        
        QS2CPCD = function(qs.meter.time) {
            rows = splitter+1:Count(qs.meter.time)
            pdh = GetPDH()[rows,]
            
            qs.cpcd = data.frame()
            for (group in 1:GetK()) {
                members = GetMembers(group)[,1]
                
                qs.meter.cpcd = data.frame()
                for (p in sort(unique(pdh[,1]))) {
                    for (d in sort(unique(pdh[,2]))) {
                        cpcd.rows = pdh[,1]==p & pdh[,2]==d
                        qs.meter.cpcd = qs.meter.cpcd %>% Union(qs.meter.time[cpcd.rows,members] %>% colMeans(na.rm=TRUE) %>% t())
                    }
                }

                qs.cpcd = qs.cpcd %>% Join(qs.meter.cpcd %>% rowMeans(na.rm=TRUE), colnames2 = group)
            }

            return(qs.cpcd)
        }

        SummarizeAccuracy = function(qs.meter.time, qs.meter.perc, ws.width, ws.penal, root_true, root_pred) {
            qs.meter = qs.meter.time %>% colMeans(na.rm=TRUE) %>% as.data.frame()
            qs.time = qs.meter.time %>% rowMeans(na.rm=TRUE) %>% as.data.frame()
            qs.perc = qs.meter.perc %>% rowMeans(na.rm=TRUE) %>% as.data.frame()
            qs.cpcd = QS2CPCD(qs.meter.time)
            qs = c(avg = unlist(qs.meter) %>% mean(na.rm=TRUE), sd = unlist(qs.meter) %>% sd(na.rm=TRUE)) %>% t() %>% as.data.frame()
            ws.width = ws.width %>% rowMeans(na.rm=TRUE) %>% as.data.frame()
            ws.penal = ws.penal %>% rowMeans(na.rm=TRUE) %>% as.data.frame()
            ws = ws.width + ws.penal
            
            colnames(qs.meter) = "QS"
            colnames(qs.time) = "QS"
            colnames(qs.perc) = "QS"
            colnames(ws.width) = "WS width"
            colnames(ws.penal) = "WS penal"
            colnames(ws) = "WS"
            
            root_true = root_true %>% rowMeans(na.rm=TRUE) %>% unlist()
            root_pred = root_pred %>% rowMeans(na.rm=TRUE) %>% unlist()
            root_count = min(Count(root_true), Count(root_pred))
            root.all = Join(root_true[1:root_count], root_pred[1:root_count]) %>% na.omit()
            root = c("MAE"  = MAE(root.all[,1], root.all[,2]),
                     "MAPE" = MAPE(root.all[,1], root.all[,2]),
                     "RMSE" = RMSE(root.all[,1], root.all[,2]),
                     "CV"   = CVRMSE(root.all[,1], root.all[,2])) %>% t()
            
            return(list(qs=qs,
                        qs.meter.time=qs.meter.time,
                        qs.meter=qs.meter,
                        qs.time=qs.time,
                        qs.cpcd=qs.cpcd,
                        qs.perc=qs.perc,
                        ws=ws,
                        ws.width=ws.width,
                        ws.penal=ws.penal,
                        root=root))
        }
        
        TestGeneric = function(getScores) {
            hmlpsa = GetHMLPSA()
            actual = hmlpsa[(splitter+1):Count(hmlpsa),-1]

            qs.meter.time = data.frame()
            qs.meter.perc = data.frame()
            ws.width = data.frame()
            ws.penal = data.frame()
            root_true = data.frame()
            root_pred = data.frame()
            for (group in 1:GetK()) {
                members = GetMembers(group)
                n = Count(members)
                for(i in 1:n) {
                    member = members[i,1]
                    factor = members[i, 2]
                    y_true = GetAVG(actual, member)
                    res = getScores(y_true, group, n, i, member, factor)
                    qs.meter.time = Join(qs.meter.time, res$qs.meter.time, colnames2 = member)
                    qs.meter.perc = Join(qs.meter.perc, res$qs.meter.perc, colnames2 = member)
                    ws.width = Join(ws.width, res$ws.width, colnames2 = member)
                    ws.penal = Join(ws.penal, res$ws.penal, colnames2 = member)
                    root_true = Join(root_true, y_true, colnames2 = member)
                    root_pred = Join(root_pred, res$y_pred, colnames2 = member)
                }
            }
            
            return(SummarizeAccuracy(qs.meter.time, qs.meter.perc, ws.width, ws.penal, root_true, root_pred))
        }
        
        TestNaive = function() return(TestGeneric(function(y_true, group, n, i, member, factor) {
            centroid = GetCentroid(group)
            forecast = centroid[(splitter+1):Count(centroid)-naivelag,]
            return(Gauss2Scores(y_true, forecast[,1]*factor, forecast[,2]*factor, n))
        }))
        
        TestGaussian = function(proc) return(TestGeneric(function(y_true, group, n, i, member, factor) {
            forecast = GetGroupForecast(proc, group)
            return(Gauss2Scores(y_true, forecast[,1]*factor, forecast[,2]*factor, n))
        }))
        
        TestMixture = function(proc) {
            group2forecast = list()
            for (group in 1:GetK()) {
                group2forecast[[group]] = GetGroupForecast(proc, group) %>% Mix2Q()
            }
            
            return(TestGeneric(function(y_true, group, n, i, member, factor) {
                return(QR2Scores(y_true, group2forecast[[group]]*factor))
            }))
        }
        
        TestQR = function(proc) return(TestGeneric(function(y_true, group, n, i, member, factor) {
            y_qr = GetGroupForecast(proc, group) %>% GetQR(n, i)
            return(QR2Scores(y_true, y_qr))
        }))
        
        TestOne = function(proc) {
            if (proc == "DCL") return(TestGaussian(proc))
            else if (proc == "SCL") return(TestGaussian(proc))
            else if (proc == "DMN") return(TestMixture(proc))
            else if (proc == "DEL") return(TestQR(proc))
            else if (proc == "DeepAR") return(TestGaussian(proc))
            else if (proc == "Naive") return(TestNaive())
            else next
        }
        
        ExportOne = function(res, proc) {
            Log(c(proc, " QS = ", round(res$qs[,"avg"], 3), " (SD = ", round(res$qs[,"sd"], 3), ")"))
            
            test = paste(GetTestFolder(proc), "Accuracy", sep = "\\")
            
            res$qs %>% ExportCSV(test, "QS")
            res$qs.meter.time %>% ExportCSV(test, "QS-meter-time")
            res$qs.meter %>% ExportCSV(test, "QS-meter")
            res$qs.time %>% ExportCSV(test, "QS-time")
            res$qs.cpcd %>% ExportCSV(test, "QS-CPCD")
            res$qs.perc %>% ExportCSV(test, "QS-perc")
            
            res$ws %>% ExportCSV(test, "WS")
            res$ws.width %>% ExportCSV(test, "WS-width")
            res$ws.penal %>% ExportCSV(test, "WS-penal")
            
            res$root %>% ExportCSV(test, "Root", rows=TRUE)
        }
        
        for (proc in procs) TestOne(proc) %>% ExportOne(proc)
    }
    
    TestDM = function() {

        GetCaseName = function(i, j) return(paste(procs[i], procs[j], sep = " vs "))
        
        GetOneDM = function(accuracy, i, j) {
            a = accuracy[[i]]
            b = accuracy[[j]]
            n = min(Count(a), Count(b))
            a = a[1:n,]
            b = b[1:n,]

            res = data.frame()
            cols = intersect(colnames(a),colnames(b))
            for (col in cols) {
                ab = Join(a[,col], b[,col]) %>% na.omit()
                res = Join(res, DM(ab[,1], ab[,2], horizon = horizon, p = 0.99), colnames2 = col)
            }
            
            return(Join(100*rowSums(res > 0)/ncol(a), 100*rowSums(res < 0)/ncol(a), colnames1 = GetCaseName(i, j), colnames2 = GetCaseName(j, i)))
        }
        
        GetDM = function() {
            accuracy = list()
            for (i in 1:(Count(procs))) {
                accuracy[[i]] = GetQS(procs[i])
            }
            
            result = data.frame()
            for(i in 1:(Count(procs)-1)) {
                for (j in (i+1):Count(procs)) {
                    Log(c("Testing ", procs[i], " vs ", procs[j]))
                    tryCatch({
                        result = Join(result, GetOneDM(accuracy, i, j))
                    }, error = function(e) {
                        Log(e)
                    })
                }
            }
            
            return(result)
        }
        
        GetDM() %>% ExportCSV(GetTestFolder("DM"), "DM")
    }
    
    for (step in steps) {
        if (step == "TSGA") TestTSGA()
        else if (step == "JMIM") TestJMIM()
        else if (step == "AC") TestAC()
        else if (step == "Forecast") TestForecast()
        else if (step == "Accuracy") TestAccuracy()
        else if (step == "DM") TestDM()
        else Log(c(step, " step not supported."))
    }
}

