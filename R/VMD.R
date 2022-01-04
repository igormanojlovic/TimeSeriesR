# Source: https://cran.r-project.org/web/packages/vmd/index.html
# This script is required becuase the library cannot be installed from CRAN.

options("vmd.tol" = 1e-6,
        "vmd.init" = 0,
        "vmd.DC" = TRUE,
        "vmd.K" = 3,
        "vmd.tau" = 0,
        "vmd.alpha" = 2000,
        "vmd.theme.default" = list(theme_bw(),
                                   theme(panel.grid = element_blank(),
                                         legend.background = element_rect(fill = alpha('white', 0.4)))),
        "vmd.N" = 500,
        "vmd.NMin" = 1,
        "vmd.NMax" = 10000,
        "vmd.orderModes" = TRUE)

#' R6 Class of Standard Checking Utilities
#'
#' Create a new R6 Class that contains public methods of standard checking utilities
#' @rdname checker
#' @export
checker = function(){
  R6Checker$new()
}

#' @rdname checker
#' @usage NULL
#' @export
R6Checker = R6::R6Class('Checker',inherit=NULL,
    public=list(
      initialize = function(){
      },
      #Check Has Names
      checkHasNames = function(x,objnm = deparse(substitute(x))){
        `if`(!is.character(names(x)), stop(sprintf("'%s' of type ['%s'] must be a named object",objnm,paste(class(x),collapse="', '")),call.=FALSE),invisible(x))
      },

      #Check All Names from A are in B
      checkAllNamesAreIn = function(A,B,objnmA = deparse(substitute(A)), objnmB = deparse(substitute(B))){
        self$checkHasNames(A,objnmA)
        self$checkHasNames(B,objnmB)
        missing = setdiff(names(A),names(B))
        `if`(length(missing) > 0,{
          stop(sprintf("All names in '%s' must also be in '%s'. Missing: ['%s']",
                       objnmA,
                       objnmB,
                       paste(missing,collapse=", ")),
               call.=FALSE)
        },invisible(A))
      },

      checkAllNamesAreUnique = function(x,objnm = deparse(substitute(A))){
        self$checkHasNames(x,objnm)
        `if`(!identical(length(names(x)),length(unique(names(x)))), stop(sprintf("All names in '%s' must be unique.",objnm),call.=FALSE), invisible(x))
      },

      #Throws error if not numeric scalar (length 1) that is positive
      checkPositiveNumericScalar = function(x, includeZero = FALSE, objnm = deparse(substitute(x))){
        self$checkNumericScalar(x,objnm = objnm)
        self$checkRange(x, 0, aop = `if`(includeZero,'>=','>'), objnm = objnm)
      },


      #Throws error if not integer scalar (length 1) that is positive
      checkPositiveIntegerScalar = function(x,includeZero = FALSE, objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm = objnm)
        self$checkInteger(x,objnm=objnm)
        self$checkRange(x, 0, aop = `if`(includeZero,'>=','>'), objnm = objnm)
      },


      #Throws error i not numeric scalar (length 1)
      checkNumericScalar = function(x, objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm = objnm)
        self$checkNumeric(x,objnm = objnm)
      },


      #Throws error if not logical scalar (length 1)
      checkLogicalScalar = function(x,objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm=objnm)
        self$checkLogical(x, objnm = objnm)
      },


      #Throws error if not character scalar (length 1)
      checkCharacterScalar = function(x,objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm = objnm)
        self$checkCharacter(x, objnm = objnm)
      },


      #Throws error if not scalar (length 1) or a specific type
      checkScalarOfType = function(x, type = enfArg('type'), objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm=objnm)
        self$checkClass(x,type,objnm=objnm)
      },

      #Throws error if not in range [0,1]
      checkFractional = function(x, objnm = deparse(substitute(x))){
        self$checkNumeric(x, objnm = objnm)
        self$checkRange(x,0,1,aop='>=',bop="<=", objnm = objnm)
      },

      #Throws error if not in range [0,1] and scalar (length 1)
      checkFractionalScalar = function(x, objnm){
        self$checkScalar(x, objnm = objnm)
        self$checkFractional(x, objnm = objnm)
      },

      #Throws error if x is not numeric
      checkNumeric = function(x,objnm = deparse(substitute(x))){
        `if`(!all(is.numeric(x)), stop(sprintf("'%s' must be numeric",objnm),call.=FALSE), invisible(x))
      },


      #Throws error if x is not integer
      checkInteger = function(x, objnm = deparse(substitute(x))){
        `if`(!all(is.integer(x)) & !(all(is.numeric(x)) & all(x %% 1 == 0)),
             stop(sprintf("'%s' must be integer",objnm),call.=FALSE), invisible(x))
      },

      #Throws Error if Not Integer and Not scalar
      checkIntegerScalar = function(x, objnm = deparse(substitute(x))){
        self$checkInteger(x,objnm = objnm)
        self$checkScalar(x,objnm = objnm)
      },

      #Throws error if x is not character
      checkCharacter = function(x, objnm = deparse(substitute(x))){
        `if`(!is.character(x), stop(sprintf("'%s' must be character",objnm),call.=FALSE), invisible(x))
      },


      #Throws error if x is not logical
      checkLogical = function(x, objnm = deparse(substitute(x))){
        `if`(!is.logical(x), stop(sprintf("'%s' must be logical",objnm),call.=FALSE), invisible(x))
      },


      #Throws error if x does not inherit type
      checkClass = function(x,type = enfArg('type'), objnm = deparse(substitute(x))){
        `if`(!inherits(x,type), stop(sprintf("'%s' must inherit one of ['%s']",objnm,paste(type,collapse="', '")),call.=FALSE), invisible(x))
      },

      #Throws error if x is not data.frame
      checkDataFrame = function(x,objnm = deparse(substitute(x))){
        self$checkClass(x,'data.frame',objnm = objnm)
      },

      #Check list, with option to conduct check recursively, in the event that the list may be a list of lists
      checkListOfClass = function(x, type = enfArg('type'), recursive = FALSE, objnm = deparse(substitute(x))){
        self$checkCharacterScalar(type, objnm = 'type')
        self$checkClass(x,'list', objnm)
        check = sapply( `if`(recursive, flatten(x), x) , function(o){ inherits(o,type) })
        `if`(!all(check), stop(sprintf("All '%s' objects must inherit from '%s'",objnm,type),call.=FALSE), invisible(x))
      },

      #Check Data Frame is Numeric
      checkNumericDataFrame = function(x,objnm = deparse(substitute(x))){
        self$checkDataFrame(x,objnm = objnm)
        `if`(!all(sapply(as.list(x),function(o)is.numeric(o))), stop(sprintf("All columns of '%s' must be numeric.",objnm),call.=FALSE), invisible(x))
      },

      #Check Length of x
      checkLength = function(x,n = 1,op='==',objnm = deparse(substitute(x))){
        self$checkIsIn(op,c('==','>','<','>=','<='))
        n = as.integer(unique(n))
        `if`(!{any(do.call(op,args=list(length(x),n)))},
             stop(sprintf("'%s' must be of length/s '%s' ['%s']",objnm,op,paste(n,collapse="', '")),call.=FALSE),
             invisible(x))
      },


      #Check if x is a scalar (length 1)
      checkScalar = function(x, objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm = objnm)
      },


      #Check if x is not null or is not NA
      checkNotNullOrNA = function(x,objnm = deparse(substitute(x))){
        `if`(is.null(x) || is.na(x),stop(sprintf("'%s' must not be NULL or NA",objnm),call.=FALSE),invisible(x))
      },

      #Check if x is non zero length
      checkNonZeroLength = function(x,objnm = deparse(substitute(x))){
        `if`(!length(x), stop(sprintf("'%s' must not be zero length",objnm),call.=FALSE), invisible(x))
      },

      #Check if x is in valid
      checkIsIn = function(x,valid = enfArg('valid'), objnm = deparse(substitute(x)),objnmB = deparse(substitute(valid))){
        self$checkNonZeroLength(valid,objnm = objnmB)
        self$checkNotNullOrNA(valid,objnm = objnm)
        `if`(!all(x %in% valid),
             stop(sprintf("All '%s' (%s) must be in: ['%s'], missing: '%s'",
                          objnm,
                          paste(x,collapse="', '"),
                          paste(valid,collapse="', '"),
                          paste(setdiff(x,valid),collapse="', '")
             ),call.=FALSE),
             invisible(x))
      },

      #Check if any x
      checkAny = function(x,objnm = deparse(substitute(x))){
        self$checkLogical(x,objnm=objnm)
        `if`(!any(x), stop(sprintf("At least one of '%s' must be TRUE.",objnm),call.=FALSE), invisible(x))
      },

      #Check if x is in the range governed by a and b, and aop (operator) and bop (operator)
      checkRange = function(x, a = -Inf, b = Inf, aop = '>=', bop = '<=', objnm = deparse(substitute(x))){

        #Check x, a and b are numeric
        self$checkNumeric(x,objnm = objnm)
        self$checkNumeric(a,objnm = sprintf("%s (lower bound)",objnm))
        self$checkNumeric(b,objnm = sprintf("%s (upper bound)",objnm))

        #Valid Operators
        vop   = c('>','>=','<','<=','==')

        #aop must be character scalar and a valid operator
        self$checkScalarOfType(aop,'character', objnm = objnm)
        self$checkIsIn(aop,vop, objnm = objmm)

        #bop must be character scalar and a valid operator
        self$checkScalarOfType(bop,'character', objnm = objnm)
        self$checkIsIn(bop,vop, objnm = objmm)

        a = `if`(is.na(a),-Inf,a)
        b = `if`(is.na(b),+Inf,b)
        `if`(!all(do.call(aop,args=list(x,a)) & do.call(bop,args=list(x,b))),
             stop(sprintf("'%s' must be in the range ['%s' %s %s] AND ['%s' %s %s]",
                          objnm,
                          objnm,aop,a,
                          objnm,bop,b),call.=FALSE),
             invisible(x))
      },


      #Check if x is positive
      checkPositive = function(x, objnm = deparse(substitute(x))){
        self$checkRange(x, a = 0, aop='>', objnm = objnm)
      },

      #Check if x is negative
      checkNegative = function(x, objnm = deparse(substitute(x))){
        self$checkRange(x, a = 0, aop="<", objnm = objnm)
      }
    ),
    private=list(

    )
)

#' Create VMD Object
#'
#' Create instance of \code{R6Vmd}, which is an R6 implementation, ported from the original 2013 Matlab
#' code developed by Dragomiretskiy & Zosso.
#'
#' @param signal the time domain signal (1D) to be decomposed
#' @param alpha the balancing parameter of the data-fidelity constraint
#' @param tau time-step of the dual ascent (pick 0 for noise-slack)
#' @param K the number of modes to be recovered
#' @param DC true if the first mode is put and kept at DC (0-freq)
#' @param init 0 = all omegas start at 0, 1 = all omegas start uniformly distributed or 2 = all omegas initialized randomly
#' @param tol tolerance of convergence criterion, typically around 1e-6
#' @param ... any other arguments to be passed to the R6 initializer
#' @author Nicholas Hamilton, UNSW Sydney
#' @examples
#' x = seq(-2*pi,2*pi,length.out=1000)
#' signal = cos(x)
#' v = vmd(signal,DC=FALSE,tol=1e-3)
#' v$getResult()
#' plot(v)
#'
#' nv   = 1000
#' fs   = 1/nv
#' t    = (1:nv)/nv
#' freq = 2*pi*(1 - 0.5 - 1/nv)/fs
#' f_1 = 2;
#' f_2 = 24;
#' f_3 = 288;
#' f_4 = 12;
#' v_1 = (cos(2*pi*f_1*t));
#' v_2 = 1/4*(cos(2*pi*f_2*t));
#' v_3 = 1/16*(cos(2*pi*f_3*t));
#' v_4 = 1/8*(cos(2*pi*f_4*t));
#' signal = v_1 + v_2 + v_3 + v_4 + 0.5*runif(nv,min=-0.5,max=0.5);
#' v = vmd(signal,alpha=2000,tau=0,DC=FALSE,init=0,tol=1e-3,K=3,orderModes=TRUE)
#'
#' #List of Results
#' l = v$getResult()
#' names(l)
#'
#' #To Data Frame
#' df = as.data.frame(v)
#' head(df)
#'
#' #Plot Results
#' plot(v)
#' plot(v,facet='bymode',scales='free')
#' plot(v,facet='byclass',scales='free')
#'
#' #Input Spectrum
#' v$plot.input.spectrum()
#'
#' #Spectral Decomposition
#' v$plot.spectral.decomposition()
#'
#' @references
#' Variational Mode Decomposition, Dragomiretskiy & Zorro, 2013, http://dx.doi.org/10.1109/TSP.2013.2288675
#' @references
#' Original Matlab Source: https://goo.gl/fJH1d5.
#' @export
vmd = function(signal,
               alpha = getOption('vmd.alpha'),
               tau = getOption('vmd.tau'),
               K = getOption('vmd.K'),
               DC = getOption('vmd.DC'),
               init = getOption('vmd.init'),
               tol = getOption('vmd.tol'),
               ...) {
    args = c(as.list(environment()), list(...))
    do.call(R6Vmd$new, args = args)
}

#' @rdname vmd
#' @name vmd
#' @usage NULL
#' @format NULL
#' @export
R6Vmd = R6::R6Class('vmd',
  public = list(
    initialize = function(signal,
                          alpha = getOption('vmd.alpha'),
                          tau = getOption('vmd.tau'),
                          K = getOption('vmd.K'),
                          DC = getOption('vmd.DC'),
                          init = getOption('vmd.init'),
                          tol = getOption('vmd.tol'),
                          N = getOption('vmd.N'),
                          theme = getOption('vmd.theme.default'),
                          orderModes = getOption('vmd.orderModes')) {
        if (missing(signal))
            stop("'signal' argument is required", call. = FALSE)
        self$
        setSignal(signal)$
        setK(K)$
        setAlpha(alpha)$
        setTau(tau)$
        setDC(DC)$
        setInit(init)$
        setTol(tol)$
        setN(N)$
        setTheme(theme)$
        setOrderModes(orderModes)
    },
    reset = function() {
        private$varResult = list()
        invisible(self)
    },
    getEnviron = function() {
        self[['.__enclos_env__']]
    },
    getPrivate = function() {
        self$getEnviron()$private
    },
    getResult = function() {
        if (!length(private$varResult))
            self$calculate()
        invisible(private$varResult)
    },

    setSignal = function(signal) {
        private$check$checkNumeric(signal)
        private$check$checkLength(signal, 2, op = '>=')
        private$varSignal = signal
        self$reset()
        invisible(self)
    },
    setAlpha = function(alpha) {
        private$varAlpha = private$check$checkNumericScalar(alpha)
        self$reset()
        invisible(self)
    },
    setTau = function(tau) {
        private$varTau = private$check$checkNumericScalar(tau)
        self$reset()
        invisible(self)
    },
    setK = function(K) {
        K = as.integer(private$check$checkNumericScalar(K))
        private$varK = private$check$checkRange(K, 2)
        self$reset()
        invisible(self)
    },
    setDC = function(DC) {
        private$varDC = private$check$checkLogicalScalar(DC)
        self$reset()
        invisible(self)
    },
    setInit = function(init) {
        objnm = deparse(substitute(init))
        init = private$check$checkIntegerScalar(init, objnm)
        private$varInit = private$check$checkRange(init, 0, 2, objnm = objnm)
        self$reset()
        invisible(self)
    },

    setTol = function(tol) {
        private$varTol = private$check$checkNumericScalar(tol)
        self$reset()
        invisible(self)
    },

    setN = function(N) {
        a = private$check$checkNumericScalar(getOption('vmd.NMin'), objnm = 'Nmin')
        b = private$check$checkNumericScalar(getOption('vmd.NMax'), objnm = 'Nmax')
        private$varN = private$check$checkRange(N, a, b, aop = '>=', bop = "<=")
        self$reset()
        invisible(self)
    },

    setOrderModes = function(orderModes) {
        private$varOrderModes = private$check$checkLogicalScalar(orderModes)
        self$reset()
        invisible(self)
    },

    setTheme = function(theme) {
        if (inherits(theme, 'list')) private$varTheme = private$check$checkListOfClass(theme, 'theme')
        else private$varTheme = private$check$checkClass(theme, 'theme')
        #No Reset Necessary, Doesn't Influence Result, only Presentation
        invisible(self)
    },

    calculate = function() {

        # Load the Variables
        signal = self$signal #The Signal to Decompose
        alpha = self$alpha #Balancing Parameter for Data Fidelity
        K = self$K #Number of Modes
        DC = self$DC #First mode is DC if TRUE
        init = self$init #Initialization Flag, 0, 1 or 2
        tol = self$tol #Tolerance for convergence
        tau = self$tau #Time-step for dual ascent
        N = self$N;
        #Maximum number of iterations
        orderModes = self$orderModes #Order the Modes by Increasing (Final) Omegas

        #Flip aswell as mirror?
        flip = FALSE #Mirror Only

        # System Variables
        eps = .Machine$double.eps #Smallest positive floating-point number

        # Period and sampling frequency of input signal
        lenOrg = length(signal)
        fs = 1 / lenOrg

        # Extend the signal by mirroring
        hw = floor(lenOrg / 2) #The Halfwidth
        lhs = rev(head(signal, 0 + hw)) #First Half, Reversed
        if (flip) lhs = tail(lhs, 1) - c(lhs - tail(lhs, 1)) #Flipped
        rhs = rev(tail(signal, lenOrg - hw)) #Last  Half, Reversed
        if (flip) rhs = head(rhs, 1) - c(rhs - head(rhs, 1)) #Flipped
        signalMir = c(lhs, signal, rhs);
        #Mirrored Signal

        # Time Domain 0 to T (of mirrored signal)
        lenMir = length(signalMir) ##NB: Previously 'T' in original code, but T is reserved in R.
        t = seq_len(lenMir) / lenMir

        # Spectral Domain discretization
        freqs = t - 0.5 - (1.0 / lenMir)

        # For future generalizations: individual alpha for each mode
        Alpha = rep(alpha, K)

        # Construct and center f_hat
        f_hat = private$fftshift(fft(signalMir))
        f_hat_plus = f_hat
        f_hat_plus[1:floor(lenMir / 2)] = 0;

        # Matrix keeping track of every iterant, could be discarded for mem
        u_hat_plus = array(0, c(N, lenMir, K));

        # Initialization of omega_k
        omega_plus = array(0, c(N, K));
        if (init == 1) {
            omega_plus[1,] = (0.5 / K) * ((1:K) - 1)
        } else if (init == 2) {
            omega_plus[1,] = sort(exp(log(fs) + (log(0.5) - log(fs)) * runif(K)));
        }

        # If DC mode imposed, set its omega to 0
        if (DC)
            omega_plus[1, 1] = 0

        # Start with empty dual variables
        lambda_hat = array(0, c(N, lenMir))

        # Other inits
        ix = (floor(lenMir / 2) + 1):lenMir
        uDiff = Inf #update step
        n = 1 #loop counter
        sum_uk = 0 #accumulator

        # Main loop for iterative updates
        while (uDiff > tol & n < N) {

            # In the original matlab code, [A] The first mode is handled initially, and then [B] the subsequent
            # modes are looped (ie from 2:K), The following is a simplification, seeing as [A] and [B] largely
            # use the same code
            for (k in 1:K) {

                # Accumulator
                sum_uk = u_hat_plus[`if`(k == 1, n, n + 1),, `if`(k == 1, K, k - 1)] + sum_uk - u_hat_plus[n,, k]

                # Mode spectrum
                u_hat_plus[n + 1,, k] = (f_hat_plus - sum_uk - lambda_hat[n,] / 2) / (1 + Alpha[k] * (freqs - omega_plus[n, k]) ^ 2)

                # Center frequencies
                if (!DC || k > 1)
                    omega_plus[n + 1, k] = (freqs[ix] %*% (abs(u_hat_plus[n + 1, ix, k]) ^ 2)) / sum(abs(u_hat_plus[n + 1, ix, k]) ^ 2)
                }

            # Dual ascent
            lambda_hat[n + 1,] = lambda_hat[n,] + tau * (rowSums(u_hat_plus[n + 1,,]) - f_hat_plus)

            # Loop Counter
            n = n + 1

            # Converged Yet?
            uDiff = sapply(1:K, function(i) {
                a = u_hat_plus[n,, i] - u_hat_plus[n - 1,, i]
                b = Conj(a)
                (1 / lenMir) * (a %*% b)
            })
            uDiff = abs(eps + sum(uDiff))

            #Reporting
            # if (n > 0 && n %% 10 == 0)
            #    writeLines(sprintf("Iteration: %s, Diff: %.4g", n, uDiff))

            #Has it exploded?
            if (is.na(uDiff))
                stop("Problem converging, check parameters", call. = FALSE)
            }

        # Postprocessing and cleanup
        N = min(N, n)
        omega = omega_plus[1:N,]

        # Signal reconstruction
        u_hat = array(0, c(lenMir, K));
        u_hat[ix,] = u_hat_plus[N, ix,]
        u_hat[ix[1]:2,] = Conj(u_hat_plus[N, ix,])
        u_hat[1,] = Conj(u_hat[lenMir,]);

        #NB: This Differs from original (it is transpose)
        #    intentionally want consistency in having modes in columns
        u = array(0, c(lenMir, K))
        u[, 1:K] = Reduce('cbind', lapply(1:K, function(k) {
            Re(private$fftinv(private$fftshift(u_hat[, k], inverse = TRUE)))
        }))

        # Remove Mirror Part/s
        ixRow = seq_len(length(signal)) + length(lhs)
        u = u[ixRow,]
        u_hat = u_hat[ixRow,]
        freqs = freqs[ixRow]
        f_hat = f_hat[ixRow]

        # Recompute spectrum
        u_hat[, 1:K] = Reduce('cbind', lapply(1:K, function(k) {
            private$fftshift(fft(u[, k]))
        }))

        #Determine the ordering
        ixCol = `if`(orderModes, order, seq_along)(tail(omega, 1))

        #Store the Result
        private$varResult = list(signal = self$Signal,
                               u = u[, ixCol, drop = FALSE],
                               u_hat = u_hat[, ixCol, drop = FALSE],
                               omega = omega[, ixCol, drop = FALSE],
                               freqs = freqs,
                               f_hat = f_hat)

        #Done
        invisible(self)
    },

    #Extract Data Frame
    as.data.frame = function() {
        nameSig = private$getLabel("plot.nameSignal")
        nameModeDC = private$getLabel("plot.nameModeDC")
        nameModeX = private$getLabel("plot.nameModeX")
        nameModeAgg = private$getLabel("plot.nameModeAgg")

        DC = self$DC
        result = self$getResult()
        df = as.data.frame(result$u);
        colnames(df) = c(`if`(DC, nameModeDC, NULL), sprintf(nameModeX, 1:(ncol(df) - DC)))
        df[, nameModeAgg] = rowSums(df)
        df[, nameSig] = self$signal
        df$x = 1:nrow(df);
        rownames(df) = df$x
        ix = c('x', nameSig)
        df[, c(ix, setdiff(names(df), ix))]
    },

    #Generic Plot Function
    plot = function(what = 'components', ...) {
        pattern = 'plot\\.(.*)'
        vars = ls(envir = self);
        functions = gsub(pattern, '\\1', vars[grep(vars, pattern = pattern)])
        private$check$checkIsIn(what, functions)
        do.call(sprintf("plot.%s", what), args = list(...), envir = self)
    },

    #Plot the Decomposed Modes
    plot.components = function(which = 'all', facet = 'none', scales = 'fixed') {

        #Run Checks on Arguments
        chk = private$check
        chk$checkCharacterScalar(facet)
        chk$checkIsIn(facet, c('none', 'byvariable', 'bymode', 'byclass'))
        chk$checkCharacterScalar(scales)
        chk$checkIsIn(scales, c('fixed', 'free', 'free_x', 'free_y'))

        #Is there a DC Component?
        DC = self$DC

        #Special Names
        nameSig = private$getLabel("plot.nameSignal")
        nameAggregate = private$getLabel("plot.nameAggregate")
        nameModeDC = private$getLabel("plot.nameModeDC")
        nameModeAgg = private$getLabel("plot.nameModeAgg")
        nameModeX = private$getLabel("plot.nameModeX")
        nameModes = private$getLabel("plot.nameModes")
        nameModel = private$getLabel("plot.nameModel")

        #Get Result
        df = self$as.data.frame() %>%
                      reshape2::melt('x')

        #Names for Modes
        nameModesOrd = setdiff(unique(df$variable), c(nameSig, nameModeAgg))

        df$variable = as.character(df$variable)
        df$variable = factor(df$variable, levels = c(nameSig, nameModeAgg, nameModesOrd))
        df$linetype = nameModel;
        df$linetype[which(df$variable == nameSig)] = nameSig


        #Perform the Subset
        vars = levels(df$variable)
        chk$checkIsIn(which, c('all', 'modes', vars))
        variables.ss = unique(c(
        `if`('all' %in% which, vars, NULL),
        `if`('modes' %in% which, vars[grep(gsub("%i", "[0-9]+", nameModeX), vars)], NULL),
        setdiff(which, c('all', 'modes'))
      ))
        if (length(setdiff(vars, variables.ss)) > 0) #<<< Is subset even nessessary?
            df = subset(df, variable %in% variables.ss)


        #For Faceting
        vars = as.character(df$variable)
        ix = which(!{ vars %in% c(nameSig, nameModeAgg) })
        df$byvariable = df$variable

        df$bymode = nameAggregate;
        df$bymode[ix] = vars[ix];
        df$bymode = factor(df$bymode, levels = c(nameAggregate, nameModesOrd))

        df$byclass = nameAggregate;
        df$byclass[ix] = nameModes;
        df$byclass[which(DC & { vars %in% c(nameModeDC) })] = nameModeDC
        df$byclass = factor(df$byclass)


        #Construct the Plot
        base = ggplot(df, aes(x = x, y = value, color = variable, linetype = linetype)) +
        self$theme +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        geom_path() +
        guides(linetype = guide_legend(order = 1),
               color = guide_legend(order = 2)) +
        labs(title = "Variational Mode Decomposition (VMD)",
             linetype = "Series",
             color = "Mode ID")

        #Add the faceting
        if (facet != 'none') {
            fml = as.formula(sprintf("%s~.", facet))
            base = base + facet_grid(fml, scales = scales)
        }

        #Done
        base
    },

    #Plot Input with Model Overlayed
    plot.model = function(...) {

        #Global Names
        nameSig = private$getLabel("plot.nameSignal")
        nameModeAgg = private$getLabel("plot.nameModeAgg")
        nameSeries = private$getLabel("plot.nameSeries")

        #Base of the modes plot.
        args = list(...);
        args$which = c(nameSig, nameModeAgg)
        base = do.call(self$plot.components, args = args)

        #Determine the Colors
        cols = c('red', 'black');
        names(cols) = c(nameSig, nameModeAgg)

        #Adjust the plot routine and return
        base +
        guides(linetype = 'none') +
        scale_color_manual(values = cols) +
        labs(color = nameSeries) +
        theme(legend.position = c(0.01, 0.99),
              legend.justification = c(0, 1))
    },

    #Plot the Input Spectrum
    plot.input.spectrum = function() {
        result = self$getResult()

        #Global Names
        nameInput = private$getLabel("plot.nameInput")
        nameSeries = private$getLabel("plot.nameSeries")

        #Data for the Plot
        df = data.frame(x = result$freqs,
                          y = Mod(result$f_hat),
                          variable = nameInput) %>%
               subset(x > 0)

        #Colours
        cols = c('black');
        names(cols) = nameInput

        #Process the Plot
        base = ggplot(data = df, aes(x, y, color = variable)) +
        self$theme +
        theme(axis.title = element_blank(),
              legend.position = c(0.01, 0.99),
              legend.justification = c(0, 1)) +
        scale_color_manual(values = cols) +
        geom_path() +
        scale_x_log10() +
        scale_y_log10() +
        labs(title = sprintf("%s Signal Spectrum", nameInput),
             color = nameSeries)

        #Done, Return
        base
    },

    #Plot the Spectral Decomposition
    plot.spectral.decomposition = function() {
        result = self$getResult()
        df = as.data.frame(Mod(result$u_hat))
        DC = self$DC

        nameInput = private$getLabel("plot.nameInput")
        nameModeDC = private$getLabel("plot.nameModeDC")
        nameModeX = private$getLabel("plot.nameModeX")
        nameSeries = private$getLabel("plot.nameSeries")
        nameModes = private$getLabel("plot.nameModes")

        colnames(df) = c(`if`(DC, nameModeDC, NULL), sprintf(nameModeX, 1:(ncol(df) - DC)))
        df$x = result$freqs
        df[, nameInput] = Mod(result$f_hat)
        df = df %>%
                         subset(x > 0) %>%
                         reshape2::melt('x')
        df$variable = as.character(df$variable)
        df$variable = factor(df$variable, levels = c(nameInput, setdiff(unique(df$variable), nameInput)))

        modes = setdiff(levels(df$variable), nameInput)
        pal = c('black', scales::hue_pal()(length(modes)))
        names(pal) = c(nameInput, modes)

        ggplot(data = df, aes(x = x, y = value, color = variable)) +
          self$theme +
          theme(axis.title = element_blank()) +
          theme(legend.position = c(0.99, 0.99),
                legend.justification = c(1, 1)) +
          geom_path() +
          scale_x_log10() +
          scale_y_log10() +
          scale_color_manual(values = pal) +
          labs(title = "Spectral Decomposition",
               subtitle = sprintf("%s + %sx %s",
                                  nameInput, length(unique(df$variable)) - 1, nameModes),
               color = nameSeries)
    },

    #Function to set label
    setLabel = function(what, value) {
        private$check$checkCharacterScalar(what)
        private$check$checkCharacterScalar(value)
        private$check$checkIsIn(what, names(private$varLabels), objnmB = 'names(varLabels)')
        private$varLabels[what] = value
        invisible(self)
    }
  ),
  private = list(
    check = R6Checker$new(),
    varSignal = NULL,
    varAlpha = NULL,
    varTau = NULL,
    varK = NULL,
    varDC = NULL,
    varInit = NULL,
    varTol = NULL,
    varN = NULL,
    varTheme = NULL,
    varOrderModes = NULL,
    varResult = list(),

    varLabels = list(
      "plot.nameSignal" = 'Signal',
      "plot.nameModel" = 'Model',
      "plot.nameAggregate" = 'Aggregate',
      "plot.nameModeX" = 'M%i',
      "plot.nameModeDC" = "MDC",
      "plot.nameModeAgg" = "MAgg",
      "plot.nameModes" = 'Modes',
      "plot.nameInput" = 'Input',
      "plot.nameSeries" = 'Series'
    ),

    #Function to get the label
    getLabel = function(what) {
        private$check$checkCharacter(what)
        private$check$checkIsIn(what, names(private$varLabels), objnmB = 'names(varLabels)')
        private$varLabels[[what]]
    },

    #Emulate Matlab fftshift function
#last half, then first half
    fftshift = function(x, inverse = FALSE) {
        private$check$checkClass(x, c('numeric', 'complex'))
        private$check$checkLogicalScalar(inverse)
        len = length(x);
        hw = `if`(!inverse, floor(len / 2), ceiling(len / 2))
        c(x[(hw + 1):len], x[1:hw])
    },

    #Normalized Inverse Fast Fourier Transform
    fftinv = function(x) {
        private$check$checkClass(x, c('numeric', 'complex'))
        fft(x, inverse = TRUE) / length(x)
    }
  ),
  active = list(
    signal = function(signal) {
        if (missing(signal)) return(private$varSignal)
        self$setSignal(signal)
    },
    alpha = function(alpha) {
        if (missing(alpha)) return(private$varAlpha)
        self$setAlpha(alpha)
    },
    tau = function(tau) {
        if (missing(tau)) return(private$varTau)
        self$setTau(tau)
    },
    K = function(K) {
        if (missing(K)) return(private$varK)
        self$setK(K)
    },
    DC = function(DC) {
        if (missing(DC)) return(private$varDC)
        self$setDC(DC)
    },
    init = function(init) {
        if (missing(init)) return(private$varInit)
        self$setInit(init)
    },
    tol = function(tol) {
        if (missing(tol)) return(private$varTol)
        self$setTol(tol)
    },
    N = function(N) {
        if (missing(N)) return(private$varN)
        self$setN(N)
    },
    orderModes = function(orderModes) {
        if (missing(orderModes)) return(private$varOrderModes)
        self$setOrderModes(orderModes)
    },
    theme = function(theme) {
        if (missing(theme)) return(private$varTheme)
        self$setTheme(theme)
    }
  )
)

#' @rdname vmd
#' @name vmd
#' @usage NULL
#' @format NULL
#' @export
plot.vmd = function(x, ...) {
    x$plot.components(...)
}

#' @rdname vmd
#' @name vmd
#' @usage NULL
#' @format NULL
#' @export
as.data.frame.vmd = function(x, row.names, optional, ...) {
    x$as.data.frame()
}
