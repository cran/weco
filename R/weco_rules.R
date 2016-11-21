#' Western Electric Company Rules (WECO) for Shewhart Control Chart
#'
#' @docType package
#' @name weco-package
#' @useDynLib weco, .registration = TRUE
#'
#' @importFrom grDevices colors
#' @importFrom graphics axis box legend lines par plot points text
#' @importFrom stats sd
#'
#'
#' @section Introduction:
#'
#' WECO rules got the name from the quality control hand book published by the
#' Western Electric Company in 1956. These rules detect the deviation from a
#' stable process by continuously testing data against predefined abnormal
#' patterns and have been accompanied with Shewhart control chart in statistical
#' process control.
#'
#' There are eight commonly adopted abnormal patterns, namely, eight WECO rules.
#' It is of great interest to combine different WECO rules to increase the power
#' to detect deviation from a stable process. This package implements the
#' combination of multiple WECO rules.
#'
#' Note that it is necessary to minimize the probability for making false alarms
#' when the process is in control. Running length is the first time at which a
#' process experiences out-of-control signals. The average running length (ARL)
#' is frequently used as a measure in statistical process control for evaluating
#' and comparing the performances of different methods.
#'
#' @section Rules:
#'
#' This package considers the following eight WECO rules:
#'
#' \describe{
#'
#' \item{Rule 1}{1 data point is greater than 3 standard deviation
#' from the center line. (This rule is to identify single data point that is out
#' of the acceptable range.)}
#'
#' \item{Rule 2}{9 data points in a row on the same side of the center line.
#' (The ideal stable process is assumed to be up and down around the center
#' line. A large block of data points on the same side of the center line
#' indicates a process mean is shifted.)}
#'
#' \item{Rule 3}{6 data points in a row, all increasing or decreasing. (This
#' rule is also an indicator of possible mean shift.)}
#'
#' \item{Rule 4}{16 data points in a row, alternating up and down. (When data
#' points are routinely alternating up and down, it shows a high negative
#' correlation between neighboring observations, which is abnormal for a stable
#' process. For an in-control-process, it is not expected to observe correlation
#' between neighboring data points.)}
#'
#' \item{Rule 5}{2 out of 3 data points on the same side are greater than 2
#' standard deviations from the center line. (For a normally distributed
#' in-control-process, about 95% of data points should be within 2 standard
#' deviation. The chance to violate this rule is 0.00306. This rule is used to
#' detect increase in process variation.)}
#'
#' \item{Rule 6}{4 out of 5 data points on the same side are greater than 1
#' standard deviation from the center line. (For a normally distributed
#' in-control-process, about 62% of data points should be within 1 standard
#' deviation. This chance to violate this rule is 0.00553. This rule is also
#' used to detect increase in process variation.)}
#'
#' \item{Rule 7}{15 data points in a row within 1 standard deviation of the
#' center line. (Too many data points are within 1 standard deviation indicates
#' the decrease in process variation.)}
#'
#' \item{Rule 8}{8 data points in a row are greater than 1 standard deviation of
#' the center line. (This is another rule to detect increase in process
#' variance.)}
#'
#' }
#'
#' @section Graphical user interface (GUI):
#'
#' This package provides a web-based Shiny GUI. See \code{\link{run.weco}} for
#' details.
#'
#' @references
#'
#' Introduction to Statistical Quality Control. Montgomery D.C. Third Edition.
#' Wiley, New York. 1996
#'
NULL

#' Individual WECO Rule
#'
#' Apply individual WECO rules 1-8 to continuously observed data from some
#' process to detect abnormal signals. See \code{\link{weco-package}} for the
#' details of the eight WECO rules.
#'
#' @param rule WECO rule number from 1 to 8
#' @param x   A vector of continuously observed data from some process
#' @param sdx Standard deviation of the observed data
#' @param mux Mean of the observed data
#' @param ... Specifications for individual rule
#'            \describe{
#'             \item{k}{Number of "abnormal" data points}
#'             \item{l}{Number of standard deviations}
#'             \item{n}{Number of data points prior to the current point
#'            (including the current point) to be evaluated}}
#'
#' @return A class \code{weco} list contains the original data \code{x}, the mean,
#'     standard deviation, the rules, and a vector with the same length as \code{x} that
#'     contains the out of boundary status of each point
#'
#' @examples
#'
#' x <- rnorm(1000);
#'
#' ## use empirical mean and standard deviation of x
#' rst.1 <- weco.rule(1, x, l=3);
#' rst.2 <- weco.rule(2, x, k=9);
#' rst.3 <- weco.rule(3, x, k=6);
#' rst.4 <- weco.rule(4, x, k=16);
#' rst.5 <- weco.rule(5, x, k=2, n=3, l=2);
#' rst.6 <- weco.rule(6, x, k=4, n=5, l=1);
#' rst.7 <- weco.rule(7, x, k=15, l=1);
#' rst.8 <- weco.rule(8, x, k=8, l=1);
#'
#' ## use given mean and standard deviation
#' rst.1 <- weco.rule(1, x, l=3, mux=0, sdx=1);
#'
#' @seealso \code{\link{weco-package}}
#'
#' @export
#'
weco.rule <- function(rule, x, sdx=sd(x), mux=mean(x), ...) {

    if (!(rule %in% 1:8))
        return(NULL);

    weco.rst <- NULL;
    eval(parse(text=paste("weco.rst <- weco.rule", rule,
                          "(x, sdx=sdx, mux=mux, ...)",
                          sep="")));

    rst <- list(x   = x,
                sdx = sdx,
                mux = mux,
                rule = rule,
                pars = list(list(...)),
                weco = weco.rst)

    class(rst) <- "weco";
    rst
}


weco.rule1 <- function(x, l=3, sdx=sd(x), mux=mean(x)) {
    nx    <- length(x);
    rst.c <- .C("rule1",
                as.double(x),   as.integer(l),
                as.double(mux), as.double(sdx),
                as.integer(nx), as.integer(array(0,nx)));
    rst <- rst.c[[6]];
    rst
}

weco.rule2 <- function(x, k=9, sdx=sd(x), mux=mean(x)) {
    nx    <- length(x);
    rst.c <- .C("rule2",
                as.double(x),   as.integer(k),
                as.double(mux), as.integer(nx),
                as.integer(array(0,nx)));
    rst <- rst.c[[5]];
    rst
}

weco.rule3 <- function(x, k=6, sdx=sd(x), mux=mean(x)) {
    nx    <- length(x);
    rst.c <- .C("rule3",
                as.double(x),   as.integer(k),
                as.double(mux), as.integer(nx),
                as.integer(array(0,nx)));
    rst <- rst.c[[5]];
    rst
}


weco.rule4 <- function(x, k=16, sdx=sd(x), mux=mean(x)) {
    nx    <- length(x);
    rst.c <- .C("rule4",
                as.double(x),   as.integer(k),
                as.double(mux), as.integer(nx),
                as.integer(array(0,nx)));
    rst <- rst.c[[5]];
    rst
}


weco.rule5 <- function(x, k=2, n=k+1, l=2, sdx=sd(x), mux=mean(x)) {
    nx    <- length(x);
    rst.c <- .C("rule5",
                as.double(x), as.integer(k),
                as.integer(n), as.double(l),
                as.double(mux), as.double(sdx),
                as.integer(nx),
                as.integer(array(0,nx)));
    rst <- rst.c[[8]];
    rst
}

weco.rule6 <- function(x, k=4, n=k+1, l=1, sdx=sd(x), mux=mean(x)) {
    weco.rule5(x, k=k, n=n, l=1, sdx=sdx, mux=mux);
}


weco.rule7 <- function(x, k=15, l=1, sdx=sd(x), mux=mean(x)) {
    nx    <- length(x);
    rst.c <- .C("rule7",
                as.double(x),       as.integer(k),
                as.double(l),
                as.double(mux),     as.double(sdx),
                as.integer(nx),
                as.integer(array(0,nx)));
    rst <- rst.c[[7]];
    class(rst) <- "weco";
    rst
}

weco.rule8 <- function(x, k=8, l=1, sdx=sd(x), mux=mean(x)) {
    nx    <- length(x);
    rst.c <- .C("rule8",
                as.double(x),       as.integer(k),
                as.double(l),
                as.double(mux),     as.double(sdx),
                as.integer(nx),
                as.integer(array(0,nx)));
    rst <- rst.c[[7]];
    class(rst) <- "weco";
    rst
}

#' Combination of WECO Rules
#'
#' Apply a combination of multiple WECO rules to continuously observed data from some
#' process to detect abnormal signals
#'
#' @inheritParams weco.rule
#'
#' @param lst.rules List of rules with parameters. The first element in the list
#'     is the rule number
#'
#' @return A vector with the same length as \code{}{x} that contains the out of
#'         boundary status of each point
#'
#' @examples
#'
#' x         <- rnorm(1000);
#' rst.comb  <- weco.combine(x, lst.rules=list(list(1), list(2, k=9)));
#' rst.comb2 <- weco.combine(x, lst.rules=list(list(3), list(4, k=16)));
#'
#' @export
#'
weco.combine <- function(x, sdx=sd(x), mux=mean(x), lst.rules) {
    rst   <- 0;
    rules <- NULL;
    pars  <- list();
    for (i in 1:length(lst.rules)) {
        c.rule <- lst.rules[[i]];
        crl    <- c.rule[[1]];
        if (!(crl %in% 1:8))
            next;
        rules     <- c(rules, crl);
        pars[[i]] <- c.rule[-1];
        crst      <- do.call(weco.rule,
                             c(list(rule=crl,
                                    x=x,
                                    sdx=sdx,
                                    mux=mux),
                               c.rule[-1]));
        rst    <- rst + crst$weco;
    }

    weco.rst <- as.integer(rst > 0);
    rst      <- list(x   = x,
                     sdx = sdx,
                     mux = mux,
                     rule = rules,
                     pars = pars,
                     weco = weco.rst)
    class(rst) <- "weco";

    rst
}


#' Describe WECO rules
#'
#' Get the description of a WECO rule
#'
#' @inheritParams weco.rule
#' @param prt Whether to print the detailed information
#'
#'
#' @return A list that contains a text description of the rule and a vector of
#'     parameters with their default values
#'
#'
#' @examples
#'
#' info.1 <- weco.info(rule=1);
#' print(info.1$description);
#' info.1 <- weco.info(rule=1, l=2);
#' print(info.1$description);
#'
#' @export
#'
weco.info <- function(rule, ..., prt=TRUE) {
    if (!(rule %in% 1:8))
        return(NULL);

    rst <- NULL;
    eval(parse(text=paste("rst <- describe.r", rule,
                          "(...)",
                          sep="")));

    list(rule=rule,
         description=paste("Rule ", rule, ": ", rst[[1]], sep=""),
         pars=rst[[2]]);
}

describe.r1 <- function(l="l") {
    pars <- c(l=3);
    txt  <- paste("1 point > ", l, " standard deviations from center line. Default value l=3.",
                  sep="");
    list(txt, pars);
}

describe.r2 <- function(k="k") {
    txt  <- paste(k, " points in a row on same side of center line. Default value k=9.",
                  sep="");
    pars <- c(k=9);
    list(txt, pars);
}

describe.r3 <- function(k="k") {
    txt  <- paste(k, " points in a row, all decreasing or increasing. Default value k=6.",
                  sep="");
    pars <- c(k=6);
    vals <- c(6);
    list(txt, pars, vals);
}

describe.r4 <- function(k="k") {
    txt  <- paste(k, " points in a row, alternating up and down. Default value k=16.",
                  sep="");
    pars <- c(k=16);
    list(txt, pars);
}

describe.r5 <- function(k="k", n="n", l="l") {
    txt  <- paste(k, " out of ", n, " points > ",
                  l, " standard deviations from center line (same side). Default values k=2, n=3 and l=2.",
                  sep="");
    pars <- c(k=2, n=3, l=2);
    list(txt, pars);
}

describe.r6 <- function(k="k", n="n", l="l") {
    txt  <- paste(k, " out of ", n, " points > ",
                  l, " standard deviations from center line (same side). Default values k=4, n=5 and l=1.",
                  sep="");
    pars <- c(k=4, n=5, l=1);
    list(txt, pars);
}

describe.r7 <- function(k="k", l="l") {
    txt  <- paste(k, " points in a row within ", l,
                  " standard deviations of center line (either side). Default values k=15 and l=1.",
                  sep="");
    pars <- c(k=15, l=1);
    list(txt, pars);
}

describe.r8 <- function(k="k", l="l") {
    txt  <- paste(k, " points in a row >", l,
                  " standard deviations from center line (either side). Default values k=8 and l=1.",
                  sep="");
    pars <- c(k=8, l=1);
    list(txt, pars);
}



##------------------------------------
##  running length
##------------------------------------

#' Get running lengths
#'
#' @param weco.rst A class \code{weco} vector returned from
#'     \code{\link{weco.rule}} or \code{\link{weco.combine}}
#'
#' @examples
#'
#' x        <- rnorm(1000);
#' rst.comb <- weco.combine(x, lst.rules=list(list(1), list(2, k=9)));
#' weco.rl(rst.comb);
#'
#'
#' @return A vector of running lengths detected from the observed continuously
#'     observed data
#'
#' @seealso \code{\link{weco.rule}}, \code{\link{weco.combine}}
#'
#' @export
#'
weco.rl <- function(weco.rst) {

    stopifnot("weco" == class(weco.rst));

    violations <- weco.rst$weco;
    narl       <- length(which(1 == violations));

    if (0 == narl)
        return(NULL);

    rst.c <- .C("getrl",
                as.integer(violations),
                as.integer(length(violations)),
                as.integer(rep(0, narl)));
    rst <- rst.c[[3]];
    rst
}



##------------------------------------------------------------------------
##   SHINY
##-------------------------------------------------------------------------
#' Run Web-Based \code{weco} application
#'
#' Call Shiny to run \code{weco} as a web-based application. A web browser will
#' be brought up.
#'
#' @examples
#'
#' \dontrun{
#' run.weco()}
#'
#'
#' @export
#'
run.weco <- function() {
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Package Shiny needed for this function to work. Please install it.",
             call. = FALSE)
    }

    if (!requireNamespace("shinythemes", quietly = TRUE)) {
        stop("Pacakage shinythemes needed for this function to work. Please install it.",
             call. = FALSE)
    }

    if (!requireNamespace("DT", quietly = TRUE)) {
        stop("Pacakage DT needed for this function to work. Please install it.",
             call. = FALSE)
    }

    appDir <- system.file("shiny", package = "weco")
    if (appDir == "") {
        stop("Could not find Shiny directory. Try re-installing `weco`.",
             call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal");
}


##------------------------------------------------------------------------
##   PLOT
##-------------------------------------------------------------------------
get.point.col <- function(weco.rst, runin=0) {

    if (is.null(weco.rst))
        return(NULL);

    stopifnot("weco" == class(weco.rst));

    ##default
    samples <- weco.rst$x;
    cols    <- rep("black", length(samples));

    ##runin
    if (runin > 0)
        cols[1:runin] <- "gray";

    ##weco
    cols[1 == weco.rst$weco] <- "red";

    ##return
    cols
}

#' Plot WECO results
#'
#' Generate plot with the observed data with abnormalities identified
#'
#' @param x A class \code{weco} vector returned from
#'     \code{\link{weco.rule}} or \code{\link{weco.combine}}
#' @param ... The function takes the following arguments
#'            \describe{
#' \item{start}{First point for plotting}
#' \item{end}{Last point for plotting}
#' \item{selected}{Index of data points that will be highlighted}
#' \item{runin}{Number of running points that will be colored differently}
#' \item{ref}{Boolean. If true, reference lines will be given at 1, 2 and 3
#'     times standard deviation}
#' \item{ylim}{Range of Y-axis}
#' \item{cols}{Color of each data point}
#' }
#'
#' @seealso \code{\link{weco.rule}}, \code{\link{weco.combine}}
#'
#' @examples
#'
#' x        <- rnorm(1000);
#' rst.comb <- weco.combine(x, lst.rules=list(list(1), list(2, k=9)));
#' plot(rst.comb, start=10, end=80, selected = c(60,70));
#'
#' @export
#'
plot.weco <- function(x, ...) {

    weco.rst <- x;
    if (is.null(weco.rst))
        return(NULL);
    stopifnot("weco" == class(weco.rst));

    args <- list(...);
    start <- args$start;
    if (is.null(start)) {
        start <- 1;
    }

    end <- args$end;
    if (is.null(end)) {
        end <- length(weco.rst$x);
    }

    runin <- args$runin;
    if (is.null(runin)) {
        runin <- 0;
    }
    ref <- args$ref;
    if (is.null(ref)) {
        ref <- TRUE;
    }

    selected <- args$selected;
    ylim     <- args$ylim;
    cols     <- args$cols;

    sample <- weco.rst$x;
    mu     <- weco.rst$mux;
    sigma  <- weco.rst$sdx;

    if (is.null(ylim)) {
        min.y <- min(sample);
        max.y <- max(sample);

        my.l  <- min.y - 0.05*(max.y-min.y);
        my.u  <- max.y + 0.05*(max.y-min.y);

        my.half <- max(mu - my.l, my.u - mu);
        ylim  <- c(mu - my.half, mu + my.half);
    }

    if (is.null(cols))
        cols <- get.point.col(weco.rst, runin);

    ##all points in start:end
    xx <- start:end;
    plot(xx,
         sample[xx],
         xlim=c(start-1, end+1),
         ylim=ylim,
         type="b",
         xaxs="i",
         col=cols[xx], xlab="", ylab="samples");

    ##selected
    if (length(selected) > 0) {
        for (j in 1:length(selected)) {
            xx <- selected[j];
            if (xx > runin) {
                points(xx, sample[xx], col=cols[xx], cex=1.5);
                lines(c(xx, xx), ylim, lty=2, col=cols[xx]);
            }
        }
    }

    ##refrence lines
    if (ref) {
        for (i in -3:3) {
            lines(c(start-1, end+1), c(mu+i*sigma, mu+i*sigma), lty=2, col="gray");
        }
    }
}


#' Summary of WECO outcome
#'
#' Print summary of a class \code{weco} object
#'
#' @param object A class \code{weco} object returned from
#'     \code{\link{weco.rule}} or \code{\link{weco.combine}}
#' @param ... Other arguments for \code{summary}
#' @examples
#' x        <- rnorm(1000);
#' rst.comb <- weco.combine(x, lst.rules=list(list(1), list(2, k=9)));
#' summary(rst.comb);
#'
#' @export
#'
summary.weco <- function(object, ...) {
    cat("The data contains ", length(object$x), " data points.\n\n", sep="");
    cat("The stable process is assumed to be normal with \n    mean: ", object$mux,
        "\n    standard deviation: ", object$sdx, ".\n\nThe following rules are applied to find abnormalities:\n",
        sep="");
    for (i in 1:length(object$rule)) {
        cat("Rule ", object$rule[i], ":\n", sep="");
        cur.info <- do.call(weco.info, c(rule=object$rule[i],
                                         object$pars[[i]],
                                         prt=FALSE));
        cat("  ", cur.info$description, ".\n", sep="");
    }
    cat("\nThere are in total ", sum(object$weco), " points found that were abnormal.\n", sep="");
}
