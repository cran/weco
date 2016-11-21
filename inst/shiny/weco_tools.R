##rm(list=ls());
##options(error = recover, warn=2);

##------------------------------------
##  RULES
##------------------------------------
weco.r1 <- function(x, ntot=1, nbad=NULL, k=3, sdx=sd(x), mux=mean(x)) {
    nx        <- length(x);
    last.n    <- x[(nx-ntot+1):nx];
    violation <- all(abs(last.n-mux) > k * sdx);

    violation;
}


weco.r2 <- function(x, ntot=3, nbad=2, k=2, sdx=sd(x), mux=mean(x)) {
    nx        <- length(x);
    last.n    <- x[(nx-ntot+1):nx];
    violation <- all(last.n <= mux) | all(last.n >= mux);
    violation <- violation &
                 sum( abs(last.n-mux) > k*sdx) >= nbad;

    violation;
}

weco.r4 <- function(x, ntot=8, nbad=NULL, k=NULL, sdx=sd(x), mux=mean(x)) {
    nx        <- length(x);
    last.n    <- x[(nx-ntot+1):nx];
    violation <- all(last.n <= mux) | all(last.n >= mux);
    violation;
}

##Fifteen consecutive observations occurring within one Standard
## Deviation from the Average
weco.r5 <- function(x, ntot=15, nbad=NULL, k=1, sdx=sd(x), mux=mean(x)) {
    nx        <- length(x);
    last.n    <- x[(nx-ntot+1):nx];
    violation <- all(abs(last.n-mux) < k * sdx);
    violation;
}

##Fourteen consecutive observations that alternate up and down.
weco.r7 <- function(x, ntot=14, nbad=NULL, k=1, sdx=sd(x), mux=mean(x)) {
    nx        <- length(x);
    last.n    <- x[(nx-ntot+1):nx];

    violation <- TRUE;

    l.f <- last.n[1] < last.n[2];
    for (i in 2:(ntot-1)) {
        c.f <- last.n[i] < last.n[i+1];
        if (c.f == l.f) {
            violation <- FALSE;
            break;
        } else {
            l.f <- c.f;
        }
    }

    violation
}

##Six consecutive observations that trend downward or upward.
weco.r8 <- function(x, ntot=6, nbad=NULL, k=1, sdx=sd(x), mux=mean(x)) {
    nx        <- length(x);
    last.n    <- x[(nx-ntot+1):nx];
    violation <- TRUE;

    l.f <- last.n[1] < last.n[2];
    for (i in 2:(ntot-1)) {
        c.f <- last.n[i] < last.n[i+1];
        if (c.f != l.f) {
            violation <- FALSE;
            break;
        } else {
            l.f <- c.f;
        }
    }
    violation
}

##all.rules
weco.last <- function(x) {
    rst <- weco.r1(x);
    rst <- c(rst, weco.r2(x));
    rst <- c(rst, weco.r2(x, ntot=5, nbad=4, k=1));
    rst <- c(rst, weco.r4(x));
    rst <- c(rst, weco.r5(x));
    rst <- c(rst, weco.r1(x, ntot=8, k=1));
    rst <- c(rst, weco.r7(x));
    rst <- c(rst, weco.r8(x));

    rst
}

##------------------------------------
##  SIMULATION
##------------------------------------
sim.values <- function(size, n.dis=5, len.dis=2, dis.mean=0,
                       dis.var=10, norm.mean=0, norm.var=1) {

    rst   <- rnorm(size, norm.mean, sqrt(norm.var));
    dis.t <- sample(size, n.dis);

    for (i in 1:n.dis) {
        cur.dis <- rnorm(len.dis, dis.mean, sqrt(dis.var));
        xx      <- dis.t[i]:min(dis.t[i]+len.dis-1, size);
        rst[xx] <- cur.dis[1:length(xx)];
    }


    ##return
    list(samples=rst, dis=dis.t, len=len.dis);
}

##weco rules check
weco.all <- function(sample, runin=30) {
    rst <- NULL;
    for (i in (runin+1):length(sample)) {
        cur.weco <- weco.last(sample[1:i]);
        if (any(1 == cur.weco))
            rst <- rbind(rst, c(i, cur.weco));
    }

    list(runin=runin, weco=rst);
}

##------------------------------------
##  PLOT
##------------------------------------
plot.hist.0 <- function(values, p.len, ybound, center=NULL) {

    nv   <- length(values);

    if (is.null(center)) {
        xx   <- max(1, nv-p.len):nv;
    } else {
        xx <- max(1, floor(center - p.len/2)) : min(nv, ceiling(center + p.len/2));
    }

    x.lb <- min(xx);
    x.ub <- x.lb + p.len * 1.1;

    plot(xx,
         values[xx],
         xlim=c(x.lb, x.ub),
         ylim=c(-ybound, ybound),
         type="b",
         xaxs="i", col="red", xlab="", ylab="samples");

    if (!is.null(center)) {
        lines( c(center, center), c(-ybound,ybound), lty=2, col="brown");
    }

    lines( c(x.lb, x.ub), c(0,0), lty=2, col="blue");
}


