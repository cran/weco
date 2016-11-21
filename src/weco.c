#include <R.h>
#include <Rmath.h>

//get running length
void getrl(int *violations, int *n, int *rst) {
  int i;
  int nv, carl;

  nv   = 0;
  carl = 0;
  for (i = 0; i < *n; i++) {
    if (0 == violations[i]) {
      carl++;
    } else {
      rst[nv] = carl;
      nv++;
      carl = 0;
    }
  }
}


//rule1 
void rule1(double *data, int *k, double *m, double *sd, int *n, int *rst) {
  int i;
  double b;

  b = (*k) * (*sd);

  for (i = 0; i < *n; i++) {
    if (fabs(data[i] - *m) > b ) {
      rst[i] = 1;
    } else {
      rst[i] = 0;
    }
  }
}

//rule2 
int compare(double d, double m) {
  int ls;
  if (d > m) {
    ls = 1;
  } else {
    ls = -1;
  } 
  return(ls);
}

void rule2(double *data, int *k, double *m, int *n, int *rst) {
  int i;
  int ktot, lsign, csign;

  lsign  = compare(data[0], *m);
  ktot   = 1; //total same side points
  rst[0] = 0;

  for (i = 1; i < *n; i++) {
    csign = compare(data[i], *m);
    if (csign == lsign) {
      ktot ++;
      if (ktot >= *k) {
        rst[i] = 1;
      }
    } else {
      lsign = csign;
      ktot  = 1;
    }
  }
}

void rule3(double *data, int *k, double *m, int *n, int *rst) {
  int i;
  int ktot, lsign, csign;

  lsign  = compare(data[1], data[0]);
  ktot   = 2; //total increasing or decreasing points
  rst[0] = 0;
  rst[1] = 0;
  for (i = 2; i < *n; i++) {
    csign = compare(data[i], data[i-1]);

    if (csign == lsign) {
      ktot ++;
      if (ktot >= *k) {
        rst[i] = 1;
      }
    } else {
      lsign = csign;
      ktot  = 2;
    }
  }
}


void rule4(double *data, int *k, double *m, int *n, int *rst) {
  int i;
  int ktot, lsign, csign;

  lsign  = compare(data[1], data[0]);
  ktot   = 2; //total increasing or decreasing points
  rst[0] = 0;
  rst[1] = 0;
  for (i = 2; i < *n; i++) {
    csign = compare(data[i], data[i-1]);

    if (csign != lsign) {
      lsign = csign;
      ktot ++;
      if (ktot >= *k) {
        rst[i] = 1;
      }
    } else {
      ktot  = 2;
    }
  }
}


void rule5(double *data, int *k, int *outofk, double *fold, double *m, double *sd, int *n, int *rst) {
  int i, ktot, mtot;
  double tmp, b;

  b = (*fold) * (*sd);

  ktot  = 0; //total out boundary
  mtot  = 0;
  for (i=0; i < *outofk; i++) {
    tmp = data[i] - *m; 
    if (tmp > b) {
      ktot++;
    } else if (tmp < -b) {
      mtot++;
    }

    if (ktot >= *k | mtot >= *k)
      rst[i] = 1;
  }

  for (i = *outofk; i < *n; i++) {

    //remove the first
    tmp = data[i - (*outofk)] - *m;
    if (tmp > b ) {
      ktot--;
    } else if (tmp < -b) {
      mtot--;
    } 

    //append the current
    tmp = data[i] - *m; 
    if (tmp > b) {
      ktot++;
    } else if (tmp < -b) {
      mtot++;
    }

    if (ktot >= *k | mtot >= *k)
      rst[i] = 1;
  }
}

void rule7(double *data, int *k, double *fold, double *m, double *sd, int *n, int *rst) {
  int i, ktot;
  double b, tmp;

  b = (*fold) * (*sd);

  ktot  = 0; //total out boundary
  for (i = 0; i < *n; i++) {

    tmp = data[i] - *m; 
    if (tmp < b & tmp > -b) {
      ktot++;
    } else {
      ktot = 0;
    }

    if (ktot >= *k)
      rst[i] = 1;
  }
}


void rule8(double *data, int *k, double *fold, double *m, double *sd, int *n, int *rst) {
  int i, ktot;
  double b, tmp;

  b = (*fold) * (*sd);

  ktot  = 0; //total out boundary
  for (i = 0; i < *n; i++) {

    tmp = data[i] - *m; 

    if (tmp > b | tmp < -b) {
      ktot++;
    } else {
      ktot = 0;
    }

    if (ktot >= *k)
      rst[i] = 1;
  }
}
