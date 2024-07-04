#include "XDS_types.h"

#include <RcppCommon.h>

namespace Rcpp {
  template <> SEXP wrap(const QueueElement& x);
  template <> QueueElement as(SEXP x);
}

#include <Rcpp.h>

namespace Rcpp {
  template <> SEXP wrap(const QueueElement& x) {
    return x.getInfo();
  }

  template <> QueueElement as(SEXP x) {
    Rcpp::List list(x);
    float priority = Rcpp::as<float>(list["priority"]);
    Rcpp::RObject additionalInfo = list["additionalInfo"];
    return QueueElement(priority, additionalInfo);
  }
}

//' @useDynLib XDS
//' @export
// [[Rcpp::export]]
Rcpp::XPtr<Rcpp::QueueElement> createQueueElement(float priority, Rcpp::RObject additionalInfo) {
  return Rcpp::XPtr<Rcpp::QueueElement>(new Rcpp::QueueElement(priority, additionalInfo));
}

//' @useDynLib XDS
//' @export
// [[Rcpp::export]]
Rcpp::List getQueueElementInfo(Rcpp::XPtr<Rcpp::QueueElement> x) {
  return x->getInfo();
}
