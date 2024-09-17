#include <RcppCommon.h>

# ifdef XDS_TYPES_HPP
  #include "XDS_types.h"
# else
  #include "../inst/include/XDS_types.h" // This is the header file that contains the QueueElement class definition.
# endif


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
    int   eventType = Rcpp::as<int>(list["eventType"]);
    Rcpp::RObject additionalInfo = list["additionalInfo"];
    return QueueElement(priority, eventType, additionalInfo);
  }
}

//' @useDynLib XDS
//' @export
// [[Rcpp::export]]
Rcpp::XPtr<Rcpp::QueueElement> createQueueElement(float priority, int eventType, Rcpp::RObject additionalInfo) {
  return Rcpp::XPtr<Rcpp::QueueElement>(new Rcpp::QueueElement(priority, eventType, additionalInfo));
}

//' @useDynLib XDS
//' @export
// [[Rcpp::export]]
void releaseQueueElement(Rcpp::XPtr<Rcpp::QueueElement> x) {
  x.release();
}

//' @useDynLib XDS
//' @export
// [[Rcpp::export]]
Rcpp::List getQueueElementInfo(Rcpp::XPtr<Rcpp::QueueElement> x) {
  return x->getInfo();
}
