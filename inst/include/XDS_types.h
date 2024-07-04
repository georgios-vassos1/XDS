#include <RcppCommon.h>
#include <Rcpp.h>

namespace Rcpp {
  struct QueueElement {
    float priority;
    Rcpp::RObject additionalInfo;

    QueueElement(float priority, const Rcpp::RObject& additionalInfo) 
      : priority(priority), additionalInfo(additionalInfo) {}
    
    bool operator<(const QueueElement& other) const {
      return priority > other.priority;
    }

    float getPriority() const {
      return priority;
    }

    Rcpp::RObject getAdditionalInfo() const {
      return additionalInfo;
    }

    Rcpp::List getInfo() const {
      return Rcpp::List::create(Rcpp::Named("priority") = priority,
                                Rcpp::Named("additionalInfo") = additionalInfo);
    }
  };

  template <> SEXP wrap(const QueueElement& x);
  template <> QueueElement as(SEXP x);
}
