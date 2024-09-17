#ifndef XDS_TYPES_HPP
#define XDS_TYPES_HPP

#include <RcppCommon.h>
#include <Rcpp.h>

namespace Rcpp {
  struct QueueElement {
    float priority;
    int eventType;
    Rcpp::RObject additionalInfo;

    QueueElement(float priority, const int eventType, const Rcpp::RObject& additionalInfo) 
      : priority(priority), eventType(eventType), additionalInfo(additionalInfo) {}
 
    bool operator<(const QueueElement& other) const {
      return priority > other.priority;
    }

    float getPriority() const {
      return priority;
    }

    float getEventType() const {
      return eventType;
    }

    Rcpp::RObject getAdditionalInfo() const {
      return additionalInfo;
    }

    Rcpp::List getInfo() const {
      return Rcpp::List::create(Rcpp::Named("priority") = priority,
                                Rcpp::Named("eventType") = eventType,
                                Rcpp::Named("additionalInfo") = additionalInfo);
    }
  };

  template <> SEXP wrap(const QueueElement& x);
  template <> QueueElement as(SEXP x);
}

#endif
