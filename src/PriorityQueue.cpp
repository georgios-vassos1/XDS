#include <Rcpp.h>
#include <queue>
using namespace Rcpp;

// Define a struct to represent elements of the priority queue
struct QueueElement {
  float priority;
  Rcpp::RObject additionalInfo; // Rcpp::RObject is a generic representation of R objects
  
  QueueElement(float priority, const Rcpp::RObject& additionalInfo) 
    : priority(priority), additionalInfo(additionalInfo) {}
  
  // Define custom comparison function for priority queue ordering
  bool operator<(const QueueElement& other) const {
    return priority > other.priority; // Order by priority (min heap)
  }

  // Getter for priority
  float getPriority() const {
    return priority;
  }

  // Getter for additionalInfo
  Rcpp::RObject getAdditionalInfo() const {
    return additionalInfo;
  }

  // Getter to retrieve both priority and additionalInfo
  Rcpp::List getInfo() const {
    return Rcpp::List::create(Rcpp::Named("priority") = priority,
                              Rcpp::Named("additionalInfo") = additionalInfo);
  }
};

// Define a conversion mechanism from QueueElement to SEXP
namespace Rcpp {
  template <>
  SEXP wrap(const QueueElement& element) {
    return element.getInfo();
  }
}

// Expose QueueElement as an Rcpp class
// [[Rcpp::export]]
Rcpp::XPtr<QueueElement> createQueueElement(float priority, Rcpp::RObject additionalInfo) {
  return Rcpp::XPtr<QueueElement>(new QueueElement(priority, additionalInfo));
}

// Define an R function to call QueueElement methods
// [[Rcpp::export]]
Rcpp::List getQueueElementInfo(Rcpp::XPtr<QueueElement> x) {
  return x->getInfo();
}
