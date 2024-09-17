#include <Rcpp.h>
#include <queue>

#ifdef XDS_TYPES_HPP
  #include "XDS_types.h"
#else
  #include "../inst/include/XDS_types.h" // This is the header file that contains the QueueElement class definition.
#endif

using namespace Rcpp;


// Define a simple wrapper class around std::priority_queue
class PriorityQueueWrapper {
  private:
    std::priority_queue<QueueElement> pq;

  public:
    void push(const Rcpp::XPtr<QueueElement>& element) {
      pq.push(*element);
    }

    Rcpp::XPtr<QueueElement> pop() {
      if (!pq.empty()) {
        // Here, true is passed as the second argument to Rcpp::XPtr constructor, which indicates 
        // that Rcpp::XPtr owns the pointer and is responsible for deleting it. Rcpp::XPtr will 
        // automatically call the appropriate deleter when the object goes out of scope, ensuring 
        // proper memory deallocation.
        Rcpp::XPtr<QueueElement> topElement(new QueueElement(pq.top()), true);
        pq.pop();
        return topElement;
      } else {
        return Rcpp::XPtr<QueueElement>((QueueElement*)nullptr);
      }
    }

    Rcpp::List top() const {
      if (!pq.empty()) {
        return pq.top().getInfo();
      } else {
        Rcpp::List emptyList;
        emptyList.attr("names") = CharacterVector::create("priority", "eventType", "additionalInfo");
        return emptyList;
      }
    }

    bool empty() const {
      return pq.empty();
    }
};

// Expose the PriorityQueueWrapper class to R using Rcpp
RCPP_MODULE(PriorityQueueModule) {
  class_<PriorityQueueWrapper>("PriorityQueueWrapper")
    .constructor()
    .method("push",  &PriorityQueueWrapper::push)
    .method("pop",   &PriorityQueueWrapper::pop)
    .method("top",   &PriorityQueueWrapper::top)
    .method("empty", &PriorityQueueWrapper::empty);
}

