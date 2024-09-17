#include <Rcpp.h>
#include <queue>
#include <stack>
#include <functional>
#include <algorithm>


using namespace Rcpp;

// Define a simple template wrapper class around std::queue using SEXP for generic type
class QueueWrapper {
  private:
    std::queue<SEXP> q;

  public:
    // Method to add an element to the queue (enqueue)
    void enqueue(SEXP element) {
      q.push(element);
    }

    // Method to remove and return the front element of the queue (dequeue)
    SEXP dequeue() {
      if (!q.empty()) {
        SEXP frontElement = q.front();
        q.pop();
        return frontElement;
      } else {
        stop("Queue is empty.");
      }
    }

    // Method to get the front element of the queue without removing it
    SEXP front() const {
      if (!q.empty()) {
        return q.front();
      } else {
        stop("Queue is empty.");
      }
    }

    // Check if the queue is empty
    bool empty() const {
      return q.empty();
    }

    // Get the size of the queue
    int size() const {
      return q.size();
    }

    void printQueue() const {
      std::queue<SEXP> temp = q;  // Make a copy of the queue to avoid modifying the original queue
      Rcpp::Rcout << "Queue contents: ";
      while (!temp.empty()) {
        // Convert the SEXP to an appropriate type for printing, such as a string
        std::string val = Rcpp::as<std::string>(temp.front());
        Rcpp::Rcout << val << " ";  // Print the content
        temp.pop();  // Remove the front element from the copy
      }
      Rcpp::Rcout << std::endl;  // End with a newline
    }

    // Method to convert the queue to an R vector (as a List)
    Rcpp::List toRVector() const {
      std::queue<SEXP> temp = q;  // Make a copy of the queue
      Rcpp::List result;

      while (!temp.empty()) {
        result.push_back(temp.front());  // Add front element to result list
        temp.pop();
      }

      return result;
    }

    // Method to swap contents with another queue
    void swap(QueueWrapper& other) {
      std::swap(q, other.q);
    }

    // Method to reverse the contents of the queue
    void reverse() {
      std::stack<SEXP> tempStack;  // Use a stack to reverse the queue

      // Move elements from queue to stack (which reverses the order)
      while (!q.empty()) {
        tempStack.push(q.front());
        q.pop();
      }

      // Move elements back to the queue
      while (!tempStack.empty()) {
        q.push(tempStack.top());
        tempStack.pop();
      }
    }

    // Method to merge another queue into this one
    void merge(const QueueWrapper& other) {
      std::queue<SEXP> temp = other.q;  // Copy the other queue

      // Add all elements from the other queue to this queue
      while (!temp.empty()) {
        q.push(temp.front());
        temp.pop();
      }
    }

    // Method to clear the queue
    void clear() {
      while (!q.empty()) {
        q.pop();
      }
    }

    // Method to find if an element exists in the queue
    bool contains(SEXP element) const {
      std::queue<SEXP> temp = q;  // Make a copy of the queue

      while (!temp.empty()) {
        if (temp.front() == element) {
          return true;  // Found the element
        }
        temp.pop();
      }

      return false;  // Element not found
    }

    // Method to calculate the sum of numeric elements in the queue
    double sum() const {
      std::queue<SEXP> temp = q;
      double total = 0.0;
      
      while (!temp.empty()) {
        if (Rf_isNumeric(temp.front())) {  // Check if the element is numeric
          total += as<double>(temp.front());
        }
        temp.pop();
      }
      
      return total;
    }

    // Method to check if two queues are equal
    bool equals(const QueueWrapper& other) const {
      std::queue<SEXP> temp1 = q;
      std::queue<SEXP> temp2 = other.q;
      
      // Compare each element
      while (!temp1.empty() && !temp2.empty()) {
        if (temp1.front() != temp2.front()) {
          return false;
        }
        temp1.pop();
        temp2.pop();
      }
      
      // Both queues should be empty if they are equal
      return temp1.empty() && temp2.empty();
    }

    // Method to apply a function to each element in the queue
    void apply(Function func) {
      std::queue<SEXP> newQueue;
      
      while (!q.empty()) {
        SEXP element = q.front();
        q.pop();
        
        // Apply the R function to the element and push the result to the new queue
        newQueue.push(func(element));
      }
      
      q = newQueue;  // Replace the old queue with the modified queue
    }
};

// TODO: if necessary, make QueueWrapper an Rcpp type similar to QueueElement
// // Expose swap as an external function
// //' @useDynLib XDS
// //' @export
// // [[Rcpp::export]]
// void swap_queues(Rcpp::XPtr<QueueWrapper> q1, Rcpp::XPtr<QueueWrapper> q2) {
//   q1->swap(*q2);
// }
// 
// // Expose merge as an external function
// //' @useDynLib XDS
// //' @export
// // [[Rcpp::export]]
// void merge_queues(Rcpp::XPtr<QueueWrapper> q1, Rcpp::XPtr<QueueWrapper> q2) {
//   q1->merge(*q2);
// }
// 
// // Expose equals as an external function
// //' @useDynLib XDS
// //' @export
// // [[Rcpp::export]]
// bool equals_queues(Rcpp::XPtr<QueueWrapper> q1, Rcpp::XPtr<QueueWrapper> q2) {
//   return q1->equals(*q2);
// }


// Expose the QueueWrapper class to R using Rcpp
RCPP_MODULE(QueueModule) {
  class_<QueueWrapper>("QueueWrapper")
    .constructor()
    .method("enqueue", &QueueWrapper::enqueue)
    .method("dequeue", &QueueWrapper::dequeue)
    .method("front",   &QueueWrapper::front)
    .method("empty",   &QueueWrapper::empty)
    .method("size",    &QueueWrapper::size)
    .method("printQueue", &QueueWrapper::printQueue)
    .method("toRVector", &QueueWrapper::toRVector)
    .method("reverse", &QueueWrapper::reverse)
    .method("clear", &QueueWrapper::clear)
    .method("contains", &QueueWrapper::contains)
    .method("sum", &QueueWrapper::sum)
    .method("apply", &QueueWrapper::apply);
}
