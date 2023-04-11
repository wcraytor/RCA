// CqaMap.hpp

#ifndef CqaMap_hpp
#define CqaMap_hpp

#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


class CqaMap {

 // struct cqaComp : public std::binary_function(const int&,const int&);

 private: std::map<int, double >  cqaMap;

 // Constructor
  public:  CqaMap(const NumericVector&, const NumericVector& ) ;

  double Find(double  ) ;
  ~CqaMap() ;

};




#endif
