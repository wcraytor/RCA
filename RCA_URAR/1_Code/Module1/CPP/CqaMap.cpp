///////////////////////////////////////////////////////////////////////////////////////////////
// Author:       Wm. Bert Craytor
// Location:     242 Clifton Rd., Pacifica, CA 94044, USA
// Date:         07/02/2021
// Description:  Rcpp/C++ Class to map CQA scores to residuals
//
// Notes:        1.  This program is free software; you can redistribute it and/or modify
//                   it under the terms of the GNU General Public License as published by
//                   the Free Software Foundation; either version 2 of the License, or
//                   (at your option) any later version.
//
//               2.  This program is distributed in the hope that it will be useful,
//                   but WITHOUT ANY WARRANTY; without even the implied warranty of
//                   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//                   GNU General Public License for more details.
//
//               3.  A copy of the GNU General Public License is available at
//                   http://www.r-project.org/Licenses
//
///////////////////////////////////////////////////////////////////////////////////////////////
#include <Rcpp.h>
 

#include <iostream>
#include <vector>
#include <map>
#include <cmath>
#include <string>
#include <iterator>
#include <algorithm>
using namespace Rcpp;


#define TRACE_MODE 0
#define TEST_MODE 0

// CqaMap:  Create a map of CQA vs Residual values, to be used to assign a residual value to the
//          Subject property based on an estimate for it's CQA value.
// Note:    Internally the CQA values 0.0-10.0 are converted to integers 0-100 for easier access

class CqaMap {

    //  Note that the 0.0-10.0 CQA values are multiplied by 10 and converted to
    //  integers in the range of 0-100.  Also, if the input has multiple CQA values (when rounded)
    //  their scores are average to get the residual for that particular integer CQA value.
    //  Furthermore, if some integers are skipped due to having fewer than 100 input values, those
    //  values are linearly interpolated between the two surrounding input values.
    private: std::map<int, double >  cqaMap;

    // Constructor
    // vectors cqa and residual have the same length and map cqa scores which are doubles
    // from 0.0 to 10.0 to residuals produced by the R::earth() analysis.
    // Both vectors are assumed to be sorted from least value to greatest.
    // Plotting  CQA vs Residual should produce the characteristic curve for property
    // residuals (an elongated S reversed and rotated 90 degrees clockwise, constrained by monotonicity.)
    public:  CqaMap(const NumericVector& cqa,const NumericVector& residual) {

      // Check for various input conditions:  TODO - these conditions are not yet fully tested:
      // 1.  cqa and residual are the same length
      // 2.  both vectors monotonic, i.e strictly increasing.
      // 3.  cqa values are double between 0.0 and 10.0 inclusive
      // 4.  Residuals are not constrained in value , but assumed reasonably large
      // 5.  There should be at least about 5 input values

      
      double currResidual = (double) (round(residual[0]));
      double lastResidual = 0;
     
      
      for(int j=0;j<= 1000; j++) {
        cqaMap.insert(std::pair<int, double>(j,0.00));
      }
     
     int cqaSz = cqa.size()-2;
     int lastIdx = 0;
     lastResidual = currResidual;
     cqaMap[0] = currResidual;
     
     for (int i =0;i < (int) cqaSz; i++) {
       int idx = (int) (std::round(cqa[i] * 100));
       currResidual = (double)(round(residual[i]));
       cqaMap[idx] = currResidual;
       
       int idxDiff = idx - lastIdx  ;
       
       if(idxDiff > 1)
       {
         int residualInc = (currResidual - lastResidual)/idxDiff;
         
         for(int g=1;g< idxDiff;g++) {
           cqaMap[idx - idxDiff +g] = lastResidual + residualInc;
         }
        
         lastResidual = currResidual;
       }
       lastIdx = idx;
       
      
     }
     
     for(int m=lastIdx;m<=1000;m++) {
       cqaMap[m] = lastResidual;
     }
       
     
#ifdef TRACE_MODE
      //std::for_each(cqaMap.begin(),cqaMap.end(),[](std::pair<int, double> element)  {
        //std::cout <<  " CQA: " << element.first << " Residual: " << element.second << std::endl;
      //});

      //auto x1 = cqaMap.find(55)->second;
      //auto x2 = cqaMap.find(66)->second;
      //std::cout << "x1: " << x1 << " x2: " << x2 << std::endl;
#endif
    }; // end of constructor

    // Finds the residual for a CQA value.
    double Find(double cqa) {
      if(cqa < 0.0001) {
        cqa=0.0;
      } else if(cqa > 9.9999) {
        cqa= 10.0;
      }
      
      int fCqa = (int)(round(cqa * 100));
      return (double)cqaMap.find(fCqa)->second;
    };

    ~CqaMap() {   }
  };
  
 
// Here is our Rcpp export:

RCPP_EXPOSED_CLASS(CqaMap)

// Class export:
RCPP_MODULE(CQA_MAP) {
  using namespace Rcpp;
    class_<CqaMap>("CqaMap")
    .constructor<const NumericVector&,const NumericVector&>()
    .method("Find",   &CqaMap::Find);
  }



  // Spurious example from Rcpp docs
  //[[Rcpp::export]]
  NumericVector convolveCpp(const NumericVector& a,
                            const NumericVector& b) {
    int na = a.size(), nb = b.size();
    int nab = na + nb - 1;
    NumericVector xab(nab);
    for (int i = 0; i < na; i++)
      for (int j = 0; j < nb; j++)
        xab[i + j] += a[i] * b[j];
    return xab;
  }
