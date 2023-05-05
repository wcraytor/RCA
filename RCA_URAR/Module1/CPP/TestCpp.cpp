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

// [[Rcpp::export]]
DataFrame modifyDataFrame(DataFrame df) {

  // access the columns
  NumericVector res = df["res"];
  NumericVector cqa = df["cqa"];
  int cn = cqa.size();
  NumericVector cqa2[100];
  std::cout << "3. size: " << cn << '\n';

  for(int i=0;i<=cn;i++) {
      cqa2[i] =  cqa[i]*100;
    }

  NumericVector cqa100(100);
  for(int i = 0;i<=100;i++) {
    cqa100[i] = i;
    }

  // make some changes
  //.sort( );


  // return a new data frame
  return DataFrame::create(_["res"]= res, _["cqa"]= cqa);
}

// [[Rcpp::export]]
List DateExample(DateVector & dv, DatetimeVector & dtv) {

  Function formatDate("format.Date");
  Function formatDatetime("format.POSIXct");

  Rprintf("\nIn C++, seeing the following date value\n");
  for (int i=0; i<dv.size(); i++) {

    //Rcout << as<std::string>(formatDate(wrap(dv[i]))) << std::endl;
    dv[i] = dv[i] + 7;		// shift a week
  }
  Rprintf("\nStep 4\n");
  Rprintf("\nIn C++, seeing the following datetime value\n");
  for (int i=0; i<dtv.size(); i++) {

    //Rcout << as<std::string>(formatDatetime(wrap(dtv[i]))) << std::endl;
    dtv[i] = dtv[i] + 0.250;    // shift 250 millisec
  }

  // Build result set to be returned as a list to R.
  return List::create(Named("date",   dv),
                      Named("datetime", dtv));
}

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}
// [[Rcpp::export]]
RcppExport SEXP makeDataFrame(SEXP inX) {
  Rcpp::DataFrame dfin(inX);
  Rcpp::List myList(dfin.length());
  Rcpp::CharacterVector namevec;
  std::string namestem = "Column Heading ";
  for (int i=0;i<dfin.length();i++) {
    myList[i] = dfin(i); // adding vectors
    namevec.push_back(namestem+std::string(1,(char)(((int)'a') + i))); // making up column names
  }
  myList.attr("names") = namevec;
  Rcpp::DataFrame dfout(myList);
  return dfout;
}

// Spurious example from Rcpp documentation
// [[Rcpp::export]]
std::vector<int> FibCpp0(int n)
{
  // Error checking
  if(n <= 0)
  {
    throw std::range_error("n must be a positive integer");
  }

  // Allocate memory
  std::vector<int> out(n);
  out[0]=1;

  // Compute additional terms
  if(n > 0)
  {
    out[1]=1;
    int i;
    for(i=2; i<n; i++)
    {
      out[i] = out[i-1] + out[i-2];
    }
  }

  return out;
}
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//


//timesTwo(42)

