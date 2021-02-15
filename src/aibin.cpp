#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

// C++ implementation of the Adaptive Inteligent Binning
// See man page of the R wrapper

//' Inner product
//'
//' Calculate the inner product of the bin Value Equation 
//' @keywords internal
double inner_product(NumericVector v, double r){
  double p = pow( ( max(v) - v[0] ) * ( max(v) - v[v.length() - 1] ) , r );
  return p;
}

//' Calculate bin value (Vb)
//'
//' @param bin A matrix containing the whole spectral range.
//'   Cols 0-2  are : bins, bin_start, bin_end, then come samples (col-wise)
//' @param r resolution
//' @param noise noise value
//' @param left,right index of bin_end for the current bin
//'   (C++ index 0 to length-1). 
//' @returns A dbl, the bin value
//' @keywords internal
// [[Rcpp::export]]
double bin_value(NumericMatrix bin, 
                 double r,
                 int left,
                 int right){
  
  // sample wise inner product will be stored in vector
  NumericVector v = (bin.ncol() -3);
  
  for(int i=0; i<bin.ncol() -3 ; ++i){
    NumericMatrix subbin = bin( Range(left, right) , _ );
    v[i] = inner_product( subbin( _ , i ) , r);
  }
  return mean(v);
}

//' Find a new division splitting position for the current bin
//'
//' @param bin A matrix containing the whole spectral range.
//'   Cols 0-2  are : bins, bin_start, bin_end, then come samples (col-wise)
//' @param r resolution
//' @param noise noise value
//' @param left,right index of bin_end for the current bin
//'   (C++ index 0 to length-1). 
//' @returns A dbl, the new optimal splitting position or 
//'   NAN if not splitting is possible
//' @keywords internal
double divide_bin(NumericMatrix bin, 
                  double r, 
                  double noise,
                  int left,
                  int right){
  
  double vb = bin_value(bin, r, left, right);
  
  // Vectors will store iteration results
  NumericVector vb1 = (right - left);
  NumericVector vb2 = (right - left);

  // iterate over all positions except the last
  int numpoints = right - left;
  for (int i=0; i < numpoints; ++i){
    vb1[i] = bin_value(bin, r, left, left+i );
    vb2[i] = bin_value(bin, r, left+i+1, right);
  }

  // Find max vbsum index and convert to whole spectrum index
  NumericVector vbsum = vb1 + vb2;
  int j = which_max(vbsum);

  // Bin Evaluation Criterion, return index or NaN
  if (vbsum[j] > vb && vb1[j] > noise && vb2[j] > noise){
    return j+ left;
  } else {
    return -1 ;
  }
}


//' Reccursively split the bin 
//'
//' @param bin A matrix containing the whole spectral range.
//'   Cols 0-2  are : bins, bin_start, bin_end, then come samples (col-wise)
//' @param r resolution
//' @param noise noise value
//' @param left,right index of bin_end for the current bin
//'   (C++ index 0 to length-1). 
//' @param indices storage of splitting indices, should be initializied with `right`.
//' @returns A vector of splitting indices 
//' @keywords internal
// [[Rcpp::export]]
IntegerVector recc_split(NumericMatrix bin, 
                         double r, 
                         double noise, 
                         int left,
                         int right,
                         IntegerVector indices){
   
  // Division test
  int split = divide_bin(bin, r, noise, left, right);

  // if non divisable append the right most index and return
  if ( split > left ){
    indices.push_back(split);
    
    // find splits in left bin
    IntegerVector indices_new = recc_split(bin, r, noise, left, split, indices);
    
    // move on to the next bin
    IntegerVector next_indices = indices_new[indices_new > split];
    int next =  min(next_indices);
    IntegerVector all_indices = recc_split(bin, r, noise, split, next, indices_new);
    return all_indices;
    
  } else {
    return indices;
  }
}

//' Find the max bin value in the noise region
//' 
//' @param bin A matrix containing the noise region
//'   Cols 0-2  are : bins, bin_start, bin_end, then come samples (col-wise)
//' @param R resolution
//' @returns A dbl, maximum bin value in the noise region
//' @keywords internal
// [[Rcpp::export]]
double vnoise(NumericMatrix bin, double r){
    // Bucket noise region
    IntegerVector init_breaks = IntegerVector::create(bin.nrow());
    IntegerVector breaks = recc_split(bin, r, 0, 0, bin.nrow(), init_breaks);
    IntegerVector breaks_sorted = breaks.sort();
    
    // Get max bin value
    NumericVector vb = (breaks_sorted.length());
    for (int i=1; i<breaks_sorted.length(); ++i){
      vb[i] = bin_value(bin, r, breaks_sorted[i-1], breaks_sorted[i]);
    }
    
    return max(vb);
}

