#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

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
  NumericMatrix subbin = bin( Range(left, right) , _ );
  
  for(int i=0; i<bin.ncol() -3 ; ++i){
    NumericVector sampleval = subbin( _ , i );
    double vbsample = pow( ( max(sampleval) - sampleval[0] ) * ( max(sampleval) - sampleval[sampleval.length() - 1] ) , r );
    v[i] = vbsample;
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
// [[Rcpp::export]]
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

//' Bucket a spectral region. 
//'
//' @param bin A matrix containing the whole spectral range.
//'   Cols 0-2  are : bins, bin_start, bin_end, then come samples (col-wise)
//' @param r resolution
//' @returns A vector of splitting indices 
//' @keywords internal
IntegerVector aibinning_wrapper(NumericMatrix bin, double r, double vnoise){
  IntegerVector init_breaks = IntegerVector::create(bin.nrow()-1);
  IntegerVector breaks = recc_split(bin, r, vnoise, 0, bin.nrow()-1, init_breaks);
  IntegerVector breaks_sorted = breaks.sort();
  return breaks_sorted;
}

//' Adaptive Intelligent Binning Algorithm
//'
//' Entry point from R for the C++ implementation
//'
//' @param spectra, noise A mtrix containing spectra or nosie regions.
//'   Cols 0-2  are : bins, bin_start, bin_end, then come samples (col-wise)
//' @param R resolution (0>R>=1)
//' @returns vector of integers containing the split indices.
//' @keywords internal
// [[Rcpp::export]]
IntegerVector aibin_cpp(NumericMatrix spectra,
                        NumericMatrix noise,
                        double r){
  // Split noise
  IntegerVector noisebreaks = aibinning_wrapper(noise, r, 0);

  // Get max bin value
  NumericVector vb = (noisebreaks.length());
  for (int i=1; i<noisebreaks.length(); ++i){
    vb[i] = bin_value(noise, r, noisebreaks[i-1], noisebreaks[i]);
  }
  double vnoise = max(vb);

  // Bucket spectra
  IntegerVector breaks = aibinning_wrapper(spectra, r, vnoise);
  
  return breaks;
  }
