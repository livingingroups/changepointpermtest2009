// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::dvec rcpparma_change_point_test_fit(arma::dvec bx, arma::dvec by, int32_t q, int32_t N, double alpha) {
  int32_t nobs = bx.n_elem;
  int32_t goal_no = 1;
  
  arma::dvec bxdiff = arma::diff(bx);
  arma::dvec bydiff = arma::diff(by);
  arma::dvec Rsumperm(N, arma::fill::zeros);    // Store random permutation statistics
  arma::dvec Pr(nobs, arma::fill::ones);        // Store p-values
  arma::dvec sig(nobs, arma::fill::zeros);      // Indicator vector for change points
  
  while (goal_no < nobs - q) {
    int32_t k = 0;
    double P = 1.0;
    Pr.fill(1.0);
    
    // Continue increasing k to find the last significant value
    while ((P > alpha) && (goal_no + q + k < nobs)) {
      k++;
      
      // Calculate distances
      double R1 = std::sqrt(std::pow(bx(goal_no+k-1) - bx(goal_no-1), 2) + 
                            std::pow(by(goal_no+k-1) - by(goal_no-1), 2));
      double R2 = std::sqrt(std::pow(bx(goal_no+k+q-1) - bx(goal_no+k-1), 2) + 
                            std::pow(by(goal_no+k+q-1) - by(goal_no+k-1), 2));
      double Rsum = R1 + R2;
      
      // Observed value of statistic R1 + R2
      Rsumperm(0) = Rsum;
      
      // Calculate statistic R1 + R2 for N-1 random permutations
      for (int32_t it = 1; it < N; it++) {
        // Generate random permutation
        arma::dvec u = arma::randu<arma::dvec>(k+q);
        arma::uvec perm = arma::sort_index(u);
        
        double bxr = bx(goal_no-1);
        double byr = by(goal_no-1);
        
        for (int32_t j = 0; j < k; j++) {
          bxr += bxdiff(goal_no-1+perm(j));
          byr += bydiff(goal_no-1+perm(j));
        }
        
        // Calculate distances for permuted points
        double R1perm = std::sqrt(std::pow(bxr - bx(goal_no-1), 2) + 
                                  std::pow(byr - by(goal_no-1), 2));
        double R2perm = std::sqrt(std::pow(bx(goal_no+k+q-1) - bxr, 2) + 
                                  std::pow(by(goal_no+k+q-1) - byr, 2));
        
        Rsumperm(it) = R1perm + R2perm;
      }
      
      P = arma::sum(Rsumperm >= Rsum) / static_cast<double>(N);
      Pr(k-1) = P;
    }
    
    // f is the first value of k in the current run that is significant
    int32_t f = k;
    
    // Continue increasing k to find the last significant value
    while ((P <= alpha) && (goal_no + q + k < nobs)) {
      k++;
      
      // Calculate distances
      double R1 = std::sqrt(std::pow(bx(goal_no+k-1) - bx(goal_no-1), 2) + 
                            std::pow(by(goal_no+k-1) - by(goal_no-1), 2));
      double R2 = std::sqrt(std::pow(bx(goal_no+k+q-1) - bx(goal_no+k-1), 2) + 
                            std::pow(by(goal_no+k+q-1) - by(goal_no+k-1), 2));
      double Rsum = R1 + R2;
      Rsumperm(0) = Rsum;
      
      // Calculate statistic R1 + R2 for N-1 random permutations
      for (int32_t it = 1; it < N; it++) {
        arma::dvec u = arma::randu<arma::dvec>(k+q);
        arma::uvec perm = arma::sort_index(u);
        
        double bxr = bx(goal_no-1);
        double byr = by(goal_no-1);
        
        for (int32_t j = 0; j < k; j++) {
          bxr += bxdiff(goal_no-1+perm(j));
          byr += bydiff(goal_no-1+perm(j));
        }
        
        double R1perm = std::sqrt(std::pow(bxr - bx(goal_no-1), 2) + 
                                  std::pow(byr - by(goal_no-1), 2));
        double R2perm = std::sqrt(std::pow(bx(goal_no+k+q-1) - bxr, 2) + 
                                  std::pow(by(goal_no+k+q-1) - byr, 2));
        Rsumperm(it) = R1perm + R2perm;
      }
      
      P = arma::sum(Rsumperm >= Rsum) / static_cast<double>(N);
      Pr(k-1) = P;
    }
    
    // l is the last value of k in the current run that is significant
    int32_t l = k - 1;
    
    // Apply "peak rule"
    int32_t rmin = 0;
    if (l >= f) {
      // Extract the p-values for the significant range
      arma::dvec Pr_sub = Pr.subvec(f-1, l-1);
      // Find the index of the minimum p-value
      rmin = arma::index_min(Pr_sub) + 1;
    }
    
    // Update goal_no
    if (rmin > 0) {
      goal_no = goal_no + f + rmin - 1;
      sig(goal_no-1) = 1;  // Mark as change point
    } else {
      goal_no = nobs;  // Exit condition
    }
  }
  return sig;
}