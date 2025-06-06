// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat rcpparma_change_point_test_pvalue(arma::dvec bx, arma::dvec by, int32_t q_max, int32_t N) {
  int32_t goal_no = 0;
  int32_t last_t = bx.n_elem - q_max -1;
  int32_t no_of_t = last_t - goal_no -1;

  arma::dvec bxdiff = arma::diff(bx);
  arma::dvec bydiff = arma::diff(by);
  arma::dvec Rsumperm(N, arma::fill::zeros);    // Store random permutation statistics
  arma::dvec Pr(no_of_t, arma::fill::ones);        // Store p-values
  arma::dvec sig(no_of_t, arma::fill::zeros);      // Indicator vector for change points
  
  arma::mat P(no_of_t, q_max, arma::fill::value(std::exp(2.0)));

  for (int32_t k = 1; k < no_of_t; k++) { // start of k loop
    for (int32_t q = 1; 1 < q_max; q++) { // start of q loop
      // Calculate distances
      double R1 = std::sqrt(std::pow(bx(goal_no+k-1) - bx(goal_no-1), 2) + 
                            std::pow(by(goal_no+k-1) - by(goal_no-1), 2));
      double R2 = std::sqrt(std::pow(bx(goal_no+k+q-1) - bx(goal_no+k-1), 2) + 
                            std::pow(by(goal_no+k+q-1) - by(goal_no+k-1), 2));
      double Rsum = R1 + R2;
      
      // Observed value of statistic R1 + R2
      Rsumperm(0) = Rsum;
      // Calculate statistic R1 + R2 for N-1 random permutations
      for (int32_t it = 1; it < N; it++) { // start of it loop
        // Generate random permutation
        arma::dvec u = arma::randu<arma::dvec>(k+q);
        arma::uvec perm = arma::sort_index(u);
        
        double bxr = bx(goal_no-1);
        double byr = by(goal_no-1);
        
        for (int32_t j = 0; j < k; j++) { // start of j loop
          bxr += bxdiff(goal_no-1+perm(j));
          byr += bydiff(goal_no-1+perm(j));
        } // end of j loop
        
        // Calculate distances for permuted points
        double R1perm = std::sqrt(std::pow(bxr - bx(goal_no-1), 2) + 
                                  std::pow(byr - by(goal_no-1), 2));
        double R2perm = std::sqrt(std::pow(bx(goal_no+k+q-1) - bxr, 2) + 
                                  std::pow(by(goal_no+k+q-1) - byr, 2));
        
        Rsumperm(it) = R1perm + R2perm;
      } // end of it loop
      
      P(k, q) = static_cast<double>(arma::sum(Rsumperm >= Rsum)) / static_cast<double>(N);

    } // end of q loop
    
  } // end of k loop
  
  return P;
}