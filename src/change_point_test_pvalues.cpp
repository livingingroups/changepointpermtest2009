// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat rcpparma_change_point_test_pvalue(arma::dvec bx, arma::dvec by, int32_t q_max, int32_t N) {
  int32_t goal_no = 0;
  int32_t n_obs = bx.n_elem;
  int32_t last_t = n_obs - q_max - 1;
  int32_t no_of_t = last_t - goal_no + 1;

  arma::dvec bxdiff = arma::diff(bx);
  arma::dvec bydiff = arma::diff(by);
  arma::dvec Rsumperm(N, arma::fill::zeros);       // Store random permutation statistics
  arma::mat P(n_obs, q_max, arma::fill::value(NA_REAL));

  for (int32_t q = 0; q < q_max; q++) { // start of q loop
    int32_t k_max = n_obs - q - 2;
    for (int32_t k = 0; k < k_max; k++) { // start of k loop
      // Calculate distances
      // Rcpp::Rcout << "R1" << std::endl;
      double R1 = std::sqrt(std::pow(bx(goal_no + k + 1) - bx(goal_no), 2) + 
                            std::pow(by(goal_no + k + 1) - by(goal_no), 2));
      // Rcpp::Rcout << "R2" << std::endl;
      double R2 = std::sqrt(std::pow(bx(goal_no + k + q + 2) - bx(goal_no + k + 1), 2) + 
                            std::pow(by(goal_no + k + q + 2) - by(goal_no + k + 1), 2));
      double Rsum = R1 + R2;
      // Rcpp::Rcout << q + 1 << ";" << k + 1 << ";" << R1 << ";" << R2 << std::endl;
      
      // Observed value of statistic R1 + R2
      Rsumperm(0) = Rsum;
      // Calculate statistic R1 + R2 for N-1 random permutations
      for (int32_t it = 1; it < N; it++) { // start of it loop
        // Generate random permutation
        arma::dvec u = arma::randu<arma::dvec>(k + q);
        arma::uvec perm = arma::sort_index(u);
        
        double bxr = bx(goal_no);
        double byr = by(goal_no);
        for (int32_t j = 0; j < k; j++) { // start of j loop
          bxr += bxdiff(goal_no + perm(j));
          byr += bydiff(goal_no + perm(j));
        } // end of j loop
        
        // Calculate distances for permuted points
        // Rcpp::Rcout << "R1perm" << std::endl;
        double R1perm = std::sqrt(std::pow(bxr - bx(goal_no), 2) + 
                                  std::pow(byr - by(goal_no), 2));
        // Rcpp::Rcout << "R2perm" << std::endl;
        double R2perm = std::sqrt(std::pow(bx(goal_no + k + q + 2) - bxr, 2) + 
                                  std::pow(by(goal_no + k + q + 2) - byr, 2));

        // Rcpp::Rcout << R1perm << ";" << R2perm << std::endl;

        Rsumperm(it) = R1perm + R2perm;
      } // end of it loop
      P(k, q) = static_cast<double>(arma::sum(Rsumperm >= Rsum)) / static_cast<double>(N);

    } // end of q loop
    
  } // end of k loop
 
  return P;
}
