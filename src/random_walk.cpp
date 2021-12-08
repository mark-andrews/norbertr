#include <Rcpp.h>
using namespace Rcpp;

//' Random walk approximation to a single trajectory in Drift-Diffusion Model
//'
//' Simulates a random walk with step sizes that are the square root of a
//' discrete time step t_eps. As t_eps -> 0, this random walk converges to a
//' drift-diffusion model.
//'
//' @param b Starting point in terms of proportion of distance from 0 to a.
//' @param a Upper barrier (lower is 0)
//' @param v Drift rate
//' @param t_eps Time step size
//'
//' @export
// [[Rcpp::export]]
Rcpp::List random_walk(double b, double a, double v, double t_eps = 1e-4) {

  double delta = sqrt(t_eps);

  double p = 0.5 * (1 + v * delta);

  double x = a * b;

  int tic = 0;

  double r;

  int choice;

  do {

    r = R::runif(0, 1);

    if (r < p) {
      x = x + delta;
    } else {
      x = x - delta;
    };

    tic++;

  } while ( ( x < a) & (x > 0));

  if (x > a){
    choice = 1;
  } else {
    choice = 0;
  }

  return Rcpp::List::create(Rcpp::Named("choice") = choice,
                            Rcpp::Named("time") = tic * t_eps);

}
