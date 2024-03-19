#include <Rcpp.h>

using namespace Rcpp;

// Función que calcula la probabilidad p_i
// [[Rcpp::export]]
NumericVector p_i(IntegerVector new_indice,
                  NumericVector pesos_ajustados,
                  int n, int limite, int restante){
  // Vector que guarda los p_i
  //NumericVector p_i_vec(limite - n + 1 );
  Rcpp::IntegerVector new_indice_int = Rcpp::as<Rcpp::IntegerVector>(new_indice);
  NumericVector new_freq = Rcpp::table(new_indice[Rcpp::Range(0,10)]);
  return new_freq;
  // Iteración sobre el rango de indices
  //for (int i = n + 1; i <= limite + n, ++i){
    //IntegerVector new_freq = Rcpp::table(new_indice[Rcpp::range(0,i-2)]);
    
    // Calculo de denominador y numerador
    //double numerador = pesos_ajustados[new_indice[i-1]-1]
  //}
  }
