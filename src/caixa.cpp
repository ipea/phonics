

// Algoritmo da Caixa

// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/algorithm/string.hpp>

using namespace Rcpp;
using namespace boost;
using namespace std;

#define cc         *i
#define nc         *(i + 1)
#define nnc        *(i + 2)
//#define pc          lastChar
#define NULLCHAR    (char)NULL
#define pc      *(i - 1)


// // Define métodos utilizados
bool is(string x, char c) {
  return (c != NULLCHAR && x.find_first_of(c) != std::string::npos);
}
//
// char at(string x, int i) {
//
//   try {
//     return x.at(i);
//   } catch(const out_of_range& e) {
//     return NULLCHAR;
//   }
// }
//
// string substr(string x, int i, int n) {
//
//   try {
//     return x.substr(i, n);
//   } catch(const out_of_range& e) {
//     return "";
//   }
// }


string caixa_single(string x, int maxCodeLen, bool traditional){

  //Inicia constantes utilizadas
  string alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
  string consonants = "BCDFGHJKLMNPQRSTVWXYZ";
  string soft = "AOU";
  string softer = "EI";
  string ptc = "PTC";
  string vowels = "AEIOUY";
  string RS = "RS";

  //Inicia Iterador, word e faz tratamentos
  string::iterator i;
  string word = x.substr(), meta = "";
  char lastChar = NULLCHAR;
  trim(word);
  to_upper(word);

  std::cout << "word: " << word << std::endl;

  //Corre todas as letras e retira itens estranhos
  while(meta.length() < maxCodeLen && i != word.end()){
    if(is(alpha, cc)){
      i += 1;
      meta += cc;
    } else {
      //cc == "";
      i += 1;
    }
  }

  /* A dinamica aqui é diferente da do metaphone, o desafio aqui é criar
   * uma rotina especifica que cubra todas as regras de forma sequencial.
   * Então cada regra teria um while, para correr todas as letras até a
   * formação da palavra fonetizada final.
   */

  // 1º Regra: Tratamento para consoantes: item 1.28
  //while


  return meta;
}







//' @rdname caixa
//' @name caixa
//' @title Generate phonetic versions of strings with Caixa method
//'
//' @description
//' The function \code{caixa} phonentically encodes the
//' given string using the metaphone algorithm.
//'
//' @param word string or vector of strings to encode
//' @param maxCodeLen  maximum length of the resulting encodings, in characters
//'
//' @details To be inserted
//'
//' @return a character vector containing the caixa of \code{word},
//' or an NA if the \code{word} value is NA
//'
//' @family phonics
//'
//' @examples
//' caixa("wheel")
//' caixa(c("school", "benji"))
//'
//' @useDynLib phonics
//' @importFrom Rcpp evalCpp
//' @export
//[[Rcpp::export]]
CharacterVector caixa(CharacterVector word, int maxCodeLen = 20){

  unsigned int input_size = word.size();
  CharacterVector res(input_size);

  for(unsigned int i = 0; i < input_size; i++){
    if((i % 10000) == 0){
      Rcpp::checkUserInterrupt();
    }
    if(word[i] == NA_STRING){
      res[i] = NA_STRING;
    } else {
      res[i] = caixa_single(Rcpp::as<std::string>(word[i]), maxCodeLen, true);
    }
  }

  return res;
}


