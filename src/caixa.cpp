

// Algoritmo da Caixa

// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <boost/algorithm/string.hpp>
//#include <boost/algorithm/string/replace.hpp>
//#include <iostream>

using namespace Rcpp;
using namespace boost;
using namespace std;

#define cc         *t
#define nc         *(t + 1)
#define nnc        *(t + 2)
//#define pc          lastChar
#define NULLCHAR    (char)NULL
#define pc         *(t - 1)


// Define métodos utilizados
bool is(std::string x, char c) {
  return (c != NULLCHAR && x.find_first_of(c) != std::string::npos);
}

// char at(std::string x, int i) {
//
//   try {
//     return x.at(i);
//   } catch(const std::out_of_range& e) {
//     return NULLCHAR;
//   }
// }

string att(std::string x, int *t, char n){
  x[*t] = n;
  return x;
}

string substr(std::string x, int i, int n) {

  try {
    return x.substr(i, n);
  } catch(const out_of_range& e) {
    return "";
  }
}


string caixa_single(string x, int maxCodeLen, bool traditional){

  string att(std::string x, int *t, char n);
  //Inicia indicadores de posição e variaveis
  // char cc;
  // char nc;
  // char nnc;
  // char pc;
  string word = "";
  string meta = "";
  string::iterator t;

  //Inicia constantes utilizadas
  string alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
  string consonants = "BCDFGHJKLMNPQRSTVWXYZ";
  string soft = "AOU";
  string softer = "EI";
  string ptc = "PTC";
  string vowels = "AEIOUY";
  string RS = "RS";

  //Faz tratamentos
  int i = 0;
  string pre_word = x.substr();
  boost::trim(pre_word);
  boost::to_upper(pre_word);

  for(t = pre_word.begin(); t != pre_word.end() && !isalpha(*t); t++);

  //Corre todas as letras e retira itens estranhos
  while(word.length() < maxCodeLen && word.length() < pre_word.length()){
    //cc = pre_word[i];
    if(is(alpha, cc)){
      word += cc;
      t += 1;
    } else {
      t += 1;
    }
  }

  for(t = word.begin(); t != word.end() && !isalpha(*t); t++);

  //std:cout << word.end();
  //Inicia sequenca de loops para cada regra
  /* 1 Regra: Tratamento para consoantes
   * Retira letras repetidas, que não forem dígrafos (RR e SS)
   */
  while(t != word.end()){
    //cc = word[i]; nc = word[i+1];
    //cout << "entrou " << cc << " ";
    cout << "t antes do if: " << *t;
    //cout << "\n cc: " << cc;
    //Cout << "\n nc: " << nc;
    if(nc == cc && cc != 'S' && cc != 'R'){

      //word[*t] = 'X';
      t += 1;
      cout << "\n caiu primeiro ";
    } else {
      cout << "\n caiu segundo ";
      //cout << "t: " << *t;
      //word = att(word, *t, "S");
      //cout << *t;
      //word.at(2*t) = "";
      t += 1;
    }
  }

  // // 2 Regra: Letra S entre vogais (inclusive Y), troca por Z
  // while(i != word.end()){
  //   if( cc == 'S' && is(vowels,pc) && is(vowels,nc) ){
  //     std::cout << "caiu S" << std::endl;
  //     //meta += 'Z';
  //     //at(meta,i) == 'Z';
  //     //meta[i] == 'Z';
  //     //std::cuot << meta.at(i);
  //     //char at(string x, int i)
  //     //at(meta,i) == 'Z';
  //     //meta.replace(cc,'Z');
  //     //word.at(i);
  //     meta.replace(i,1,"Z");
  //
  //     i += 1;
  //   } else {
  //     i += 1;
  //   }
  // }

  // /Users/Igor/OneDrive/Documents/IPEA/git/phonics/src/caixa.cpp
  /* A dinamica aqui é diferente da do metaphone, o desafio aqui é criar
   * uma rotina especifica que cubra todas as regras de forma sequencial.
   * Então cada regra teria um while, para correr todas as letras até a
   * formação da palavra fonetizada final.
   */

  // 1º Regra: Tratamento para consoantes: item 1.28
  //while


  return word;
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


