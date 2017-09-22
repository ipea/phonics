

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
#define nnnc       *(t + 3)
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
  //string meta = "";
  string::iterator t;
  int c;

  //Inicia constantes utilizadas
  string alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
  string consonants = "BCDFGHJKLMNPQRSTVWXYZ";
  string soft = "AOU";
  string softer = "EI";
  string ptc = "PTC";
  string vowels = "AEIOUY";
  string RS = "RS";
  string empty = "";

  //Faz tratamentos
  string pre_word = x.substr();
  boost::trim(pre_word);
  boost::to_upper(pre_word);

  for(t = pre_word.begin(); t != pre_word.end() && !isalpha(*t); t++);

  //Corre todas as letras e retira itens estranhos
  while(word.length() < maxCodeLen && word.length() < pre_word.length()){
    if(is(alpha, cc)){
      word += cc;
      t += 1;
    } else t += 1;
  }


  for(t = word.begin(); t != word.end() && !isalpha(*t); t++);


  //Inicia sequenca de loops para cada regra
  /* 1 Regra: Tratamento para consoantes
   * Retira letras repetidas, que não forem dígrafos (RR e SS)
   */
  cout << "entrou regra 1 \n";
  for (c = 0; c < word.length(); c++){
    if(nc == cc){
      word.replace(c,1,"");
      t += 1;
    } else t += 1;
  }

  // 2 Regra: Letra S entre vogais (inclusive Y), ou no final de palavras, troca por Z
  t = word.begin();
  cout << "entrou regra 2 \n";
  for (c = 0; c < word.length(); c++){
    //cout << "cc " << cc;
    if( (cc == 'S' && is(vowels,pc) && is(vowels,nc)) || (cc == 'S' && nc == ' ') || (cc == 'S' && !is(alpha,nc)) ){
      word.replace(c,1,"Z");
      t += 1;
    } else t += 1;
  }

  // 3 Regra: letra Y trocda por I
  t = word.begin();
  cout << "entrou regra 3 \n";
  for (c = 0; c < word.length(); c++){
    if( cc == 'Y' ){
      word.replace(c,1,"I");
      t += 1;
    } else t += 1;
  }

  // 4 Regra: Letra N ao final da palavra ou seguida de consoante, troca por M
  t = word.begin();
  for (c = 0; c < word.length(); c++){
    if( (cc == 'N' && nc == ' ') || (cc == 'N' && !is(alpha,nc)) || (cc == 'N' && is(consonants,nc)) ){
      cout << "caiu no if " << *word.end() << "\n";
      word.replace(c,1,"M");
      t += 1;
    } else t += 1;
  }

  /* 5 Regra: Vogal seguida de L na mesma silaba, troca L por U
   * obs: atualmente, todas as vogais seguidas de L são observadas
   * Necessário desenvolver método para identificar sílabas
   */
  cout << "entrou regra 5 \n";
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    //cout << "cc: " << cc << "\n c: " << c << "\n";
    if( is(vowels,cc) && nc == 'L' ){
      word.replace(c+1,1,"U");
      t += 2;
      c += 1;
    } else t += 1;
  }

  // 6 Regra: Dígrafos XS, KS, CS, CZ, KZ e XZ entre vogais substituídos por KIZ
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( ((cc == 'X' || cc == 'K' || cc == 'C') && (nc == 'S' || nc == 'Z')) && (is(vowels,pc)) && (is(vowels,nnc)) ){
      word.replace(c,2,"KIZ");
      t += 3;
      c += 2;
    } else t += 1;
  }

  // 7 Regra: Conjunção OEL, seguida de vogal ou ao final da palavra, troca por UEL
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( (cc=='O' && nc=='E' && nnc=='L' && is(vowels,nnnc)) || (cc=='O' && nc=='E' && nnc=='L' && nnnc==' ') || (cc=='O' && nc=='E' && nnc=='L' && !is(alpha,nnnc)) ){
      word.replace(c,3,"UEL");
      t += 3;
      c += 2;
    } else t += 1;
  }


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
      std::cout << "rodou " << std::endl;
      res[i] = caixa_single(Rcpp::as<std::string>(word[i]), maxCodeLen, true);
    }
  }

  return res;
}


