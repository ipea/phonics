

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

string substr(std::string x, int i, int n) {

  try {
    return x.substr(i, n);
  } catch(const out_of_range& e) {
    return "";
  }
}


string caixa_single(string x, int maxCodeLen, bool traditional){

  //Inicia indicadores de posição e variaveis
  string word = "";
  string::iterator t;
  string temp = "";
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

  //for(t = pre_word.begin(); t != pre_word.end() && !isalpha(*t); t++);

  //Corre todas as letras e retira itens estranhos
  t = pre_word.begin();
  //cout << "t: " << *t << "\n";
  while(word.length() < maxCodeLen && word.length() < pre_word.length()){
    if(is(alpha, cc)){
      word += cc;
      t += 1;
    } else t += 1;
  }


  // cout << "t do for: " << *t << "\n";
  // for(t = word.begin(); t != word.end() && !isalpha(*t); t++);
  // cout << "t do for: " << *t << "\n";

  //Inicia sequenca de loops para cada regra
  /* 1 Regra: Tratamento para consoantes
   * Retira letras repetidas, que não forem dígrafos (RR e SS)
   */
  //cout << word.length() << "\n";
  t = word.begin();
  //cout << "t: " << *t << "\n";
  for (c = 0; c < word.length(); c++){
    //cout << "c: " << c << "\n";
    if(nc == cc){
      word.replace(c,1,"");
      t += 1;
    } else t += 1;
  }

  // 2 Regra: Letra S entre vogais (inclusive Y), ou no final de palavras, troca por Z
  t = word.begin();
  for (c = 0; c < word.length(); c++){
    //cout << "cc " << cc;
    if( (cc == 'S' && is(vowels,pc) && is(vowels,nc)) || (cc == 'S' && nc == ' ') || (cc == 'S' && !is(alpha,nc)) ){
      word.replace(c,1,"Z");
      t += 1;
    } else t += 1;
  }

  // 3 Regra: letra Y trocda por I
  t = word.begin();
  for (c = 0; c < word.length(); c++){
    if( cc == 'Y' ){
      word.replace(c,1,"I");
      t += 1;
    } else t += 1;
  }

  // 4 Regra: Letra N ao final da palavra ou seguida de consoante (tirando o "H"), troca por M
  t = word.begin();
  for (c = 0; c < word.length(); c++){
    if( ((cc == 'N' && nc == ' ') || (cc == 'N' && !is(alpha,nc)) || (cc == 'N' && is(consonants,nc))) && nc != 'H'){
      word.replace(c,1,"M");
      t += 1;
    } else t += 1;
  }

  /* 5 Regra: Vogal seguida de L na mesma silaba, troca L por U
   * obs: atualmente, todas as vogais seguidas de L são observadas
   * Necessário desenvolver método para identificar sílabas.
   * Palavras como Seilandia, será traduzida erroneamente.
   * Para que a Regra 13 funcione, vou adicionar uma regra de nnc != H
   */
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( is(vowels,cc) && nc == 'L' && nnc != 'H'){
      word.replace(c+1,1,"U");
      t += 2;
      c += 1;
    } else t += 1;
  }

  // 6 Regra: Dígrafos XS, KS, CS, CZ, KZ e XZ entre vogais substituídos por KIZ
  /*
   * ERRO: "nobertoxsa" funciona, "nobertoxsa nobertoxsa" funciona,
   * mas "nobertoxsa nobertoxsa nobertoxsa" NÃO funciona.
   */
  t = word.begin();
  for(c = 0; c < word.length();c++){
    if( cc == 'X' || cc == 'K' || cc == 'C' ){
      if(nc == 'S' || nc == 'Z'){
        if(is(vowels,pc) && is(vowels,nnc)){
          word.replace(c,2,"KIZ");
          t += 3;
          c += 2;
        }
      }
    } else{
      t += 1;
    }
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

  // 8 Regra: Dígrafo NH, seguido de vogal, troca por NI
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if(cc == 'N' && nc == 'H' && is(vowels,nnc)){
      word.replace(c,2,"NI");
      t += 2;
      c += 1;
    } else{
      t += 1;
      //c += 1;
    }
  }

  // 9 Regra: Junção SCH seguido de vogal ou final da palavra, troca por X
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( (cc == 'S' && nc == 'C' && nnc == 'H' && is(vowels,nnnc)) || (cc == 'S' && nc == 'C' && nnc == 'H' && !is(alpha,nnnc)) ){
      word.replace(c,3,"X");
      t += 3;
      c += 2;
    } else {
      t += 1;
    }
  }

  // 10 Regra: Junção SC, seguido de E ou I no meio da palavra, troca por S
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'S' && nc == 'C' && is("EI",nnc) ){
      word.replace(c,2,"S");
      t += 2;
      c += 1;
    } else {
      t += 1;
    }
  }

  // 11 Regra: Junção ICT seguida de vogal, troca por IT
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'I' && nc == 'C' && nnc == 'T' && is(vowels,nnnc) ){
      word.replace(c,3,"IT");
      t += 3;
      c += 2;
    } else{
      t += 1;
    }
  }

  // 12 Regra: Junção CH no final de palavra, troca por K, no meio da palavra, troca por X
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if ( cc == 'C' && nc == 'H' ){
      if ( nnc == ' ' || !is(alpha,nnc) ){
        word.replace(c,2,"K");
        t += 2;
        c += 1;
      } else {
        word.replace(c,2,"X");
        t += 2;
        c += 1;
      }
    } else{
      t += 1;
    }
  }

  // 13 Regra: LH seguido de vogal, troca por LI
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'L' && nc == 'H' && is(vowels,nnc) ){
      word.replace(c,2,"LI");
      t += 2;
      c += 1;
    } else{
      t += 1;
    }
  }

  // 14 Regra: DTH troca por DTI
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'D' && nc == 'T' && nnc == 'H' ){
      word.replace(c,3,"DTI");
      t += 3;
      c += 2;
    } else {
      t += 1;
    }
  }

  // 15 Regra: TH no final de palavra, troca por TI, se seguido por vogal, troca por T
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'T' && nc == 'H' ){
      if( nnc == ' ' || !is(alpha,nnc) ){
        word.replace(c,2,"TI");
        t += 2;
        c += 1;
      } else{
        if( is(vowels,nnc) ){
          word.replace(c,2,"T");
          t += 2;
          c += 1;
        }
      }
    } else{
      t += 1;
    }
  }

  // 16 Regra: Letra G seguida de E ou I, troca por J
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'G' && is("EI",nc) ){
      word.replace(c,1,"J");
      t += 2;
      c += 1;
    } else{
      t += 1;
    }
  }

  // 17 Regra: GU se seguido por E ou I, troca por G
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'G' && nc == 'U' && is("EI",nnc) ){
      word.replace(c,2,"G");
      t += 2;
      c += 1;
    } else {
      t += 1;
    }
  }

  // 18 Regra: QU seguido de A ou O, troca por KU, se seguido de E ou I, troca por K
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'Q' && nc == 'U' ){
      if( nnc == 'A' || nnc == 'O' ){
        word.replace(c,2,"KU");
        t += 2;
        c += 1;
      } else{
        if( nnc == 'E' || nnc == 'I' ){
          word.replace(c,2,"K");
          t += 2;
          c += 1;
        }
      }
    } else {
      t += 1;
    }
  }

  // 19 Regra: C seguido de qlqr letra que nao E ou I, troca por K, se seguido de E ou I, troca por S
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'C' ){
      if( is("EI",nc) ){
        word.replace(c,1,"S");
        t += 1;
      } else {
        word.replace(c,1,"K");
        t += 1;
      }
    } else {
      t += 1;
    }
  }

  // 20 Regra: A letra H, caso nao fizer parte dos digrafos NH, CH ou LH, será suprimida
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'H' && pc != 'N' && pc != 'C' && pc != 'L' ){
      word.replace(c,1,"");
      t += 1;
    } else{
      t += 1;
    }
  }

  /* 21 Regra: Letras D, F, P, B e T seguidas de consoantes ou final das palavras, mantem e acresce o I
   * Vai uma consideração: A palavra EMPRESA cai nessa regra (EMPIREZA) mas nao me parece correto...
   */
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( (is("DFPBT",cc) && is(consonants,nc)) || (is("DFPBT",cc) && (nc == ' ' || !is(alpha,nc))) ){
      temp = word[c];
      temp.append("I");
      word.replace(c,1,temp);
      t += 2;
      c += 1;
    } else {
      t += 1;
    }
  }

  // 22 Regra: X ao final das palavras, troca por KIZ, se depois de uma vogal, NA MESMA SILABA, TROCA POR Z
  /* Mesmo problema da Regra 5.
   * Necessário identificar silabas!
   */
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'X' ){
      if( nc == ' ' || !is(alpha,nc) ){
        word.replace(c,1,"KIZ");
        t += 1;
      }
      // else{
      //   if( is(vowels,pc) ){
      //     word.replace(c,1,"Z");
      //     t += 1;
      //   }
      // }
    } else{
      t += 1;
    }
  }

  /* 23 Regra: ES no inicio ou meio da palavra NA MESMA SILABA, e seguida de uma consoante, troca por IS
   * Se seguido de vogal ou branco, troca por IZ (no entanto, a Regra 2 diz que S entre vogais ou no final de palavras,
   * troca por Z, então nao acho que essa regra vai cair alguma vez.)
   */
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'E' && nc == 'S' ){
      if( is(vowels,nnc) || nnc == ' ' || !is(alpha,nnc) ){
        word.replace(c,2,"IZ");
        t += 2;
        c += 1;
      }
      // else{
      //   if( !is(alpha,pc) || pc == ' ' ){
      //     //adicionar regra para mesma silaba
      //     word.replace(c,2,"IS");
      //     t += 2;
      //     c += 1;
      //   }
      // }
    } else{
      t += 1;
    }
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
      res[i] = caixa_single(Rcpp::as<std::string>(word[i]), maxCodeLen, true);
    }
  }

  return res;
}


