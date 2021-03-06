// Copyright (c) 2015, James P. Howard, II <jh@jameshoward.us>
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
//
//     Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in
//     the documentation and/or other materials provided with the
//     distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/algorithm/string.hpp>

using namespace Rcpp;
using namespace boost;
using namespace std;

#define cc         *i
#define nc         *(i + 1)
#define nnc        *(i + 2)
#define pc          lastChar
#define NULLCHAR    (char)NULL
#define pczin      *(i - 1)

bool is(string x, char c) {
  return (c != NULLCHAR && x.find_first_of(c) != std::string::npos);
}

char at(string x, int i) {

  try {
    return x.at(i);
  } catch(const out_of_range& e) {
    return NULLCHAR;
  }
}

string substr(string x, int i, int n) {

  try {
    return x.substr(i, n);
  } catch(const out_of_range& e) {
    return "";
  }
}


string metaphone_single(string x, int maxCodeLen, bool traditional) {
  string alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  string soft = "EIY";
  string vowels = "AEIOU";

  string::iterator i;
  string word = x.substr(), meta = "";
  char lastChar = NULLCHAR;

  trim(word);
  to_upper(word);

  /*
  * First, we will handle a few special cases.  The Metaphone of the
  * null string is, itself, the null string.  The Metaphone of a
  * single character is itself, capitalized, as appropriate.
  */
  for(i = word.begin(); i != word.end() && !isalpha(*i); i++);
  if(i == word.end())
    return "";
  if(word.length() == 1)
    return(word);

  switch (cc) {
  case 'A':
    meta += nc == 'E' ? nc : cc;
    i += 1;
    break;
  case 'G':
  case 'K':
  case 'P':
    if (nc == 'N') {
      meta += nc;
      i += 2;
    }
    break;
  case 'W':
    if (nc == 'R') {
      meta += nc;
      i += 2;
    } else if (nc == 'H' || is(vowels, nc)) {
      meta += 'W';
      i += 2;
    }
    break;
  case 'X':
    meta += 'S';
    i += 1;
    break;
  case 'E':
  case 'I':
  case 'O':
  case 'U':
    meta += cc;
    i++;
    break;
  }

  while(meta.length() < maxCodeLen && i != word.end()) {
    if(cc != 'C' && pc == cc)
      i++;
    else {
      switch(cc) {
      case 'B':
        if (pc != 'M')
          meta += cc;
        break;
      case 'C':
        if (is(soft, nc)) {
          if (nc == 'I' && nnc == 'A') {
            meta += 'X';
          } else if (pc != 'S') {
            meta += 'S';
          }
        } else if (nc == 'H') {
          meta += !traditional && (nnc == 'R' || pc == 'S') ? 'K' : 'X';
          i++;
        } else {
          meta += 'K';
        }
        break;
      case 'D':
        if (nc == 'G' && is(soft, nnc)) {
          meta += 'J';
          i++;
        } else {
          meta += 'T';
        }
        break;
      case 'G':
        if (nc == 'H') {
          if(!(is("BDH", at(word, distance(word.begin(), i) - 3)) ||
             at(word, distance(word.begin(), i) - 4) == 'H')) {
            meta += 'F';
            i++;
          }
        } else if(nc == 'N') {
          if (is(alpha, nnc) && substr(word, distance(word.begin(), i) + 1, 3) != "NED") {
            meta += 'K';
          }
        } else if(is(soft, nc) && pc != 'G') {
          meta += 'J';
        } else {
          meta += 'K';
        }
        break;
      case 'H':
        if(is(vowels, nc) && !is("CGPST", pc))
          meta += cc;
        break;
      case 'K':
        if (pc != 'C') {
          meta += 'K';
        }
        break;
      case 'P':
        meta += nc == 'H' ? 'F' : cc;
        break;
      case 'Q':
        meta += 'K';
        break;
      case 'S':
        if(nc == 'I' && is("AO", nnc)) {
          meta += 'X';
        } else if(nc == 'H') {
          meta += 'X';
          i += 1;
        } else if(!traditional && substr(word, distance(word.begin(), i) + 1, 3) == "CHW") {
          meta += 'X';
          i += 2;
        } else {
          meta += 'S';
        }
        break;
      case 'T':
        if(nc == 'I' && is("AO", nnc)) {
          meta += 'X';
        } else if(nc == 'H') {
          meta += '0';
          i += 1;
        } else if(substr(word, distance(word.begin(), i) + 1, 2) != "CH") {
          meta += 'T';
        }
        break;
      case 'V':
        meta += 'F';
        break;
      case 'W':
      case 'Y':
        if(is(vowels, nc))
          meta += cc;
        break;
      case 'X':
        meta += "KS";
        break;
      case 'Z':
        meta += 'S';
        break;
      case 'F':
      case 'J':
      case 'L':
      case 'M':
      case 'N':
      case 'R':
        meta += cc;
        break;
      default:
        break;
      }
      pc = cc;
      i++;
    }
  }
  return meta;
}

string metaphone_single_br(string x, int maxCodeLen, bool traditional) {

  //Inicia constantes utilizadas
  string alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
  string consonants = "BCDFGHJKLMNPQRSTVWXYZ";
  string soft = "AOU";
  string softer = "EI";
  string ptc = "PTC";
  string vowels = "AEIOU";
  string RS = "RS";

  //Inicia Iterador, word e faz tratamentos
  string::iterator i;
  string word = x.substr(), meta = "";
  char lastChar = NULLCHAR;
  trim(word);
  to_upper(word);

  for(i = word.begin(); i != word.end() && !isalpha(*i); i++);

  if(i == word.end())
    return "";
  if(word.length() == 1)
    return(word);

  // Loop principal função pt-br
  while(meta.length() < maxCodeLen && i != word.end()){

    //std::cout << cc << nc << pczin << std::endl;

    // Exclui caracteres estranhos
    if(!is(alpha, cc)){
      i += 1;
    }

    //adiciona regra: Retirar letras repetidas que nao sejam SS e RR:
    if(nc == cc && !is(RS,cc) && is(consonants, cc)){
      //std::cout << 'Caiu';
      i += 1;
    }

    //if( is(vowels, cc) ){
    //  cout << "Caiu vogal";
    //}


    // vogais ficam apenas no inicio das palavras
    if(is(vowels, cc)){
      if(i == word.begin() || (pczin == ' ')){
        meta += cc;
        i += 1;
      } else {
        i+=1;
      }
    }

    switch (cc) {
    case ' ':
      meta += ' ';
      i += 1;
      break;
    case 'B':
    case 'D':
    case 'F':
    case 'J':
    case 'K':
    case 'M':
    case 'V':
      meta += cc;
      i+=1;
      break;
    case 'C':
      if(is(soft, nc)){
        meta += 'K';
        i += 1;
      } else {
        if(is(softer,nc)){
          meta += 'S';
          i += 1;
        } else {
          if(nc == 'H' && nnc == 'R'){
            meta += 'K';
            i += 3;
          } else {
            if(nc == 'H'){
              meta += 'X';
              i += 2;
            } else {
              if(!is(alpha, nc) || nc == ' '){
                meta += 'K';
                i += 1;
              } else {
                meta += 'K';
                i += 1;
              }
            }
          }
        }
      }
      break;
    //case 'Ç':
    //  meta += 'S';
    //  i += 1;
    //  break;
    case 'G':
      if(is(soft,nc)){
        meta += 'G';
        i += 1;
      } else {
        if(is(softer,nc)){
          meta += 'J';
          i += 1;
        } else {
          if(nc == 'H' && is(softer, nnc)){
            meta += 'J';
            i += 2;
          } else {
            if(nc == 'H' && is(consonants, nnc)){
              meta += 'G';
              i += 2;
            } else {
              i += 1;
            }
          }
        }
      }
      break;
    case 'H':
      if((i == word.begin() && is(vowels, nc)) || (pczin == ' ' && is(vowels, nc))){
        meta += nc;
        i += 1;
      } else {
        meta += '0';
        i += 1;
      }
      break;
    case 'L':
      if(nc == 'H'){
        meta += '1';
        i += 2;
      } else {
        if(is(vowels, nc)){
          meta += 'L';
          i += 1;
        } else {
          meta += 'L';
          i += 1;
        }
      }
      break;
    case 'N':
      if(!is(alpha, nc) || nc == ' '){
        meta += 'M';
        i += 1;
      } else {
        if(nc == 'H'){
          meta += '3';
          i += 2;
        } else {
          meta += 'N';
          i += 1;
        }
      }
      break;
    case 'P':
      if(nc == 'H'){
        meta += 'F';
        i += 2;
      } else {
        meta += 'P';
        i += 1;
      }
      break;
    case 'Q':
      meta += 'K';
      i += 1;
      break;
    case 'R':
      if(i == word.begin() || pczin == ' '){
        meta += '2';
        i += 1;
      } else {
        if(nc == ' ' || !is(alpha, nc)){
          meta += '2';
          i += 1;
        } else {
          if(nc == 'R'){
            meta += '2';
            i += 2;
          } else {
            if(is(vowels,pczin) && is(vowels,nc)){
              meta += 'R';
              i += 1;
            } else {
              if((i != word.begin() && is(consonants,nc)) || (pczin == ' ' && is(consonants, nc))){
                meta += 'R';
                i += 1;
              } else {
                if(is(consonants, pczin) && is(vowels, nc)){
                  meta += 'R';
                  i += 1;
                } else {
                  meta += 'R';
                  i += 1;
                }
              }
            }
          }
        }
      }
      break;
    case 'S':
      //std::cout << pczin << nc << std::endl;
      //std::cout << "cu";
      if( is(vowels,pczin) && is(vowels,nc) ){
        //std::cout << 'caiu';
        meta += 'Z';
        i += 1;
      } else{
        if(nc == 'S'){
          meta += 'S';
          i += 2;
        } else {
          if(nc == 'H'){
            meta += 'X';
            i += 2;
          } else {
            if(nc == 'C' && is(softer, nnc)){
              meta += 'S';
              i += 2;
            } else {
              if(nc == 'C' && is(soft, nnc)){
                meta += 'S';
                meta += 'C';
                i += 2;
              } else {
                if(nc == 'C' && nnc == 'H'){
                  meta += 'X';
                  i += 3;
                } else {
                  meta += 'S';
                  i += 1;
                }
              }
            }
          }
        }
      }
      break;
    case 'T':
      if(nc == 'H'){
        meta += 'T';
        i += 2;
      } else {
        meta += 'T';
        i += 1;
      }
      break;
    case 'W':
      if(nc == 'L' && is(vowels, nnc)){
        meta += 'V';
        i += 1;
      } else {
        if(nc == 'R' && is(vowels, nnc)){
          meta += 'V';
          i += 1;
        } else {
          if(is(consonants, nc)){
            meta += '0';
            i += 1;
          } else {
            i += 1;
          }
        }
      }
      break;
    case 'X':
      if(!is(alpha, pczin) || pczin == ' '){
        meta += 'X';
        i += 1;
      } else {
        if(pczin == 'E' && is(softer, nc)){
          meta += 'X';
          i += 1;
        } else {
          if(pczin == 'E' && is(soft, nc)){
            meta += 'K';
            meta += 'S';
            i += 1;
          } else {
            if(pczin == 'E' && is(ptc, nc)){
              meta += 'S';
              i += 1;
            } else {
              if(pczin == 'E' && is(alpha, nc) && nc != ' '){
                meta += 'K';
                meta += 'S';
                i += 1;
              } else {
                meta += 'X';
                i += 1;
              }
            }
          }
        }
      }
      break;
    case 'Y':
      if(i == word.begin() || pczin == ' '){
        meta += 'I';
        i += 1;
      } else {
        i += 1;
      }
      break;
    case 'Z':
      if(!is(alpha, nc) || nc == ' '){
        meta += 'S';
        i += 1;
      } else {
        meta += 'Z';
        i += 1;
      }
      break;

    }
  }

  return meta;
}

//' @rdname metaphone
//' @name metaphone
//' @title Generate phonetic versions of strings with Metaphone
//'
//' @description
//' The function \code{metaphone} phonentically encodes the
//' given string using the metaphone algorithm.
//'
//' @param word string or vector of strings to encode
//' @param maxCodeLen  maximum length of the resulting encodings, in characters
//'
//' @details There is some discrepency
//' with respect to how the metaphone algorithm actually works. For
//' instance, there is a version in the Java Apache Commons library.
//' There is a version provided within PHP. These do not provide the same
//' results.  On the questionable theory that the implementation in PHP
//' is probably more well known, this code should match it in output.
//'
//' This implementation is based on a Javascript implementation which is
//' itself based on the PHP internal implementation.
//'
//' The variable \code{maxCodeLen} is the limit on how long the returned
//' metaphone should be.
//'
//' @return a character vector containing the metaphones of \code{word},
//' or an NA if the \code{word} value is NA
//'
//' @family phonics
//'
//' @examples
//' metaphone("wheel")
//' metaphone(c("school", "benji"))
//'
//' @useDynLib phonics
//' @importFrom Rcpp evalCpp
//' @export
//[[Rcpp::export]]
CharacterVector metaphone(CharacterVector word, int maxCodeLen = 10, std::string language = "us") {

  unsigned int input_size = word.size();
  CharacterVector res(input_size);

  for(unsigned int i = 0; i < input_size; i++){
    if((i % 10000) == 0){
      Rcpp::checkUserInterrupt();
    }
    if(word[i] == NA_STRING){
      res[i] = NA_STRING;
    } else {
      if(language == "pt-br"){
        res[i] = metaphone_single_br(Rcpp::as<std::string>(word[i]), maxCodeLen, true);
      } else {
        res[i] = metaphone_single(Rcpp::as<std::string>(word[i]), maxCodeLen, true);
      }
    }

  }

  return res;
}
