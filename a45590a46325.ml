(* Autores: Ruben Ferraz - a45590
            Ruben Serrano - a46325 *)

(* Fontes: sobre apontadores e referências - http://di.ubi.pt/~desousa/PF/aula_pf2-pp.pdf
           sobre recurividade - http://di.ubi.pt/~desousa/PF/aula_pf3-pp.pdf
           sobre o número de Motzkin - https://en.wikipedia.org/wiki/Motzkin_number *)

open Z
open Printf

exception Input_Incorreto

let rec motzkinFunc n =
  (* Verificação do input 'n'. Caso seja menor que 0 ou maior que 10000 gera uma exceção 'Input_Incorreto' *)
  if n < 0 || n > 10000 then raise Input_Incorreto
  else
    (*A variável 'seqValues' cria uma referência para um tuplo de dois valores, sendo esses os primeiros valores da sequência de Motzkin.
      É através do segundo valor que é calculado o próximo valor da sequência *)
    let seqValues = ref (Z.of_int 0, Z.of_int 1) in
    for i = 0 to n do
      (*As variáveis 'first' e 'last' extraem os valores referenciados no tuplo 'seqValues'.*)
      let first, last = !seqValues in
      (* (2*i + 1) * first  *)
      let left = ((Z.of_int 2 * Z.of_int i) + Z.of_int 1) * first in
      (* (3*i - 3) * last *)
      let right = ((Z.of_int 3 * Z.of_int i) - Z.of_int 3) * last in
      (* (2*i + 1) * first + (3*i - 3) * last *)
      let numerator = left + right in
      (* ((2*i + 1) * first + (3*i - 3) * last) / (i+2) *)
      let denominator = Z.of_int i + Z.of_int 2 in
      (*Atualização da variável 'seqValues'.*)
      seqValues := (numerator / denominator, first)
    done;
    (*As variáveis 'motzkin1' e 'motzkin2' extraem os valores referenciados no novo tuplo 'seqValues'.*)
    let motzkin1, motzkin2 = !seqValues in
    printf "%s\n" (Z.to_string (Z.abs motzkin1))

let () =
  (* Função que lê o standard input e armazena-o na variável 'aValue'.*)
  let aValue = read_line () in
  (* Conversão da string 'aValue' para inteiro.*)
  let n = int_of_string aValue in
  (* Chamada da função 'motzkinFunc' com o inteiro convertido anteriormente, 'n'.*)
  motzkinFunc n

(* Exemplo de execução:

   Entrada: 5

   Saída: 21
*)


(*------------------------------------------------------------------------------------------------------------------------------------------*)
(*versão corrigida recursiva*)

open Z
open Printf

exception Input_Incorreto

let rec motzkinFunc n =
  if n < 0 || n > 10000 then raise Input_Incorreto
  else if n = 0 then Z.of_int 1
  else if n = 1 then Z.of_int 1
  else if n = 2 then Z.of_int 2
  else
    let rec loop i first last =
      if i > n then first
      else
        let left = ((Z.of_int 2 * Z.of_int i) + Z.of_int 1) * first in
        let right = ((Z.of_int 3 * Z.of_int i) - Z.of_int 3) * last in
        let numerator = left + right in
        let denominator = Z.of_int i + Z.of_int 2 in
        let next_value = numerator / denominator in
        loop (Z.to_int (Z.of_int i + Z.of_int 1)) next_value first
    in
    loop 3 (Z.of_int 2) (Z.of_int 1)

let () =
  let aValue = read_line () in
  let n = int_of_string aValue in
  printf "%s\n" (Z.to_string (motzkinFunc n))
