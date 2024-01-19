(* Autores: Ruben Ferraz - a45590
            Ruben Serrano - a46325 *) 
                  

(*Função que lê um par de números inteiros, separados por um espaço, introduzidos pelo utilizador*)            
let par_inteiros() =
  Scanf.scanf" %d %d" (fun x y -> (x, y))

 (*Função que cálcula o lucro máximo dado o tamanho do bolo "n", o número de tamanho de fatias "m" e uma lista de preços*) 
let lucro_max n m preco =
  (*Função que cria um Array com (n+1) elementos e inicializa em 0*)
  let profit = Array.make (n+1) 0 in  
  (*Para cada par (i,j) na lista dos preços, atualiza o Array de lucros com o lucro máximo possível*)
  List.iter (fun(i, j) ->
  (*Ciclo que percorre a lista de preços e calcula o lucro máximo*)
    for tam = i to n do
      profit.(tam) <- max profit.(tam) (j + profit.(tam - i))
    done) preco;
    profit.(n)

let() =
  (*Função que lê o valor do tamanho do bolo e o número de fatias*)
    let (n, m) = par_inteiros() in
  (*Função que inicializa a lista de preços com os valorez lidos*)
    let preco = List.init m (fun _ -> par_inteiros()) in
  (*Função que calcula o resultado*)
    let resultado = lucro_max n m preco in
  (*Imprime o lucro máximo*)
    Printf.printf "%d\n" resultado



(*Exemplo de Entrada:

   8
   7 
   1 2
   2 4
   3 8
   5 12
   6 17
   7 17
   8 20
   
   Exemplo de Saída:
   
   21
   
   *)