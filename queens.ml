let attack (i1,j1) (i2,j2) = 
    (* la reina en la casilla (i1,j1) amenaza la casilla (i2,j2)? *)
    i1 = i2 || 
    j1 = j2 ||
    abs (i2-i1) = abs (j2-j1);;
	
let rec compatible p l = 
    (* una reina en la casilla p estaría a salvo de todas las de la lista l? *)
    not (List.exists (attack p) l);; 
	
let simplify sol = 
    List.map snd (List.sort compare sol);;

(* Para ahorrar espacio, simplify toma una solución al problema expresada como lista de 
   pares (fila, columna) y la transforma en la lista de columnas que corresponden a cada 
   una de las filas de la 1 a la n *)
(* Así, por ejemplo simplify [(4, 3); (3, 1); (2, 4); (1, 2)] = [2; 4; 1; 3], que contiene 
   la misma información de una forma más compacta. Puede hacerse porque hay exactamente 
   una reina en cada fila *)

(*	
(* expand sería una función "recíproca" para simplify; aunque no la necesitamos aquí. *)
(* Así, por ejemplo, expand [2; 4; 1; 3] =  [(1, 2); (2, 4); (3, 1); (4, 3)]  *)
let expand l = List.init (List.length l) (fun i -> (i+1, List.nth l i));;	
(* definición obviamente mejorable *)
*)

(*Se halla unicamente la solucion para primera mitad del tablero
   y posteriormente se gira este para obtener el resto de soluciones.
   Si el tablero es impar, no se hallan las soluciones con i = 2 y j > mitad del tablero
   para evitar obtener duplicados*)
let get_sol n =
    let odd = n mod 2 <> 0 in
    let rec search_all_from path (i,j) =
        if i > n then [simplify path] 
         else if (j > n)||(i=1 && if odd then j>(n/2+1) else j>(n/2))||(odd && i=2 && j>((n/2)+1)&& snd(List.hd path)=(n/2+1)) then [] 
        else if compatible (i,j) path then
            List.rev_append(List.rev(search_all_from path (i,j+1))) (search_all_from ((i,j)::path) (i+1,1))
        else search_all_from path (i,j+1)
    in search_all_from [] (1,1);;  

(*Giro para obtener el resto de soluciones*)
let mirror n l = 
let rec aux i = function 
| [] -> List.rev i
| h::t -> aux ((n+1-h)::i) t
in aux [] l;;

(*Si el tablero es de tamaño 1 no se obtienen duplicados*)
(*Se da el resultado girado seguido del original*)
let all_queens n = 
    if n > 1 then
    let rec aux i = function
    | [] -> i
    | h::t -> aux ([mirror n h] @ [h] @ i) t
    in aux [] (get_sol n)
else get_sol n;; 

