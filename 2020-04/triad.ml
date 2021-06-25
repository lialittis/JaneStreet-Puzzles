(* The idea is learned from Willem Hoek *)

module type Triangular = sig
  val n_of_triads : int
  val n_of_dots : int
  val n_of_lines : int
  type is_seperated = Yes | No | Unknown
  val set_flag : unit -> is_seperated
end

(* if you want to use an integer to create this module, you have to use this way *)
let mk_Triangular (n:int) =
  (module struct
    type is_seperated = Yes | No | Unknown
    let n_of_lines = n
    let n_of_dots = n * (n + 1) / 2
    let n_of_triads = 0
    let set_flag () = if (n_of_dots mod 3 = 0) then Unknown else No
  end:Triangular)

let tri = mk_Triangular 3
(* A first class module *)
(* Its variables and methods can only be used inside of a function *)

open Printf

let print (module T:Triangular) =
  let nol = T.n_of_lines in
  printf "number of lines: %i\n" nol;
  let nod = T.n_of_dots in
  printf "number of dots: %i\n" nod;
  let flag = T.set_flag () in
  let res = 
  match flag with
  | T.Yes -> "yes"
  | T.No -> "no"
  | T.Unknown -> "unknown"
  in
  printf "If is could be seperated by triads : %s\n" res


(* test *)

(*
let test () = print (mk_Triangular 8)

;;
test ()
*)


(* Backtracking step to test if the triangular could be seperated by triads *)

(* A concern was that the run-time might be very long for high values of N. 
   Worse case the run-time is polynomial - O(n^k), with k somewhere between 
   2-4. However with the known constraints such as the the sides and corners,
   it might not be so bad.
*)


(* 
   Grid Coordinates

   |  1  2  3  4
  -+-----------------> x-value
   |
1  !  0  1  1  1           Every 0 is a dot in the triangular grid
   |                       Sample here is where N=4
2  |  0  0  1  1
   |                       If grid value is:
3  |  0  0  0  1           0 -> empty, available to place a triad
   |                       1 -> occupied / unavailable
4  |  0  0  0  0  
   | 
y-value

     Up         Down

      X         X 0          
     0 0         0     

*)


