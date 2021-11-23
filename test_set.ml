module Sets = Set.Make(Int)

let unset = Sets.empty 
let unset = Sets.add 1 unset
let unset = Sets.add 1 unset
let unset = Sets.add 3 unset
let unset = Sets.add 4 unset
let unset = Sets.add 5 unset
let unset = Sets.add 6 unset
let unset = Sets.add 9 unset


(*
  @requires n < cardinal(set)
  @ensures retourne le n ième élément (ordre de comparaison classique) du set passé en deuxième argument
  @raises Failure "empty set" si n > cardinal(set)
  *)
let rec get_nth n set = 
  if Sets.is_empty set then
    raise (Failure "empty set") (* n est alors négatif si on a respecte la convention n < cardinal(set) *)
  else
    let hd = Sets.min_elt set in
    if (n =0) then
      hd
    else
      get_nth (n-1) (Sets.remove hd set)


  (*
    @requires Set non vide
    @ensures retourne un élément pris aléatoirement dans le set
    @raises Failure "empty set" si le set initial est vide
  *)
  let pickRandomFromSet set = 
    let size = Sets.cardinal set in
    if (size = 0 ) then raise (Failure "empty set")
    else
      let chosen = Random.int size in
      get_nth chosen set


  let rec pickNRandomFromSet n set = 
    let size = Sets.cardinal set in
    if (n > size) then raise (Failure "empty set")
    else
      if (n = 0) then Sets.empty
      else
        let r = pickRandomFromSet set in
        Sets.add r (pickNRandomFromSet (n-1) (Sets.remove r set)) 



let printIntSet set = 
  Sets.fold (fun xi -> fun acc -> begin print_int xi ; print_string " ; " end) set ()

let () = Random.init 2 ;;



printIntSet unset ;;
print_int (pickRandomFromSet unset);;

printIntSet (pickNRandomFromSet 3 unset);;

