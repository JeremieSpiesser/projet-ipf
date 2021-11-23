module type Noeud = sig
module Set_int :
  sig
    type elt = Int.t
    type t = Stdlib__set.Make(Int).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
  type t = {
    x : float;
    y : float;
    id : int;
    voisins : Set_int.t;
    relai : bool;
  }
  val createNoeud : float -> float -> int -> 'a -> t
  val addVoisin : t -> Set_int.elt -> t
  val removeVoisin : t -> Set_int.elt -> t
  val memVoisin : t -> Set_int.elt -> bool
  val changeCords : float -> float -> t -> t
  val moveAleatNoeud : t -> t
  end





module Graphe_euclidien(X:Noeud) = struct
  module Map_int = Map.Make(Int) 
  type t = {
    poids: float; (* Poids du graphe = somme des poids des arrêtes du graphe*)
    a: X.t Map_int.t; (* map int -> noeud_euclidien contenant tous les points *)
    distance: X.t -> X.t -> float; (* fonction qui calcule la distance entre 2 noeuds *)
    relais : X.Set_int.t;(* Ensemble des indices des points relais du graphe *)
    depart: X.Set_int.t;(* Ensemble des indices des points de départ du graphe*)
    compteur : int ; (* Entier qui se doit d'être >0 et d'être supérieur à l'id maximal présent dans a, relais et depart*)
  }

let empty_graph = {
  poids = 0.0;
  a = Map_int.empty;
  distance = (fun pA -> fun pB -> let part1 = (pA.x -. pB.x) in 
                                  let part2 = (pA.y -. pB.y) in 
                                  Float.sqrt ( part1 *. part1 +. part2 *. part2));
  relais = X.Set_int.empty;
  depart = X.Set_int.empty;
  compteur=0
}


(**
  @requires rien
  @ensures retourne le graphe qui a le poids minimum
  @raises rien
*)
let bestGraphe g1 g2 = 
  if (g1.poids > g2.poids) then g2
  else g1

(**
  @requires rien
  @ensures retourne un nouveau graphe avec un nouveau noeud créé selon les paramètres (sans voisins), ainsi l'id du noeud ajouté
  @raises rien
*)  
let addNoeud x y relai graphe = 
  let newId = ((graphe.compteur)+1) in
  let newNoeud = X.createNoeud x y newId relai in
  if relai then (newId,{graphe with a = Map_int.add  (newId)  newNoeud graphe.a ; relais = X.Set_int.add newId graphe.relais ; compteur = graphe.compteur +1 })
  else (newId, {graphe with a = Map_int.add  (newId)  newNoeud graphe.a ; depart = X.Set_int.add newId graphe.depart; compteur = graphe.compteur +1 })


(**
  @requires un graphe qui contienne déjà les points pointA et pointB à relier (les 2 indices)
  @ensures envoie un nouveau graphe avec a et b reliés
  @raises Not_found si les points d'indices idA et idB ne sont pas dans le graphe
*)
let addLink idA idB graphe = 
  let pA = Map_int.find idA graphe.a in
  let pA = X.addVoisin pA idB in
  let pB = Map_int.find idB graphe.a in
  let pB = X.addVoisin pB idA in
  let dab = graphe.distance pA pB in
  let annuaire = Map_int.update idA (fun  _ -> Some pA) graphe.a in
  {
    graphe with a = Map_int.update idB (fun _ -> Some pB) annuaire ; 
                poids = graphe.poids +. (dab)
  }

  (**
  @requires un graphe qui contient déjà les points pointA et pointB à dé-relier (les 2 indices)
  @ensures envoie un nouveau graphe avec a et b non reliés
  @raises Not_found si les points d'indices idA et idB ne sont pas dans le graphe
*)

let removeLink idA idB graphe = 
  let pA = Map_int.find idA graphe.a in
  let pA = X.removeVoisin pA idB in
  let pB = Map_int.find idB graphe.a in
  let pB = X.removeVoisin pB idA in
  let dab = graphe.distance pA pB in
  let annuaire = Map_int.update idA (fun  _ -> Some pA) graphe.a in
  {
    graphe with a = Map_int.update idB (fun _ -> Some pB) annuaire ; 
                poids = graphe.poids -. (dab)
  }



(**
  @requires un graphe qui contienne bien un noeud d'indice id
  @ensures retourne un nouveau graphe, ou le noeud d'indice id a été supprimé, ainsi que
           toutes ses éventuelles connexions a ses voisins
  @raises Not_found si id n'est pas dans le graphe
*)
let removeNoeud id graphe = 
  let a = graphe.a in
  let p = Map_int.find id a in
  let voisins = p.voisins in
  let g  = X.Set_int.fold (fun i -> fun g -> removeLink id i graphe) voisins graphe in
  if p.relai then   { g with a = Map_int.remove id graphe.a ; relais = X.Set_int.remove id graphe.relais}
  else { g with a = Map_int.remove id graphe.a ; depart = X.Set_int.remove id graphe.depart}
    
  

(**
  @requires le noeud n'a pas besoin d'être dans le graphe (seule sont lues ses coordonnées et ses voisins qui eux doivent être dans le graphe)
  @ensures retourne le poids local (ie la somme des distances de ce point a ses voisins (directs)) du noeud passé en argument
  @raises Not_Found si un des voisins du noeud n'est pas dans le graphe
*)
  let computePoidsLocal noeud graphe = 
    X.Set_int.fold (fun xi -> fun acc -> graphe.distance (noeud) (Map_int.find xi graphe.a)  +. acc) noeud.voisins 0.0



(**
  @requires un id de noeud qui appartient bien au graphe
  @ensures retourne un nouveau graphe dans lequel le noeud d'indice idNoeud a été déplacé aléatoirement
  @raises Not_found si l'id du noeud n'est pas dans la map en question
*)
  let moveAleatNoeud idNoeud graphe = 
    let noeud = Map_int.find idNoeud graphe.a in
    let oldPoidsLocal = computePoidsLocal noeud graphe in
    let newNoeud = X.moveAleatNoeud noeud in
    let newPoidsLocal = computePoidsLocal newNoeud graphe in
    {graphe with poids = (graphe.poids +. newPoidsLocal -. oldPoidsLocal) ; 
                 a = Map_int.update idNoeud (fun _ -> Some newNoeud) graphe.a
    }

(**
  @requires n < cardinal(set)
  @ensures retourne le n ième élément (ordre de comparaison classique) du set passé en deuxième argument
  @raises Failure "empty set" si n > cardinal(set)
  *)
  let rec get_nth n set = 
    if X.Set_int.is_empty set then
      raise (Failure "empty set") (* n est alors négatif si on a respecte la convention n < cardinal(set) *)
    else
      let hd = X.Set_int.min_elt set in
      if (n =0) then
        hd
      else
        get_nth (n-1) (X.Set_int.remove hd set)

(**
  @requires Set non vide
  @ensures retourne un élément pris aléatoirement dans le set
  @raises Failure "empty set" si le set initial est vide
*)
let pickRandomFromSet set = 
  let size = X.Set_int.cardinal set in
  if (size = 0 ) then raise (Failure "empty set")
  else
    let chosen = Random.int size in
    get_nth chosen set

(**
    @requires n < cardinal(set)
    @ensures retourne un set composé de n entiers pris aléatoirement dans le set
    @raises Failure "trop peu d'elements" si n > size
*) 
let rec pickNRandomFromSet n set = 
  let size = X.Set_int.cardinal set in
  if (n > size) then raise (Failure "trop peu d'elements")
  else
    if (n = 0) then X.Set_int.empty
    else
      let r = pickRandomFromSet set in
      X.Set_int.add r (pickNRandomFromSet (n-1) (X.Set_int.remove r set)) 

(**
      @requires un graphe avec au moins un point relai
      @ensures retourne le couple (idDuRelaiModifié, nouveauGraphe) avec nouveauGraphe le graphe dans lequel le point
               d'indice idDuRelaiModifié a été déplacé aléatoirement
      @raises Failure "empty set" si le graphe n'a aucun point relai
*)
  let transfoDeplacerRandomRelai graphe = 
    let chosenRelai = pickRandomFromSet graphe.relais in
    (chosenRelai , moveAleatNoeud chosenRelai graphe)


  (**
    @requires 3 couples de flottants distincts non alignés
    @ensures retourne un point choisi aléatoirement dans le triangle ou les 3 points sont ceux passés en argument
    @raises Failure "points identiques" si au moins 2 des points sont identiques ou alignés
  *)
  let tirerTriangleDans (x1,y1) (x2,y2) (x3,y3) = 
    (* if ( (x1 -. x2 -. x3) > 1e-8 && (y1 -. y2 -. y3) > 1e-8 ) then *)
    (* Shamelessly inspired by : https://math.stackexchange.com/questions/18686/uniform-random-point-in-triangle *)
    let r1 = Random.float 1.0 in
    let r2 = Random.float 1.0 in
    let sqr1 = sqrt r1 in
    ((1.0-. sqr1) *. x1 +. sqr1 *. (1.0-.r2) *. x2 +. r2 *. sqr1 *. x3
    ,(1.0-. sqr1) *. y1 +. sqr1 *. (1.0-.r2) *. y2 +. r2 *. sqr1 *. y3)
    (* else raise (Failure "points identiques") *)


(**
    @requires des couples de (coordonnées,id) qui correspondent a la réalité du graphe
    @ensures retourne un couple (id,g) avec id l'id du point relai ajouté dans le graphe, entre les points passés en argument
             le nouveau graphe est alors g
    @raises Failure "points identiques" si au moins 2 points sont identiques ou alignés
            Not_found si un des 3 points n'est pas trouvé dans le graphe
*)
  let ajoutPointRelaiEntre ((x1,y1),id1) ((x2,y2),id2) ((x3,y3),id3) graphe = 
    let x,y = tirerTriangleDans (x1 , y1) (x2,y2) (x3, y3) in
    let idNouveauRelai, g = addNoeud x y true graphe in
    let g = addLink idNouveauRelai id1  g in
    let g = addLink idNouveauRelai id2 g in
    let g = addLink idNouveauRelai id3 g in
    (idNouveauRelai, g)



(**
    @requires un graphe connexe qui a au moins 3 points
    @ensures retourne un couple (i,g) ou i est l'id du point relai ajouté dans le graphe passé en argument (le nouveau graphe
             est alors g)
    @raises Failure "empty set" ou "pas assez de points" dans le graphe s'il n'y a pas assez de points dans le graphe
*)
(**
    @requires un graphe connexe qui a au moins 3 points
    @ensures retourne un couple (i,g) ou i est l'id du point relai ajouté dans le graphe passé en argument (le nouveau graphe
             est alors g)
    @raises Failure "empty set" ou "pas assez de points" dans le graphe s'il n'y a pas assez de points dans le graphe
*)
let ajoutRelaiTriangle graphe = 
  let choixTypeP1 = Random.int 2 in
  if ( choixTypeP1 = 0 || X.Set_int.cardinal graphe.relais = 0 ) then (* Choix aléatoire : on part d'un point de départ*)
    let idRandomDepart = pickRandomFromSet graphe.depart in 
    (* Par construction du graphe, tous les points sont au moins reliés a un autre point *)
    let randomDepart = Map_int.find idRandomDepart graphe.a in
    let idVoisin1 = pickRandomFromSet (randomDepart.voisins) in
    let voisin1 = Map_int.find idVoisin1 graphe.a in
      if (X.Set_int.cardinal voisin1.voisins >= 2) then
        let idVoisin2 = pickRandomFromSet (X.Set_int.remove (idRandomDepart) (voisin1.voisins)) in
        let voisin2 = Map_int.find idVoisin2 graphe.a in
        let idMilieu,ng = ajoutPointRelaiEntre ((randomDepart.x, randomDepart.y),idRandomDepart) ((voisin1.x, voisin1.y),idVoisin1) ((voisin2.x,voisin2.y),idVoisin2) graphe in
        let ng = removeLink idVoisin1 idMilieu ng in
        let ng = removeLink idVoisin1 idMilieu ng in
        let ng = removeLink idRandomDepart idMilieu ng in
        let ng = removeLink idVoisin2 idMilieu ng in 
        idMilieu, ng  
      else if ( X.Set_int.cardinal randomDepart.voisins >= 2 ) then
            let idVoisin2 = pickRandomFromSet (X.Set_int.remove (idVoisin1) (randomDepart.voisins)) in
            let voisin2 = Map_int.find idVoisin2 graphe.a in
            let idMilieu,ng = ajoutPointRelaiEntre ((randomDepart.x, randomDepart.y),idRandomDepart) ((voisin1.x, voisin1.y),idVoisin1) ((voisin2.x,voisin2.y),idVoisin2) graphe in
            let ng = removeLink idVoisin1 idMilieu ng in
            let ng = removeLink idVoisin1 idMilieu ng in
            let ng = removeLink idRandomDepart idMilieu ng in
            let ng = removeLink idVoisin2 idMilieu ng in 
            idMilieu, ng  
          else
          (* Ni le point de départ ni le voisin choisi n'ont de voisin -> c'est qu'il n'y a que 2 points dans le graphe*)
          raise (Failure "pas assez de points dans le graphe")
        
    else 
      let idRandomDepart = pickRandomFromSet graphe.relais in 
      (* Par construction du graphe, tous les points sont au moins reliés a un autre point *)
      let randomDepart = Map_int.find idRandomDepart graphe.a in
      let idVoisin1 = pickRandomFromSet (randomDepart.voisins) in
      let voisin1 = Map_int.find idVoisin1 graphe.a in
        if (X.Set_int.cardinal voisin1.voisins >= 2) then
          let idVoisin2 = pickRandomFromSet (X.Set_int.remove (idRandomDepart) (voisin1.voisins)) in
          let voisin2 = Map_int.find idVoisin2 graphe.a in
          let idMilieu, ng = ajoutPointRelaiEntre ((randomDepart.x, randomDepart.y),idRandomDepart) ((voisin1.x, voisin1.y),idVoisin1) ((voisin2.x,voisin2.y),idVoisin2) graphe in
          let ng = removeLink idVoisin1 idMilieu ng in
          let ng = removeLink idVoisin1 idMilieu ng in
          let ng = removeLink idRandomDepart idMilieu ng in
          let ng = removeLink idVoisin2 idMilieu ng in 
          idMilieu, ng          
        else if ( X.Set_int.cardinal randomDepart.voisins >= 2 ) then
              let idVoisin2 = pickRandomFromSet (X.Set_int.remove (idVoisin1) (randomDepart.voisins)) in
              let voisin2 = Map_int.find idVoisin2 graphe.a in
              let idMilieu, ng = ajoutPointRelaiEntre ((randomDepart.x, randomDepart.y),idRandomDepart) ((voisin1.x, voisin1.y),idVoisin1) ((voisin2.x,voisin2.y),idVoisin2) graphe in
              let ng = removeLink idVoisin1 idMilieu ng in
              let ng = removeLink idVoisin1 idMilieu ng in
              let ng = removeLink idRandomDepart idMilieu ng in
              let ng = removeLink idVoisin2 idMilieu ng in 
              idMilieu, ng          
            
            else
            (* Ni le point de départ ni le voisin choisi n'ont de voisin -> c'est qu'il n'y a que 2 points dans le graphe*)
            raise (Failure "pas assez de points dans le graphe")
  
  (**
    @requires idRelai correspond bien a l'identifiant d'un point relai qui existe dans le graphe
              ce point relai a au moins un voisin qui 
    @ensures retourne un nouveau graphe ou le point relai choisi a subi la deuxième transformation
             possible pour les graphes euclidien (voir sujet)
    @raises Not_found si l'id du point relai n'est pas dans le graphe, Failure "que des voisins relais" 
            si le point relai n'a aucun voisin non relai (que des points de départ)
  *)
  let transfoFusionVoisin idRelai graphe = 
    let pRelai = Map_int.find idRelai graphe.a in
    let voisinsRelai = X.Set_int.filter (fun id -> let p = Map_int.find id graphe.a in not(p.relai) ) pRelai.voisins in
    if X.Set_int.cardinal voisinsRelai < 1 then
      failwith "que des voisins relais"
    else
      let chosen = pickRandomFromSet voisinsRelai in
      let g = removeNoeud idRelai graphe in
      X.Set_int.fold (fun i -> fun acc -> addLink i chosen acc) pRelai.voisins g

  
  (**
    @requires un graphe qui contient au moins un point relai, et 
              pour lequel tous ses points relai ont au moins un voisin non relai
    @ensures retourne (id,g) ou id est l'id du point relai choisi aléatoirement qui a subi la deuxième transformation de l'énoncé
    @raises Failure "que des voisins relais" si le point relai choisi aléatoirement n'a aucun voisin non relai (que des points de départ)
            en théorie cette exception n'arrive pas si on ne modifie le graphe que grâce aux fonctions définies dans ce module
  *)
  let transfoFusionRandomVoisin graphe = 
    let chosen = pickRandomFromSet graphe.relais in
    (chosen, transfoFusionVoisin chosen graphe)
        

  (**
      @requires rien 
      @ensures retourne la liste des coordonnées des points du graphe
      @raises rien
  *)
  let exportCoordonnees graphe = 
    let getX p = (p:X.t).x in
    let getY p = (p:X.t).y in
    Map_int.fold (fun k -> fun d-> fun acc -> (getX d,getY d)::acc) graphe.a [] 

    
  (**
    @requires id dans le graphe
    @ensures retourne la liste des arrêtes connectées au point d'indice id
    @raises Not_found si id n'est pas dans graphe.a
  *)
  let listeArretesVoisins id graphe = 
    let p = Map_int.find id graphe.a in
    let cp = ((p:X.t).x,(p:X.t).y) in
    X.Set_int.fold (fun iVoisin -> fun acc ->  
      let pVoisin = Map_int.find iVoisin graphe.a  in
      let cv = ((pVoisin:X.t).x , (pVoisin:X.t).y) in
      (cp , cv)::acc) p.voisins []


  (**
      @requires rien
      @ensures renvoie une liste de points corresponant a la liste des arrêtes du graphe
      @raises rien
  *)
  let exportCotes graphe = 
    (* On fabrique une fonction de comparaison du pauvre, qui donne 1 ou -1 ou 0 juste pour pouvoir avoir
       une liste de float*float * float* float ou chaque arrête n'apparaît qu'une fois
       problème : ça ne marche pas ... *)
    let c = fun ((x1,y1),(z1,t1)) -> fun ((x2,y2),(z2,t2)) ->
                  let c1 = Float.compare x1 x2 in
                  let c2 = Float.compare y1 y2 in
                  let c3 = Float.compare z1 z2 in
                  let c4 = Float.compare t1 t2 in

                  if (c1 > 0) then 1
                  else if (c1 < 0) then -1
                  else (* c1 = 0*)
                    if (c2 > 0) then 1
                    else if (c2 < 0) then -1
                    else (* c2 = 0*)
                      if (c3 > 0) then 1
                      else if (c3 < 0) then -1
                      else (* c3 = 0*)
                        if (c4 >0) then 1
                        else if (c4 < 0) then -1
                        else 0 
    in
    let l = Map_int.fold (fun k -> fun v -> fun acc -> (listeArretesVoisins k graphe)@acc) graphe.a [] in
    let l = List.sort_uniq c l in 
    (*Float.t = float visiblement *)
    l
  


  (**
    @requires n entier positif plus petit strictement que la taille de la liste
    @ensures pick et pop le n ième element de la liste
    @raises rien 
  *)
  let rec pickAndPopnList n list = 
    if n = 0 then (List.hd list, List.tl list)
    else let tete, queue = pickAndPopnList (n-1) (List.tl list) in
    (tete, (List.hd list)::queue)


  (**
    @requires size = la taille de la list
    @ensures retourne un couple (choisi, liste) avec choisi l'élément choisi aléatoirement dans la liste
    @raises rien
  *)
  let pickRandomFromList size list = 
    let chosen = Random.int size in
    pickAndPopnList chosen list

  (**
    @requires sizel = taille de lpoints, 
    @ensures retourne un graphe auquel on a ajouté les points de départ correspondant a la liste des coordonées 
             de la liste lpoints passée en argument
    @raises rien
  *)
  let rec ajouterListePointsDepart lpoints sizel graphe =
    if lpoints = [] then graphe
    else
      let chosen,list = pickRandomFromList sizel lpoints in
      let x, y = chosen in
      let autresDepart = graphe.depart in
      let (id, g) = addNoeud x y false graphe in

      if (Map_int.cardinal g.a = 1) then 
        ajouterListePointsDepart list (sizel -1) g
      else
        let chosenDepart = pickRandomFromSet autresDepart in
        let g = addLink id chosenDepart g in
        ajouterListePointsDepart list (sizel -1) g


  (**
    @requires n >=9 nombre d'itérations
    @ensures retourne un graphe qui a subi n itérations d'améliorations
              les améliorations sont choisies aléatoirement
    @raises rien
  *)
  let rec evolveGraph n graphe = 
    if n <= 0 then graphe
    else 
      
      if X.Set_int.cardinal graphe.relais <= 1 then
        let (_,g) = ajoutRelaiTriangle graphe in
        let (_,g) = ajoutRelaiTriangle g in
        let (_,g) = ajoutRelaiTriangle g in
        evolveGraph (n-1) (bestGraphe graphe g)
      else
      let choix = Random.int 3 in
    match choix with
    |0 -> let (_,g) = ajoutRelaiTriangle graphe in  evolveGraph (n-1) (bestGraphe graphe g)
    |1 -> let (_,g) = transfoFusionRandomVoisin graphe in  evolveGraph (n-1) (bestGraphe graphe g)
    |_ -> let (_,g) = transfoDeplacerRandomRelai graphe in  evolveGraph (n-1) (bestGraphe graphe g)


  (**
    @requires rien
    @ensures retourne la liste des arrêtes obtenues par itérations successives
             la fonction fait 10 itérations par défaut
    @raises rien (normalement...)
  *)
  let euclidian lp = 
    let lp = List.map (fun (x,y) -> (float_of_int x, float_of_int y)) lp in
    let g = empty_graph in
    let g = ajouterListePointsDepart lp (List.length lp) g in
    let n = 10 in (* On choisit 10 itérations *)
    let g = evolveGraph n g in
    exportCotes g
    



end


