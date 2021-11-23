module type Noeud =
  sig
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
module Graphe_euclidien :
  functor (X : Noeud) ->
    sig
      module Map_int :
        sig
          type key = Int.t
          type 'a t = 'a Stdlib__map.Make(Int).t
          val empty : 'a t
          val is_empty : 'a t -> bool
          val mem : key -> 'a t -> bool
          val add : key -> 'a -> 'a t -> 'a t
          val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
          val singleton : key -> 'a -> 'a t
          val remove : key -> 'a t -> 'a t
          val merge :
            (key -> 'a option -> 'b option -> 'c option) ->
            'a t -> 'b t -> 'c t
          val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
          val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
          val iter : (key -> 'a -> unit) -> 'a t -> unit
          val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
          val for_all : (key -> 'a -> bool) -> 'a t -> bool
          val exists : (key -> 'a -> bool) -> 'a t -> bool
          val filter : (key -> 'a -> bool) -> 'a t -> 'a t
          val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
          val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
          val cardinal : 'a t -> int
          val bindings : 'a t -> (key * 'a) list
          val min_binding : 'a t -> key * 'a
          val min_binding_opt : 'a t -> (key * 'a) option
          val max_binding : 'a t -> key * 'a
          val max_binding_opt : 'a t -> (key * 'a) option
          val choose : 'a t -> key * 'a
          val choose_opt : 'a t -> (key * 'a) option
          val split : key -> 'a t -> 'a t * 'a option * 'a t
          val find : key -> 'a t -> 'a
          val find_opt : key -> 'a t -> 'a option
          val find_first : (key -> bool) -> 'a t -> key * 'a
          val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
          val find_last : (key -> bool) -> 'a t -> key * 'a
          val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
          val map : ('a -> 'b) -> 'a t -> 'b t
          val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
          val to_seq : 'a t -> (key * 'a) Seq.t
          val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
          val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
          val of_seq : (key * 'a) Seq.t -> 'a t
        end
      type t = {
        poids : float;
        a : X.t Map_int.t;
        distance : X.t -> X.t -> float;
        relais : X.Set_int.t;
        depart : X.Set_int.t;
        compteur : int;
      }
      val empty_graph : t
      val bestGraphe : t -> t -> t
      val addNoeud : float -> float -> bool -> t -> int * t
      val addLink : Map_int.key -> X.Set_int.elt -> t -> t
      val removeLink : Map_int.key -> X.Set_int.elt -> t -> t
      val removeNoeud : Map_int.key -> t -> t
      val computePoidsLocal : X.t -> t -> float
      val moveAleatNoeud : Map_int.key -> t -> t
      val get_nth : int -> X.Set_int.t -> X.Set_int.elt
      val pickRandomFromSet : X.Set_int.t -> X.Set_int.elt
      val pickNRandomFromSet : int -> X.Set_int.t -> X.Set_int.t
      val transfoDeplacerRandomRelai : t -> X.Set_int.elt * t
      val tirerTriangleDans :
        float * float -> float * float -> float * float -> float * float
      val ajoutPointRelaiEntre :
        (float * float) * X.Set_int.elt ->
        (float * float) * X.Set_int.elt ->
        (float * float) * X.Set_int.elt -> t -> int * t
      val ajoutRelaiTriangle : t -> int * t
      val transfoFusionVoisin : Map_int.key -> t -> t
      val transfoFusionRandomVoisin : t -> X.Set_int.elt * t
      val exportCoordonnees : t -> (float * float) list
      val listeArretesVoisins :
        Map_int.key -> t -> ((float * float) * (float * float)) list
      val exportCotes : t -> ((Float.t * Float.t) * (Float.t * Float.t)) list
      val pickAndPopnList : int -> 'a list -> 'a * 'a list
      val pickRandomFromList : int -> 'a list -> 'a * 'a list
      val ajouterListePointsDepart : (float * float) list -> int -> t -> t
      val evolveGraph : int -> t -> t
      val euclidian :
        (float * float) list ->
        ((Float.t * Float.t) * (Float.t * Float.t)) list
    end
