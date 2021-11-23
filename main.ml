
module NE = Noeud_euclidien ;;
module GE = Graphe_euclidien(NE);;

let g = GE.empty_graph;;
(* let (i,g) = GE.addNoeud 5.0 5.0 false g ;;
let (i,g) = GE.addNoeud 6.0 5.0 false g ;;
let (i,g) = GE.addNoeud 3.0 1.0 false g ;; *)
let lp = [(0.0,1.0) ; (1.0 , 0.0) ; (3.0 , 4.0) ; (6.0,6.0)] ;;
let size = List.length lp;;


Random.init 1;;
let size = List.length lp ;;
let g = GE.ajouterListePointsDepart lp size g;;


(* let g = GE.evolveGraph 10 g ;; *)
let i,g = GE.ajoutRelaiTriangle g ;;
let i,g = GE.ajoutRelaiTriangle g ;;
let i,g = GE.transfoFusionRandomVoisin g ;;


let points = GE.exportCoordonnees g ;;

let arretes = GE.exportCotes g;;

let p1 = GE.Map_int.find 1 g.a;;
let p2 = GE.Map_int.find 2 g.a;;


NE.Set_int.elements (p1.voisins);;
NE.Set_int.elements p2.voisins;;