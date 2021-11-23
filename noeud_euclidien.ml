module Noeud_euclidien = 
struct
  module Set_int = Set.Make(Int) 
  type t = {
    x: float;
    y: float;
    id: int; (* Index du noeud dans la map des noeuds *)
    voisins: Set_int.t; (* liste des index des noeuds voisins *)
    relai: bool (* true si point relai, false sinon *)
}


  (**
    @requires rien
    @ensures retourne un noeud_euclidien avec aucun voisins
    @raises rien
  *)
  let createNoeud x y id relai = 
    {
      x=x;
      y=y;
      id=id;
      (** poids_local = 0.0; *)
      voisins = Set_int.empty;
      relai = false
    }

  (**
    @requires rien 
    @ensures renvoie un nouveau noeud_euclidien auquel on a ajouté le voisin passé en argument (poids_local non mis a jour attention)
    @raises rien
  *)
  let addVoisin noeud indexVoisin = {
    noeud with voisins =  Set_int.add indexVoisin noeud.voisins          
  }

  (**
  @requires rien
  @ensures renvoie un nouveau noeud_euclidien auqel on a enlevé le voisin passé en argument (attention poids_local n'est pas mis a jour)
  @raises rien
  *)
  let removeVoisin noeud indexVoisin = 
    {noeud with voisins = Set_int.remove indexVoisin noeud.voisins}


  (**
    @requires rien
    @ensures renvoie un booléen qui vaut true si l'indexVoisin fait partie des voisins du noeud
    @raises rien
  *)
  let memVoisin noeud indexVoisin =
    Set_int.mem indexVoisin noeud.voisins 


  (**
    @requires rien
    @ensures renvoie un nouveau noeud avec les nouvelles coordonnées passées en argument (aucun autre champ modifié)
    @raises rien
  *)
  let changeCords newX newY noeud = {
    noeud with x = newX;
              y = newY;
  }

  (**
    @requires rien
    @ensures renvoie un nouveau noeud qui a son poids = à newPoids passé en argument
    @raises rien
  *)
  (** let changePoids noeud newPoids = {
    noeud with poids_local = newPoids
  } *)

  (**
    @requires rien
    @ensures renvoie un nouveau noeud qui a été déplacé aléatoirement selon la règle précisée dans le rapport (dans un carré centré en le point, de taille max(x,y))
    @raises rien
  *)
  let moveAleatNoeud noeud = 
    let maxLen = Float.max noeud.x noeud.y in
    changeCords (noeud.x +. 2.0 *. (Random.float maxLen)) (noeud.y +. 2.0 *. (Random.float maxLen)) noeud
end



