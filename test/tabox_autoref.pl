:- discontiguous cnamea/1.
:- discontiguous cnamena/1.
:- discontiguous rname/1.
:- discontiguous equiv/2.

% Sculpture autoreferent avec sculpteur 
rname(creePar).
equiv(sculpture, and(objet, all(creePar,sculpteur))).
equiv(sculpteur,and(personne,some(aCree,sculpture))).
cnamea(sculpture).
cnamena(sculpteur).
cnamea(personne).

% Joueur auto referent avec jeu
cnamea(jeu).
cnamena(joueur).
rname(aJoue).
rname(jouePar).
cnamea(gamer).
equiv(joueur, and(gamer, some(aJoue, jeu))).
equiv(jeu, and(jeu, all(jouePar, or(joueur, gamer)))).

% Marque auto referent avec entreprise et fabriquant
cnamea(chevrolet).
cnamena(marque).
cnamena(entreprise).
cnamena(frabriquant).
cnamea(voiture).
rname(aFabrique).
equiv(marque, and(entreprise, all(creePar, chevrolet))).
equiv(entreprise, or(fabriquant, personne)).
equiv(fabriquant, and(marque , some(aFabrique, voiture))).
