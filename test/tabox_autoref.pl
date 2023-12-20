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
equiv(joueur, and(gamer, some(jeu, aJoue))).
cnamea(gamer).

% Marque auto referent avec entreprise et fabriquant
cnamea(chevrolet).
cnamena(marque).
cnamena(entreprise).
cnamena(frabriquant).
cnamea(voiture).
rname(aFabrique).
equiv(marque, and(entreprise, all(creePar, chevrolet))).
equiv(entreprise, or(frabriquant, personne)).
equiv(fabriquant, and(marque , some(aFabrique, voiture))).
