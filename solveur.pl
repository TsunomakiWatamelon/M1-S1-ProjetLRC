% Projet LRC 2023-2024
% M1 Informatique SU
% Robin Soares & Herve Nguyen

programme() :- load_files('tabox.pl').

% ##### Partie 1 #####

setof((Concept, Definition), equiv(Concept, Definition), TBox).
setof((Instance, Concept), inst(Instance,Concept), ABoxC).
setof((Instance1, Instance2, Role), instR(Instance1, Instance2, Role), ABoxR).

% Correction sémantique

setof(X, cnamea(X), CA).
setof(X, cnamena(X), CA).
setof(X, iname(X), Id).
setof(X, rname(X), R).

isCA(X) :- member(X, CA).
isCNA(X) :- member(X, CNA).
isId(X) :- member(X, Id).
isR(X) :- member(X, R).


% Correction syntaxique

concept(Concept) :- isCA(Concept).
concept(Concept) :- isCNA(Concept).
concept(False).
concept(True).
concept(not(Concept)) :- concept(Concept).
concept(and(ConceptX,ConceptY)) :- concept(ConceptX), concept(ConceptY).
concept(or(ConceptX, ConceptY)) :- concept(ConceptX), concept(ConceptY).
concept(some(Role, Concept)) :- isR(Role), concept(ConceptY).
concept(all(Role, Concept)) :- isR(Role), concept(ConceptY).

% Prédicats