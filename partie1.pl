% Projet LRC 2023-2024
% M1 Informatique SU
% Robin Soares & Herve Nguyen
% Partie 1

% ##### Partie 1 #####

premiere_etape(Tbox,Abi,Abr) :- 
    initTBox(InitT),
    initABoxC(InitAC),
    initABoxR(Abr),
    write(InitT), nl,

    (
        correctionTBox(InitT) -> 
        write('Info : TBox valide') ;
        write('Erreur : TBox invalide'), fail
    ), nl,
    (
        correctionABoxC(InitAC) -> 
        write('Info : ABoxC (instanciation de concepts) valide') ; 
        write('Erreur : ABox (instanciation de concepts) invalide'), fail
    ), nl,
    (
        correctionABoxR(Abr) -> 
        write('Info : ABoxR (instanciation de rôles) valide') ; 
        write('Erreur : ABox (instanciation de rôles) invalide'), fail
    ), nl,
    (
        traitement_Abox(InitAC, Abi) ->
        write('Info : ABox traitée')
    ), nl,
    (
        traitement_Tbox(InitT, Tbox) ->
        write('Info : TBox traitée')
    ), nl.


% On cree liste qui representera la TBox et les deux listes qui represente la ABox

initTBox(TBox) :- setof((Concept, Definition), equiv(Concept, Definition), TBox).
initABoxC(ABoxC) :- setof((Instance, Concept), inst(Instance,Concept), ABoxC).
initABoxR(ABoxR) :- setof((Instance1, Instance2, Role), instR(Instance1, Instance2, Role), ABoxR).

% Correction sémantique

getListe(CA, CNA, ID, R) :-
    setof(X, cnamea(X), CA),
    setof(X, cnamena(X), CNA), 
    setof(X, iname(X), ID), 
    setof(X, rname(X), R).


isCA(X)  :- getListe(CA, CNA, ID, R), member(X, CA), not(member(X, CNA)), not(member(X, ID)), not(member(X, R)).
isCNA(X) :- getListe(CA, CNA, ID, R), member(X, CNA), not(member(X, CA)), not(member(X, ID)), not(member(X, R)).
isId(X)  :- getListe(CA, CNA, ID, R), member(X, ID), not(member(X, CA)), not(member(X, CNA)), not(member(X, R)).
isR(X)   :- getListe(CA, CNA, ID, R), member(X, R), not(member(X, CA)), not(member(X, CNA)), not(member(X, ID)).


% Correction syntaxique

concept(Concept) :- isCA(Concept).
concept(Concept) :- isCNA(Concept).
concept(anything).
concept(nothing).
concept(not(Concept)) :- concept(Concept).
concept(and(ConceptX,ConceptY)) :- concept(ConceptX), concept(ConceptY).
concept(or(ConceptX, ConceptY)) :- concept(ConceptX), concept(ConceptY).
concept(some(Role, Concept)) :- isR(Role), concept(Concept).
concept(all(Role, Concept)) :- isR(Role), concept(Concept).

% Correction Generale

correctionTBox([]).
correctionTBox([(Concept, Definition) | Reste]) :-
    (concept(Concept) -> true ; write('Erreur : '), write(Concept), write('n\'est pas un concept'), nl, false),
    (concept(Definition) -> true ; write('Erreur : '), write(Definition), write('n\'est pas un concept'), nl, false),
    (not(autoref(Concept)) -> true ; write('Erreur : '), write(Concept), write('est autoreferent'), nl, false),
    (not(autoref(Definition)) -> true ; write('Erreur : '), write(Definition), write('est autoreferent'), nl, false),
    correctionTBox(Reste).


correctionABoxC([]).
correctionABoxC([(Instance, Concept) | Reste]) :-
    (isId(Instance) -> true ; write('Erreur : '), write(Instance), write('n\'est pas une Id d\'instance'), false),
    concept(Concept),
    not(autoref(Concept)),
    correctionABoxC(Reste).

correctionABoxR([]).
correctionABoxR([(Instance1, Instance2, Role) | Reste]) :- isId(Instance1), isId(Instance2), isR(Role), correctionABoxR(Reste).


% Prédicats autoref et definition atomique

autoref(Concept) :-
    equiv(Concept, Expression),
    (not(conceptAutoref(Concept, Expression)) -> write('Info : Aucun concept auto-référent détécté') ; false).

conceptAutoref(Concept, Concept).
conceptAutoref(Concept, some(Expression)) :-
    conceptAutoref(Concept, Expression).
conceptAutoref(Concept, not(Expression)) :-
    conceptAutoref(Concept, Expression).
conceptAutoref(Concept, and(Expression1, Expression2)) :- 
    conceptAutoref(Concept, Expression1);
    conceptAutoref(Concept, Expression2).
conceptAutoref(Concept, or(Expression1, Expression2)) :- 
    conceptAutoref(Concept, Expression1);
    conceptAutoref(Concept, Expression2).
conceptAutoref(Concept, some(_, Expression)) :-
    conceptAutoref(Concept, Expression).
conceptAutoref(Concept, all(_, Expression)) :-
    conceptAutoref(Concept, Expression).


% Obtenir une definition ne contenant que de concepts atomiques
definitionAtomique(Definition, Definition) :-
    cnamea(Definition).
definitionAtomique(Definition, Res) :-
    equiv(Definition, X), definitionAtomique(X, Res). % Obtenir une expression equivalente composé de termes atomiques
definitionAtomique(not(Definition), not(Res)) :-
    definitionAtomique(Definition, Res).
definitionAtomique(or(D1,D2), or(R1,R2)) :-
    definitionAtomique(D1,R1),
    definitionAtomique(D2,R2).
definitionAtomique(and(D1,D2), and(R1,R2)) :-
    definitionAtomique(D1,R1),
    definitionAtomique(D2,R2).
definitionAtomique(some(Role, Definition), some(Role, Res)) :-
    definitionAtomique(Definition, Res).
definitionAtomique(all(Role,Definition), all(Role, Res)) :-
    definitionAtomique(Definition, Res).


% Remplacer les concepts de la TBox ou ABox originale par des Concepts composés de termes atomiques
% Et en NNF
remplacement([], []).
remplacement([(Concept, Definition) | Reste], [(Concept, DefinitionTraite) | ResteTraite]) :-
    definitionAtomique(Definition, Atomique),
    nnf(Atomique, DefinitionTraite),
    remplacement(Reste, ResteTraite).

% Traitement

traitement_Tbox(Initial, TBox) :- remplacement(Initial, TBox).
traitement_Abox(Initial, ABoxC) :- remplacement(Initial, ABoxC).

