% Projet LRC 2023-2024
% M1 Informatique SU
% Robin Soares & Herve Nguyen

:- discontiguous cnamea/1.

programme() :- load_files('tabox.pl'),
               premiere_etape(Tbox,Abi,Abr),
               deuxieme_etape(Abi,Abi1,TBox),
               troisieme_etape(Abi1,Abr).

% ####################################################################
% ############################# Partie 1 #############################
% ####################################################################

premiere_etape(Tbox,Abi,Abr) :- 
    initTBox(InitT),
    initABoxC(InitAC),
    initABoxR(Abr),

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
        write('Info : ABox traitee')
    ), nl,
    (
        traitement_Tbox(InitT, Tbox) ->
        write('Info : TBox traitee')
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

% On verifie que le concept est bien parmi les concepts atomiques connus
isCA(X)  :- getListe(CA, CNA, ID, R), member(X, CA), not(member(X, CNA)), not(member(X, ID)), not(member(X, R)).
% On verifie que le concept est bien parmi les concepts non atomiques connus
isCNA(X) :- getListe(CA, CNA, ID, R), member(X, CNA), not(member(X, CA)), not(member(X, ID)), not(member(X, R)).
% On verifie que l'instance est bien parmi les instances connues
isId(X)  :- getListe(CA, CNA, ID, R), member(X, ID), not(member(X, CA)), not(member(X, CNA)), not(member(X, R)).
% On verifie que le role est bien parmi les roles connus
isR(X)   :- getListe(CA, CNA, ID, R), member(X, R), not(member(X, CA)), not(member(X, CNA)), not(member(X, ID)).


% Correction syntaxique

% Verifie si un concept est bien formé
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

% Verifie si la TBox est correcte
correctionTBox([]).
correctionTBox([(Concept, Definition) | Reste]) :-
    (concept(Concept) -> true ; write('Erreur : '), write(Concept), write(' n\'est pas un concept'), nl, false),
    (concept(Definition) -> true ; write('Erreur : '), write(Definition), write(' n\'est pas un concept'), nl, false),
    (not(autoref(Concept)) -> true ; write('Erreur : '), write(Concept), write(' est autoreferent'), nl, false),
    (not(autoref(Definition)) -> true ; write('Erreur : '), write(Definition), write(' est autoreferent'), nl, false),
    correctionTBox(Reste).

% Verifie si la ABox des concepts est correcte
correctionABoxC([]).
correctionABoxC([(Instance, Concept) | Reste]) :-
    (isId(Instance) -> true ; write('Erreur : '), write(Instance), write('n\'est pas une Id d\'instance'), false),
    concept(Concept),
    not(autoref(Concept)),
    correctionABoxC(Reste).

% Verifie si la ABox des roles est correcte
correctionABoxR([]).
correctionABoxR([(Instance1, Instance2, Role) | Reste]) :- isId(Instance1), isId(Instance2), isR(Role), correctionABoxR(Reste).


% Prédicats autoref et definition atomique

% Verifie si un concept est autoreferent
autoref(Concept) :-
    equiv(Concept, Expression),
    (not(conceptAutoref(Concept, Expression, [])) ->
        write('Info : '), write(Concept), write(" n'est pas autoreferent"), nl, false
    ; write('Info : '), write(Concept), write(" est soit autoréférent ou bien contient une expression autoréférente"), nl, true).

% Verifie si un concept est autoreferent (Parcourss recursif de l'expression)
conceptAutoref(Concept, Concept, _).

conceptAutoref(Concept, ConceptA, Visited) :-
    member(ConceptA, Visited);
    equiv(ConceptA, ConceptB),
    conceptAutoref(Concept, ConceptB, [ConceptA | Visited]).

conceptAutoref(Concept, not(Expression), Visited) :-
    member(not(Expression), Visited);
    conceptAutoref(Concept, Expression, [not(Expression) | Visited]).

conceptAutoref(Concept, and(Expression1, Expression2), Visited) :- 
    member(and(Expression1, Expression2), Visited);
    conceptAutoref(Concept, Expression1, [and(Expression1, Expression2) | Visited]);
    conceptAutoref(Concept, Expression2, [and(Expression1, Expression2) | Visited]).

conceptAutoref(Concept, or(Expression1, Expression2), Visited) :- 
    member(or(Expression1, Expression2), Visited);
    conceptAutoref(Concept, Expression1, [or(Expression1, Expression2) | Visited]);
    conceptAutoref(Concept, Expression2, [or(Expression1, Expression2) | Visited]).

conceptAutoref(Concept, some(R, Expression), Visited) :-
    member(some(R, Expression), Visited);
    conceptAutoref(Concept, Expression, [some(R, Expression) | Visited]).

conceptAutoref(Concept, all(R, Expression), Visited) :-
    member(all(R, Expression), Visited);
    conceptAutoref(Concept, Expression, [all(R, Expression) | Visited]).


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


% ####################################################################
% ############################# Partie 2 #############################
% ####################################################################

deuxieme_etape(Abi,Abi1,Tbox) :-
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
    nl,write("Input : Entrez le numero du type de proposition que vous voulez demontrer :"),nl,
    write("Input : \"1\" Une instance donnee appartient a un concept donne."), nl,
    write("Input : \"2\" Deux concepts n\'ont pas d\'elements en commun(ils ont une intersection vide)."),nl, read(R), suite(R,Abi,Abi1,Tbox).


% ##### Type 1 #####

% Saisie de l'instance et du concept pour les propositions de type 1
acquisition_type1_instance(Inst) :- 
    nl,write("Input : Entrez le nom de l\'instance de votre proposition :"),nl,read(Inst).

acquisition_type1_concept(C) :- 
    nl,write("Input : Entrez le nom du concept de votre proposition :"),nl,read(C).

% Traitement de la proposition de type 1
acquisition_prop_type1(Abi, [(Inst, NCFinal) | Abi]) :-
    acquisition_type1_instance(Inst),
    (isId(Inst) -> true; write("Warning : "), write(Inst), write(" n'est pas une instance"), nl, false),
    acquisition_type1_concept(C),
    (concept(C) -> true; write("Warning : "), write(C), write(" n'est pas un concept"), nl, false), nl,
    write('Info : Proposition a demontrer \"'), write(Inst), write(' : '), affiche_concept(C), write('\"'), nl,
    definitionAtomique(not(C), NCA),
    nnf(NCA, NCFinal).

% ##### Type 2 #####

% Saisie des deux concepts pour les propositions de type 2
acquisition_type2_concept(C,1) :-
    nl,write("Input : Entrez le nom du premier concept C1 de votre proposition :"),nl,read(C).
acquisition_type2_concept(C,2) :-
    nl,write("Input : Entrez le nom du deuxieme concept C2 de votre proposition :"),nl,read(C).

% Traitement de la proposition de type 2
acquisition_prop_type2(Abi, [(Inst, and(CA1Final, CA2Final))|Abi]) :-
    genere(Inst),
    acquisition_type2_concept(C1,1),
    (concept(C1) -> true; write("Warning : "), write(C1), write(" n'est pas un concept"), nl, false),
    acquisition_type2_concept(C2,2),
    (concept(C2) -> true; write("Warning : "), write(C2), write(" n'est pas un concept"), nl, false), nl,
    write('Info : Proposition a demontrer \"'), affiche_concept(C1), write(' ⊓ '), affiche_concept(C2), write(' ⊑ ⊥\" '), nl,
    definitionAtomique(C1, CA1), definitionAtomique(C2, CA2),
    nnf(CA1, CA1Final), nnf(CA2, CA2Final).

suite(1,Abi,Abi1,Tbox) :- 
    acquisition_prop_type1(Abi,Abi1),!.
suite(2,Abi,Abi1,Tbox) :- 
    acquisition_prop_type2(Abi,Abi1),!.

suite(_,Abi,Abi1,Tbox) :-
    nl,write("Warning : Cette reponse est incorrecte."),nl,
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).


% ####################################################################
% ############################# Partie 3 #############################
% ####################################################################

troisieme_etape(Abi,Abr) :- 
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), nl,
    not(resolution(Lie,Lpt,Li,Lu,Ls,Abr)) -> write("Result : La proposition initiale a ete demontree"), nl, true; write("Result : La proposition initiale n\'a pas pu etre demontree"), nl, false.

% ##### tri_Abox #####
% Genere les 5 listes de la ABox etendue pour pouvoir appliquer les regles de resolution

tri_Abox([], [], [], [], [], []).

tri_Abox([(I, some(R,C))|Abi],[(I, some(R,C)) | Lie],Lpt,Li,Lu,Ls) :-
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).

tri_Abox([(I, all(R,C))|Abi],Lie,[(I, all(R,C)) | Lpt],Li,Lu,Ls) :-
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).

tri_Abox([(I, and(C1,C2))|Abi],Lie,Lpt, [(I, and(C1,C2)) | Li],Lu,Ls) :-
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).

tri_Abox([(I, or(C1,C2))|Abi],Lie,Lpt,Li,[(I, or(C1,C2)) | Lu],Ls) :-
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).

tri_Abox([(I, C) | Abi],Lie,Lpt,Li,Lu,[(I, C) | Ls]) :-
    cnamea(C),
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).

tri_Abox([(I, not(C)) | Abi],Lie,Lpt,Li,Lu,[(I, not(C)) | Ls]) :-
    cnamea(C),
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).

% ##### resolution #####
% Applique les regles de resolution de maniere recursive suivant les regles de resolution

resolution(Lie, Lpt, Li, Lu, Ls, Abr) :-
    not(contient_clash(Ls)),
    complete_some(Lie, Lpt, Li, Lu, Ls, Abr).

resolution([], Lpt, Li, Lu, Ls, Abr) :-
    not(contient_clash(Ls)),
    deduction_all([], Lpt, Li, Lu, Ls, Abr).

resolution([], [], Li, Lu, Ls, Abr) :- 
    not(contient_clash(Ls)),
    transformation_and([], [], Li, Lu, Ls, Abr).

resolution([], [], [], Lu, Ls, Abr) :-
    not(contient_clash(Ls)),
    transformation_or([], [], [], Lu, Ls, Abr).

resolution([], [], [], [], Ls, Abr) :-
    not(contient_clash(Ls)).

% Règle pour vérifier si X est un membre de la liste

% ##### contient_clash #####

% Verifie si la ABox etendue contient un clash
contient_clash([]) :- false.
contient_clash([(I, C) | Reste]) :-
    nnf(not(C), C1),
    member((I, C1), Reste) -> write("CLASH : Un clash a ete detecte"), nl,true ; contient_clash(Reste).

% ##### complete_some #####

% Applique la regle ∃
complete_some([(A, some(R,C)) | Lie],Lpt,Li,Lu,Ls,Abr) :-
    write("####################################################################"), nl, nl,
    write("Application de la regle ∃ sur : "), nl, write("    "), affiche_concept((A, some(R,C))), nl, nl,
    genere(B),
    evolue((B, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    affiche_evolution_Abox(Ls, [(A, some(R,C)) | Lie], Lpt, Li, Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, [(A, B, R) | Abr]),
    write("####################################################################"), nl, nl,
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, [(A, B, R) | Abr]).

% ##### deduction_all #####

% Applique la regle ∀
deduction_all(Lie, [(I, all(R, C)) | Lpt], Li, Lu, Ls, Abr) :-
    write("####################################################################"), nl, nl,
    write("Application de la regle ∀ sur : "), nl, write("    "), affiche_concept((I, all(R,C))), nl, nl,
    write("Abr"), nl, affiche_Abr(Abr), nl,
    findall((B, C), member((I, B, R), Abr), L),
    evolue_multi(L, Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    affiche_evolution_Abox(Ls, Lie, [(I, all(R, C)) | Lpt], Li, Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr),
    write("####################################################################"), nl, nl,
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr).

% ##### transformation_and #####

% Applique la regle ⊓
transformation_and(Lie,Lpt,[(I, and(C1, C2)) | Li],Lu,Ls,Abr) :-
    write("####################################################################"), nl, nl,
    write("Application de la regle ⊓ sur : "), nl, write("    "), affiche_concept((I, and(C1, C2))), nl, nl,
    evolue((I, C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    evolue((I, C2), Lie1, Lpt1, Li1, Lu1, Ls1, Lie2, Lpt2, Li2, Lu2, Ls2),
    affiche_evolution_Abox(Ls, Lie, Lpt, [(I, and(C1, C2)) | Li], Lu, Abr, Ls2, Lie2, Lpt2, Li2, Lu2, Abr),
    write("####################################################################"), nl, nl,
    resolution(Lie2, Lpt2, Li2, Lu2, Ls2, Abr).

% ##### transformation_or #####

% Applique la regle ⊔
transformation_or(Lie,Lpt,Li,[(I, or(C1, C2)) | Lu],Ls,Abr) :-
    transformation_or_node(1, C1, Lie,Lpt,Li,[(I, or(C1, C2)) | Lu],Ls,Abr),
    transformation_or_node(2, C2, Lie,Lpt,Li,[(I, or(C1, C2)) | Lu],Ls,Abr).

% Applique la regle ⊔ sur un noeud
transformation_or_node(Node, C, Lie,Lpt,Li,[(I, or(C1, C2)) | Lu],Ls,Abr) :-
    write("####################################################################"), nl, nl, 
    write("Application de la regle ⊔ sur : "), nl, write("    "), affiche_concept((I, or(C1, C2))), nl,
    write("Branche "), write(Node), nl, nl,
    evolue((I, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    affiche_evolution_Abox(Ls, Lie, Lpt, Li, [(I, or(C1, C2)) | Lu], Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr),
    write("####################################################################"), nl, nl,
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr).

% ##### evolue #####
% Ajoute une assertion a la ABox etendue (dans la liste appropriee)

% Si l'assertion de concept est deja dans la ABox etendue, on ne fait rien
evolue((I, C), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    cnamea(C),
    member((I, C), Ls).

% Si l'assertion de concept n'est pas dans la ABox etendue, on l'ajoute dans Ls
evolue((I, C), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(I, C) | Ls]) :-
    cnamea(C),
    not(member((I, C), Ls)).

% Si l'assertion de concept (de la forme not(C)) est deja dans la ABox etendue, on ne fait rien
evolue((I, not(C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    cnamea(C),
    member((I, not(C)), Ls).

% Si l'assertion de concept (de la forme not(C)) n'est pas dans la ABox etendue, on l'ajoute dans Ls
evolue((I, not(C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(I, not(C)) | Ls]) :-
    cnamea(C),
    not(member((I, not(C)), Ls)).

% Si l'assertion de concept (forme I : ∃R.C) est deja dans la ABox etendue, on ne fait rien
evolue((I, some(R, C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, some(R, C)), Lie).
% Si l'assertion de concept (forme I : ∃R.C) n'est pas dans la ABox etendue, on l'ajoute dans Lie
evolue((I, some(R, C)), Lie, Lpt, Li, Lu, Ls, [(I, some(R,C)) | Lie], Lpt, Li, Lu, Ls) :-
    not(member((I, some(R, C)), Lie)).

% Si l'assertion de concept (forme I : ∀R.C) est deja dans la ABox etendue, on ne fait rien
evolue((I, all(R, C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, all(R, C)), Lpt).
% Si l'assertion de concept (forme I : ∀R.C) n'est pas dans la ABox etendue, on l'ajoute dans Lpt
evolue((I, all(R, C)), Lie, Lpt, Li, Lu, Ls, Lie, [(I, all(R,C)) | Lpt], Li, Lu, Ls) :-
    not(member((I, all(R, C)), Lpt)).

% Si l'assertion de concept (forme I : C1 ⊓ C2) est deja dans la ABox etendue, on ne fait rien
evolue((I, and(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, and(C1, C2)), Li).
% Si l'assertion de concept (forme I : C1 ⊓ C2) n'est pas dans la ABox etendue, on l'ajoute dans Li
evolue((I, and(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, [(I, and(C1,C2)) | Li], Lu, Ls) :-
    not(member((I, and(C1, C2)), Li)).

% Si l'assertion de concept (forme I : C1 ⊔ C2) est deja dans la ABox etendue, on ne fait rien
evolue((I, or(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, or(C1, C2)), Lu).
% Si l'assertion de concept (forme I : C1 ⊔ C2) n'est pas dans la ABox etendue, on l'ajoute dans Lu
evolue((I, or(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Ls) :-
    not(member((I, or(C1, C2)), Lu)).

% ##### evolue_multi #####

% evolue sur une liste de concepts
evolue_multi([Elem | Reste], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    evolue(Elem, Lie, Lpt, Li, Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
    evolue_multi(Reste, Lie2, Lpt2, Li2, Lu2, Ls2, Lie1, Lpt1, Li1, Lu1, Ls1).
evolue_multi([], Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls).

% ##### affichage #####

% affichage de l'evolution de la ABox entre deux etats
affiche_evolution_Abox(Ls, Lie, Lpt, Li, Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr1) :-
    write("Info : Etat de la ABox étendue de départ :"), nl, nl,
    affiche_ABox(Ls, Lie, Lpt, Li, Lu, Abr), nl,

    write("Info : Etat de la ABox étendue d'arrivée :"), nl, nl,
    affiche_ABox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1), nl.

% affichage de la ABox (totalite)
affiche_ABox(Ls, Lie, Lpt, Li, Lu, Abr) :-
    affiche_Abr(Abr),
    affiche_Abi(Lie),
    affiche_Abi(Lpt),
    affiche_Abi(Li),
    affiche_Abi(Lu),
    affiche_Abi(Ls), nl.

% affichage de la ABox (Assertion de role)
affiche_Abr([]).
affiche_Abr([(A, B, R) | Reste]) :-
    write("   "),
    write("<"),write(A), write(","), write(B), write("> : "), write(R), nl,
    affiche_Abr(Reste).

% affichage de la ABox (Assertion de concept)
affiche_Abi([]).
affiche_Abi([(A, C) | Reste]) :-
    write("   "),
    write(A), write(" : "), affiche_concept(C), nl,
    affiche_Abi(Reste).

% Affichade d'un concept
affiche_concept(anything) :-
    write("⊤").
affiche_concept(nothing) :- 
    write("⊥").
affiche_concept((I, C)) :-
    write(I), write(" : "), affiche_concept(C).
affiche_concept(some(R,C)) :-
    write("∃"), write(R), write("."), affiche_concept(C).
affiche_concept(all(R,C)) :-
    write("∀"), write(R), write("."), affiche_concept(C).
affiche_concept(and(C1,C2)) :-
    write("("), affiche_concept(C1), write(" ⊓ "), affiche_concept(C2), write(")").
affiche_concept(or(C1,C2)) :-
    write("("), affiche_concept(C1), write(" ⊔ "), affiche_concept(C2), write(")").
affiche_concept(C) :-
    cnamea(C),
    write(C).
affiche_concept(C) :-
    cnamena(C),
    write(C).
affiche_concept(not(C)) :-
    write("¬"), affiche_concept(C).


% ####################################################################
% ###################### Predicats auxilliaires ######################
% ####################################################################

setvalue(X, X).

% definis dans le sujet

% Obtenir la NNF d'une expression
nnf(not(and(C1,C2)),or(NC1,NC2)):- nnf(not(C1),NC1),
nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)):- nnf(not(C1),NC1),
nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)):- nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)):- nnf(not(C),NC),!.
nnf(not(not(X)),Y):- nnf(X,Y),!.
nnf(not(X),not(X)):-!.
nnf(and(C1,C2),and(NC1,NC2)):- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)):- nnf(C1,NC1), nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)):- nnf(C,NC),!.
nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).

% member(X,L) : prédicat prédéﬁni, teste si l’élément X appartient à la liste L.

% concat(L1,L2,L3) : concatène les deux listes L1 et L2 et renvoie la liste L3
concat([],L1,L1).
concat([X|Y],L1,[X|L2]) :- concat(Y,L1,L2).

% enleve(X,L1,L2) : supprime X de la liste L1 et renvoie la liste résultante dans L2.
enleve(X,[X|L],L) :-!.
enleve(X,[Y|L],[Y|L2]) :- enleve(X,L,L2).

% genere(Nom) : génère un nouvel identiﬁcateur qui est fourni en sortie dans Nom

compteur(1).
genere(Nom) :- 
    compteur(V),
    nombre(V,L1),
    concat([105,110,115,116],L1,L2),
    V1 is V+1,
    dynamic(compteur/1),
    retract(compteur(V)),
    dynamic(compteur/1),
    assert(compteur(V1)),nl,nl,nl,
    name(Nom,L2).

nombre(0,[]).
nombre(X,L1) :-
    R is (X mod 10),
    Q is ((X-R)//10),
    chiffre_car(R,R1),
    char_code(R1,R2),
    nombre(Q,L),
    concat(L,[R2],L1).

chiffre_car(0,'0').
chiffre_car(1,'1').
chiffre_car(2,'2').
chiffre_car(3,'3').
chiffre_car(4,'4').
chiffre_car(5,'5').
chiffre_car(6,'6').
chiffre_car(7,'7').
chiffre_car(8,'8').
chiffre_car(9,'9').


% ####################################################################
% ############################# Tests ################################
% ####################################################################

:- dynamic(saved_output/1).

% Save the current output stream

mute :-
    tell('output_test_partie3.txt'),
    assert(saved_output('output_test_partie3.txt')).

% Restore the saved output stream
unmute :-
    retract(saved_output(SavedStream)),
    tell(SavedStream),
    told.

test() :-
    load_files("./test/tabox_autoref.pl"),
    (test_autoref() -> write("test_autoref: OK"), nl; write("test_autoref: FAIL"), nl),
    unload_file("./test/tabox_autoref.pl"),

    load_files("tabox.pl"),
    (test_concept() -> write("test_concept: OK"), nl; write("test_concept: FAIL"), nl),

    (test_partie3() -> write("test_partie3: OK"), nl; write("test_partie3: FAIL"), nl),
    delete_file("output_test_partie3.txt"),

    unload_file("tabox.pl").

test_autoref() :-
    autoref(sculpture),
    autoref(joueur),
    autoref(marque).

test_concept() :-
    (concept(and(personne,some(aCree,sculpture)))-> true; false),
    (concept(objet)-> true; false),
    (concept(existepas)-> false; true).

test_partie3() :-
    getAbi(Abi),
    getAbr(Abr),
    mute,
    (troisieme_etape(Abi,Abr) -> unmute, false; unmute, true).


getAbi([(inst1,and(and(personne,some(aCree,sculpture)),and(personne,some(aEcrit,livre)))),(david,sculpture),(joconde,objet),(michelAnge,personne),(sonnets,livre),(vinci,personne)]).
getAbr([(michelAnge,david,aCree),(michelAnge,sonnets,aEcrit),(vinci,joconde,aCree)]).