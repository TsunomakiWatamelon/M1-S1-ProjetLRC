% Projet LRC 2023-2024
% M1 Informatique SU
% Robin Soares & Herve Nguyen
% Partie 3

% ##### Partie 3 #####

troisieme_etape(Abi,Abr) :- 
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
    resolution(Lie,Lpt,Li,Lu,Ls,Abr), nl,
    write('Youpiiiiii, on a demontre la proposition initiale !!!').



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

tri_Abox([], _, _, _, _, _).

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

contient_clash([(I, C) | Reste]) :-
    nnf(C, C1),
    member((I, not(C1)), Reste).
    
complete_some([(A, some(R,C)) | Lie],Lpt,Li,Lu,Ls,Abr) :-
    genere(B),
    evolue((B, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    affiche_evolution_Abox(Ls, [(A, some(R,C)) | Lie], Lpt, Li, Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, [(A, B, R) | Abr]),
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, [(A, B, R) | Abr]).
complete_some([], _, _, _, _, _).

deduction_all(Lie, [(I, all(R, C)) | Lpt], Li, Lu, Ls, Abr) :-
    setof((B, C), member((I, B, R), Abr), L),
    evolue_multi(L, Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    affiche_evolution_Abox(Ls, Lie, [(I, all(R, C)) | Lpt], Li, Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr),
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr).
deduction_all(_, [], _, _, _, _).

transformation_and(Lie,Lpt,[(I, and(C1, C2)) | Li],Lu,Ls,Abr) :-
    evolue((I, C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    evolue((I, C2), Lie1, Lpt1, Li1, Lu1, Ls1, Lie2, Lpt2, Li2, Lu2, Ls2),
    affiche_evolution_Abox(Ls, Lie, Lpt, [(I, and(C1, C2)) | Li], Lu, Abr, Ls2, Lie2, Lpt2, Li2, Lu2, Abr),
    resolution(Lie2, Lpt2, Li2, Lu2, Ls2, Abr).
transformation_and(_, _, [], _, _, _).

transformation_or(Lie,Lpt,Li,[(I, or(C1, C2)) | Lu],Ls,Abr) :-
    evolue((I, C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    evolue((I, C2), Lie1, Lpt1, Li1, Lu1, Ls1, Lie2, Lpt2, Li2, Lu2, Ls2),
    affiche_evolution_Abox(Ls, Lie, Lpt, Li, [(I, or(C1, C2)) | Lu], Abr, Ls2, Lie2, Lpt2, Li2, Lu2, Abr),
    resolution(Lie2, Lpt2, Li2, Lu2, Ls2, Abr).
transformation_or(_, _, _, [], _, _).

evolue((I, C), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    cnamea(C),
    member((I, C), Ls).
evolue((I, C), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(I, C) | Ls]) :-
    cnamea(C),
    not(member((I, C), Ls)).

evolue((I, not(C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    cnamea(C),
    member((I, not(C)), Ls).
evolue((I, not(C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(I, not(C)) | Ls]) :-
    cnamea(C),
    not(member((I, not(C)), Ls)).

evolue((I, some(R, C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, some(R, C)), Lie).
evolue((I, some(R, C)), Lie, Lpt, Li, Lu, Ls, [(I, some(R,C)) | Lie], Lpt, Li, Lu, Ls) :-
    not(member((I, some(R, C)), Lie)).

evolue((I, all(R, C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, all(R, C)), Lpt).
evolue((I, all(R, C)), Lie, Lpt, Li, Lu, Ls, Lie, [(I, all(R,C)) | Lpt], Li, Lu, Ls) :-
    not(member((I, all(R, C)), Lpt)).

evolue((I, and(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, and(C1, C2)), Li).
evolue((I, and(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, [(I, and(C1,C2)) | Li], Lu, Ls) :-
    not(member((I, and(C1, C2)), Li)).

evolue((I, or(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, or(C1, C2)), Lu).
evolue((I, or(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Ls) :-
    not(member((I, or(C1, C2)), Lu)).


evolue_multi([Elem | Reste], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    evolue(Elem, Lie, Lpt, Li, Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
    evolue_multi(Reste, Lie2, Lpt2, Li2, Lu2, Ls2, Lie1, Lpt1, Li1, Lu1, Ls1).
evolue_multi([], Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls).

affiche_Abr([]).
affiche_Abr([(A, B, R) | Reste]) :-
    write("<"),write(A), write(","), write(B), write("> : "), write(R), nl,
    affiche_Abr(Reste).

affiche_Abi([]).
affiche_Abi([(A, C) | Reste]) :-
    write(A), write(" : "), affiche_concept(C), nl,
    affiche_Abi(Reste).

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
affiche_concept(not(C)) :-
    write("¬"), affiche_concept(C).

affiche_ABox(Ls, Lie, Lpt, Li, Lu, Abr) :-
    affiche_Abr(Abr), nl,
    affiche_Abi(Lie), nl,
    affiche_Abi(Lpt), nl,
    affiche_Abi(Li), nl,
    affiche_Abi(Lu), nl,
    affiche_Abi(Ls), nl.

affiche_evolution_Abox(Ls, Lie, Lpt, Li, Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr1) :-
    nl, nl, write("Etat de la ABox étendue de départ :"), nl, nl,
    affiche_ABox(Ls, Lie, Lpt, Li, Lu, Abr), nl, nl,

    nl, nl, write("Etat de la ABox étendue d'arrivée :"), nl, nl,
    affiche_ABox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1), nl, nl.


