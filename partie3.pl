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

tri_Abos([(I, not(C)) | Abi],Lie,Lpt,Li,Lu,[(I, not(C)) | Ls]) :-
    cnamea(C),
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).

tri_Abox([], _, _, _, _, _).
    
