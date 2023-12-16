% Projet LRC 2023-2024
% M1 Informatique SU
% Robin Soares & Herve Nguyen
% Partie 2

% ##### Partie 2 #####

deuxieme_etape(Abi,Abi1,Tbox) :-
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
    nl,write("Entrez le numero du type de proposition que vous voulez demontrer :"),nl,
    write("1 Une instance donnee appartient a un concept donne."), nl,
    write("2 Deux concepts n\'ont pas d\'elements en commun(ils ont une intersection vide)."),nl, read(R), suite(R,Abi,Abi1,Tbox).

acquisition_type1_instance(Inst) :- 
    nl,write("Entrez le nom de l\'instance de votre proposition :"),nl,read(Inst).

acquisition_type1_concept(C) :- 
    nl,write("Entrez le nom du concept de votre proposition :"),nl,read(C).

acquisition_prop_type1(Abi, [(Inst, NCFinal) | Abi],Tbox) :-
    acquisition_type1_instance(Inst),acquisition_type1_concept(C),
    isId(Inst), concept(C),
    definitionAtomique(not(C), NCA),
    nnf(NCA, NCFinal).

acquisition_type2_concept(C1,C2) :-
    nl,write("Entrez le nom du premier concept C1 de votre proposition :"),nl,read(C1),
    nl,write("Entrez le nom du deuxieme concept C2 de votre proposition :"),nl,read(C2).

acquisition_prop_type2(Abi, [(inst, and(NCA1Final, NCA2Final))|Abi], TBox) :-
    acquisition_type2_concept(C1, C2),
    concept(C1), concept(C2),
    definitionAtomique(not(C1), NCA1), definitionAtomique(not(C2), NCA2),
    nnf(NCA1, NCA1Final), nnf(NCA2, NCA2Final).

suite(1,Abi,Abi1,Tbox) :- 
    acquisition_prop_type1(Abi,Abi1,Tbox),!.
suite(2,Abi,Abi1,Tbox) :- 
    acquisition_prop_type2(Abi,Abi1,Tbox),!.
suite(R,Abi,Abi1,Tbox) :- 
    nl,write('Cette reponse est incorrecte.'),nl,
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).