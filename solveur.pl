% Projet LRC 2023-2024
% M1 Informatique SU
% Robin Soares & Herve Nguyen

compteur(1).

programme() :- load_files('tabox.pl'),
               load_files('aux.pl'),
               load_files('partie1.pl'),
               load_files('partie2.pl'),
               premiere_etape(Tbox,Abi,Abr),
               deuxieme_etape(Abi,Abi1,TBox).

