:- dynamic(saved_output/1).

% Save the current output stream
mute :-
    tell('output_test_partie3.txt'),
    assert(saved_output('output_test_partie3.txt')).

% Restore the saved output stream
unmute :-
    retract(saved_output(SavedStream)),
    tell(SavedStream).

test() :-
    load_files("solveur.pl"),
    (test_autoref() -> write("test_autoref: OK"), nl; write("test_autoref: FAIL"), nl),

    load_files("tabox.pl"),
    (test_concept() -> write("test_concept: OK"), nl; write("test_concept: FAIL"), nl),
    (test_partie3() -> write("test_partie3: OK"), nl; write("test_partie3: FAIL"), nl),
    unload_file("tabox.pl").
    unload_file("solveur.pl").

test_autoref() :-
    load_files("./test/tabox_autoref.pl"),
    autoref(sculpture),
    autoref(joueur),
    autoref(marque),
    unload_file("./test/tabox_autoref.pl").

test_concept() :-
    (concept(and(personne,some(aCree,sculpture)))-> true; false),
    (concept(objet)-> true; false),
    (concept(existepas)-> false; true).

test_partie3() :-
    mute,
    getAbi(Abi),
    getAbr(Abr),
    (troisieme_etape(Abi1,Abr) -> unmute, true; unmute, false).

getAbi([(inst1,and(and(personne,some(aCree,sculpture)),and(personne,some(aEcrit,livre)))),(david,sculpture),(joconde,objet),(michelAnge,personne),(sonnets,livre),(vinci,personne)]).
getAbr([(michelAnge,david,aCree),(michelAnge,sonnets,aEcrit),(vinci,joconde,aCree)]).




