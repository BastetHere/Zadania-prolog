# Zadania-prolog
Zadanie 1
%Fakty
ojciec(robert, kasia).
ojciec(robert, ania).
ojciec(robert, alek).
ojciec(robert, antek).
ojciec(karol, robert).
ojciec(karol, ala).
ojciec(henryk, iwona).
ojciec(henryk, staszek).
matka(ela, kasia).
matka(ela, ania).
matka(ela, alek).
matka(ela, antek).
matka(teresa, robert).
matka(teresa, ala).
matka(ala, iwona).
matka(ala, staszek).
malzenstwo(ela, robert).
malzenstwo(teresa, karol).
malzenstwo(ala, henryk).
kobieta(ela).
kobieta(ania).
kobieta(kasia).
kobieta(teresa).
kobieta(ala).
kobieta(iwona).
mezczyzna(robert).
mezczyzna(alek).
mezczyzna(karol).
mezczyzna(staszek).
mezczyzna(henryk).
mezczyzna(antek).

%Reguła rodzica
rodzic(X, Y) :- ojciec(X, Y).
rodzic(X, Y) :- matka(X, Y).

%Reguła przodka
przodek(X, Y) :- rodzic(X, Y).
przodek(X, Y) :- rodzic(X, Z), przodek(Z, Y).


%Reguła siostry
siostra(X, Y) :-
    rodzic(Z, X),
    rodzic(Z, Y),
    X\= Y,
    kobieta(X).

%Reguła brata
brat(X, Y) :-
    rodzic(Z, X),
    rodzic(Z, Y),
    X\= Y,
    mezczyzna(X).

%Reguła kuzyna
kuzyn(X, Y) :-
    (ojciec(A, X), ojciec(B, Y);
    ojciec(A, X), matka(B, Y);
    matka(A, X), ojciec(B, Y);
    matka(A, X), matka(B, Y)),
    (brat(A, B);
    siostra(A, B)).

Wyniki:
przodek(robert, alek).
true

siostra(ania, kasia).
true

siostra(alek, kasia).
false

brat(alek, kasia).
true

brat(alek, ala).
false

kuzyn(alek, iwona).
true

kuzyn(alek, ala).
false





Zadanie 2
%Fakty lotniska
lotnisko(lublin).
lotnisko(warszawa).
lotnisko(wroclaw).
lotnisko(londyn).
lotnisko(gdansk).
lotnisko(madryt).
lotnisko(paryz).
lotnisko(oslo).

%Fakty połączenia i koszty
polaczenie(lublin, warszawa, 100).
polaczenie(warszawa, londyn, 500).
polaczenie(londyn, paryz, 600).
polaczenie(gdansk, wroclaw, 200).
polaczenie(madryt, oslo, 700).
polaczenie(wroclaw, oslo, 450).
polaczenie(paryz, lublin, 350).

%Reguła bezpośredniego połączenia
bezposrednie_polaczenie(X, Y) :- polaczenie(X, Y, _).
bezposrednie_polaczenie(X, Y) :- polaczenie(Y, X, _).

%Reguła koszt podróży między bezpośrednimi lotniskami
koszt_podrozy(X, Y, C) :- polaczenie(X, Y, C).
koszt_podrozy(X, Y, C) :- polaczenie(Y, X, C).

%Reguła najkrótsza droga (opcja 1)
najkrotsza_droga(X, Y, Trasa, Koszt) :-
    findall([C, T], trasa(X, Y, T, C), Wyniki),
    sort(Wyniki, [[Koszt, Trasa] | _]).

%Reguła wyznaczenie trasy (rekurencyjnie)
trasa(X, Y, [X, Y], C) :- polaczenie(X, Y, C).
trasa(X, Y, [X | ResztaTrasy], C) :-
    polaczenie(X, Z, C1),
    Z \= Y,
    trasa(Z, Y, ResztaTrasy, C2),
    \+ member(X, ResztaTrasy), % Zapobieganie cyklom
    C is C1 + C2, !.

%Reguła najkrótsza trasa (opcja 2 użycie algorytmu DFS z wagami)
%najkrotsza_droga(X, Y, Trasa, Koszt) :-
%    dfs(X, Y, [X], Trasa, Koszt). %Głowna reguła, X oznacza początkowe lotnisko, Y lotnisko docelowe, [X] to początkowa liczba odwiedzanych lotnisk
%   Trasa to ostateczna trasa od X do Y, Koszt to łączny koszt podróży, ta reguła wywołuje rekurencyjną procedurę dfs, która realizuje wyszukiwanie trasy.
%
%dfs(X, X, Trasa, Trasa, 0). %Koniec rekurencji, gdy aktualne lotnisko X jest docelowym lotniskiem X wyszukiwanie się kończy, trasa pozostaje taka sama, koszt wynosi 0 bo nie ma dalszych połączeń
%dfs(X, Y, Historia, Trasa, Koszt) :-
%    polaczenie(X, Z, C), %Sprawdza czy istnieje bezpośrednie połączenie z X do Z o koszcie C
%    \+ member(Z, Historia), % Zapobiega cyklom, ponownemu odwiedzenia lotniska Z
%    dfs(Z, Y, [Z|Historia], Trasa, KosztOgon), %Rekurencyjnie wywołuje dfs, dodając Z do historii odwiedzonych lotnisk.
%    Koszt is C + KosztOgon. %Łączny Koszt obliczany jest jako suma kosztu bieżącego połączenia C i kosztu trasy z Z do Y (KosztOgon).
    
Wyniki:
bezposrednie_polaczenie(warszawa, lublin).
true

bezposrednie_polaczenie(warszawa, oslo).
false

koszt_podrozy(paryz, lublin, C).
C = 350

najkrotsza_droga(lublin, londyn, Trasa, Koszt).
Koszt = 600,
Trasa = [londyn, warszawa, lublin]




Zadanie 3 (SWI)
:- dynamic produkt/3.

% Fakty
produkt(ksiazka, 50, sektor_a).
produkt(dlugopis, 200, sektor_b).
produkt(zeszyt, 120, sektor_c).
produkt(kalkulator, 0, sektor_d).
produkt(plecak, 15, sektor_e).

% Reguła dostępność w magazynie
dostepny_produkt(X) :-
    produkt(X, Ilosc, _),
    Ilosc > 0.

% Reguła przeniesienie do innego sektora
przenies_produkt(X, NowySektor) :-
    produkt(X, Ilosc, _),
    retract(produkt(X, Ilosc, _)),
    assertz(produkt(X, Ilosc, NowySektor)).

% Reguła uzupelnienie stanu
uzupelnij_stan(X, N) :-
    produkt(X, Ilosc, Sektor),
    retract(produkt(X, Ilosc, Sektor)),
    NowaIlosc is Ilosc + N,
    assertz(produkt(X, NowaIlosc, Sektor)).

Wyniki:

dostepny_produkt(zeszyt).
true

przenies_produkt(zeszyt, sektor_a).
true

uzupelnij_stan(kalkulator, 15).
true




Zadanie 4
:- dynamic polaczenie/3.

%Fakty
polaczenie(warszawa, lodz, 9).
polaczenie(lodz, lublin, 4).
polaczenie(lublin, gdansk, 8).
polaczenie(warszawa, krakow, 6).
polaczenie(krakow, gdansk, 10).
polaczenie(lodz, krakow, 7).

%Reguła bezpośrednie połączenie
droga(X, Y) :-
    polaczenie(X, Y, _).
droga(X, Y) :-
    polaczenie(Y, X, _).

%Reguła czas przejazdu
czas_przejazdu(X, Y, T) :-
    polaczenie(X, Y, T).
czas_przejazdu(X, Y, T) :-
    polaczenie(Y, X, T).
czas_przejazdu(X, Y, T) :-
    polaczenie(X, Z, T1),
    czas_przejazdu(Z, Y, T2),
    T is T1 + T2.

%Reguła najkrótsza trasa
najkrotsza_trasa(X, Y, Trasa, Czas) :-
    setof((CzasTmp, TrasaTmp), znajdz_trase(X, Y, [X], TrasaTmp, CzasTmp), [(Czas, Trasa) | _]).

%Znajdowanie jednej trasy i jej czasu
znajdz_trase(X, X, Sciezka, Sciezka, 0).
znajdz_trase(X, Y, Historia, Trasa, Czas) :-
    polaczenie(X, Z, CzasOdcinka),
    \+ member(Z, Historia),
    znajdz_trase(Z, Y, [Z | Historia], Trasa, CzasReszty),
    Czas is CzasOdcinka + CzasReszty.

Wyniki:
droga(lublin, warszawa).
false

czas_przejazdu(lodz, gdansk, T).
T = 12

najkrotsza_trasa(warszawa, gdansk, Trasa, Czas).
Czas = 16,
Trasa = [gdansk, krakow, warszawa]



Zadanie 5 (SWI)

:- dynamic ksiazka/3.
:- dynamic ocena/3.
:- dynamic preferencja/2.

%Fakty
ksiazka('Harry Potter', 'J.K. Rowling', fantasy).
ksiazka('Władca Pierścieni', 'J.R.R. Tolkien', fantasy).
ksiazka('Zbrodnia i kara', 'Fiodor Dostojewski', powiesc).
ksiazka('1984', 'George Orwell', powiesc).
ksiazka('Hobbit', 'J.R.R. Tolkien', fantasy).
ksiazka('Lalka', 'Boleslaw Prus', powiesc).
ocena(jan, 'Harry Potter', 4).
ocena(jan, 'Władca Pierścieni', 5).
ocena(jan, 'Zbrodnia i kara', 3).
ocena(jan, '1984', 4).
ocena(jan, 'Hobbit', 3).
ocena(jan, 'Lalka', 2).
ocena(anna, 'Harry Potter', 4).
ocena(anna, 'Władca Pierścieni', 3).
ocena(anna, 'Zbrodnia i kara', 5).
ocena(anna, '1984', 5).
ocena(anna, 'Hobbit', 2).
ocena(anna, 'Lalka', 3).
preferencja(jan, fantasy).
preferencja(anna, powiesc).

%Reguła polecane książki
polecane_ksiazki(Uzytkownik, Gatunek, ListaKsiazek) :-
    findall(Tytul,
        (preferencja(Uzytkownik, Gatunek),
         ksiazka(Tytul, _, Gatunek),
         ocena(Uzytkownik, Tytul, Ocena),
         Ocena >= 4),
        ListaKsiazek).

%Reguła dodawanie nowej oceny
dodaj_ocene(Uzytkownik, Tytul, Ocena) :-
    retractall(ocena(Uzytkownik, Tytul, _)),  
    assertz(ocena(Uzytkownik, Tytul, Ocena)).

Wyniki:
polecane_ksiazki(jan, fantasy, Lista).
Lista = ['Harry Potter', 'Władca Pierścieni']

dodaj_ocene(jan, 'Hobbit', 4).
true

polecane_ksiazki(jan, fantasy, Lista).
Lista = ['Harry Potter', 'WÅ‚adca PierÅ›cieni', 'Hobbit'].
