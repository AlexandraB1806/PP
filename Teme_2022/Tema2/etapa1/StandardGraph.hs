{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns, S.fromList es)

{-
    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = fst

{-
    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = snd

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
{-
 Prin constructia (snd graph) accesez muchiile grafului dat ca parametru.
Scopul este sa pastrez doar acele muchii din graf care respecta pattern-ul (nod, _)
(unde nod = nodul din care pornesc arcele), deci aplic functionala filter pentru seturi.
Odata ce am filtrat muchiile potrivite, voi alege nodurile destinatie.
-}
outNeighbors node graph = S.map snd $ S.filter (\ (n, _) -> n == node) (snd graph)

{-
    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
-- Asemanator ca la functia anterioara, doar ca ma intereseaza nodurile sursa.
inNeighbors node graph = S.map fst $ S.filter (\ (_, n) -> n == node) (snd graph)

{-
    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}

removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph =
    -- Daca nodul exista in graf
    if (S.member node (fst graph))
        -- Voi lua seturi separate in care voi pastra elementele ramase
        then let remaining_nodes = S.filter (\ n -> n /= node) (fst graph)
                 -- Tratez cele 2 cazuri in care poate aparea un nod
                 remaining_edges_src = S.filter (\ (n, _) -> n /= node) (snd graph)
                 remaining_edges_dest = S.filter (\ (_, n) -> n /= node) (snd graph)
                 -- Fac intersectia celor 2 seturi pentru a avea setul final
                 remaining_edges = S.intersection remaining_edges_src remaining_edges_dest
                 -- Formez noul graf
             in (remaining_nodes, remaining_edges)
    -- Nodul nu exista in graf, deci returnez graful neschimbat
    else graph

{-
    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news graph =
    -- Daca lista de noduri in care va fi impartit nodul vechi nu este vida
    if (news /= [])
        then let out_neighbours = outNeighbors old graph -- (X, vecin)
                 in_neighbours = inNeighbors old graph -- (vecin, X)
                 old_nodes = fst $ removeNode old graph
                 old_edges = snd $ removeNode old graph
                 nodes_to_add = S.fromList news
                 -- Nodurile noului graf = reuniunea dintre nodurile ramase dupa eliminarea nodului
                 -- impartit si nodurile de adaugat
                 new_nodes = S.union old_nodes nodes_to_add
                 {-
                 Atat pentru vecinii in care nodul intra, cat si pentru cei din care nodul iese,
                 voi face produsul cartezian intre nodurile ce vor fi inserate si acesti vecini.
                 Muchiile noului graf = reuniunea dintre muchiile ramase dupa eliminarea nodului
                 impartit si muchiile nou formate
                 -}
                 new_edges = S.union old_edges
                            (S.union (S.cartesianProduct nodes_to_add out_neighbours) (S.cartesianProduct in_neighbours nodes_to_add))
             in (new_nodes, new_edges)
    -- Altfel, doar elimin nodul vechi si arcele in care era implicat
    else removeNode old graph

{-
    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph =
    let initial_nodes = fst graph
        -- Nodurile care respecta propietatea impusa sunt salvate separat intr-un alt set
        nodes_to_merge = S.filter prop (fst graph)
        new_intermediate_nodes = S.difference initial_nodes nodes_to_merge -- Practic, elimin nodurile care trebuie imbinate
    -- Daca in urma aplicarii propietatii ce trebuie respectate de nodurile ce vor fi imbinate,
    -- nodurile ramase difera de cele initiale
    in  if (initial_nodes /= new_intermediate_nodes)
            then        
                let new_nodes = S.insert node new_intermediate_nodes -- Adaug si nodul ce reprezinta imbinarea nodurilor eliminate
                    initial_edges = snd graph
                    edges_to_delete_out = S.filter (\ (elem, _) -> S.member elem nodes_to_merge) initial_edges
                    edges_to_delete_in = S.filter (\ (_, elem) -> S.member elem nodes_to_merge) initial_edges
                    -- Nodurile care trebuie inlocuite
                    edges_to_delete = S.union edges_to_delete_out edges_to_delete_in
                    -- Nodurile care nu trebuie sterse din graful initial
                    remaining_edges_from_initial = S.difference initial_edges edges_to_delete
                    -- Prima oara parcurg muchiile care trebuie sterse si verific atat pentru primul membru al unei perechi,
                    -- cat si pentru cel de-al doilea daca se afla in setul de noduri ce respecta propietatea impusa
                    equal_new_edges = S.map (\ (elem1, elem2) -> if (S.member elem1 nodes_to_merge && S.member elem2 nodes_to_merge) then
                                                                 (node, node) else (elem1, elem2)) edges_to_delete
                    -- Parcurg muchiile actualizate anterior (equal_new_edges) si schimb primul element al perechii daca este cazul
                    out_new_edges = S.map (\ (elem1, elem2) -> if (S.member elem1 nodes_to_merge) then (node, elem2) else (elem1, elem2)) equal_new_edges
                    -- Parcurg muchiile actualizate anterior (out_new_edges) si schimb al doilea element al perechii daca este cazul
                    in_new_edges = S.map (\ (elem1, elem2) -> if (S.member elem2 nodes_to_merge) then (elem1, node) else (elem1, elem2)) out_new_edges
                    -- Noilea muchii sunt reprezentate de cele care trebuie lasate nemodificate din graful initial + cele care au fost modificate
                    new_edges = S.union remaining_edges_from_initial in_new_edges
                in (new_nodes, new_edges)
        else graph
