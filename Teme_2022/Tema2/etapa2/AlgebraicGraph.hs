module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node node) = S.singleton node
nodes (Overlay first_graph second_graph) = S.union (nodes first_graph) (nodes second_graph)
nodes (Connect first_graph second_graph) = S.union (nodes first_graph) (nodes second_graph)


{-
    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node node) = S.empty
-- Reuniunea dintre muchiile existente in cele 2 grafuri
edges (Overlay first_graph second_graph) = S.union (edges first_graph) (edges second_graph)
-- Fac produsul cartezian intre nodurile din graf pentru a forma legaturile. Rezultatul este
-- reprezentat de aceste legaturi formate si muchiile deja existente in cele 2 grafuri.
edges (Connect first_graph second_graph) = S.union (edges (Overlay first_graph second_graph))
                                                    (S.cartesianProduct (nodes first_graph) (nodes second_graph))

{-
    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors _ Empty = S.empty
outNeighbors node (Node n) = S.empty
outNeighbors node (Overlay first_graph second_graph) = S.union (outNeighbors node first_graph) (outNeighbors node second_graph)
outNeighbors node (Connect first_graph second_graph)
    -- Daca nodul este prezent in primul graf, stiu ca are conexiuni spre al doilea,
    -- deci returnez nodurile prezente in al doilea
    | S.member node (nodes first_graph) = nodes second_graph
    -- Daca nodul nu este prezent in primul graf, nu are conexiuni catre al doilea
    | S.notMember node (nodes first_graph) = S.empty

{-
    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors _ Empty = S.empty
inNeighbors node (Node n) = S.empty
inNeighbors node (Overlay first_graph second_graph) = S.union (inNeighbors node first_graph) (inNeighbors node second_graph)
inNeighbors node (Connect first_graph second_graph)
    -- Exista conexiuni de la al doilea graf catre primul, deci o sa returnez nodurile din primul
    -- graf, dar voi cauta in continuare recursiv pe al doilea graf
    | S.member node (nodes second_graph) = S.union (nodes first_graph) (inNeighbors node second_graph)
    -- Daca nodul nu este prezent in al doilea graf, nu are conexiuni catre primul
    | S.notMember node (nodes second_graph) = S.empty

{-
    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph =
    -- Tratez, in cadrul functiei auxiliare, fiecare caz posibil pentru graful de tip algebric.
    let
        removeHelper Empty = Empty
        -- Cazul principal este cand ajungem la un nod din graf. Verific egalitatea acestuia
        -- cu nodul pe care vreau sa il elimin. Daca este diferit, il pastrez, iar in caz
        -- contrar, pun Empty in locul sau; automat se sterg si muchiile catre care nodul respectiv
        -- avea conexiuni.
        removeHelper (Node toRemove) = if (toRemove == node) then Empty else (Node toRemove)
        removeHelper (Overlay first_graph second_graph) = Overlay (removeHelper first_graph) (removeHelper second_graph)
        removeHelper (Connect first_graph second_graph) = Connect (removeHelper first_graph) (removeHelper second_graph)
    -- Am creat o functie auxiliara pe care o aplic doar pe graf (poate varia)
    in removeHelper graph

{-
    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news graph =
    let
        splitHelper Empty = Empty
        splitHelper (Node toSplit) = if (toSplit /= old) then (Node toSplit) else foldl (\ acc elem -> Overlay (Node elem) acc) Empty news
        splitHelper (Overlay first_graph second_graph) = Overlay (splitHelper first_graph) (splitHelper second_graph)
        splitHelper (Connect first_graph second_graph) = Connect (splitHelper first_graph) (splitHelper second_graph)
    in splitHelper graph

{-
    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph =
    let
        mergeHelper Empty = Empty
        -- Daca nodul gasit indeplineste propietatea, atunci poate fi inlocuit cu noul nod.
        -- Altfel, las nodul neschimbat.
        mergeHelper (Node toBeReplaced) = if (prop toBeReplaced == True) then (Node node) else (Node toBeReplaced)
        mergeHelper (Overlay first_graph second_graph) = Overlay (mergeHelper first_graph) (mergeHelper second_graph)
        mergeHelper (Connect first_graph second_graph) = Connect (mergeHelper first_graph) (mergeHelper second_graph)
    in mergeHelper graph
