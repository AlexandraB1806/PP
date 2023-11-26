module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)

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
edges (Overlay first_graph second_graph) = S.union (edges first_graph) (edges second_graph)
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
    | S.member node (nodes first_graph) = nodes second_graph
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
    | S.member node (nodes second_graph) = S.union (nodes first_graph) (inNeighbors node second_graph)
    | S.notMember node (nodes second_graph) = S.empty


{-
    Instanțiați clasa Num cu tipul (AlgebraicGraph a), astfel încât:
    - un literal întreg să fie interpretat ca un singur nod cu eticheta egală
      cu acel literal
    - operația de adunare să fie intepretată ca Overlay
    - operația de înmulțire să fie interpretată drept Connect.

    Celelalte funcții din clasă nu sunt relevante. Veți obține warning-uri
    pentru neimplementarea lor, dar puteți să le ignorați.

    După instanțiere, veți putea evalua în consolă expresii ca:

    > 1 :: AlgebraicGraph Int
    Node 1
    
    > 1*(2+3) :: AlgebraicGraph Int
    Connect (Node 1) (Overlay (Node 2) (Node 3))
-}
instance Num a => Num (AlgebraicGraph a) where
    fromInteger a = Node (fromInteger a)
    (+) = Overlay
    (*) = Connect

{-
    Instanțiați clasa Show cu tipul (AlgebraicGraph a), astfel încât
    reprezentarea sub formă de șir de caractere a unui graf să reflecte
    expresiile aritmetice definite mai sus. Puteți pune un nou rând de paranteze
    la fiecare subexpresie compusă.

    Exemple:

    > Node 1
    1

    > Connect (Node 1) (Overlay (Node 2) (Node 3))
    (1*(2+3))
-}
instance Show a => Show (AlgebraicGraph a) where
    show Empty = " "
    show (Node g) = show g
    show (Overlay first_graph second_graph) = "(" ++ (show first_graph) ++ "+" ++ (show second_graph) ++ ")"
    show (Connect first_graph second_graph) = "(" ++ (show first_graph) ++ "*" ++ (show second_graph) ++ ")"

{-
    Observați că instanța predefinită de Eq pentru tipul (AlgebraicGraph a)
    nu surprinde corect egalitatea a două grafuri, deoarece același graf
    conceptual poate avea două descrieri simbolice diferite.
    
    Prin urmare, instanțiați clasa Eq cu tipul (AlgebraicGraph a), astfel încât
    să comparați propriu-zis mulțimile de noduri și de arce.

    Exemple:

    > Node 1 == 1
    True

    > Node 1 == 2
    False

    > angle == 1*2 + 1*3
    True

    > triangle == (1*2)*3
    True
-}
instance Ord a => Eq (AlgebraicGraph a) where
    g1 == g2 = (nodes g1 == nodes g2) && (edges g1 == edges g2)

{-
    Extinde un graf existent, atașând noi subgrafuri arbitrare în locul nodurilor
    individuale. Funcția primită ca prim parametru determină această
    corespondență între noduri și subgrafuri. Observați că tipul etichetelor
    noi (b) poate diferi de al etichetelor vechi (a).

    Exemplu:

    > extend (\n -> if n == 1 then 4+5 else Node n) $ 1*(2+3)
    ((4+5)*(2+3))
-}
extend :: (a -> AlgebraicGraph b) -> AlgebraicGraph a -> AlgebraicGraph b
extend f Empty = Empty
extend f (Node a) = f a
extend f (Overlay g1 g2) = Overlay (extend f g1) (extend f g2)
extend f (Connect g1 g2) = Connect (extend f g1) (extend f g2)

{-
    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Implementați splitNode folosind extend!
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
{-
Primesc pe rand nodurile din targets. Daca nu avem noduri cu care sa inlocuim
nodul de splituit, returnam graful gol. In caz contrar, voi lua de fiecare
data primul nod din lista targets, voi face graf din el si voi aplica
recursiv functia splitNode pe restul listei si graful existent.
Daca nu am ajuns la nodul de splituit, il lasam nemodificat.
-}
splitNode node targets = extend (\ n -> if (n == node) then (if (null targets)
                                                            then Empty
                                                            else Overlay (Node (head targets)) (splitNode node (tail targets) (Node n)))
                                                        else (Node n))

{-
    Instanțiați clasa Functor cu constructorul de tip AlgebraicGraph, astfel
    încât să puteți aplica o funcție pe toate etichetele unui graf.
    fmap reprezintă generalizarea lui map pentru orice fel de structură.

    Implementați fmap folosind extend!

    Exemplu:

    > fmap (+ 10) $ 1*(2+3) :: AlgebraicGraph Int
    (11*(12+13))
-}
instance Functor AlgebraicGraph where
    -- fmap :: (a -> b) -> AlgebraicGraph a -> AlgebraicGraph b
    fmap f graph = extend (\ elem -> Node (f elem)) graph

{-
    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Implementați mergeNodes folosind fmap!
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node = fmap (\ n -> if prop n then node else n)

{-
    Filtrează un graf, păstrând doar nodurile care satisfac proprietatea dată.

    Implementați filterGraph folosind extend!
    
    Exemplu:

    > nodes $ filterGraph odd $ 1*(2+3)
    fromList [1,3]

    > edges $ filterGraph odd $ 1*(2+3)
    fromList [(1,3)]
-}
filterGraph :: (a -> Bool) -> AlgebraicGraph a -> AlgebraicGraph a
filterGraph prop graph = extend (\ elem -> if prop elem then (Node elem) else Empty) graph

{-
    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Implementați removeNode folosind filterGraph!
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = filterGraph (\ n -> n /= node) graph