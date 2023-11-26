module Algorithms where

import qualified Data.Set as S
import StandardGraph

{-
    În etapa 1, prin graf înțelegem un graf cu reprezentare standard.
    În etapele următoare, vom experimenta și cu altă reprezentare.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Graph a = StandardGraph a

{-
    Funcție generală care abstractizează BFS și DFS pornind dintr-un anumit nod,
    prin funcția de îmbinare a listelor care constituie primul parametru.
    
    Cele două liste primite ca parametru de funcția de îmbinare sunt lista
    elementelor deja aflate în structură (coadă/stivă), respectiv lista
    vecinilor nodului curent, proaspăt expandați.

    Căutarea ar trebui să țină cont de eventualele cicluri.

    Hint: Scrieți o funcție auxiliară care primește ca parametru suplimentar
    o mulțime (set) care reține nodurile vizitate până în momentul curent.
-}

-- Functie auxiliara pentru eliminarea duplicatelor dintr-o lista.
removeDuplicatesRight :: (Eq a) => [a] -> [a]
removeDuplicatesRight l = foldr (\x acc -> if elem x acc then acc else (x : acc)) [] l


searchHelper :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> Graph a              -- graful
       -> [a]                  -- container in care retin nodurile ce urmeaza a fi vizitate (stiva / coada)
       -> S.Set a              -- nodurile vizitate sunt introduse intr-un set
       -> [a]                  -- lista cu nodurile vizitate pentru a le pastra in ordinea corecta
       -> [a]                  -- lista obținută în urma parcurgerii
searchHelper f graph unvisited_nodes visited_nodes visited_nodes_list =
    let current_node = head unvisited_nodes -- extrag din container nodul pe care doresc sa il vizitez
        aux_visited_nodes_set = S.insert current_node visited_nodes -- actualizez setul de noduri vizitate (ordine crescatoare)
        aux_visited_nodes_list = current_node : visited_nodes_list -- in acelasi timp, actualizez si lista cu noduri vizitate (ordinea corecta)
        adj_list = S.toList $ outNeighbors current_node graph -- Creez lista vecinilor nodului curent: (node, neighbour)
        -- Vreau sa pastrez doar nodurile din lista de vecini care inca nu au fost vizitate, deci nu se afla in lista de noduri vizitate
        next_correct_nodes = filter (\ node -> notElem node visited_nodes_list) adj_list
        -- Daca containerul s-a golit, inseamna ca am vizitat toate nodurile din graf deci returnez rezultatul.
        -- In caz contrar, apelez recursiv functia auxiliara creata.
    in if (unvisited_nodes == []) then reverse $ (removeDuplicatesRight visited_nodes_list)
                                  else searchHelper f graph (f unvisited_nodes next_correct_nodes) aux_visited_nodes_set aux_visited_nodes_list

search :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f node graph = searchHelper f graph [node] S.empty []

{-
    Strategia BFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}

-- Noile noduri sunt introduse la final pentru a simula coada.
-- Voi elimina duplicatele din lista noilor vecini ai nodului curent.
bfs :: Ord a => a -> Graph a -> [a]
bfs =
    let f = \ queue next_nodes -> tail $ queue ++ (removeDuplicatesRight next_nodes)
    in search f

{-
    Strategia DFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > dfs 1 graph4 
    [1,2,4,3]
    
    > dfs 4 graph4
    [4,1,2,3]
-}

-- Noile noduri sunt introduse pe rand la inceput pentru a simula stiva.
-- Daca nu mai am noduri de vizitat, atunci returnez ce am in stiva.
dfs :: Ord a => a -> Graph a -> [a]
dfs =
   let f = \ stack next_nodes -> if (next_nodes == []) then (tail stack) else (head next_nodes) : stack
   in search f

{-
    Funcția numără câte noduri intermediare expandează strategiile BFS,
    respectiv DFS, în încercarea de găsire a unei căi între un nod sursă
    și unul destinație, ținând cont de posibilitatea absenței acesteia din graf.
    Numărul exclude nodurile sursă și destinație.

    Modalitatea uzuală în Haskell de a preciza că o funcție poate să nu fie
    definită pentru anumite valori ale parametrului este constructorul de tip
    Maybe. Astfel, dacă o cale există, funcția întoarce
    Just (numărBFS, numărDFS), iar altfel, Nothing.

    Hint: funcția span.

    Exemple:

    > countIntermediate 1 3 graph4
    Just (1,2)

    Aici, bfs din nodul 1 întoarce [1,2,3,4], deci există un singur nod
    intermediar (2) între 1 și 3. dfs întoarce [1,2,4,3], deci sunt două noduri
    intermediare (2, 4) între 1 și 3.

    > countIntermediate 3 1 graph4
    Nothing

    Aici nu există cale între 3 și 1.
-}


countIntermediate :: Ord a
                  => a                 -- nodul sursă
                  -> a                 -- nodul destinație
                  -> StandardGraph a   -- graful
                  -> Maybe (Int, Int)  -- numărul de noduri expandate de BFS/DFS
countIntermediate from to graph =
    let search_bfs = bfs from graph
        search_dfs = dfs from graph
        -- Cum in urma parcurgerii BFS/DFS am aceleasi noduri, este suficient sa verific
        -- ca nodul destinatie se afla in lista obtinuta in urma parcurgerii.
    in if (elem to search_bfs)
                -- Pastrez doar nodurile de la cel sursa pana la cel destinatie, fara a-l include
                -- pe cel destinatie.
        then let res_bfs = fst $ span (\ elem -> elem /= to) search_bfs
                 res_dfs = fst $ span (\ elem -> elem /= to) search_dfs
                 -- Nu pun in calcul nodul sursa
                 len_bfs = length res_bfs - 1
                 len_dfs = length res_dfs - 1
             in Just (len_bfs, len_dfs)
        else Nothing
