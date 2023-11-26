{-# LANGUAGE MultiParamTypeClasses #-}

module TestGraph where

import TestPP
import qualified Data.Set as S
import AlgebraicGraph
import Modular
import Data.Function (on)


testGraph1 :: AlgebraicGraph Int
testGraph1 = Overlay
                (Connect
                    (Node 1)
                    (Overlay (Node 2) (Node 3))
                )
                (Node 4)

testGraph2 :: AlgebraicGraph Int
testGraph2 = Overlay
                    (Connect (Node 1) (Node 2))
                    (Overlay
                        (Connect (Node 3) (Node 4))
                        (Overlay
                            (Connect (Node 4) (Node 5))
                            (Connect (Node 5) (Node 3))
                        )
                    )

testGraph3 :: AlgebraicGraph Int
testGraph3 = Overlay
                    (Connect
                        (Node 1)
                        (Overlay
                            (Node 4)
                            (Connect (Node 2) (Node 3))
                        )
                    )
                    (Connect (Node 4) (Node 1))

testGraph4 :: AlgebraicGraph Int
testGraph4 = Overlay
                    (Overlay 
                        (Connect
                            (Node 1)
                            (Connect (Node 2) (Node 3))
                        )
                        (Connect
                            (Node 3)
                            (Connect (Node 4) (Node 5))
                        )
                    )
                    (Connect (Node 5) (Node 1))

testGraph5 :: AlgebraicGraph Int
testGraph5 = Overlay
                    (Overlay
                        (Overlay
                            (Connect
                                (Node 1)
                                (Overlay (Node 2) (Node 6))
                            )
                            (Overlay
                                (Connect
                                    (Node 6)
                                    (Overlay (Node 3) (Node 7))
                                )
                                (Connect
                                    (Overlay (Node 3) (Node 7))
                                    (Node 5)
                                )
                            )
                        )
                        (Connect (Node 2) (Node 3))
                    )
                    (Node 4)

testGraph6 :: AlgebraicGraph Int
testGraph6 = Overlay
                    (Overlay
                        (Connect
                            (Node 1)
                            (Overlay
                                (Node 2)
                                (Overlay (Node 3) (Node 4))
                            )
                        )
                        (Overlay
                            (Connect (Node 2) (Node 5))
                            (Connect (Node 3) (Node 6))
                        )
                    )
                    (Overlay
                        (Connect (Node 4) (Node 7))
                        (Connect (Node 6) (Node 8))
                    )

testGraph7 :: AlgebraicGraph Int
testGraph7 = Overlay
                    (Connect
                        (Node 1)
                        (Overlay
                            (Connect (Node 2) (Node 4))
                            (Node 3)
                        )
                    )
                    (Connect (Node 4) (Node 1))
        
testNodes :: TestData
testNodes = tests 1 10
    [
        testVal "test nodes 1" (S.fromList [1, 2, 3, 4]) (nodes testGraph1),
        testVal "test nodes 2" (S.fromList [1, 2, 3, 4, 5]) (nodes testGraph2),
        testVal "test nodes 3" (S.fromList [1, 2, 3, 4]) (nodes testGraph3),
        testVal "test nodes 4" (S.fromList [1, 2, 3, 4,5 ]) (nodes testGraph4),
        testVal "test nodes 5" (S.fromList [1, 2, 3, 4, 5, 6, 7]) (nodes testGraph5),
        testVal "test nodes 6" (S.fromList [1, 2, 3, 4, 5, 6, 7, 8]) (nodes testGraph6),
        testVal "test nodes 7" (S.fromList [1, 2, 3, 4]) (nodes testGraph7)
    ]
    
testEdges :: TestData
testEdges = tests 2 10
    [
        testVal "test edges 1" (S.fromList [(1, 2),(1, 3)]) (edges testGraph1),
        testVal "test edges 2" (S.fromList [(1,2),(3,4),(4,5),(5,3)]) (edges testGraph2),
        testVal "test edges 3" (S.fromList [(1,2),(1,3),(1,4),(2,3),(4,1)]) (edges testGraph3),
        testVal "test edges 4" (S.fromList [(1,2),(1,3),(2,3),(3,4),(3,5),(4,5),(5,1)]) (edges testGraph4),
        testVal "test edges 5" (S.fromList [(1,2),(1,6),(2,3),(3,5),(6,3),(6,7),(7,5)]) (edges testGraph5),
        testVal "test edges 6" (S.fromList [(1,2),(1,3),(1,4),(2,5),(3,6),(4,7),(6,8)]) (edges testGraph6),
        testVal "test edges 7" (S.fromList [(1,2),(1,3),(1,4),(2,4),(4,1)]) (edges testGraph7)
    ]

testOutNeighbors :: TestData
testOutNeighbors = tests 3 15
    [ 
        testVal "test outNeighbors 1" (S.fromList [2, 3]) (outNeighbors 1 testGraph1),
        testVal "test outNeighbors 2" (S.fromList []) (outNeighbors 3 testGraph1),
        testVal "test outNeighbors 3" (S.fromList [5]) (outNeighbors 4 testGraph2),
        testVal "test outNeighbors 4" (S.fromList [2, 3, 4]) (outNeighbors 1 testGraph3),
        testVal "test outNeighbors 5" (S.fromList []) (outNeighbors 3 testGraph3),
        testVal "test outNeighbors 6" (S.fromList [2, 6]) (outNeighbors 1 testGraph5)   
    ]

testInNeighbors :: TestData
testInNeighbors = tests 4 15
    [ 
        testVal "test inNeighbors 1" (S.fromList []) (inNeighbors 1 testGraph1),
        testVal "test inNeighbors 2" (S.fromList [1]) (inNeighbors 3 testGraph1),
        testVal "test inNeighbors 3" (S.fromList [3]) (inNeighbors 4 testGraph2),
        testVal "test inNeighbors 4" (S.fromList [4]) (inNeighbors 1 testGraph3),
        testVal "test inNeighbors 5" (S.fromList [1, 2]) (inNeighbors 3 testGraph3),
        testVal "test inNeighbors 6" (S.fromList [5]) (inNeighbors 1 testGraph4)   
    ]
    
makeTestUnit :: Ord a => AlgebraicGraph a -> (S.Set a, S.Set (a, a))
makeTestUnit g = (nodes g, edges g)

testRemoveNode :: TestData
testRemoveNode = tests 5 15
    [ 
        testVal "test removeNode 1" (makeTestUnit res1) (makeTestUnit (removeNode 1 testGraph1)),
        testVal "test removeNode 2" (makeTestUnit res2) (makeTestUnit (removeNode 3 testGraph1)),
        testVal "test removeNode 3" (makeTestUnit res3) (makeTestUnit (removeNode 4 testGraph2)),
        testVal "test removeNode 4" (makeTestUnit res4) (makeTestUnit (removeNode 2 testGraph3)),
        testVal "test removeNode 5" (makeTestUnit res5) (makeTestUnit (removeNode 1 testGraph3)),
        testVal "test removeNode 6" (makeTestUnit res6) (makeTestUnit (removeNode 5 testGraph4))   
    ]
    where
        res1 = Overlay (Node 2) (Overlay (Node 3) (Node 4))
        res2 = Overlay (Connect (Node 1) (Node 2)) (Node 4)
        res3 = Overlay (Connect (Node 1) (Node 2)) (Connect (Node 5) (Node 3))
        res4 = Overlay (Connect (Node 1) (Overlay (Node 3) (Node 4))) (Connect (Node 4) (Node 1))
        res5 = Overlay (Connect (Node 2) (Node 3)) (Node 4)
        res6 = Overlay (Connect (Node 1) (Connect (Node 2) (Node 3))) (Connect (Node 3) (Node 4))


testSplitNode :: TestData
testSplitNode = tests 6 20
    [ 
        testVal "test splitNode 1" (makeTestUnit res1) (makeTestUnit (splitNode 1 [5, 6, 7] testGraph1)),
        testVal "test splitNode 2" (makeTestUnit res2) (makeTestUnit(splitNode 4 [5, 6] testGraph1)),
        testVal "test splitNode 3" (makeTestUnit res3) (makeTestUnit (splitNode 3 [6, 7] testGraph2)),
        testVal "test splitNode 4" (makeTestUnit res4) (makeTestUnit (splitNode 2 [] testGraph3)),
        testVal "test splitNode 5" (makeTestUnit res5) (makeTestUnit (splitNode 3 [5, 6] testGraph3)),
        testVal "test splitNode 6" (makeTestUnit res6) (makeTestUnit (splitNode 5 [7] testGraph4))   
    ]
    where
        res1 = Overlay (Overlay (Overlay (Connect (Node 7) (Overlay (Node 2) (Node 3))) (Connect (Node 6) (Overlay (Node 2) (Node 3)))) (Connect (Node 5) (Overlay (Node 2) (Node 3)))) (Node 4)
        res2 = Overlay (Connect (Node 1) (Overlay (Node 2) (Node 3))) (Overlay (Node 5) (Node 6))
        res3 = Overlay (Connect (Node 1) (Node 2)) (Overlay (Connect (Overlay (Node 6) (Node 7)) (Node 4)) (Overlay (Connect (Node 4) (Node 5)) (Connect (Node 5) (Overlay (Node 6) (Node 7)))))
        res4 = Overlay (Connect (Node 1) (Overlay (Node 3) (Node 4))) (Connect (Node 4) (Node 1))
        res5 = Overlay (Overlay (Connect (Connect (Node 1) (Node 2)) (Overlay (Node 5) (Node 6))) (Connect (Node 4) (Node 1))) (Connect (Node 1) (Node 4))
        res6 = Overlay (Overlay (Connect (Node 1) (Connect (Node 2) (Node 3))) (Connect (Node 3) (Connect (Node 4) (Node 7)))) (Connect (Node 7) (Node 1))


testMergeNodes :: TestData
testMergeNodes = tests 7 15
    [ 
        testVal "test mergeNodes 1" (makeTestUnit res1) (makeTestUnit (mergeNodes (\x -> elem x [2, 3]) 5 testGraph1)),
        testVal "test mergeNodes 2" (makeTestUnit res2) (makeTestUnit (mergeNodes (\x -> elem x [1, 2]) 5 testGraph1)),
        testVal "test mergeNodes 3" (makeTestUnit res3) (makeTestUnit (mergeNodes (\x -> elem x []) 7 testGraph2)),
        testVal "test mergeNodes 4" (makeTestUnit res4) (makeTestUnit (mergeNodes (\x -> elem x [1, 2, 3]) 5 testGraph3)),
        testVal "test mergeNodes 5" (makeTestUnit res5) (makeTestUnit (mergeNodes (\x -> elem x [1, 4]) 5 testGraph3)),
        testVal "test mergeNodes 6" (makeTestUnit res6) (makeTestUnit (mergeNodes (\x -> elem x [1, 2, 3]) 6 testGraph4))   
    ]
    where
        res1 = Overlay (Connect (Node 1) (Node 5)) (Node 4)
        res2 = Overlay (Connect (Node 5) (Overlay (Node 3) (Node 5))) (Node 4)
        res3 = Overlay (Connect (Node 1) (Node 2)) (Overlay (Connect (Node 3) (Node 4)) (Overlay (Connect (Node 4) (Node 5)) (Connect (Node 5) (Node 3))))
        res4 = Connect (Node 5) (Connect (Node 4) (Node 5))
        res5 = Connect (Node 5) (Overlay (Node 5) (Connect (Node 2) (Node 3)))
        res6 = Connect (Node 6) (Overlay (Connect (Node 4) (Node 5)) (Connect (Node 5) (Node 6)))


testMapSingle :: TestData
testMapSingle = tests 8 10
    [
        testVal "test mapSingle 1" [[0,2,3,4,5],[1,0,3,4,5],[1,2,0,4,5],[1,2,3,0,5],[1,2,3,4,0]] (mapSingle (*0) [1,2,3,4,5]),
        testVal "test mapSingle 2" [[51]] (mapSingle (+50) [1]),
        testVal "test mapSingle 3" [["not lucky","ion","carusel","germania","zar","strings"],
                                    ["ana","not lucky","carusel","germania","zar","strings"],
                                    ["ana","ion","lucky","germania","zar","strings"],
                                    ["ana","ion","carusel","not lucky","zar","strings"],
                                    ["ana","ion","carusel","germania","not lucky","strings"],
                                    ["ana","ion","carusel","germania","zar","lucky"]] (mapSingle (\x -> if length x == 7 then "lucky" else "not lucky") ["ana", "ion", "carusel", "germania", "zar", "strings"]),
        testVal "testMapSingle 4"  ["ttring","suring","stsing","strjng","striog","strinh"] (mapSingle (toEnum . (+1) . fromEnum) "string"),
        testVal "testMapSingle 5" [] (mapSingle (+10) [])
    ]

testPartitions :: TestData
testPartitions = tests 9 10
    [
        testWith "test partitions 1" "valoarea" "coincide" [[[1]]] f (partitions [1]),
        testWith "test partitions 2" "valoarea" "coincide" [[[1],[2]],[[1,2]]] f (partitions [1,2]),
        testWith "test partitions 3" "valoarea" "coincide" [[[1],[2],[3]],[[1,2],[3]],[[2],[1,3]],[[1],[2,3]],[[1,2,3]]] f (partitions [1,2,3])
    ]
    where
        f = (==) `on` toSets

toSets :: Ord a => [[[a]]] -> S.Set (S.Set (S.Set a))
toSets = S.fromList . map (S.fromList . map S.fromList)

checkAll = vmCheck $ algebraicGraph ++ modular
    where
        algebraicGraph = [testNodes, testEdges, testOutNeighbors, testInNeighbors, testRemoveNode, testSplitNode, testMergeNodes]
        modular = [testMapSingle, testPartitions]