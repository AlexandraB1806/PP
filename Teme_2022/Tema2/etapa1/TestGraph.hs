{-# LANGUAGE MultiParamTypeClasses #-}

module TestGraph where

import TestPP
import qualified Data.Set as S
import StandardGraph
import Algorithms


testGraph1 :: StandardGraph Int
testGraph1 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 3)]

testGraph12 :: StandardGraph Int
testGraph12 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

testGraph2 :: StandardGraph Int
testGraph2 = fromComponents [1, 2, 3, 4, 5] [(1, 2), (3, 4), (4, 5), (5, 3)]

testGraph22 :: StandardGraph Int
testGraph22 = fromComponents [1, 2, 3, 5, 1, 4, 4] [(1, 2), (3, 4), (4, 5), (5, 3), (3, 4), (4, 5), (5, 3)]

testGraph3 :: StandardGraph Int
testGraph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

testGraph32 :: StandardGraph Int
testGraph32 = fromComponents [1, 2, 1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (1, 2), (1, 4), (2, 3), (1, 3)]

testGraph4 :: StandardGraph Int
testGraph4 = fromComponents [1, 2, 3, 4, 5] [(1, 2), (1, 3), (2, 3), (3, 4), (4, 5), (5, 1), (3, 5)]

testGraph5 :: StandardGraph Int
testGraph5 = fromComponents [1, 2, 3, 4, 5, 6, 7] [(1, 2), (2, 3), (3, 5), (1, 6), (6, 3), (6, 7), (7, 5)]

testGraph6 :: StandardGraph Int
testGraph6 = fromComponents [1, 2, 3, 4, 5, 6, 7, 8] [(1, 2), (1, 3), (1, 4), (2, 5), (3, 6), (4, 7), (6, 8)]

testGraph7 :: StandardGraph Int
testGraph7 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

testGraphConstruction :: TestData
testGraphConstruction = tests 1 5
    [ 
        testCond "test graph equality 1" (testGraph1 == testGraph12),
        testCond "test graph equality 2" (testGraph2 == testGraph22),
        testCond "test graph equality 3" (testGraph3 == testGraph32)       
    ]
testOutNeighbors :: TestData
testOutNeighbors = tests 2 10
    [ 
        testVal "test outNeighbors 1" (S.fromList [2, 3]) (outNeighbors 1 testGraph1),
        testVal "test outNeighbors 2" (S.fromList []) (outNeighbors 3 testGraph1),
        testVal "test outNeighbors 3" (S.fromList [5]) (outNeighbors 4 testGraph2),
        testVal "test outNeighbors 4" (S.fromList [2, 3, 4]) (outNeighbors 1 testGraph3),
        testVal "test outNeighbors 5" (S.fromList []) (outNeighbors 3 testGraph3),
        testVal "test outNeighbors 6" (S.fromList [2, 6]) (outNeighbors 1 testGraph5)   
    ]

testInNeighbors :: TestData
testInNeighbors = tests 3 10
    [ 
        testVal "test inNeighbors 1" (S.fromList []) (inNeighbors 1 testGraph1),
        testVal "test inNeighbors 2" (S.fromList [1]) (inNeighbors 3 testGraph1),
        testVal "test inNeighbors 3" (S.fromList [3]) (inNeighbors 4 testGraph2),
        testVal "test inNeighbors 4" (S.fromList [4]) (inNeighbors 1 testGraph3),
        testVal "test inNeighbors 5" (S.fromList [1, 2]) (inNeighbors 3 testGraph3),
        testVal "test inNeighbors 6" (S.fromList [5]) (inNeighbors 1 testGraph4)   
    ]

testRemoveNode :: TestData
testRemoveNode = tests 4 15
    [ 
        testVal "test removeNode 1" res1 (removeNode 1 testGraph1),
        testVal "test removeNode 2" res2 (removeNode 3 testGraph1),
        testVal "test removeNode 3" res3 (removeNode 4 testGraph2),
        testVal "test removeNode 4" res4 (removeNode 2 testGraph3),
        testVal "test removeNode 5" res5 (removeNode 1 testGraph3),
        testVal "test removeNode 6" res6 (removeNode 5 testGraph4)   
    ]
    where
        res1 = fromComponents [2, 3, 4] []
        res2 = fromComponents [1, 2, 4] [(1, 2)]
        res3 = fromComponents [1, 2, 3, 5] [(1, 2), (5, 3)]
        res4 = fromComponents [1, 3, 4] [(1, 4), (4, 1), (1, 3)]
        res5 = fromComponents [2, 3, 4] [(2, 3)]
        res6 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 3), (2, 3), (3, 4)]


testSplitNode :: TestData
testSplitNode = tests 5 20
    [ 
        testVal "test splitNode 1" res1 (splitNode 1 [5, 6, 7] testGraph1),
        testVal "test splitNode 2" res2 (splitNode 4 [5, 6] testGraph1),
        testVal "test splitNode 3" res3 (splitNode 3 [6, 7] testGraph2),
        testVal "test splitNode 4" res4 (splitNode 2 [] testGraph3),
        testVal "test splitNode 5" res5 (splitNode 3 [5, 6] testGraph3),
        testVal "test splitNode 6" res6 (splitNode 5 [7] testGraph4)   
    ]
    where
        res1 = fromComponents [5, 6, 7, 2, 3, 4] [(5, 2), (5, 3), (6, 2), (6, 3), (7, 2), (7, 3)]
        res2 = fromComponents [1, 2, 3, 5, 6] [(1, 2), (1, 3)]
        res3 = fromComponents [1, 2, 6, 7, 4, 5] [(1, 2), (6, 4), (7, 4), (4, 5), (5, 6), (5, 7)]
        res4 = fromComponents [1, 3, 4] [(1, 4), (4, 1), (1, 3)]
        res5 = fromComponents [1, 2, 4, 5, 6] [(1, 2), (1, 4), (4, 1), (2, 5), (2, 6), (1, 5), (1, 6)]
        res6 = fromComponents [1, 2, 3, 4, 7] [(1, 2), (1, 3), (2, 3), (3, 4), (4, 7), (7, 1), (3, 7)]


testMergeNodes :: TestData
testMergeNodes = tests 6 15
    [ 
        testVal "test mergeNodes 1" res1 (mergeNodes (\x -> elem x [2, 3]) 5 testGraph1),
        testVal "test mergeNodes 2" res2 (mergeNodes (\x -> elem x [1, 2]) 5 testGraph1),
        testVal "test mergeNodes 3" res3 (mergeNodes (\x -> elem x []) 7 testGraph2),
        testVal "test mergeNodes 4" res4 (mergeNodes (\x -> elem x [1, 2, 3]) 5 testGraph3),
        testVal "test mergeNodes 5" res5 (mergeNodes (\x -> elem x [1, 4]) 5 testGraph3),
        testVal "test mergeNodes 6" res6 (mergeNodes (\x -> elem x [1, 2, 3]) 6 testGraph4)   
    ]
    where
        res1 = fromComponents [1, 4, 5] [(1,5)]
        res2 = fromComponents [3, 4, 5] [(5,3),(5,5)]
        res3 = fromComponents [1, 2, 3, 4, 5] [(1, 2), (3, 4), (4, 5), (5, 3)]
        res4 = fromComponents [4,5] [(4,5),(5,4),(5,5)]
        res5 = fromComponents [2,3,5] [(2,3),(5,2),(5,3),(5,5)]
        res6 = fromComponents [4,5,6] [(4,5),(5,6),(6,4),(6,5),(6,6)]


testBFS :: TestData
testBFS = tests 7 15
    [ 
        testVal "test bfs 1" res1 (bfs 1 testGraph1),
        testVal "test bfs 2" res2 (bfs 2 testGraph1),
        testVal "test bfs 3" res3 (bfs 5 testGraph2),
        testVal "test bfs 4" res4 (bfs 4 testGraph3),
        testVal "test bfs 5" res5 (bfs 1 testGraph4),
        testVal "test bfs 6" res6 (bfs 1 testGraph5),
        testVal "test bfs 7" res7 (bfs 6 testGraph5),
        testVal "test bfs 8" res8 (bfs 1 testGraph6),
        testVal "test bfs 9" res9 (bfs 4 testGraph6),
        testVal "test bfs 10" res10 (bfs 1 testGraph7)
    ]
    where
        res1 = [1, 2, 3]
        res2 = [2]
        res3 = [5, 3, 4]
        res4 = [4, 1, 2, 3]
        res5 = [1, 2, 3, 4, 5]
        res6 = [1, 2, 6, 3, 7, 5]
        res7 = [6, 3, 7, 5]
        res8 = [1, 2, 3, 4, 5, 6, 7, 8]
        res9 = [4, 7]
        res10 = [1, 2, 3, 4]


testDFS :: TestData
testDFS = tests 8 15
    [ 
        testVal "test dfs 1" res1 (dfs 1 testGraph1),
        testVal "test dfs 2" res2 (dfs 2 testGraph1),
        testVal "test dfs 3" res3 (dfs 5 testGraph2),
        testVal "test dfs 4" res4 (dfs 4 testGraph3),
        testVal "test dfs 5" res5 (dfs 1 testGraph4),
        testVal "test dfs 6" res6 (dfs 1 testGraph5),
        testVal "test dfs 7" res7 (dfs 6 testGraph5),
        testVal "test dfs 8" res8 (dfs 1 testGraph6),
        testVal "test dfs 9" res9 (dfs 4 testGraph6),
        testVal "test dfs 10" res10 (dfs 1 testGraph7)
    ]
    where
        res1 = [1, 2, 3]
        res2 = [2]
        res3 = [5, 3, 4]
        res4 = [4, 1, 2, 3]
        res5 = [1, 2, 3, 4, 5]
        res6 = [1, 2, 3, 5, 6, 7]
        res7 = [6, 3, 5, 7]
        res8 = [1, 2, 5, 3, 6, 8, 4, 7]
        res9 = [4, 7]
        res10 = [1, 2, 4, 3]

testCountIntermediate :: TestData
testCountIntermediate = tests 9 15
    [ 
        testVal "test countIntermediate 1" (Just (1, 1)) (countIntermediate 1 3 testGraph1),
        testVal "test countIntermediate 2" Nothing (countIntermediate 2 4 testGraph1),
        testVal "test countIntermediate 3" (Just (1, 1)) (countIntermediate 3 5 testGraph2),
        testVal "test countIntermediate 4" (Just (1, 1)) (countIntermediate 1 3 testGraph3),
        testVal "test countIntermediate 5" Nothing (countIntermediate 2 1 testGraph3),
        testVal "test countIntermediate 6" (Just (3, 3)) (countIntermediate 4 3 testGraph4),
        testVal "test countIntermediate 7" (Just (1, 3)) (countIntermediate 1 6 testGraph5),
        testVal "test countIntermediate 8" (Just (2, 1)) (countIntermediate 6 5 testGraph5),
        testVal "test countIntermediate 9" (Just (6, 4)) (countIntermediate 1 8 testGraph6),
        testVal "test countIntermediate 10" Nothing (countIntermediate 2 7 testGraph6)
    ]

checkAll = vmCheck $ standardGraph ++ algorithms
    where
        standardGraph  = [testGraphConstruction, testOutNeighbors, testInNeighbors, testRemoveNode, testSplitNode, testMergeNodes]
        algorithms     = [testBFS, testDFS, testCountIntermediate]