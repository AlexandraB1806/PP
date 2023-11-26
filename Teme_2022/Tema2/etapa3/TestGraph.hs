{-# LANGUAGE MultiParamTypeClasses #-}

module TestGraph where

import TestPP
import qualified Data.Set as S
import AlgebraicGraph
import Modular


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

testGraph8 :: AlgebraicGraph String
testGraph8 = Connect
                (Connect
                    (Node "apple")
                    (Overlay (Node "pineapple") (Node "banana"))
                )
                (Overlay (Node "orange") (Node "pear"))

testGraph9 :: AlgebraicGraph Int
testGraph9 = Overlay
                    (Overlay
                        (Connect
                            (Node 1)
                            (Overlay
                                (Node 2)
                                (Overlay (Node 3) (Node 4))
                            )
                        )
                        (Overlay
                            (Connect (Node 5) (Node 6))
                            (Connect (Node 7) (Node 8))
                        )
                    )
                    (Overlay
                        (Connect (Node 9) (Node 10))
                        (Connect (Node 11) (Node 12))
                    )

makeTestUnit :: Ord a => AlgebraicGraph a -> (S.Set a, S.Set (a, a))
makeTestUnit g = (nodes g, edges g)

testNum :: TestData
testNum = tests 1 10
    [ 
        testVal "test Num 1" (makeTestUnit res1) (makeTestUnit testGraph1),
        testVal "test Num 2" (makeTestUnit res2) (makeTestUnit testGraph2),
        testVal "test Num 3" (makeTestUnit res3) (makeTestUnit testGraph3),
        testVal "test Num 4" (makeTestUnit res4) (makeTestUnit testGraph4),
        testVal "test Num 5" (makeTestUnit res5) (makeTestUnit testGraph5),
        testVal "test Num 6" (makeTestUnit res6) (makeTestUnit testGraph6)
    ]
    where
        res1 = ((1*(2+3))+4)
        res2 = ((1*2)+((3*4)+((4*5)+(5*3))))
        res3 = ((1*(4+(2*3)))+(4*1))
        res4 = (((1*(2*3))+(3*(4*5)))+(5*1))
        res5 = ((((1*(2+6))+((6*(3+7))+((3+7)*5)))+(2*3))+4)
        res6 = (((1*(2+(3+4)))+((2*5)+(3*6)))+((4*7)+(6*8)))

testShow :: TestData
testShow = tests 2 10
    [
        testVal "test Show 1" res1 (show testGraph1),
        testVal "test Show 2" res2 (show testGraph2),
        testVal "test Show 3" res3 (show testGraph3),
        testVal "test Show 4" res4 (show testGraph4),
        testVal "test Show 5" res5 (show testGraph5),
        testVal "test Show 6" res6 (show testGraph6)
    ]
    where
        res1 = "((1*(2+3))+4)"
        res2 = "((1*2)+((3*4)+((4*5)+(5*3))))"
        res3 = "((1*(4+(2*3)))+(4*1))"
        res4 = "(((1*(2*3))+(3*(4*5)))+(5*1))"
        res5 = "((((1*(2+6))+((6*(3+7))+((3+7)*5)))+(2*3))+4)"
        res6 = "(((1*(2+(3+4)))+((2*5)+(3*6)))+((4*7)+(6*8)))"


testEq :: TestData
testEq = tests 3 10
    [
        testCond "test Eq 1" (testGraph1 == res1),
        testCond "test Eq 2" (testGraph2 == res2),
        testCond "test Eq 3" (testGraph3 == res3),
        testCond "test Eq 4" (testGraph4 == res4),
        testCond "test Eq 5" (testGraph5 == res5),
        testCond "test Eq 6" (testGraph6 == res6)
    ]
    where
        res1 = ((1*(2+3))+4)
        res2 = ((1*2)+((3*4)+((4*5)+(5*3))))
        res3 = ((1*(4+(2*3)))+(4*1))
        res4 = (((1*(2*3))+(3*(4*5)))+(5*1))
        res5 = ((((1*(2+6))+((6*(3+7))+((3+7)*5)))+(2*3))+4)
        res6 = (((1*(2+(3+4)))+((2*5)+(3*6)))+((4*7)+(6*8)))


testExtend :: TestData
testExtend = tests 4 15
    [
        testVal "test extend 1" res1 (extend (\n -> if n == 1 then 4+5 else Node n) testGraph1),
        testVal "test extend 2" res2 (extend (\n -> if even n then 4+5 else 1*2) testGraph2),
        testVal "test extend 3" res3 (extend (\n -> if odd n then 6 else 1*2) testGraph3),
        testVal "test extend 4" res4 (extend (\n -> if even n then (Connect (Node n) (Node $ n + 1)) else (Overlay (Node n) (Node $ n + 1))) testGraph4),
        testVal "test extend 5" res5 (extend (\n -> if odd n then Node n else 7) testGraph5),
        testVal "test extend 6" res6 (extend (\n -> if even n then ((4+5)*(2+3)) else 1*2) testGraph6)
    ]
    where
        res1 = ((4+5)*(2+3))
        res2 = (((1*2)*(4+5))+(((1*2)*(4+5))+(((4+5)*(1*2))+((1*2)*(1*2)))))
        res3 = ((6*((1*2)+((1*2)*6)))+((1*2)*6))
        res4 = ((((1+2)*((2*3)*(3+4)))+((3+4)*((4*5)*(5+6))))+((5+6)*(1+2)))
        res5 = ((((1*(7+7))+((7*(3+7))+((3+7)*5)))+(7*3))+7)
        res6 = ((((1*2)*(((4+5)*(2+3))+((1*2)+((4+5)*(2+3)))))+((((4+5)*(2+3))*(1*2))+((1*2)*((4+5)*(2+3)))))+((((4+5)*(2+3))*(1*2))+(((4+5)*(2+3))*((4+5)*(2+3)))))


testSplitNode :: TestData
testSplitNode = tests 5 15
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

testFmap:: TestData
testFmap = tests 6 5
    [
        testVal "test fmap 1" res1 $ fmap (+ 10) testGraph1,
        testVal "test fmap 2" res2 $ fmap ((+ 1) . (* 2)) testGraph2,
        testVal "test fmap 3" res3 $ fmap ((* 5) . (* 3)) testGraph3,
        testVal "test fmap 4" res4 $ fmap (* 2) testGraph4,
        testVal "test fmap 5" res5 $ fmap (^ 2) testGraph5,
        testVal "test fmap 6" res6 $ fmap (10 - ) testGraph6
    ]
    where
        res1 = ((11*(12+13))+14)
        res2 = ((3*5)+((7*9)+((9*11)+(11*7))))
        res3 = ((15*(60+(30*45)))+(60*15))
        res4 = (((2*(4*6))+(6*(8*10)))+(10*2))
        res5 = ((((1*(4+36))+((36*(9+49))+((9+49)*25)))+(4*9))+16)
        res6 = (((9*(8+(7+6)))+((8*5)+(7*4)))+((6*3)+(4*2)))

testMergeNodes :: TestData
testMergeNodes = tests 7 10
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

testRemoveNode :: TestData
testRemoveNode = tests 8 5
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

testFilterGraph :: TestData
testFilterGraph = tests 9 10
    [
        testVal "test filterGraph 1" (makeTestUnit res1) (makeTestUnit (filterGraph odd testGraph1)),
        testVal "test filterGraph 2" (makeTestUnit res2) (makeTestUnit (filterGraph even testGraph2)),
        testVal "test filterGraph 3" (makeTestUnit res3) (makeTestUnit (filterGraph (>2) testGraph3)),
        testVal "test filterGraph 4" (makeTestUnit res4) (makeTestUnit (filterGraph (`elem` [1, 5]) testGraph4)),
        testVal "test filterGraph 5" (makeTestUnit res5) (makeTestUnit (filterGraph (\k -> length [ x | x <- [2..k], k `mod` x == 0] == 1) testGraph6)),
        testVal "test filterGraph 6" (makeTestUnit res6) (makeTestUnit (filterGraph ((>5) . length) testGraph8))
    ]
    where
        res1 = Connect (Node 1) (Node 3)
        res2 = Overlay (Node 2) (Overlay (Node 4) (Node 4))
        res3 = Overlay (Overlay (Node 4) (Node 3)) (Node 4)
        res4 = Overlay (Overlay (Node 1) (Node 5)) (Connect (Node 5) (Node 1))
        res5 = Overlay (Overlay (Overlay (Node 2) (Node 3)) (Overlay (Connect (Node 2) (Node 5)) (Node 3))) (Node 7)
        res6 = Connect (Overlay (Node "pineapple") (Node "banana")) (Node "orange")

testIsModule :: TestData
testIsModule = tests 10 10
    [
        testVal "test isModule 1" True (isModule (S.fromList [4]) testGraph1),
        testVal "test isModule 2" True (isModule (S.fromList [1, 2, 3]) testGraph1),
        testVal "test isModule 3" False (isModule (S.fromList [1, 2, 4]) testGraph1),
        testVal "test isModule 4" True (isModule (S.fromList [1, 2]) testGraph2),
        testVal "test isModule 5" True (isModule (S.fromList [2, 3]) testGraph3),
        testVal "test isModule 6" False (isModule (S.fromList [1, 4]) testGraph3),
        testVal "test isModule 7" True (isModule (S.fromList [7, 8]) testGraph9),
        testVal "test isModule 8" True (isModule (S.fromList [1, 2, 3, 4, 5, 6, 7, 8]) testGraph9),
        testVal "test isModule 9" False (isModule (S.fromList [1, 5]) testGraph9)
    ]

testIsModularPartition :: TestData
testIsModularPartition = tests 11 8
    [
        testVal "test isModularPartition 1" True (isModularPartition part1 testGraph1),
        testVal "test isModularPartition 2" True (isModularPartition part2 testGraph1),
        testVal "test isModularPartition 3" False (isModularPartition part3 testGraph1),
        testVal "test isModularPartition 4" True (isModularPartition part4 testGraph1),
        testVal "test isModularPartition 5" False (isModularPartition part5 testGraph2),
        testVal "test isModularPartition 6" True (isModularPartition part6 testGraph9),
        testVal "test isModularPartition 7" True (isModularPartition part7 testGraph9)
    ]
    where
        part1 = S.fromList [S.fromList [1], S.fromList [2], S.fromList [3], S.fromList [4]]
        part2 = S.fromList [S.fromList [1, 2, 3, 4]]
        part3 = S.fromList [S.fromList [1, 2], S.fromList [3, 4]]
        part4 = S.fromList [S.fromList [1, 2, 3], S.fromList [4]]
        part5 = S.fromList [S.fromList [1, 2, 3]]
        part6 = S.fromList [S.fromList [1, 2, 3, 4], S.fromList [5, 6, 7, 8], S.fromList [9, 10, 11, 12]]
        part7 = S.fromList [S.fromList [1, 2, 3, 4], S.fromList [5, 6], S.fromList [7, 8], S.fromList [9, 10, 11, 12]]

testMaximalModularPartition :: TestData
testMaximalModularPartition = tests 12 12
    [
        testVal "test maximalModularPartition 1" res1 (modularlyDecompose testGraph1),
        testVal "test maximalModularPartition 2" res2 (modularlyDecompose testGraph2),
        testVal "test maximalModularPartition 3" res3 (modularlyDecompose testGraph3),
        testVal "test maximalModularPartition 4" res4 (modularlyDecompose testGraph4),
        testVal "test maximalModularPartition 5" res5 (modularlyDecompose testGraph5),
        testWith "test maximalModularPartition 6" "valoarea" "apare" [res61, res62] (flip elem) (modularlyDecompose testGraph8)
    ]
    where
        res1 = S.fromList [S.fromList [1,2,3], S.fromList [4]]
        res2 = S.fromList [S.fromList [1,2], S.fromList [3,4,5]]
        res3 = S.fromList [S.fromList [1], S.fromList [2,3], S.fromList [4]]
        res4 = S.fromList [S.fromList [1], S.fromList [2], S.fromList [3], S.fromList [4], S.fromList [5]]
        res5 = S.fromList [S.fromList [1,2,3,5,6,7], S.fromList [4]]
        res61 = S.fromList [S.fromList ["apple","banana","pineapple"], S.fromList ["orange","pear"]]
        res62 = S.fromList [S.fromList ["apple"], S.fromList ["banana","orange","pear","pineapple"]]

checkAll = vmCheck $ algebraicGraph ++ modular
    where
        algebraicGraph = [
            testNum, testShow, testEq, testExtend, testSplitNode, testFmap, testMergeNodes,
            testRemoveNode, testFilterGraph
            ]
        modular = [testIsModule, testIsModularPartition, testMaximalModularPartition]