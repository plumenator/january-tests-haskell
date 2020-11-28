module BDDSpec where

import Data.Map
import Test.Hspec

import BDD

spec :: Spec
spec = do
  testLookUp
  testCheckSat
  testSat
  testSimplify
  testRestrict
  testBuildBDD
  testApplyEliminate
  testApplyShare
  testBuildROBDD

testLookUp :: Spec
testLookUp = do
  describe "lookUp" $ do
    it "returns the value when the key is given" $
      lookUp "two" [("one", 1), ("two", 2)] `shouldBe` 2

testCheckSat :: Spec
testCheckSat = do
  describe "checkSat" $ do
    it "bdd2" $
      checkSat bdd2 [(1, True), (2, False)] `shouldBe` True
    it "bdd7" $
      checkSat bdd7 [(3, True), (2, False), (9, True)] `shouldBe` False

testSat :: Spec
testSat = do
  describe "sat" $ do
    it "bdd1" $
      sat bdd1 `shouldBe` []
    it "bdd2" $
      sat bdd2 `shouldBe` [[(1, False), (2, False)], [(1, False), (2, True)], [(1, True), (2, False)]]
    it "bdd8" $
      sat bdd8 `shouldBe` [[(1, False)], [(1, True)]]

testSimplify :: Spec
testSimplify = do
  describe "simplify" $ do
    it "returns Prim True" $
      simplify (Not (Prim False)) `shouldBe` Prim True
    it "returns Prim False" $
      simplify (Or (Prim False) (Prim False)) `shouldBe` Prim False
    it "returns the input as is" $
      simplify (And (IdRef 3) (Prim True)) `shouldBe` And (IdRef 3) (Prim True)

testRestrict :: Spec
testRestrict = do
  describe "restrict" $ do
    it "b7 2 True" $
      restrict b7 2 True `shouldBe` Or (Not (IdRef 3)) (Or (Prim True) (Not (IdRef 9)))
    it "and then, 9 False" $
      restrict (restrict b7 2 True) 9 False `shouldBe` Or (Not (IdRef 3)) (Prim True)
    it "and then, 3 True" $
      restrict (restrict (restrict b7 2 True) 9 False) 3 True `shouldBe` Prim True

testBuildBDD :: Spec
testBuildBDD = do
  describe "buildBDD" $ do
    it "b1" $
      fmap fromList (buildBDD b1 []) `shouldBe` fmap fromList bdd1
    it "b2" $
      fmap fromList (buildBDD b2 [1, 2]) `shouldBe` fmap fromList bdd2
    it "b3" $
      fmap fromList (buildBDD b3 [1]) `shouldBe` fmap fromList bdd3'
    it "b4" $
      fmap fromList (buildBDD b4 [2, 3, 7]) `shouldBe` fmap fromList bdd4
    it "b5" $
      fmap fromList (buildBDD b5 [2, 3, 7]) `shouldBe` fmap fromList bdd5'
    it "b6" $
      fmap fromList (buildBDD b6 [1, 2, 3, 4]) `shouldBe` fmap fromList bdd6
    it "b7" $
      fmap fromList (buildBDD b7 [2, 3, 9]) `shouldBe` fmap fromList bdd7'
    it "b8" $
      fmap fromList (buildBDD b8 [1]) `shouldBe` fmap fromList bdd8

testApplyEliminate :: Spec
testApplyEliminate = do
  describe "testApplyEliminate" $ do
    it "applyEliminate first order" $
      fmap fromList (applyEliminate (2, [(2, (1, 1, 5)), (5, (2, 0, 0))]))
      `shouldBe` fmap fromList (2, [(2, (1, 1, 0))])
    it "applyEliminate second order" $
      fmap fromList (applyEliminate (2, [(2, (1, 1, 5)), (5, (2, 10, 11)), (10, (3, 0, 0)), (11, (3, 0, 0))]))
      `shouldBe` fmap fromList (2, [(2, (1, 1, 0))])
    it "applyEliminate disjunction of conjunctions" $
      fmap fromList (applyEliminate (2, [(2, (1, 4, 5)), (4, (2, 8, 9)), (8, (3, 16, 17)), (16, (4, 0, 0)), (17, (4, 0, 1)), (9, (3, 18, 19)), (18, (4, 0, 0)), (19, (4, 0, 1)), (5, (2, 10, 11)), (10, (3, 20, 21)), (20, (4, 0, 0)), (21, (4, 0, 1)), (11, (3, 22, 23)), (22, (4, 1, 1)), (23, (4, 1, 1))]))
      `shouldBe` fmap fromList (2, [(2, (1, 4, 5)), (4, (2, 8, 9)), (8, (3, 0, 17)), (17, (4, 0, 1)), (9, (3, 0, 19)), (19, (4, 0, 1)), (5, (2, 10, 1)), (10, (3, 0, 21)), (21, (4, 0, 1))])
    it "lose root" $
      fmap fromList (applyEliminate (2,[(2,(2,4,4)),(4,(1,1,0))])) `shouldBe` fmap fromList (2,[(2, (1, 1, 0))])

testApplyShare :: Spec
testApplyShare = do
  describe "testApplyShare" $ do
    it "applyShare first order" $
      fmap fromList (applyShare (2, [(2, (1, 4, 5)), (4, (2, 1, 0)), (5, (2, 1, 0))]))
      `shouldBe` fmap fromList (2, [(2, (1, 4, 4)), (4, (2, 1, 0))])
    it "applyShare second order" $
      fmap fromList (applyShare (2, [(2,(1,4,5)),(4,(2,8,9)),(8,(3,0,17)), (17,(4,0,1)),(9,(3,0,19)),(19,(4,0,1)), (5,(2,10,1)),(10,(3,0,21)),(21,(4,0,1)),(22,(4,0,1))]))
      `shouldBe` fmap fromList (2, [(2, (1, 4, 5)), (4, (2, 8, 8)), (8, (3, 0, 17)), (17, (4, 0, 1)), (5, (2, 8, 1))])

testBuildROBDD :: Spec
testBuildROBDD = do
  describe "buildROBDD" $ do
    it "b9 1 2" $
      fmap fromList (buildROBDD b9 [1, 2]) `shouldBe` fmap fromList bdd9
    it "b9 2 1" $
      fmap fromList (buildROBDD b9 [1, 2]) `shouldBe` fmap fromList bdd9
    it "b6 1 3 2 4" $
      fmap fromList (buildROBDD b6 [1, 3, 2, 4]) `shouldBe` fmap fromList bdd61324
    it "b6 1 2 3 4" $
      fmap fromList (buildROBDD b6 [1, 2, 3, 4]) `shouldBe` fmap fromList bdd61234


  
b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))
b9 = Or (And (Not (IdRef 1)) (Prim True)) (And (IdRef 1) (Or (And (IdRef 2) (Prim False)) (And (Not (IdRef 2)) (Prim False))))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd3' = (2,[(2,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd5' = (2,[(4,(3,8,9)),(2,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd61324 = (2, [(2, (1, 4, 5)), (4, (3, 0, 18)), (18, (4, 0, 1)), (5, (3, 10, 11)), (10, (2, 0, 1)), (11, (2, 18, 1))])
bdd61234 = (2, [(2, (1, 8, 5)), (8, (3, 0, 17)), (5, (2, 8, 1)), (17, (4, 0, 1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd7' = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])
bdd9 = (2, [(2, (1, 1, 0))])
