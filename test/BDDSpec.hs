module BDDSpec where

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
  testBuildROBOD

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
    it "testSimplify" $
      pending

testRestrict :: Spec
testRestrict = do
  describe "restrict" $ do
    it "testRestrict" $
      pending

testBuildBDD :: Spec
testBuildBDD = do
  describe "buildBDD" $ do
    it "testBuildBDD" $
      pending

testBuildROBOD :: Spec
testBuildROBOD = do
  describe "buildROBOD" $ do
    it "testBuildROBOD" $
      pending


  
b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


