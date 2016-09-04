import Test.Hspec
import Control.Exception (evaluate)

import Monadster.Lib (
                     M(M)
                     , runM
                     , map1M
                     , map2M
                     , returnM
                     , bindM
                     , Label
                     , VitalForce(..)
                     , getVitalForce
                     , DeadLeftLeg(..)
                     , LiveLeftLeg(..)
                     , makeLiveLeftLegM
                     , DeadLeftBrokenArm(..)
                     , LiveLeftArm(..)
                     , makeLiveLeftBrokenArm
                     , healBrokenArmM
                     , DeadRightLowerArm(..)
                     , DeadRightUpperArm(..)
                     , LiveRightLowerArm(..)
                     , LiveRightUpperArm(..)
                     , LiveRightArm(..)
                     , makeLiveRightLowerArm
                     , makeLiveRightUpperArm
                     , armSurgeryM
                     , DeadBrain(..)
                     , Skull(..)
                     , makeLiveBrain
                     , headSurgery
                     , LiveBrain(..)
                     , LiveHead(..)
                     , DeadHeart(..)
                     , LiveHeart(..)
                     , BeatingHeart(..)
                     , makeLiveHeart
                     , makeBeatingHeart
                     , LiveBody(..)
                     , bodyM
                     )
--
-- These test functions more or less line
-- up with the testing sections from the article.
--
main :: IO ()
main = hspec $ do
  testHelpers
  testVitalForce
  testLeftLeg
  testLeftArm
  testRightArm
  testHead
  testHeart
  testBody

testHelpers = do
    describe "Helpers" $ do
      it "run the function in the holder" $ do
        part `shouldBe` ("Leg"::Label)
        units vf `shouldBe` (10::Int)
        where m = M (\vf -> ("Leg"::Label, vf))
              (part, vf) = runM m $ VitalForce 10

testVitalForce = do
    describe "Vital Force" $ do
      it "creates a vital force" $ do
        units (VitalForce 10) `shouldBe` (10::Int)
      it "uses some vital force" $ do
        units oneUnit `shouldBe` (1::Int)
        units left `shouldBe` (9::Int)
        where
          (oneUnit, left) = getVitalForce $ VitalForce 10

testLeftLeg = do
    describe "Left Leg" $ do
      it "creates a Live Left Leg" $ do
        units remainingVitalForce `shouldBe` (9::Int)
        part `shouldBe` ("Boris"::Label)
        units partForce `shouldBe` (1::Int)
        where vf = VitalForce 10
              deadLeftLeg = DeadLeftLeg "Boris"
              (liveLeftLeg, remainingVitalForce) = runM (makeLiveLeftLegM deadLeftLeg) vf
              (LiveLeftLeg part partForce) = liveLeftLeg

testLeftArm = do
    describe "Left Arm" $ do
      it "creates a Live Left Arm" $ do
        units remainingVitalForce `shouldBe` (8::Int)
        part `shouldBe` ("Victor"::Label)
        units partForce `shouldBe` (1::Int)
        where remainingAfterLeftLeg = VitalForce 9
              deadLeftArm = DeadLeftBrokenArm "Victor"
              leftArmM = healBrokenArmM $ makeLiveLeftBrokenArm deadLeftArm
              (liveLeftArm, remainingVitalForce) = runM leftArmM remainingAfterLeftLeg 
              (LiveLeftArm part partForce) = liveLeftArm

testRightArm = do
    describe "Right Arm" $ do
      it "creates a Live Right Arm" $ do
        units remainingVitalForce `shouldBe` (6::Int)
        lowerArm `shouldBe` (LiveRightLowerArm "Tom" $ VitalForce 1)
        upperArm `shouldBe` (LiveRightUpperArm "Jerry" $ VitalForce 1)
        where
           remainingAfterLeftArm = VitalForce 8
           deadRightLowerArm = DeadRightLowerArm "Tom"
           lowerRightArmM = makeLiveRightLowerArm deadRightLowerArm

           deadRightUpperArm = DeadRightUpperArm "Jerry"
           upperRightArmM = makeLiveRightUpperArm deadRightUpperArm

           rightArmM = armSurgeryM lowerRightArmM upperRightArmM
           (liveRightArm, remainingVitalForce) = runM rightArmM remainingAfterLeftArm
           (LiveRightArm lowerArm upperArm) = liveRightArm

testHead = do
    describe "Head" $ do
      it "creates a Live Head" $ do
        units remainingVitalForce `shouldBe` (5::Int)
        brain liveHead `shouldBe` LiveBrain "Abby Normal" (VitalForce 1)
        skull liveHead `shouldBe` Skull "Yorick"
        where
           remainingAfterRightArm = VitalForce 6
           deadBrain = DeadBrain "Abby Normal"
           formedSkull = Skull "Yorick"
           liveBrainM = makeLiveBrain deadBrain
           skullM = returnM formedSkull
           headSurgeryM = map2M headSurgery
           headM = headSurgeryM liveBrainM skullM
           (liveHead, remainingVitalForce) = runM headM remainingAfterRightArm

testHeart = do
    describe "Heart" $ do
      it "crates a Beating Heart" $ do
        units remainingVitalForce `shouldBe` (3::Int)
        liveHeart `shouldBe` LiveHeart "Anne" (VitalForce 1)
        where
           remainingAfterHead = VitalForce 5
           deadHeart = DeadHeart "Anne"
           liveHeartM = makeLiveHeart deadHeart
           beatingHeartM = bindM makeBeatingHeart liveHeartM
           (beatingHeart, remainingVitalForce) = runM beatingHeartM remainingAfterHead
           (BeatingHeart liveHeart _) = beatingHeart

testBody = do
    describe "Body" $ do
      it "creates a Live Body" $ do
        units remainingVitalForce `shouldBe` (0::Int)
        where
           vf = VitalForce 10
           (body, remainingVitalForce) = runM bodyM vf 
 
