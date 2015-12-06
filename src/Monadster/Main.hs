{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--
-- A Haskell Implementation Following
--   http://fsharpforfunandprofit.com/posts/monadster/
--
-- Things to know:
--   http://learnyouahaskell.com/making-our-own-types-and-typeclasses#record-syntax
--   http://learnyouahaskell.com/making-our-own-types-and-typeclasses#derived-instances
--
module Monadster.Main where


-- Each body part has a label
type Label = String

--
-- Vital Force
--
data VitalForce = VitalForce {units :: Int}  
                deriving(Eq, Show)

getVitalForce :: VitalForce -> (VitalForce, VitalForce)
getVitalForce vitalForce =
    let oneUnit = VitalForce 1 
        remaining = VitalForce (units vitalForce - 1)
        in (oneUnit, remaining)

--
-- Left Leg
--
data DeadLeftLeg = DeadLeftLeg Label
                 deriving(Eq, Show)

data LiveLeftLeg = LiveLeftLeg Label VitalForce
                 deriving(Eq, Show)

type MakeLiveLeftLeg = DeadLeftLeg -> VitalForce -> (LiveLeftLeg, VitalForce)
makeLiveLeftLeg :: MakeLiveLeftLeg
makeLiveLeftLeg deadLeftLeg vitaForce =
    let (DeadLeftLeg label) = deadLeftLeg
        (oneUnit, remainingVitalForce) = getVitalForce vitaForce
        liveLeftLeg = LiveLeftLeg label oneUnit
        in (liveLeftLeg, remainingVitalForce)

data M liveBodyPart = M (VitalForce -> (liveBodyPart, VitalForce))
runM (M f) vitaForce = f vitaForce

makeLiveLeftLegM :: DeadLeftLeg -> M LiveLeftLeg
makeLiveLeftLegM deadLeftLeg =
    let becomeAlive = makeLiveLeftLeg deadLeftLeg
        in M becomeAlive

--
-- Left Arm
--
data DeadLeftBrokenArm = DeadLeftBrokenArm Label
                       deriving(Eq, Show)

data LiveLeftBrokenArm = LiveLeftBrokenArm Label VitalForce
                       deriving(Eq, Show)

data LiveLeftArm = LiveLeftArm Label VitalForce
                 deriving(Eq, Show)

type HealBrokenArm = LiveLeftBrokenArm -> LiveLeftArm
healBrokenArm (LiveLeftBrokenArm label vf) = LiveLeftArm label vf

makeHealedLeftArm liveBrokenArmM =
    let healWhileAlive vitalForce =
          let (liveBrokenArm, remainingVitalForce) = runM liveBrokenArmM vitalForce
              healedArm = healBrokenArm liveBrokenArm
              in (healedArm, remainingVitalForce)
          in M healWhileAlive

mapM f bodyPartM =
    let transformWhileAlive vitalForce =
          let (bodyPart, remainingVitalForce) = runM bodyPartM vitalForce
              updatedBodyPart = f bodyPart
              in (updatedBodyPart, remainingVitalForce)
          in M transformWhileAlive

healBrokenArmM = Monadster.Main.mapM healBrokenArm

makeLiveLeftBrokenArm deadLeftBrokenArm =
    let (DeadLeftBrokenArm label) = deadLeftBrokenArm
        becomeAlive vitalForce = 
          let (oneUnit, remainingVitalForce) = getVitalForce vitalForce
              liveLeftBrokenArm = LiveLeftBrokenArm label oneUnit
              in (liveLeftBrokenArm, remainingVitalForce)
        in M becomeAlive
    
--
-- Right Arm
--
data DeadRightLowerArm = DeadRightLowerArm Label
                       deriving(Eq, Show)

data DeadRightUpperArm = DeadRightUpperArm Label
                       deriving(Eq, Show)

data LiveRightLowerArm = LiveRightLowerArm Label VitalForce
                       deriving(Eq, Show)

data LiveRightUpperArm = LiveRightUpperArm Label VitalForce
                       deriving(Eq, Show)

data LiveRightArm = LiveRightArm { lowerArm :: LiveRightLowerArm
                                 , upperArm :: LiveRightUpperArm}
                  deriving(Eq, Show)

armSurgery lowerArm upperArm =
    LiveRightArm {lowerArm=lowerArm, upperArm=upperArm}

map2M f m1 m2 =
    let becomeAlive vitalForce =
          let (v1, remainingVitalForce) = runM m1 vitalForce
              (v2, remainingVitalForce2) = runM m2 remainingVitalForce
              v3 = f v1 v2
              in (v3, remainingVitalForce2)
          in M becomeAlive

makeLiveRightLowerArm (DeadRightLowerArm label) =
    let becomeAlive vitalForce =
          let (oneUnit, remainingVitalForce) = getVitalForce vitalForce
              liveRightLowerArm = LiveRightLowerArm label oneUnit
              in (liveRightLowerArm, remainingVitalForce)
          in M becomeAlive

makeLiveRightUpperArm (DeadRightUpperArm label) =
    let becomeAlive vitalForce =
          let (oneUnit, remainingVitalForce) = getVitalForce vitalForce
              liveRightUpperArm = LiveRightUpperArm label oneUnit
              in (liveRightUpperArm, remainingVitalForce)
          in M becomeAlive

armSurgeryM = map2M armSurgery

--
-- Testing
--
vf = VitalForce 10

-- Left Leg
deadLeftLeg = DeadLeftLeg "Boris"
leftLegM = makeLiveLeftLegM deadLeftLeg
(liveLeftLeg, remainingAfterLeftLeg) = runM leftLegM vf

-- Left Arm
deadLeftArm = DeadLeftBrokenArm "Victor"
leftArmM = healBrokenArmM $ makeLiveLeftBrokenArm deadLeftArm
(liveLeftArm, remainingAfterLeftArm) = runM leftArmM remainingAfterLeftLeg 

-- Right Arm
deadRightLowerArm = DeadRightLowerArm "Tom"
lowerRightArmM = makeLiveRightLowerArm deadRightLowerArm

deadRightUpperArm = DeadRightUpperArm "Jerry"
upperRightArmM = makeLiveRightUpperArm deadRightUpperArm

rightArmM = armSurgeryM lowerRightArmM upperRightArmM
(liveRightArm, remainingAfterRightArm) = runM rightArmM remainingAfterLeftArm

