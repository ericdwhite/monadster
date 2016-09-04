{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--
-- A Haskell Implementation Following
--   http://fsharpforfunandprofit.com/posts/monadster/
--
-- Things to know about Haskell
--   http://learnyouahaskell.com/making-our-own-types-and-typeclasses#record-syntax
--   http://learnyouahaskell.com/making-our-own-types-and-typeclasses#derived-instances
--   http://www.imada.sdu.dk/~rolf/Edu/DM509/E06/haskell-operatorer.pdf
--
-- Hints on reading F# code
--   Operator Reference - https://msdn.microsoft.com/en-us/library/dd233228.aspx
--
module Monadster.Lib 
(
  M(M)
  , runM
  , map1M
  , map2M
  , returnM
  , bindM
  , applyM

  , Label

  , VitalForce (..)
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
  , LiveRightUpperArm(..)
  , LiveRightLowerArm(..)
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

  , LiveBody
  , bodyM

  -- For testing
  , createBody
  , leftLegM
  , rightLegM
  , leftArmM
  , rightArmM
  , headM
  , beatingHeartM
)
where

--
-- Helpers
--
data M liveBodyPart = M (VitalForce -> (liveBodyPart, VitalForce))

runM (M f) vitalForce = f vitalForce

map1M f bodyPartM =
    let transformWhileAlive vitalForce =
          let (bodyPart, remainingVitalForce) = runM bodyPartM vitalForce
              updatedBodyPart = f bodyPart
              in (updatedBodyPart, remainingVitalForce)
          in M transformWhileAlive

map2M f m1 m2 =
    let becomeAlive vitalForce =
          let (v1, remainingVitalForce) = runM m1 vitalForce
              (v2, remainingVitalForce2) = runM m2 remainingVitalForce
              v3 = f v1 v2
              in (v3, remainingVitalForce2)
          in M becomeAlive

returnM part = 
    let becomeAlive vitalForce = (part, vitalForce)
    in M becomeAlive

bindM f bodyPartM =
    let becomeAlive vitalForce =
          let (bodyPart, remainingVitalForce) = runM bodyPartM vitalForce
          in runM (f bodyPart) remainingVitalForce
    in M becomeAlive

applyM mf mx =
    let becomeAlive vitalForce =
          let (f, remainingVitalForce) = runM mf vitalForce
              (x, remainingVitalForce2) = runM mx remainingVitalForce2
              y = f x
          in (y, remainingVitalForce2)
    in M becomeAlive

(<*>) = applyM

--
-- Each body part has a label
--
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

makeLiveLeftBrokenArm :: DeadLeftBrokenArm -> M LiveLeftBrokenArm
makeLiveLeftBrokenArm deadLeftBrokenArm =
    let (DeadLeftBrokenArm label) = deadLeftBrokenArm
        becomeAlive vitalForce = 
          let (oneUnit, remainingVitalForce) = getVitalForce vitalForce
              liveLeftBrokenArm = LiveLeftBrokenArm label oneUnit
              in (liveLeftBrokenArm, remainingVitalForce)
        in M becomeAlive

healBrokenArm :: LiveLeftBrokenArm -> LiveLeftArm
healBrokenArm (LiveLeftBrokenArm label vf) = LiveLeftArm label vf

-- This is the non generic implementation of map1M
makeHealedLeftArm :: M LiveLeftBrokenArm -> M LiveLeftArm
makeHealedLeftArm liveBrokenArmM =
    let healWhileAlive vitalForce =
          let (liveBrokenArm, remainingVitalForce) = runM liveBrokenArmM vitalForce
              healedArm = healBrokenArm liveBrokenArm
              in (healedArm, remainingVitalForce)
          in M healWhileAlive

healBrokenArmM = map1M healBrokenArm

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
-- Head
--
data DeadBrain = DeadBrain Label
               deriving(Eq, Show)

data Skull = Skull Label
           deriving(Eq, Show)

data LiveBrain = LiveBrain Label VitalForce
               deriving(Eq, Show)

data LiveHead = LiveHead {brain :: LiveBrain, skull :: Skull}
              deriving(Eq, Show)

headSurgery brain skull =
    LiveHead { brain=brain, skull=skull }

makeLiveBrain (DeadBrain label) =
    let becomeAlive vitalForce =
          let (oneUnit, remainingVitalForce) = getVitalForce vitalForce
              liveBrain = LiveBrain label oneUnit
          in (liveBrain, remainingVitalForce)
    in M becomeAlive

--
-- Beating Heart
--
data DeadHeart = DeadHeart Label
               deriving(Eq, Show)

data LiveHeart = LiveHeart Label VitalForce
               deriving(Eq, Show)

data BeatingHeart = BeatingHeart LiveHeart VitalForce
                  deriving(Eq, Show)

makeLiveHeart (DeadHeart label) =
    let becomeAlive vitalForce =
          let (oneUnit, remainingVitalForce) = getVitalForce vitalForce
              liveHeart = LiveHeart label oneUnit
          in (liveHeart, remainingVitalForce)
    in M becomeAlive

makeBeatingHeart liveHeart =
    let becomeAlive vitalForce =
          let (oneUnit, remainingVitalForce) = getVitalForce vitalForce
              beatingHeart = BeatingHeart liveHeart oneUnit
          in (beatingHeart, remainingVitalForce)
    in M becomeAlive

--
-- Live Body
--
leftLegM :: M LiveLeftLeg
leftLegM = makeLiveLeftLegM $ DeadLeftLeg "Boris"
rightLegM = leftLegM

leftArmM :: M LiveLeftArm
leftArmM = healBrokenArmM $ makeLiveLeftBrokenArm $ DeadLeftBrokenArm "Victor"

lowerRightArmM :: M LiveRightLowerArm
lowerRightArmM = makeLiveRightLowerArm $ DeadRightLowerArm "Tom"

upperRightArmM :: M LiveRightUpperArm
upperRightArmM = makeLiveRightUpperArm $ DeadRightUpperArm "Jerry"

rightArmM :: M LiveRightArm
rightArmM = armSurgeryM lowerRightArmM upperRightArmM

liveBrainM :: M LiveBrain
liveBrainM = makeLiveBrain $ DeadBrain "Abby Normal"

skullM :: M Skull
skullM = returnM $ Skull "Yorick"

headSurgeryM = map2M headSurgery

headM :: M LiveHead
headM = headSurgeryM liveBrainM skullM

beatingHeartM :: M BeatingHeart
beatingHeartM = bindM makeBeatingHeart $ makeLiveHeart $ DeadHeart "Anne"

data LiveBody = LiveBody {
                leftLeg :: LiveLeftLeg
              , rightLeg :: LiveLeftLeg
              , leftArm :: LiveLeftArm
              , rightArm :: LiveRightArm
              , aHead :: LiveHead
              , heart :: BeatingHeart
              }
              deriving(Eq, Show)

createBody :: LiveLeftLeg -> LiveLeftLeg -> LiveLeftArm -> LiveRightArm -> LiveHead -> BeatingHeart -> LiveBody
createBody leftLeg rightLeg leftArm rightArm aHead beatingHeart =
    LiveBody {
             leftLeg = leftLeg
             , rightLeg = rightLeg
             , leftArm = leftArm
             , rightArm = rightArm
             , aHead = aHead
             , heart = beatingHeart
             }

bodyM = 
    returnM createBody
    Monadster.Lib.<*> leftLegM
    Monadster.Lib.<*> rightLegM
    Monadster.Lib.<*> leftArmM
    Monadster.Lib.<*> rightArmM
    Monadster.Lib.<*> headM
    Monadster.Lib.<*> beatingHeartM

