#Dr Frankenfunctor and the Monadster, a Haskell implementation

This is a version of the Monadster code in Haskell based on the blog posts: http://fsharpforfunandprofit.com/posts/monadster/

I created this as I was learning Haskell following functional programming articles on: http://fsharpforfunandprofit.com 

Any feedback is more than welcome!

##git tags:
  * instalment-1.0 corresponds to: http://fsharpforfunandprofit.com/posts/monadster/
  * instalment-2.0 corresponds to: http://fsharpforfunandprofit.com/posts/monadster-2/ 

##Trying it out

    1. Launch ghci:
```bash
       $ stack build
       $ stack ghci
```

    2. Start with some Vital Force
```haskell
        λ> -- Create some vital force 
           let vf = VitalForce 10
           VitalForce {units = 10}
```

    3. Create body parts
```haskell
        λ> -- Left Leg
           let deadLeftLeg = DeadLeftLeg "Boris"
           let (liveLeftLeg, remainingAfterLeftLeg) = runM (makeLiveLeftLegM deadLeftLeg) vf

        λ> -- Left Arm
           let deadLeftArm = DeadLeftBrokenArm "Victor"
           let leftArmM = healBrokenArmM $ makeLiveLeftBrokenArm deadLeftArm
           let (liveLeftArm, remainingAfterLeftArm) = runM leftArmM remainingAfterLeftLeg 

        λ> -- Right Arm
           let deadRightLowerArm = DeadRightLowerArm "Tom"
           let lowerRightArmM = makeLiveRightLowerArm deadRightLowerArm

           let deadRightUpperArm = DeadRightUpperArm "Jerry"
           let upperRightArmM = makeLiveRightUpperArm deadRightUpperArm

           let rightArmM = armSurgeryM lowerRightArmM upperRightArmM
           let (liveRightArm, remainingAfterRightArm) = runM rightArmM remainingAfterLeftArm

        λ> -- Head 
           let deadBrain = DeadBrain "Abby Normal"
           let skull = Skull "Yorick"
           let liveBrainM = makeLiveBrain deadBrain
           let skullM = returnM skull
           let headSurgeryM = map2M headSurgery
           let headM = headSurgeryM liveBrainM skullM
           let (liveHead, remainingAfterHead) = runM headM remainingAfterRightArm

        λ> -- Beating Heart
           let deadHeart = DeadHeart "Anne"
           let liveHeartM = makeLiveHeart deadHeart
           let beatingHeartM = bindM makeBeatingHeart liveHeartM
           let (beatingHeart, remainingAfterHeart) = runM beatingHeartM remainingAfterHead

        λ> -- Body 
           let vf = VitalForce 10
           let (body, remainingVitalForce) = runM bodyM vf

           NOTE: There is an issue with the remainingVitalForce which hangs ghci (Must be recursive some how?)
```

    4. Example of checking the results
```haskell
        λ> liveLeftLeg
           LiveLeftLeg "Boris" (VitalForce {units = 1})

        λ> liveLeftArm
           LiveLeftArm "Victor" (VitalForce {units = 1})

        λ> liveRightArm
           LiveRightArm {lowerArm = LiveRightLowerArm "Tom" (VitalForce {units = 1}),
                         upperArm = LiveRightUpperArm "Jerry" (VitalForce {units = 1})}
```

##Running tests

    1. Lauchh ghci in test mode:
```bash
      $ stack ghci --test monadster:test:monadster-test
      λ> main
      or
      y> :test -- reload and run tests
````

    2. From the command line:
```bash
     $ stack test
```
