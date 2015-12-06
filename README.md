#Dr Frankenfunctor and the Monadster, a Haskell implementation

This is a version of the Monadster code in Haskell based on the blog posts: http://fsharpforfunandprofit.com/posts/monadster/

I created this as I was learning Haskell following functional programming articles on: http://fsharpforfunandprofit.com 

Any feedback is more than welcome!

##git tags:
  * instalment-1.0 corresponds to: http://fsharpforfunandprofit.com/posts/monadster/


##Trying it out: git tag (instalment-1.0)

    1. Launch ghci:
```bash
       $ cabal exec ghci src/Monadster/Main.hs
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
           let leftLegM = makeLiveLeftLegM deadLeftLeg
           let (liveLeftLeg, remainingAfterLeftLeg) = runM leftLegM vf

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
```

    4. Check the results
```haskell
        λ> liveLeftLeg
           LiveLeftLeg "Boris" (VitalForce {units = 1})

        λ> liveLeftArm
           LiveLeftArm "Victor" (VitalForce {units = 1})

        λ> liveRightArm
           LiveRightArm {lowerArm = LiveRightLowerArm "Tom" (VitalForce {units = 1}),
                         upperArm = LiveRightUpperArm "Jerry" (VitalForce {units = 1})}
```

