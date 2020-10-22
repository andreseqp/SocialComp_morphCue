---
output:
  pdf_document: default
---
# The evolution of badges of status with learners

## The Hawk-Dove game

Classical Hawk-Dove game. Hawks compete with doves in a population of 
reproductive individuals.

![Hawk-dove game](Simulations//mutType_//basicHawkDove.png "H-Dgame")


## The effect of learning 

A population of individuals that estimate a value and a preference for a 
particular behaviour. In our case they specify the probability of playing dove 
in the Hawk-Dove game. 

### Result when individuals do NOT vary in their quality

![Learning equilibrium when individuals do not vary in quality](Simulations//QualStDv_//hawkDoveLearn.png "HD learn qual. invariance")

### Results when individuals vary in their quality

![Learning equilibrium when individuals vary in quality](Simulations//QualStDv_//hawkDoveLearn_0.1.png "HD learn qual. 0.1 ")

### Over all effect of variance

![ Effect of variation in quality for the learning equilibrium](Simulations//QualStDv_//effectQualVariance.png "Quality variance effect")

### How does learning work?

![Function approximation for0 the actor and the critic](cartoonRBF.png "Learning cartoonRBF")

### How does learning look like in the Hawk-Dove game

![Within population variation in the learning parameters at the end of learning for three scenarios of variation in the quality of individuals](Simulations//QualStDv_//WeightsVarQualSt.png "Quality invariance")

# How do learners fare against the "pure" types

![Evolutionary dynamics of the learners competing against pure strategies](Simulations//mutType_//evolDynTypes.png "Types competition")

## How do the learning dynamics look like

![Learning dynamics of the individuals in a population](Simulations//mutType_//learnDyn200.png "Learning dynamics")

## What's next?

Let reaction norm evolve, under different initial conditions. 


 

