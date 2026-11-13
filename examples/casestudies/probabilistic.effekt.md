---
layout: docs
title: Probabilistic Samling
permalink: docs/casestudies/probabilistic
---

# Probabilistic Sampling

In this case study we apply the probabilistic sampling methods implemented in the probs library and apply them to some of the samples from the Iris Flower Dataset.

---

First we need some inputs as well as definitions for the iris flower kinds and the flower model.

```
import probs
import pile
import stream


type Kind {
  Setosa()
  Versicolor()
  Virginica()
}

record Flower(sepalLength: Double, sepalWidth: Double, petalLength: Double, petalWidth: Double)

record Model(setosa: Flower, versicolor: Flower, virginica: Flower)
// get input data
val data = [(Setosa(), Flower(5.0,3.4,1.5,0.2)), (Versicolor(), Flower(5.9,3.0,4.2,1.5)), (Virginica(), Flower(5.9,3.0,4.2,1.5)),
             (Setosa(), Flower(4.8,3.4,1.6,0.2)), (Versicolor(), Flower(6.0,2.2,4.0,1.0))]

```
We implement a `sampling` function for the flowers, that creates a random flower based on the measurements in cm given in the dataset.
It uses the variance of the values in the dataset as the variance in the sample calls for each of the four measurements of a flower.
```
def sampleFlower(flower: Flower): Flower / random = {
  val sepalLength = sampleGaussian(flower.sepalLength, 0.686)
  val sepalWidth  = sampleGaussian(flower.sepalWidth, 0.190)
  val petalLength = sampleGaussian(flower.petalLength, 3.116)
  val petalWidth  = sampleGaussian(flower.petalWidth, 0.581)
  Flower(sepalLength, sepalWidth, petalLength, petalWidth)
}

def sampleFlowers(model: Model) = {
  val kind = choose([Setosa(), Versicolor(), Virginica()])
  kind match {
    case Setosa() => sampleFlower(model.setosa)
    case Versicolor() => sampleFlower(model.versicolor)
    case Virginica() => sampleFlower(model.virginica)
  }
}
```
Equivalently we implement the `observe` function for the flowers which determines if a flower is probable to appear
in the context of the flowers provided in the dataset and represented in the model.
```
def observeFlower(observedFlower: Flower, flower: Flower) = {
  observeGaussian(observedFlower.sepalLength, flower.sepalLength, 0.686)
  observeGaussian(observedFlower.sepalWidth,  flower.sepalWidth, 0.190)
  observeGaussian(observedFlower.petalLength, flower.petalLength, 3.116)
  observeGaussian(observedFlower.petalWidth,  flower.petalWidth, 0.581)
}

def observeFlowers(flowers: List[(Kind, Flower)], model: Model) = {
  flowers.foreach {
    case (Setosa(), flower) => flower.observeFlower(model.setosa)
    case (Versicolor(), flower) => flower.observeFlower(model.versicolor)
    case (Virginica(), flower) => flower.observeFlower(model.virginica)
  }
}
```
Given the input of the dataset in a list, we can build the `model` of the flowers in the dataset. 
We start a-priori with uniform distributed flowers between the respective minimum and maximum measurements
for each of the four dimensions of the flowers.
```
def learnModel(flowers: List[(Kind, Flower)]): Model / {random, loss} = {
  val model = Model(Flower(sampleUniform(4.0, 8.0), sampleUniform(2.0, 4.5), sampleUniform(1.0, 7.0), sampleUniform(0.0, 2.5)),
                    Flower(sampleUniform(4.0, 8.0), sampleUniform(2.0, 4.5), sampleUniform(1.0, 7.0), sampleUniform(0.0, 2.5)),
                    Flower(sampleUniform(4.0, 8.0), sampleUniform(2.0, 4.5), sampleUniform(1.0, 7.0), sampleUniform(0.0, 2.5)))
  observeFlowers(flowers, model)
  return model
}
```
With the prediction function we use the learned model to predict the kind of a new flower we just measured.
```
def predictFlower(model: Model, flower: Flower): Kind / {fork, fail, loss} = {
  val kind = choose([Setosa(), Versicolor(), Virginica()])
  kind match {
    case Setosa() => flower.observeFlower(model.setosa)
    case Versicolor() => flower.observeFlower(model.versicolor)
    case Virginica() => flower.observeFlower(model.virginica)
  }
  return kind
}

```
Now we learn the model from our input `data` and then predict the kind of a flower with the measurements
`(6.0, 2.0, 1.5, 2.0)`. With the function `repeat` we generate 10 of these predictions for the kind.
Because this is based on probability, our prediction is not deterministic and thus the outputs describe a
distribution over the possible iris kinds for this flower.

```
def linearCongruentialGenerator[R](seed: Int) { prog: => R / random } : R = {
  // parameters from Numerical Recipes (https://en.wikipedia.org/wiki/Linear_congruential_generator)
  val a = 1664525
  val c = 1013904223
  val m = 1073741824
  var x: Int = seed;
  def next() = { x = mod((a * x + c), m); x }
  try { prog() }
  with random {
    resume(next().toDouble / m.toDouble)
  }
}
```

```effekt:repl
for[Kind]{
  with linearCongruentialGenerator(1)
  repeat(10){
  do emit(enumerationSampling {
    val model = learnModel(data)
    val predictedKind = predictFlower(model, Flower(6.0, 2.0, 1.5, 2.0))
    return predictedKind
  })}
} {
  case Setosa() => println("Setosa")
  case Versicolor() => println("Versicolor")
  case Virginica() => println("Virginica")
}
``` 
This case study was implemented in the research project "Repeatedly Handling Rejection" by Sina Schäfer,
in which the library `probs` for probabilistic programming was also implemented.