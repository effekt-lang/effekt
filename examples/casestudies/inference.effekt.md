---
layout: docs
title: Inference
permalink: docs/casestudies/inference
---

# Inference
This case study shows how we can perform inference over probabilistic models with the help of effects.

---

We require some imports

```
import list
extern pure def pow(x: Double, y: Double): Double =
  js "Math.pow(${x}, ${y})"
```

We need some effects to peform probabilistic operations

```
effect sample(dist: Distribution): Double
effect observe(value: Double, dist: Distribution): Unit
effect random(): Double
effect weight(prob: Probability): Unit
interface Emit[A] {
  def emit(element: A): Unit
}
```

With the effect `sample`, we can sample from a given distribution, and with the effect `observe` we can determine if it is possible to get a given value from a distribution.

We also need to define some type aliases:

```
type Distribution{
  Gaussian(mean: Double, variance: Double)
  Uniform(lower: Double, upper: Double)
  Beta(mean: Double, sampleSize: Double)
}
type Probability = Double
type Trace = List[Double]
```

Currently, we can support models with Gaussian, uniform or beta distributions.

```
def draw(dist: Distribution): Double / random = {
  dist match {
    case Gaussian(mean, variance) =>
      val u1 = do random();
      val u2 = do random();
      val sigma = variance
      //box-muller transform
      val mag = sigma * sqrt(-2.0 * log(u1))
      val z0 = mag * cos(2.0 * PI * u2)
      return z0 + mean

    case Uniform(lower, upper) =>
      val x = do random()
      return (lower + x * upper)

    case Beta(mean, sampleSize) =>
      val alpha = mean * sampleSize
      return draw(Gaussian(0.5, 1.0 / (4.0 * (2.0 * alpha + 1.0))))
  }
}
def gamma(z0: Double): Double = {
  var z = z0
  //g is a small integer and p is a list of coeficients dependent on g
  val g = 7.0
  val p = [0.99999999999980993,
    676.5203681218851,
    -1259.1392167224028,
    771.32342877765313,
    -176.61502916214059,
    12.507343278686905,
    -0.13857109526572012,
    9.9843695780195716 * pow(10.0, 0.0 - 6.0),
    1.5056327351493116 * pow(10.0, 0.0 - 7.0)]
  // takes the i-th element of a list
  def takeith(i: Double, p0: List[Double]): Double = {
    p0 match {
      case Nil() => panic("empty list")
      case Cons(p, ps) =>
        if(i == 1.0) {p}
        else {takeith(i - 1.0, ps)}
    }
  }
  //lanczos approximation
  if (z < 0.5) {
    val y = PI / (sin(PI * z) * gamma(1.0 - z))
    return y
  } else {
    z = z - 1.0
    var x = takeith(1.0, p)
    var i = 2.0
    while (i <= 9.0) {
      x = x + (takeith(i, p) / (z + i - 1.0))
      i = i + 1.0
    }
    val t = z + g + 0.5
    val y = sqrt(2.0 * PI) * pow(t, (z + 0.5)) * exp(0.0 - t) * x
    return y
  }
}
//probability density function for Gaussian distribution
def density(value: Double, dist: Distribution): Double = {
  dist match {
    case Gaussian(mean, variance) =>
      val density = 1.0 / (sqrt(variance) * (sqrt(2.0 * PI))) * exp((0.0 - 1.0 / 2.0) * (square(value - mean) / variance))
      return density

    case Uniform(lower, upper) =>
      if (lower < value && value < upper) { 1.0 / (upper - lower) }
      else { 0.0 }

    case Beta(mean, sampleSize) =>
      val alpha = mean * sampleSize
      val beta = (1.0 - mean) * sampleSize
      val density = (gamma(alpha + beta) / (gamma(alpha) * gamma(beta))) * pow(value, (alpha - 1.0)) * pow((1.0 - value), (beta - 1.0))
      return density
  }
}
```

We define default handlers for all effects in this library

```
def handleSample[R] { program: () => R / sample } = {
  try { program() }
  with sample { dist => resume(draw(dist)) }
}
def handleObserve[R] { program: () => R / observe } = {
  try { program() }
  with observe { (value, dist) => do weight(density(value, dist)); resume(()) }
}
def handleRandom[R] { program: () => R / random }: R = {
  try { program() }
  with random { () => resume(random()) }
}
// pseudo random number generator
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
def handleWeight[R] { program: () => R / weight }: (R, Double)  = {
  var current = 1.0
  try { (program(), current) }
  with weight { (prob) =>
    current = current * prob;
    resume(())
  }
}
```

With the effect `emit` it is also possible to limit possibly infinite loops. This allows for flexible numbers of steps to be performed with any algorithm that uses the effect `emit`.

```
def onEmit[A] { handler: A => Unit } { program: => Any / Emit[A] }: Unit = {
  try { program(); () }
  with Emit[A] {
    def emit(element) = { handler(element); resume(()) }
  }
}

def limit[A](n: Int) { program: => Any / Emit[A] }: Unit / Emit[A] = {
  var steps = n
  try { program(); () }
  with Emit[A] {
    def emit(element) = {
      do emit(element);
      steps = steps - 1;
      if (steps > 0) { resume(()) }
    }
  }
}

def collect[A,R](n: Int){ program: () => R / Emit[A] }: List[A] = {
  var result: List[A] = Nil()
  var steps = n
  try { program(); () }
  with Emit[A] {
    def emit(element) = {
      result = Cons(element, result);
      steps = steps - 1;
      if (0 < steps) { resume(()) }
    }
  }
  result.reverse
}
```

## Rejection Sampling
The effect `Weight` is also the basis of the rejection handling algorithm

```
def handleRejection[R] { program: () => R / weight }: R / random = {
  try { program() }
  with weight { (prob) =>
    if (do random() < prob) { resume(()) }
    else { handleRejection{ program } }
  }
}
```

It is possible to construct the rejection sampling algorithm with the handlers of this library:

```
def rejectionSampling[R](n: Int) { program: () => R / { sample, observe } }: Unit / { random, Emit[R] } = {
  def loop(): Unit / Emit[R] = {
    with handleSample
    with handleRejection
    with handleObserve
    do emit(program())
    loop()
  }
  loop()
}
```

## Slice Sampling

```
def sliceSamplingAlgo[R]() { program: () => R / weight } = {
  val (result, prob) = handleSample { handleWeight { program() }}

  def step(result0: R, prob0: Probability) = handleRejection {
    val (result1, prob1) = handleWeight { program() }
    if (prob1 < prob0) { do weight(prob1 / prob0) }
    (result1, prob1)
  }
  def loop(result: R, prob: Probability): Unit / Emit[R] = {
    do emit(result)
    val (result1, prob1) = step(result, prob)
    loop(result1, prob1)
  }
  loop(result, prob)
}
```

## Metropolis-Hastings

### Tracing
A trace records past samples used by an algorithm. The trace also controls where the algorithm draws samples from in the next iterations.
Constructing traces is possible by handling the effect `Sample`, not with the default handler, but one that draws new samples and records them in a trace.

```
def handleTracing[R] { program: () => R / sample }: (R, Trace) / sample = {
  try { (program(), []) }
  with sample { (dist) =>
    val d = do sample(dist);
    val (r, trace) = resume(d);
    (r, Cons(d, trace))
  }
}
def handleReusingTrace[R](trace0: Trace) { program: () => R / sample } = handleObserve {
  var trace = trace0
  try { program() }
  with sample { (dist) =>
    trace match {
      case Nil() => panic("empty trace")
      case Cons(t, ts) =>
        do observe(t, dist)
        trace = ts; resume(t)
    }
  }
}
```

### Proposing samples
How the candidates of this algorithm are proposed can vary between different implementations of the algorithm and thus creating inference of the Metropolis-Hastings algorithm.
The Metropolis-Hastings algorithm proposes a new trace based on the old trace, by recursively adding noise to the samples in the old trace and thus creating a new trace.

```
def propose(trace: Trace): Trace / sample = {
  trace match {
    case Nil() => []
    case Cons(t, ts) =>
      val noise = do sample(Gaussian(0.0, 1.0))
      val proposal = t + noise
      Cons(proposal, propose(ts))
  }
}
```

### Metropolis-Hastings algorithm
The algorithm is implemented with a helper function

```
def metropolisStep[A](prob0: Probability, trace0: Trace) { program: Trace => (A, Probability) } = {
  val trace1 = propose(trace0)
  val (result1, prob1) = program(trace1)
  if (prob1 < prob0) {
    do weight(prob1 / prob0)
  }
  ((result1, trace1), prob1)
}
```

`metropolisStep` is then called in the algorithm implementation

```
def metropolisHastingsAlgo[A] { program: () => A / { sample, weight } } = {
  val ((result0, trace0), prob0) = handleWeight {
    with handleSample
    with handleTracing
    program()  
  }
  def loop(result: A, trace: Trace, prob: Probability): Unit / Emit[A] = {
    do emit(result)
    val ((result1, trace1), prob1) =
    handleSample {
      with handleRejection
      metropolisStep(prob, trace) { trace =>
        with handleWeight
        with handleReusingTrace(trace)
        program()
      }
    }
    loop(result1, trace1, prob1)
  }
  loop(result0, trace0, prob0)
}
```

### Single-Site Metropolis-Hastings
To perform inference over the Metropolis-Hastings algorithm, and therefore creating the single-site Metropolis-Hastings algorithm, we just need to alter the implementation of the `propose` function.
In this implementation, the noise is added to only one random sample in the trace and the other samples are reused.

```
def proposeSingleSite(trace: Trace): Trace / sample = {
  val tsize = toDouble(size(trace))
  val rand = do sample(Uniform(0.0, tsize))
  def putAti(i: Int, p0: List[Double]): List[Double] = {
    p0 match {
      case Nil() => []
      case Cons(p, ps) =>
        if (i == 0) {
          val noise = do sample(Gaussian(0.0, 1.0))
          Cons(p + noise, ps)
        }
        else {
          Cons(p, putAti(i - 1, ps))
        }
    }
  }
  putAti(floor(rand), trace)
}
```

The single-site Metropolis-Hastings algorithm is implemented like the Metropolis-Hastings algorithm above.

```
def metropolisStepSingleSite[A](prob0: Probability, trace0: Trace) { program: Trace => (A, Probability) } = {
  val trace1 = proposeSingleSite(trace0)
  val (result1, prob1) = program(trace1)
  if (prob1 < prob0){
    do weight(prob1 / prob0)
  }
  ((result1, trace1), prob1)
}
def metropolisHastingsSingleSiteAlgo[A] {program: () => A / { sample, weight } } = {
  val ((result0, trace0), prob0) = handleWeight{
    with handleSample
    with handleTracing
    program()
  }
  def loop(result: A, trace: Trace, prob: Probability): Unit / Emit[A] = {
    do emit(result)
    val ((result1, trace1), prob1) = handleSample {
      with handleRejection
      metropolisStepSingleSite(prob, trace) { trace => 
        with handleWeight
        with handleReusingTrace(trace)
        program()
      }
    }
    loop(result1, trace1, prob1)
  }
  loop(result0, trace0, prob0)
}
```

## Wrappers
In order to make it easier to use these algorithms without having to call the various effect handlers, we constructed wrappers for the algorithms implemented in this library.

```
def sliceSampling[R](n: Int) { program: () => R / { sample, observe } }: Unit / { random, Emit[R] } = {
  with handleSample
  with sliceSamplingAlgo[R]
  with handleObserve
  program()
}
def metropolisHastings[R](n: Int) { program: () => R / { sample, observe } }: Unit / { random, Emit[R] } = {
  with metropolisHastingsAlgo[R]
  with handleObserve
  program()
}
def metropolisHastingsSingleSite[R](n: Int) { program: () => R / { sample, observe } }: Unit / { random, Emit[R] } = {
  with metropolisHastingsSingleSiteAlgo[R]
  with handleObserve
  program()
}
```

## Examples

### Linear Regression
As a short example on how the effects `Sample` and `Observe` can be used in an model, we construct the linear regression model.

```
record Point(x: Double, y: Double)

def show(p: Point): String = {
  def rounded(d: Double) = round(d, 3).show
  "Point(" ++ p.x.rounded ++ ", " ++ p.y.rounded ++ ")"
}

def linearRegression(observations: List[Point]) = {
  val m = do sample(Gaussian(0.0, 3.0))
  val c = do sample(Gaussian(0.0, 2.0))
  observations.foreach {
    case Point(x,y) => do observe(y, Gaussian((m * x + c), 1.0))
  }
  return Point(m, c)
}
```

### Robot Movements
It is also possible to construct bigger examples on which we can apply the algorithms of this library.
In this example, the movements of a robot on a 2D-plane are observed. In the center of this plane, at the coordinates (0, 0), there is a radar station that can measure the distance to the robot at any point in time.
The current state of the robot, consisting of position and velocity.

```
record State(x: Double, y: Double, vx: Double, vy: Double)

def show(s: State): String = {
  val State(x, y, vx, vy) = s
  def rounded(d: Double) = round(d, 3).show
  "State(" ++ x.rounded ++ ", " ++ y.rounded ++ ", " ++ vx.rounded ++ ", " ++ vy.rounded ++ ")"
}
```

In this example we also define a type alias for `Measurement`

```
type Measurement = Double
// movement of the robot in one time intervall
def move(s: State): State / sample = {
  s match {
    case State(x, y, vx, vy) =>
      val xa = do sample(Gaussian(0.0, 1.0))
      val ya = do sample(Gaussian(0.0, 1.0))
      return State(x+vx+xa, y+vy+ya, vx+xa, vy+ya)
  }
}
// measures distance to robot and observes how probable position is given current state
def measure(s: State, m: Measurement): Unit / observe = {
  s match {
    case State(x, y, vx, vy) =>
      val dist = sqrt(square(x) + square(y))
      do observe(m, Gaussian(dist, 0.4))
  }
}
// approximates next step based on current position and next distance measurement
def step(s0: State, m1: Measurement): State / { sample, observe } = {
  val s1 = move(s0)
  measure(s1, m1)
  return s1
}
```

### Epidemic Spreading
This examples simulates a population in an epidemic with the SIR model. The SIR model divides the population into susceptible **S**, infected **I** and recovered **R**.

```
record Population(susceptible: Double, infected: Double, recovered: Double)

def show(s: Population): String = {
  val Population(susceptible, infected, recovered) = s
  def rounded(d: Double) = round(d, 3).show
  "Population(" ++ susceptible.rounded ++ ", " ++ infected.rounded ++ ", " ++ recovered.rounded ++ ")"
}

//multiply s,i,r parts of population with transition probabilitys to get new state of population
def progression(p: Population): Population / sample = {
  p match {
    case Population(s, i, r) =>
      val noise = do sample(Gaussian(0.0, 0.01))
      val s1 = 0.5 * s - noise
      val i1 = 0.3 * i + 0.5 * s + 0.1 * r + noise
      val r1 =  0.9 * r + 0.7 * i
      return Population(s1, i1, r1)
  }
}
//testing the popultaion(like covid test)
def test(p: Population, pr: Double): Unit / observe = {
  p match {
    case Population(s, i, r) =>
      val mean = i
      val sampleSize = 100.0
      do observe(pr, Beta(mean, sampleSize))
  }
}
//approximate next state of population based on current state
def step(p0: Population, pr1: Double): Population / { sample, observe } = {
  val p1 = progression(p0)
  test(p1, pr1)
  return p1
}
```

Calling the algorithms on the examples.

```
type Path = List[State]

def main() = {
  // random number generator used by all examples
  with linearCongruentialGenerator(1)

  // show instances for the different emitted values
  with onEmit[Point] { s => println(s.show) };
  with onEmit[State] { s => println(s.show) };
  with onEmit[Path] { path =>
    println("Path(")
    path.foreach { state => println("  " ++ state.show) }
    println(")")
  };
  with onEmit[Population] { p => () };
  with onEmit[List[Population]] { path =>
    println("[")
    path.foreach { p => println("  " ++ p.show) }
    println("]")
  };

  // Linear Regression
  limit[Point](5) {
    with rejectionSampling[Point](5)

    linearRegression([
      Point(5.0, 5.0),
      Point(1.0, 1.0),
      Point(-2.0, -2.0),
      Point(3.0, 3.0),
      Point(20.0, 20.0),
      Point(5.0, 5.0)
    ])
  }

  // Robot movements

  limit[State](5) {
    with linearCongruentialGenerator(1);
    with sliceSampling[State](5)

    val init = State(0.0, 3.0, 2.0, 0.0)
    step(init, 5.0)
  }

  limit[Path](5) {
    with sliceSampling[Path](1);

    var nextState = State(0.0, 3.0, 2.0, 0.0)
    var nextdis = 5.0
    var m = 5
    var path = [nextState]
    while (m > 0) {
      nextState = step(nextState, nextdis)
      val noise = do sample(Gaussian(0.0, 1.0))
      nextdis = nextdis + noise
      path = append(path, [nextState])
      m = m - 1
    }
    path
  }

  // Epidemic Spreading
  limit[Population](5) {
    with metropolisHastings[Population](5)

    val init = Population(0.7, 0.2, 0.1)
    step(init, 0.8)
  }


  limit[List[Population]](1) {
    with metropolisHastings[List[Population]](1)

    var nextPop = Population(0.7, 0.2, 0.1)
    var nextpr = 0.8
    var m = 5
    var path : List[Population] = [nextPop]
    while (m > 0) {
      nextPop = step(nextPop, nextpr)
      val noise = do sample(Gaussian(0.0, 0.01))
      var nextpr1 = nextpr + noise
      if (not(nextpr1 < 0.0 || nextpr1 > 1.0)) { nextpr = nextpr1 }
      path = append(path, [nextPop])
      m = m - 1
    }
    path
  }
}
```

The other algorithms of this library can be called alike on these examples.
