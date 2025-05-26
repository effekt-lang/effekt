---
layout: docs
title: Sequential Monte Carlo
draft: true
permalink: docs/casestudies/smc
---

# Sequential Monte Carlo


```effekt:hide
import list
import option
import array

extern type Cont[A, R]
extern def apply[A, R](k: Cont[A, R], x: A): R =
  js "RUN(function (ks, k) { ${k}(${x}, ks, k) })"
extern def cont[A, R] {k: A => R }: Cont[A, R] =
  js "${box k}"
```

In this case study we implement the Sequential Monte Carlo algorithm for doing
probabilistic inference. The idea is to run multiple instances of some
probabilistic process (so called particles) and occasionally resample from
the collection of these instances, while they are still running, according to
the weight they picked up so far.

We define the following `SMC` effect to model probabilistic processes and the
`Measure` effect to deal with the results.
```
interface SMC {
  def resample(): Unit
  def uniform(): Double
  def score(d: Double): Unit
}
```

We can use the `SMC` effect to define some probabilistic programs.
```
def bernoulli(p: Double) = do uniform() < p

def biasedGeometric(p: Double): Int / SMC = {
  do resample();
  val x = bernoulli(p);
  if (x) {
    do score(log(1.5));
    1 + biasedGeometric(p)
  } else { 1 }
}
```
Here `bernoulli` draws from a Bernoulli distribution (a biased coin flip) and
`biasedGeometric` draws from a Geometric distribution with a bias towards
smaller numbers.

## A SMC Handler
A particle consists of its current _weight_ (or "score"), the _age_ (number of resampling generations it survived -- not used at the moment),
and the continuation that corresponds to the remainder of its computation.
```
record Particle(weight: Double, age: Int, cont: Cont[Unit, Unit])
record Measurement[R](weight: Double, data: R)

record Particles[R](moving: List[Particle], done: List[Measurement[R]])
```

Using the above data types, we can define our SMC handler as follows:
```
def smcHandler[R](numberOfParticles: Int) {
  // should maintain the number of particles.
  resample: Particles[R] => Particles[R]
} { p: () => R / SMC } = {
  var currentWeight = 1.0;
  var particles: List[Particle] = Nil()
  var measurements: List[Measurement[R]] = Nil()
  var currentAge = 0;

  def checkpoint(cont: Cont[Unit, Unit]) =
    particles = Cons(Particle(currentWeight, currentAge, cont), particles)

  def run(p: Particle): Unit = {
    currentWeight = p.weight;
    currentAge = p.age;
    p.cont.apply(())
  }

  def run() = {
    val Particles(ps, ms) = resample(Particles(particles, measurements));
    particles = Nil();
    measurements = ms;
    ps.foreach { p => p.run }
  }

  repeat(numberOfParticles) {
    currentWeight = 1.0;
    try {
      val res = p();
      measurements = Cons(Measurement(currentWeight, res), measurements)
    } with SMC {
      def resample() = checkpoint(cont { t => resume(t) })
      def uniform() = resume(random())
      def score(d) = { currentWeight = currentWeight * d; resume(()) }
    }
  }

  while (not(particles.isEmpty)) { run() }
  measurements
}
```
It runs `numberOfParticles`-many instances of the provided program `p` under a handler that collects the
continuation whenever a particle encounters a call to `resample`. Once all particles either finished their
computation or hit a `resample`, the handler passes the list of live particles to the argument function `resample`.
This argument function then draws particles from the given list according to their weights to obtain a new list
of particles. Thus, the new list will likely not contain particles with small weights while there will likely be
multiple copies of particles with large weights.

## Resampling
We now implement such a (naive) resampling function. It proceeds by first filling an array (100 times the particle count)
with a number of copies of the particles relative to their weight.
Then it picks new particles at random, resetting the weights in the new list.
```
def resampleUniform[R](particles: Particles[R]): Particles[R] = {
  val Particles(ps, ms) = particles;
  val total = ps.totalWeight + ms.totalWeight
  val numberOfParticles = ps.size + ms.size
  val targetSize = numberOfParticles * 100;

  var newParticles: List[Particle] = Nil();
  var newMeasurements: List[Measurement[R]] = Nil();

  // select new particles by drawing at random
  // this is a very naive implementation with O(numberOfParticles^2) worst case.
  def draw() = {
    val targetWeight = random() * total;
    var currentWeight = 0.0;
    var remainingPs = ps
    var remainingMs = ms
    while (currentWeight < targetWeight) {
      (remainingPs, remainingMs) match {
        case (Nil(), Nil()) => <> // ERROR should not happen
        case (Cons(p, rest), _) =>
          currentWeight = currentWeight + p.weight
          if (currentWeight >= targetWeight) {
            newParticles = Cons(Particle(1.0, p.age, p.cont), newParticles)
          } else { remainingPs = rest }
        case (Nil(), Cons(m, rest)) =>
          currentWeight = currentWeight + m.weight
          if (currentWeight >= targetWeight) {
            newMeasurements = Cons(Measurement(1.0, m.data), newMeasurements)
          } else { remainingMs = rest }
      }
    }
  }

  repeat(numberOfParticles) { draw() }

  Particles(newParticles, newMeasurements)
}

// helper function to compute the total weight
def totalWeight(ps: List[Particle]): Double = {
  var totalWeight = 0.0
  ps.foreach { case Particle(w, _, _) =>
    totalWeight = totalWeight + w
  }
  totalWeight
}
def totalWeight[R](ps: List[Measurement[R]]): Double = {
  var totalWeight = 0.0
  ps.foreach { case Measurement(w, _) =>
    totalWeight = totalWeight + w
  }
  totalWeight
}
```

Now we have everything available to define `smc` as `smcHandler` using `resampleUniform`:
```
def smc[R](numberOfParticles: Int) { p: () => R / SMC } =
  smcHandler[R](numberOfParticles) { ps => resampleUniform(ps) } { p() }
```



## Importance Sampling
Of course the above handler is not the only one. We can define an even simpler handler
that performs importance sampling by sequentially running each particle to the end.
```
def importance[R](n: Int) { p : => R / SMC } = {
  var measurements: List[Measurement[R]] = Nil()
  n.repeat {
    var currentWeight = 1.0;
    try {
      val result = p();
      measurements = Cons(Measurement(currentWeight, result), measurements)
    } with SMC {
      def resample() = resume(())
      def uniform() = resume(random())
      def score(d) = { currentWeight = currentWeight * d; resume(()) }
    }
  }
  measurements
}
```

### Running the Examples

```effekt:hide
extern async def sleep(n: Int): Unit =
  jsWeb "$effekt.capture(k => window.setTimeout(() => k(null), n))"

// here we set a time out to allow rerendering
extern async def reportMeasurementJS[R](w: Double, d: R): Unit =
  jsWeb "$effekt.capture(k => { showPoint(w, d); window.setTimeout(() => k(null), 0)})"

extern async def reportDiscreteMeasurementJS[R](w: Double, d: R): Unit =
  jsWeb "$effekt.capture(k => { showPoint(w, d, { discrete: true }); window.setTimeout(() => k(null), 0)})"


// here we set a time out to allow rerendering
extern io def setupGraphJS(): Unit =
  jsWeb "setup()"
```
To visualize the results, we define the following helper function `report` that
handles `Measure` effects by adding the data points to a graph (below).
```
def report[R](interval: Int, ms: List[Measurement[R]]) = {
  setupGraphJS();
  ms.foreach { m =>
    reportMeasurementJS(m.weight, m.data);
    sleep(interval)
  }
}
```
```effekt:hide
def reportDiscrete[R](interval: Int, ms: List[Measurement[R]]) = {
  setupGraphJS();
  ms.foreach { m =>
    reportDiscreteMeasurementJS(m.weight, m.data);
    sleep(interval)
  }
}
```

Running SMC and importance sampling now is a matter of composing the handlers.
```
def runSMC(numberOfParticles: Int) =
  report(20, smc(numberOfParticles) { biasedGeometric(0.5) })
```

```
def runImportance(numberOfParticles: Int) =
  report(20, importance(numberOfParticles) { biasedGeometric(0.5) })
```

We have also prepared a handler called `reportDiscrete` to experiment with examples that
have non-integer return types:
```
def runDiscrete(numberOfParticles: Int) =
  reportDiscrete(0, smc(numberOfParticles) {
    if (bernoulli(0.5)) { "hello" } else { "world" }
  })
```

```effekt:repl
runDiscrete(15)
runSMC(100)
```


In the below REPL you can try the examples. Click `run` and then try entering `runSMC(100)` (then click `run` again):
```effekt:repl

```


<div id="graph"></div>
<p>Particles: <span id="count"></span></p>

<script src="https://d3js.org/d3.v6.js"></script>


<script>

// Good articles on d3:
// - https://bost.ocks.org/mike/selection/
// - https://bost.ocks.org/mike/join/

var count = 0
const counter = document.getElementById("count")


// holds our data
var data = []

const margin = {top: 30, right: 30, bottom: 70, left: 60},
    width = 460 - margin.left - margin.right,
    height = 400 - margin.top - margin.bottom;

// append the svg object to the body of the page
const svg = d3.select("#graph")
  .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

// X axis
const x = d3.scaleBand().range([ 0, width ]).padding(0.2);

// Y axis
const y = d3.scaleLinear().domain([0, 1]).range([ height, 0]);

// Axis
const xaxis = svg.append("g")
  .attr("class", "x axis")
  .attr("transform", "translate(0," + height + ")")
  .call(d3.axisBottom(x))

const yaxis = svg.append("g")
  .attr("class", "y axis")
  .call(d3.axisLeft(y));

const bar = svg.selectAll(".bar")
  .data(data)


function scaleDiscrete(data) {
  x.domain(data.map(function(d) { return d.value; }))
  xaxis.call(d3.axisBottom(x))
}

function scaleLinear(data) {
  const minValue = d3.min(data, function(d) { return d.value; })
  const maxValue = d3.max(data, function(d) { return d.value; })

  x.domain(range(minValue, maxValue))
  xaxis.call(d3.axisBottom(x))
}


function setup() {
  data = []
  count = 0
  counter.innerHTML = ""
}

// returns an array from n to m (including both ends)
function range(n, m) {
  var arr = [];
  while (n <= m) { arr.push(n); n++ }
  return arr
}

function render(data) {

  // use this for rescaling the y axis:
  //y.domain([0, d3.max(data, function(d) { return d.weight; })]);
  //yaxis.call(d3.axisLeft(y));

  function xValue(d) { return x(d.value) }
  function yWeight(d) { return y(d.weight) }
  function barHeight(d) { return height - y(d.weight) }
  const barWidth = x.bandwidth()


  // Bars
  const bar = svg.selectAll(".bar")
    .data(data);

  bar.exit()
    .remove();

  bar.enter()
    // code that will be run to add new elements
    .append("g")
      .attr("class", "bar")
    .append("rect")
      .attr("x", xValue)
      .attr("y", yWeight)
      .attr("width", barWidth)
      .attr("height", barHeight)
      .attr("fill", "#69b3a2")
    .merge(bar)
      // code that will be run to update elements
      .transition()
      .duration(0)
      .select("rect")
        .attr("x", xValue)
        .attr("y", yWeight)
        .attr("width", barWidth)
        .attr("height", barHeight)
}


function normalize(data) {
  var sum = 0;
  for (let entry of data) {
    sum = sum + entry.weight
  }
  var newdata = []
  for (let entry of data) {
     newdata.push({ value: entry.value, weight: entry.weight / sum })
  }
  return newdata;
}

function addPointToData(weight, value) {
  var found = false;
  for (var i = 0; i < data.length; i++) {
    let el = data[i]
    if (el.value === value) {
      el.weight = el.weight + weight;
      found = true
    }
  }
  if (!found) {
    data.push({ weight, value })
  }
}

function showPoint(weight, value, options) {
  count = count + 1;
  counter.innerHTML = count;
  addPointToData(weight, value)
  const normalized = normalize(data)
  if (!!options && !!options.discrete) {
    scaleDiscrete(normalized)
  } else {
    scaleLinear(normalized)
  }
  render(normalized)
}

</script>