---
layout: docs
title: Inference
permalink: docs/casestudies/scheduler
---

# Scheduler

```effekt:prelude:hide
import dequeue
```

Writing a safe, cooperatively threaded scheduler in an effect safe manner can be hard, as it requires treating continuations as first-class values.
Continuations might indirectly close over other capabilities, and we want to prevent that capabilities from leaving their defining scope indirectly through continuations.

First, we define an `Proc` as the interface for our scheduler.
This models a process that either can
- `yield` such that other scheduled processes can be executed,
- `fork` itself such two new processes are spawned such that one continues with `true` and the other with `false`
- or `exit` once it is done.

```effekt
interface Proc {
  def yield(): Unit
  def fork(): Bool
  def exit(): Unit
}
```

For the handler of a program `prog` that requires the `Proc` effect to be handled, we first create a `queue`, storing thunks of the continuation of processes.
We define `yield` to push a thunk of the continuation onto the queue.
Similarly, `fork` pushes two thunks of the continuation onto the queue. One will resume with `true` and the other with `false`.
Finally, `exit`ing just is a NOP.
In `run`, we go through the `queue` and continuously pop the `queue` to get the last recently enqueued process, and run he thunked continuation.
Forcing a thunk resumes a process right where `yield` or `fork` was called.

```effekt
def scheduler { prog: => Unit / Proc } = region this {
  var queue: Dequeue[() => Unit at {this, prog}] in this = emptyQueue();

  def run(): Unit = queue.popBack match {
    case None() => ()
    case Some((k, q)) =>
      queue = q
      k();
      run()
  }
  try { prog() } with Proc {
    def yield() = {
      queue = queue.pushFront(box { resume(()) })
    }
    def fork() = {
      queue = queue
        .pushFront(box { resume(true) })
        .pushFront(box { resume(false) })
    }
    def exit() = ()
  }

  run()
}
```

Note that as `prog` is a second-class argument, it can use additional capabilities that are not reflected in its type (but are on its binder) due to Effekt's contextual effect polymorphism.
In particular, these capabilities may be captured on the continuation term resume.
For that reason, we have to annotate it as a capture for the first-class function stored in the `queue`.
Furthermore, since the handler uses mutable state, namely `queue`, `resume` also captures it when `box`ing it.
However, as those capabilities are second-class, they cannot leak through the resumption and the entire program is safe. In particular, the resumption cannot leak even though it is stored in the mutable cell `queue`, as `queue` is second-class itself and valid only within the context of the scheduler region.
You can think about it like a lifetime annotation: we are only allowed to use the thunked continuations in the form of first-class functions, while `queue` is still alive.
Since `run` is within the same region `queue` is allocated in, forcing the thunks is allowed.

For convenience, we define a helper function for `fork`ing:

```effekt
def fork { b1: => Unit / Proc } { b2: => Unit / Proc } =
  if (do fork()) { b1() } else { b2() }
```

Finally, we run some examples using our `scheduler` by `fork`ing once to get two concurrent processes and then letting each run concurrently.

```
def main() = {
  scheduler {
    fork {
      println("hello 1")
      do yield()
      println(1)
      do yield()
      println(2)
      do exit()
      println(3)
    } {
      println("hello 2")
      do yield()
      println(4)
      do yield()
      println(5)
      do yield()
      println(6)
    }
  }
}
```

Try guessing first what you expect the output to be and then run it:

```effekt:repl
main()
```
