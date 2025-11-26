module thesis_trmc/kk_custom_map

fun fibonacci(n : int) : div int
  fib(n, 0, 1)


fun fib(n : int, x1 : int, x2 : int) : div int
  if n<=0 then x1 else fib(n - 1, x2, x1+x2)

tail fun custom_map(list : list<a>, ^f : a -> e b) : e list<b>
  match list
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x), xx.custom_map(f))

pub fun main()
  custom_map([1,7,8,4],fibonacci).println()