import sys

nesting = int(sys.argv[1])

print("import examples/benchmarks/runner")

rec_constructor = "Rec0(1, 1)"

for i in range(nesting):
    if i == 0:
        print("record Rec0(a: Int, b: Int)")
        print("""
def recfunc0(m: Rec0, depth: Int): Rec0 = {
    m
}
""")
    else:
        rec_constructor = f"Rec{i}({rec_constructor}, {i} - i)"
        print(f"record Rec{i}(a: Rec{i-1}, b: Int)")
        print(f"""
def recfunc{i}(m: Rec{i}, depth: Int): Rec{i} = {{

    if (depth <= 0) {{ Rec{i}(recfunc{i-1}(m.a, 2), m.b) }}
    else {{ recfunc{i}(m, depth - 1) }}
   
}}
""")

print(f"""
def runBenchmark(n: Int): Int = {{
  def loop(i: Int, acc: Int): Int = {{
    if (i <= 0) {{ acc }}
    else {{
      val rec = {rec_constructor}
      val result = recfunc{nesting - 1}(rec, 2)
      loop(i - 1, acc + result.b)
    }}
  }}
  loop(n, 0)
}}
""")

print("def main() = benchmark(1000000){ n => runBenchmark(n) }")
