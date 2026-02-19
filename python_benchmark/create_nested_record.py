import sys

nesting = int(sys.argv[1])

print("import examples/benchmarks/runner")

rec_constructor = "Rec0(1, 1)"

for i in range(nesting):
    if i == 0:
        print("record Rec0(a: Int, b: Int)")
        print("""
def recfunc0(m: Rec0, depth: Int): Int = {
    m.a + m.b
}
""")
    else:
        rec_constructor = f"Rec{i}({rec_constructor}, {i} - i)"
        print(f"record Rec{i}(a: Rec{i-1}, b: Int)")
        print(f"""
def recfunc{i}(m: Rec{i}, depth: Int): Int = {{

    if (depth <= 0) {{ recfunc{i-1}(m.a, 2) + m.b }}
    else {{ recfunc{i}(Rec{i}(m.a, m.b + 1), depth - 1) + m.b }}
   
}}
""")

print(f"""
def runBenchmark(n: Int): Int = {{
  def loop(i: Int, acc: Int): Int = {{
    if (i <= 0) {{ acc }}
    else {{
      val rec = {rec_constructor}
      loop(i - 1, acc + recfunc{nesting - 1}(rec, 2))
    }}
  }}
  loop(n, 0)
}}
""")

print("def main() = benchmark(1000000){ n => runBenchmark(n) }")
