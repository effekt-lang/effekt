import sys

nesting = int(sys.argv[1])

print("import examples/benchmarks/runner")

rec_constructor = "Rec0(1, 1)"

for i in range(nesting):
    if i == 0:
        print("record Rec0(a: Int, b: Int)")
        print("""
def recfunc0(m: Rec0): Int = {
    m.a + m.b
}
""")
    else:
        rec_constructor = f"Rec{i}({rec_constructor}, 1)"
        print(f"record Rec{i}(a: Rec{i-1}, b: Int)")
        print(f"""
def recfunc{i}(m: Rec{i}): Int = {{
    recfunc{i-1}(m.a) + m.b
}}
""")

print(f"""
def runBenchmark(n: Int): Int = {{
  def loop(i: Int, acc: Int): Int = {{
    if (i <= 0) {{ acc }}
    else {{
      val rec = {rec_constructor}
      loop(i - 1, acc + recfunc{nesting - 1}(rec))
    }}
  }}
  loop(n, 0)
}}
""")

print("def main() = benchmark(1000000){ n => runBenchmark(n) }")
