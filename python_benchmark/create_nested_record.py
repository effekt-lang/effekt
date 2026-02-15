import sys

nesting = int(sys.argv[1])

print("import examples/benchmarks/runner")

endrecord = "record Rec0(a: Int, b: Int)" 
endfunc = """def recfunc0(m: Rec0): Int = {
    m.a + m.b
}
"""

recacc = "Rec0(1, 1)"

for i in range(nesting):
    if i == 0:
        print(endrecord)
        print(endfunc)
    else:
        recacc = f"Rec{i}({recacc}, 1)"
        print(f"record Rec{i}(a: Rec{i-1}, b: Int)")
        print(f"""def recfunc{i}(m: Rec{i}): Int = {{
    recfunc{i-1}(m.a) + m.b
}}
""")

print(f"""
def runBenchmark(n: Int): Int = {{
  def loop(i: Int, acc: Int): Int = {{
    if (i <= 0) {{ acc }}
    else {{
      rec = {recacc}
      loop(i - 1, acc + recfunc{nesting}(rec))
    }}
  }}
  loop(n, 0)
}}
""")

print("def main() = benchmark(1000000){ n => runBenchmark(n) }")
