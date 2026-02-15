import sys

length = int(sys.argv[1])

print("import examples/benchmarks/runner")

list = [ str(i) for i in range(length)]

rec = f"record Rec(x{": Int, x".join(list)}: Int)" 
recfunc = f"""def recfunc(m: Rec): Int = {{
   m.x{" + m.x".join(list)} 
}}
"""
print(rec)
print(recfunc)

print(f"""
def runBenchmark(n: Int): Int = {{
  def loop(i: Int, acc: Int): Int = {{
    if (i <= 0) {{ acc }}
    else {{
      val rec = Rec(i + {" , i + ".join(list)})
      loop(i - 1, acc + recfunc(rec))
    }}
  }}
  loop(n, 0)
}}
""")

print("def main() = benchmark(1000000){ n => runBenchmark(n) }")
