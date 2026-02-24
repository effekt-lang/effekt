import sys

length = int(sys.argv[1])

print("import examples/benchmarks/runner")

fields = [str(i) for i in range(length)]
field_types = ", ".join([f"x{i}: Int" for i in fields])
result_field_sum = " + ".join([f"result.x{i}" for i in fields])
field_values = ", ".join([f"i + {i}" for i in fields])
shifted_fields = ", ".join([f"m.x{i}" for i in (fields[-1:] + fields[:-1])])

print(f"record Rec({field_types})")
print(f"""
def recfunc(m: Rec, depth: Int): Rec = {{
    if (depth <= 0) {{ m }}
    else {{recfunc(Rec({shifted_fields}), depth - 1)}}
}}
""")

print(f"""
def runBenchmark(n: Int): Int = {{
  def loop(i: Int, acc: Int): Int = {{
    if (i <= 0) {{ acc }}
    else {{
      val rec = Rec({field_values})
      val result = recfunc(rec, 2)
      loop(i - 1, acc + {result_field_sum})
    }}
  }}
  loop(n, 0)
}}
""")

print("def main() = benchmark(1000000){ n => runBenchmark(n) }")
