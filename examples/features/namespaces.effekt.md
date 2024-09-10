---
layout: docs
title: Namespaces
permalink: docs/tutorial/namespaces
---

# Namespaces

For defininig a namespace, the keyword `namespace` followed by the name of the namespace is to be used. Enclosed in curly braces, the namespace can include
function, type and effect definitions as well as further nested namespaces:

```
namespace outer {
  
  def double(n: Int): Int = n * 2

  namespace inner {
    val x = 21
  }
}
```

You may use double colons for accessing namespaces:

```effekt:repl
outer::inner::double(outer::inner::x)
```
