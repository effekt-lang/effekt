// @ts-check
// This code is derived from the SOM benchmarks, see AUTHORS.md file.
//
// Copyright (c) 2015-2016 Stefan Marr <git@stefan-marr.de>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the 'Software'), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

class Element {
  constructor(v) {
    this.val = v;
    this.next = null;
  }

  length() {
    if (this.next === null) {
      return 1;
    }
    return 1 + this.next.length();
  }
}
class List {
  benchmark() {
    const result = this.tail(
      this.makeList(15),
      this.makeList(10),
      this.makeList(6)
    );
    return result.length();
  }

  makeList(length) {
    if (length === 0) {
      return null;
    }
    const e = new Element(length);
    e.next = this.makeList(length - 1);
    return e;
  }

  isShorterThan(x, y) {
    let xTail = x;
    let yTail = y;

    while (yTail !== null) {
      if (xTail === null) { return true; }
      xTail = xTail.next;
      yTail = yTail.next;
    }
    return false;
  }

  tail(x, y, z) {
    if (this.isShorterThan(y, x)) {
      return this.tail(
        this.tail(x.next, y, z),
        this.tail(y.next, z, x),
        this.tail(z.next, x, y)
      );
    }
    return z;
  }

  verifyResult(result) {
    return 10 === result;
  }
}

exports.newInstance = () => new List();
