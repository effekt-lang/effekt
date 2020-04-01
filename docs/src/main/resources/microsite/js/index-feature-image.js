(function() {
  document.querySelectorAll("#masthead .features-image").forEach(function (div) {

    console.log(div)
    div.innerHTML = ["<div class='pseudo-window'>",
    "  <div class='window-titlebar'>",
    "    <div class='button-close'></div>",
    "    <div class='button-minimize'></div>",
    "    <div class='button-maximize'></div>",
    "  </div>",
    "  <div class='window-contents'>",
    "<pre><code class='language-effekt'>def eager[R] { p: R / { Flip, Fail, Error } } = try {",
    "  Success(p())",
    "} with {",
    "  case Flip() => resume(true) match {",
    "    case Failure(msg) => resume(false)",
    "    case Success(res) => Success(res)",
    "    case ParseError(msg) => ParseError(msg)",
    "  }",
    "  case Fail(msg) => Failure(msg)",
    "  case Error(msg) => ParseError(msg)",
    "}",
    "</code></pre>",
    "  </div>",
    "</div>"].join("\n")
  })
})()
