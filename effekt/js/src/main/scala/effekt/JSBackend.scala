package effekt

trait Backend {
  def compiler: generator.js.JavaScriptWeb | generator.vm.VM
  def runner: Unit
  def name: String
}

class JSBackend extends Backend {
  val compiler = generator.js.JavaScriptWeb()
  val runner = ()
  val name = "js-web"
}

class VMBackend extends Backend {
  val compiler = generator.vm.VM()
  val runner = ()
  val name = "vm"
}

