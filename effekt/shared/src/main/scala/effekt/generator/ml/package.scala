package effekt
package generator
package ml

import effekt.context.Context
import effekt.core.*
import effekt.symbols.{ Module, Symbol, Wildcard, Name }

import scala.language.implicitConversions
import effekt.util.paths.*


def uniqueName(id: Symbol): String = id.name.toString.replace("?", "").replace("!", "") + "_" + id.id

def name(id: Symbol): MLName = MLName(uniqueName(id))

def name(id: Name): MLName = MLName(id.name)

def constructorName(id: Symbol): MLName = MLName(firstToUpper(uniqueName(id)))

def constructorName(id: Name): MLName = MLName(firstToUpper(id.name))


def freshName(s: String): MLName =
  MLName(s + Symbol.fresh.next())


def typeName(id: Symbol): MLName = MLName(firstToLower(uniqueName(id)))

def typeName(id: Name): MLName = MLName(firstToLower(id.name))

def termName(id: Symbol): MLName = MLName(firstToLower(uniqueName(id)))

def termName(id: Name): MLName = MLName(firstToLower(id.name))

def freshTypeName(s: String): MLName =
  MLName(firstToLower(s + Symbol.fresh.next()))

def firstToLower(s: String): String = {
  if (s.isEmpty) ""
  else s.substring(0, 1).toLowerCase + s.substring(1)
}

def firstToUpper(s: String): String = {
  if (s.isEmpty) ""
  else s.substring(0, 1).toUpperCase + s.substring(1)
}
