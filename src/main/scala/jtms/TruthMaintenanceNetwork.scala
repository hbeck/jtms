package jtms

import jtms.core._

import scala.collection.immutable.HashMap

object TruthMaintenanceNetwork {
  def apply() = new SimpleNetwork()
}

trait TruthMaintenanceNetwork {

  var rules: Set[Rule] = Set()

  var cons: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  var supp: Map[Atom, Set[Atom]] = new HashMap[Atom, Set[Atom]]
  var label: Map[Atom, Label] = new HashMap[Atom, Label]

  var suppJ: Map[Atom, Option[Rule]] = new HashMap[Atom, Option[Rule]]

  def justifications(a: Atom): Set[Rule]

  def allAtoms(): Set[Atom]

  def inAtoms(): Set[Atom]

  def hasUnknown(): Boolean

  def affected(a: Atom): Set[Atom]

  def repercussions(a: Atom): Set[Atom]

  def valid(rule: Rule): Boolean

  def invalid(rule: Rule): Boolean

  def posValid(rule: Rule): Boolean

  def unknownCons(a: Atom): Set[Atom]

  def clearSupport(a: Atom): Unit

  def setInSupport(a: Atom, rule: Rule): Unit

  def setOutSupport(a: Atom, atoms: Set[Atom]): Unit

  def addSupport(a: Atom, newAtom: Atom): Unit

  def updateLabel(a: Atom, label: Label): Unit

  def register(rule: Rule): Unit

  def deregister(rule: Rule): Unit


}
