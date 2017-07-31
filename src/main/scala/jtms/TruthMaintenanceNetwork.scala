package jtms

import jtms.core._

import scala.annotation.tailrec
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

  //
  //
  //

  //J
  def justifications(a: Atom): Set[Rule] = rules.filter(_.head == a)

  def allAtoms: Set[Atom] = rules.flatMap(_.atoms)

  def inAtoms = allAtoms.filter(label(_) == in)

  def hasUnknown = allAtoms.exists(label(_) == unknown)

  //affected(a) = {x ∈ cons(a) | a ∈ supp(x)}
  def affected(a: Atom): Set[Atom] = cons(a).filter(supp(_) contains a)

  def repercussions(a: Atom) = trans(affected, a)

  def valid(rule: Rule) = (rule.pos.forall(label(_) == in)) && (rule.neg.forall(label(_) == out))

  def invalid(rule: Rule) = (rule.pos.exists(label(_) == out)) || (rule.neg.exists(label(_) == in))

  def posValid(rule: Rule) = (rule.pos.forall(label(_) == in)) && (!rule.neg.exists(label(_) == in))

  def unknownCons(a: Atom) = cons(a).filter(label(_) == unknown)

  def clearSupport(a: Atom): Unit

  def setInSupport(a: Atom, rule: Rule): Unit

  def setOutSupport(a: Atom, atoms: Set[Atom]): Unit

  def addSupport(a: Atom, newAtom: Atom): Unit

  def updateLabel(a: Atom, label: Label): Unit

  def register(rule: Rule): Unit

  def register(a: Atom): Unit

  def deregister(a: Atom): Unit

  def deregister(rule: Rule): Unit

  def trans[T](f: T => Set[T], t: T): Set[T] = {
    trans(f)(f(t))
  }

  @tailrec
  final def trans[T](f: T => Set[T])(s: Set[T]): Set[T] = {
    val next = s.flatMap(f)
    val nextSet = next ++ s
    if (s == nextSet || next.isEmpty) {
      return s
    }
    trans(f)(nextSet)
  }

}
