package jtms

import jtms.core._

import scala.annotation.tailrec
import scala.util.Random

class SimpleNetwork(random: Random = new Random()) extends TruthMaintenanceNetwork {

  override def justifications(a: Atom) = rules.filter(_.head == a)

  override def allAtoms() = rules.flatMap(_.atoms)

  override def inAtoms() = allAtoms.filter(label(_) == in)

  override def hasUnknown() = allAtoms.exists(label(_) == unknown)

  override def affected(a: Atom) = cons(a).filter(supp(_) contains a)

  override def repercussions(a: Atom) = trans(affected, a)

  override def valid(rule: Rule) = rule.pos.forall(label(_) == in) && rule.neg.forall(label(_) == out)

  override def invalid(rule: Rule) = rule.pos.exists(label(_) == out) || rule.neg.exists(label(_) == in)

  override def posValid(rule: Rule) = rule.pos.forall(label(_) == in) && !rule.neg.exists(label(_) == in)

  override def unknownCons(a: Atom) = cons(a).filter(label(_) == unknown)

  override def register(rule: Rule) {
    if (rules.contains(rule)) return

    rules = rules + rule

    rule.atoms.foreach(register)
    rule.body.foreach { atom =>
      cons = cons.updated(atom, cons(atom) + rule.head)
    }
  }

  def register(a: Atom) {
    if (!label.isDefinedAt(a)) {
      label = label.updated(a, out)
      cons = cons.updated(a, Set[Atom]())
      clearSupport(a)
    }
  }

  override def deregister(rule: Rule) {
    if (!rules.contains(rule)) return

    rules = rules - rule

    val remainingAtoms = rules.flatMap(_.atoms)

    rule.atoms.diff(remainingAtoms).foreach(deregister)
    rule.body.intersect(remainingAtoms).foreach(removeDeprecatedCons(rule))

  }

  def removeDeprecatedCons(rule: Rule)(a: Atom) {
    if (!justifications(rule.head).exists(_.body.contains(a))) {
      cons = cons.updated(a, cons(a) - rule.head)
    }
  }

  def deregister(a: Atom) {
    label = label - a
    cons = cons - a
    supp = supp - a
  }

  override def clearSupport(a: Atom) {
    supp = supp.updated(a, Set())
    suppJ = suppJ.updated(a, None)
  }

  override def setOutSupport(a: Atom, atoms: Set[Atom]) {
    supp = supp.updated(a, atoms)
    suppJ = suppJ.updated(a, None)
  }

  override def setInSupport(a: Atom, rule: Rule) {
    suppJ = suppJ.updated(rule.head, Some(rule))
    supp = supp.updated(a, rule.body)
  }

  override def addSupport(a: Atom, newAtom: Atom) {
    supp = supp.updated(a, supp(a) + newAtom)
  }

  override def updateLabel(a: Atom, newLabel: Label) {
    label = label.updated(a, newLabel)
  }

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
