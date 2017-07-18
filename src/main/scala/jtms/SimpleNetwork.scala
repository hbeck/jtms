package jtms

import jtms.core.{Atom, Label, Rule, out}

import scala.util.Random

class SimpleNetwork(random: Random = new Random()) extends TruthMaintenanceNetwork {

  override def register(a: Atom) = {
    if (!label.isDefinedAt(a)) {
      label = label.updated(a, out)
      cons = cons.updated(a, Set[Atom]())
      clearSupport(a)
    }
  }

  override def deregister(a: Atom) = {
    label = label - a
    cons = cons - a
    supp = supp - a
  }

  override def deregister(rule: Rule): Boolean = {
    if (!(rules contains rule)) return false

    rules = rules - rule

    val remainingAtoms = rules flatMap (_.atoms)

    (rule.atoms diff remainingAtoms) foreach deregister
    (rule.body intersect remainingAtoms) foreach removeDeprecatedCons(rule)

    true
  }

  override def clearSupport(a: Atom): Unit = {
    supp = supp.updated(a, Set())
    suppJ = suppJ.updated(a, None)
  }

  override def setOutSupport(a: Atom, atoms: Set[Atom]): Unit = {
    supp = supp.updated(a, atoms)
    suppJ = suppJ.updated(a, None)
  }

  override def setInSupport(a: Atom, rule: Rule): Unit = {
    suppJ = suppJ.updated(rule.head, Some(rule))
    supp = supp.updated(a, rule.body)
  }

  override def addSupport(a: Atom, newAtom: Atom): Unit = {
    supp = supp.updated(a, supp(a) + newAtom)
  }

  override def updateLabel(a: Atom, newLabel: Label): Unit = {
    label = label.updated(a, newLabel)
  }

  override def register(rule: Rule): Boolean = {
    if (rules contains rule) return false

    rules = rules + rule

    rule.atoms foreach register
    rule.body foreach { atom =>
      cons = cons updated(atom, cons(atom) + rule.head)
    }

    true
  }
}