package jtms

import jtms.core._

import scala.util.Random

class JtmsDoyle(net: TruthMaintenanceNetwork, random: Random = new Random()) {

  def add(rule: Rule) {
    net.register(rule)
    if (net.label(rule.head) == in) {
      return
    }
    if (net.invalid(rule)) {
      net.addSupport(rule.head, findSpoiler(rule).get)
      return
    }
    val atoms = net.repercussions(rule.head) + rule.head
    update(atoms)
  }

  def remove(rule: Rule) {
    net.deregister(rule)
    if (!net.allAtoms.contains(rule.head)) return
    if (net.label(rule.head) == out) return
    if (net.suppJ(rule.head).isDefined && net.suppJ(rule.head).get != rule) return
    val atoms = net.repercussions(rule.head) + rule.head
    update(atoms)
  }

  //
  //
  //

  def update(atoms: Set[Atom]) {
    atoms.foreach(setUnknown)
    atoms.foreach(findLabel)
    atoms.foreach(chooseLabel)
  }

  def findLabel(a: Atom) {
    if (net.label(a) != unknown) return

    if (validation(a) || invalidation(a)) {
      net.unknownCons(a).foreach(findLabel)
    }
  }

  def validation(a: Atom): Boolean = {
    net.justifications(a).find(net.valid) match {
      case Some(rule) => setIn(rule); true
      case None => false
    }
  }

  def invalidation(a: Atom): Boolean = {
    if (net.justifications(a).forall(net.invalid)) {
      setOut(a)
      return true
    }
    false
  }

  def setIn(rule: Rule) {
    net.updateLabel(rule.head, in)
    net.setInSupport(rule.head, rule)
  }

  def setOut(a: Atom) {
    net.updateLabel(a, out)
    setOutSupport(a)
  }

  def setOutSupport(a: Atom) {
    val maybeAtoms: Set[Option[Atom]] = net.justifications(a).map(findSpoiler)
    if (maybeAtoms.exists(_.isEmpty)) {
      throw new RuntimeException("could not find spoiler for every justification of atom " + a)
    }
    net.setOutSupport(a, maybeAtoms.map(_.get))
  }

  def setUnknown(a: Atom) {
    net.updateLabel(a, unknown)
    net.clearSupport(a)
  }

  def chooseLabel(a: Atom) {
    if (net.label(a) != unknown) return

    if (choice(a)) {
      net.unknownCons(a).foreach(chooseLabel)
    } else {
      val aff = shuffle(net.affected(a) + a) //shuffle to overcome potential non-termination
      aff.foreach(setUnknown)
      aff.foreach(chooseLabel)
    }
  }

  def choice(a: Atom): Boolean = {
    net.justifications(a).find(net.posValid) match {
      case Some(rule) => {
        if (net.affected(a).isEmpty) setIn(rule)
        else return false
      }
      case None => setOut(a) //allowing 'unknown' instead of 'out' in spoiler!
    }
    true
  }

  //may also use an unknown atom instead of an out atom (if an out-atom cannot be found)
  def findSpoiler(rule: Rule): Option[Atom] = {
    if (random.nextDouble() < 0.5) {
      rule.pos.find(net.label(_) == out) match {
        case None => rule.neg.find(net.label(_) == in) match {
          case None => rule.pos.find(net.label(_) == unknown)
          case opt => opt
        }
        case opt => opt
      }
    } else {
      rule.neg.find(net.label(_) == in) match {
        case None => rule.pos.find(net.label(_) == out) match {
          case None => rule.pos.find(net.label(_) == unknown)
          case opt => opt
        }
        case opt => opt
      }
    }
  }


  def shuffle(atoms: Set[Atom]): Seq[Atom] = random.shuffle(atoms.toSeq)

  //
  //
  //

  type Model = Set[Atom]

  def getModel(): Option[Model] = {
    val atoms = net.inAtoms
    if (net.hasUnknown) return None
    Some(atoms)
  }

}

object JtmsDoyle {
  def apply(P: Program): JtmsDoyle = {
    val jtms = JtmsDoyle()
    P.rules foreach jtms.add
    jtms
  }

  def apply(): JtmsDoyle = new JtmsDoyle(TruthMaintenanceNetwork())
}