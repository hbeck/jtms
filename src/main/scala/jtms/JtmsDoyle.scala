package jtms

import jtms.core._

import scala.util.Random

class JtmsDoyle(network: TruthMaintenanceNetwork, random: Random = new Random()) {

  def add(rule: Rule): Unit = {
    register(rule)
    if (network.label(rule.head) == in) {
      return
    }
    if (network.invalid(rule)) {
      network.addSupport(rule.head, findSpoiler(rule).get)
      return
    }
    val atoms = network.repercussions(rule.head) + rule.head
    update(atoms)
  }

  def remove(rule: Rule): Unit = {
    deregister(rule)
    if (!(network.allAtoms contains rule.head)) return
    if (network.label(rule.head) == out) return
    if (network.suppJ(rule.head).isDefined && network.suppJ(rule.head).get != rule) return
    val atoms = network.repercussions(rule.head) + rule.head
    update(atoms)
  }

  //
  //
  //

  def register(rule: Rule): Boolean = {
    if (network.register(rule)) {
      return true
    }
    false
  }

  def update(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown
    atoms foreach findLabel
    atoms foreach chooseLabel
  }

  def findLabel(a: Atom): Unit = {
    if (network.label(a) != unknown) return

    if (validation(a) || invalidation(a)) {
      network.unknownCons(a) foreach findLabel
    }
  }

  def validation(a: Atom): Boolean = {
    network.justifications(a) find network.valid match {
      case Some(rule) => setIn(rule); true
      case None => false
    }
  }

  def invalidation(a: Atom): Boolean = {
    if (network.justifications(a) forall network.invalid) {
      setOut(a)
      return true
    }
    false
  }

  def setIn(rule: Rule) = {
    network.updateLabel(rule.head, in)
    network.setInSupport(rule.head, rule)
  }

  def setOut(a: Atom) = {
    network.updateLabel(a, out)
    setOutSupport(a)
  }

  def setOutSupport(a: Atom) {
    val maybeAtoms: Set[Option[Atom]] = network.justifications(a) map findSpoiler
    if (maybeAtoms exists (_.isEmpty)) {
      throw new RuntimeException("could not find spoiler for every justification of atom " + a)
    }
    network.setOutSupport(a, maybeAtoms map (_.get))
  }

  def setUnknown(a: Atom) = {
    network.updateLabel(a, unknown)
    network.clearSupport(a)
  }

  def chooseLabel(a: Atom): Unit = {
    if (network.label(a) != unknown)
      return

    if (choice(a)) {
      network.unknownCons(a) foreach chooseLabel
    } else {
      val aff = shuffle(network.affected(a) + a)
      aff foreach setUnknown
      aff foreach chooseLabel
    }
  }

  def choice(a: Atom): Boolean = {
    network.justifications(a) find network.posValid match {
      case Some(rule) => {
        if (network.affected(a).isEmpty) setIn(rule)
        else return false
      }
      case None => setOut(a) //allowing 'unknown' instead of 'out' in spoiler!
    }
    true
  }

  //may also use an unknown atom instead of an out atom (if an out-atom cannot be found)
  def findSpoiler(rule: Rule): Option[Atom] = {
    if (random.nextDouble() < 0.5) {
      rule.pos find (network.label(_) == out) match {
        case None => rule.neg find (network.label(_) == in) match {
          case None => rule.pos find (network.label(_) == unknown)
          case opt => opt
        }
        case opt => opt
      }
    } else {
      rule.neg find (network.label(_) == in) match {
        case None => rule.pos find (network.label(_) == out) match {
          case None => rule.pos find (network.label(_) == unknown)
          case opt => opt
        }
        case opt => opt
      }
    }
  }


  def shuffle(atoms: Set[Atom]): Seq[Atom] = random.shuffle(atoms.toSeq)

  def deregister(rule: Rule) = network.deregister(rule)

  //
  //
  //

  type Model = Set[Atom]

  def getModel(): Option[Model] = {
    val atoms = network.inAtoms
    if (network.hasUnknown) return None
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