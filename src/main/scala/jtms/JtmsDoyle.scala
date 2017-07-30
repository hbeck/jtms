package jtms

import jtms.core._

import scala.util.Random

class JtmsDoyle(tmn: TruthMaintenanceNetwork, random: Random = new Random()) {

  def add(rule: Rule): Unit = {
    tmn.register(rule)
    if (tmn.label(rule.head) == in) {
      return
    }
    if (tmn.invalid(rule)) {
      tmn.addSupport(rule.head, findSpoiler(rule).get)
      return
    }
    val atoms = tmn.repercussions(rule.head) + rule.head
    update(atoms)
  }

  def remove(rule: Rule): Unit = {
    tmn.deregister(rule)
    if (!(tmn.allAtoms contains rule.head)) return
    if (tmn.label(rule.head) == out) return
    if (tmn.suppJ(rule.head).isDefined && tmn.suppJ(rule.head).get != rule) return
    val atoms = tmn.repercussions(rule.head) + rule.head
    update(atoms)
  }

  //
  //
  //

  def update(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown
    atoms foreach findLabel
    atoms foreach chooseLabel
  }

  def findLabel(a: Atom): Unit = {
    if (tmn.label(a) != unknown) return

    if (validation(a) || invalidation(a)) {
      tmn.unknownCons(a) foreach findLabel
    }
  }

  def validation(a: Atom): Boolean = {
    tmn.justifications(a) find tmn.valid match {
      case Some(rule) => setIn(rule); true
      case None => false
    }
  }

  def invalidation(a: Atom): Boolean = {
    if (tmn.justifications(a) forall tmn.invalid) {
      setOut(a)
      return true
    }
    false
  }

  def setIn(rule: Rule) = {
    tmn.updateLabel(rule.head, in)
    tmn.setInSupport(rule.head, rule)
  }

  def setOut(a: Atom) = {
    tmn.updateLabel(a, out)
    setOutSupport(a)
  }

  def setOutSupport(a: Atom) {
    val maybeAtoms: Set[Option[Atom]] = tmn.justifications(a) map findSpoiler
    if (maybeAtoms exists (_.isEmpty)) {
      throw new RuntimeException("could not find spoiler for every justification of atom " + a)
    }
    tmn.setOutSupport(a, maybeAtoms map (_.get))
  }

  def setUnknown(a: Atom) = {
    tmn.updateLabel(a, unknown)
    tmn.clearSupport(a)
  }

  def chooseLabel(a: Atom): Unit = {
    if (tmn.label(a) != unknown)
      return

    if (choice(a)) {
      tmn.unknownCons(a) foreach chooseLabel
    } else {
      val aff = shuffle(tmn.affected(a) + a)
      aff foreach setUnknown
      aff foreach chooseLabel
    }
  }

  def choice(a: Atom): Boolean = {
    tmn.justifications(a) find tmn.posValid match {
      case Some(rule) => {
        if (tmn.affected(a).isEmpty) setIn(rule)
        else return false
      }
      case None => setOut(a) //allowing 'unknown' instead of 'out' in spoiler!
    }
    true
  }

  //may also use an unknown atom instead of an out atom (if an out-atom cannot be found)
  def findSpoiler(rule: Rule): Option[Atom] = {
    if (random.nextDouble() < 0.5) {
      rule.pos find (tmn.label(_) == out) match {
        case None => rule.neg find (tmn.label(_) == in) match {
          case None => rule.pos find (tmn.label(_) == unknown)
          case opt => opt
        }
        case opt => opt
      }
    } else {
      rule.neg find (tmn.label(_) == in) match {
        case None => rule.pos find (tmn.label(_) == out) match {
          case None => rule.pos find (tmn.label(_) == unknown)
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
    val atoms = tmn.inAtoms
    if (tmn.hasUnknown) return None
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