package jtms.core

case class Rule(head: Atom, pos: Set[Atom], neg: Set[Atom]) {

  lazy val body = pos union neg

  lazy val isFact: Boolean = pos.isEmpty && neg.isEmpty

  def atoms: Set[Atom] = pos ++ neg + head

  def ==(other: Rule): Boolean = {
    if (this.head != other.head) return false
    if (this.pos != other.pos) return false
    if (this.neg != other.neg) return false
    true
  }

  override def equals(other: Any): Boolean = other match {
    case r: Rule => this == r
    case _ => false
  }

  override def toString(): String = {
    val sb = new StringBuilder

    def result: String = sb.toString

    sb.append(head)
    if (pos.isEmpty && neg.isEmpty) {
      sb.append(".")
      return result
    }
    sb.append(" :- ")

    //pos
    if (pos.size == 1) {
      sb.append(pos.head)
    } else if (pos.size > 1) {
      sb.append(pos.head)
      pos.tail foreach (sb.append(", ").append(_))
    }

    if (neg.isEmpty) {
      sb.append(".")
      return result
    }
    if (pos.nonEmpty) {
      sb.append(", not ")
    }

    //neg
    if (neg.size == 1) {
      sb.append("not ").append(neg.head)
    } else if (neg.size > 1) {
      sb.append(neg.head)
      neg.tail foreach (sb.append(", not ").append(_))
    }

    sb.append(".")

    return result
  }
}