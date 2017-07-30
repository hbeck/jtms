package engine

import jtms.JtmsDoyle
import jtms.core.{Atom, Program, Rule}
import org.scalatest.FunSuite

/**
  * Created by hb on 30.07.17.
  */
class JtmsDoyleTests extends FunSuite {

  val E = Set[Atom]()

  test("ex 1") {

    val x = Atom("x")
    val y = Atom("y")

    val r1 = Rule(x,E,E)
    val r2 = Rule(y,Set(x),E)

    assert(r1.isFact)

    val program = Program(Set[Rule](r1,r2))
    val jtms = JtmsDoyle(program)

    jtms.getModel match {
      case None => assert(fail)
      case Some(model) => {
        assert(model contains x)
        assert(model contains y)
      }
    }

  }


}
