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

    val r1 = Rule(x, E, E)
    // x <-
    val r2 = Rule(y, Set(x), E) // y <- x

    assert(r1.isFact)

    val program = Program(Set[Rule](r1, r2))
    val jtms = JtmsDoyle(program)

    jtms.getModel match {
      case None => assert(fail)
      case Some(model) => {
        assert(model contains x)
        assert(model contains y)
      }
    }

  }

  test("ex 2,3") {

    val a = Atom("a")
    val b = Atom("b")
    val c = Atom("c")

    val r1 = Rule(a, Set(b), E)
    //a <- b
    val r2 = Rule(b, E, Set(c))
    //b <- not c
    val r3 = Rule(c, E, Set(a)) //c <- not a

    val jtms = JtmsDoyle()

    def model = jtms.getModel.get

    jtms add r1
    assert(model equals E)

    jtms add r2
    assert(model equals Set(a, b))

    jtms add r3
    if (model equals Set(a, b)) {
      jtms remove r2
      assert(model equals Set(c))
    } else {
      assert(model equals Set(c))
      jtms remove r3
      assert(model equals Set(a, b))
    }

  }

  test("ex 4-7") {

    val a = Atom("a")
    val b = Atom("b")
    val c = Atom("c")
    val d = Atom("d")
    val e = Atom("e")

    val r1 = Rule(a, Set(b), E)
    //a <- b
    val r2 = Rule(b, E, Set(c))
    //b <- not c
    val r3 = Rule(a, Set(d), E)
    //a <- d
    val r4 = Rule(d, Set(c), E)
    //d <- c
    val r5 = Rule(c, Set(d), E)
    //c <- d
    val r6 = Rule(c, E, Set(e))
    //c <- not e
    val r7 = Rule(e, E, E) // e <-

    val jtms = JtmsDoyle()

    def model = jtms.getModel.get

    jtms add r1
    assert(model equals E)

    jtms add r2
    assert(model equals Set(a, b))

    jtms add r3
    assert(model equals Set(a, b))

    jtms add r4
    assert(model equals Set(a, b))

    jtms add r5
    assert(model equals Set(a, b))

    jtms add r6
    assert(model equals Set(a, c, d))

    jtms add r7
    assert(model equals Set(a, b, e))

    //

    jtms remove r3
    assert(model equals Set(a, b, e))

  }

  test("ex 9 (odd loop - inadmissible)") {

    val x = Atom("x")

    val r = Rule(x, E, Set(x)) //x <- not x

    val jtms = JtmsDoyle()
    def model = jtms.getModel.get

    jtms add r
    assert(model equals Set(x)) //inadmissible

  }

  test("ex 10 (odd loop - non-termination)") {

    val a = Atom("a")
    val b = Atom("b")

    val r1 = Rule(a, E, Set(b)) //a <- not b
    val r2 = Rule(b, Set(a), E) //b <- a

    val jtms = JtmsDoyle()
    def model = jtms.getModel.get

    jtms add r1
    assert(model equals Set(a))

    //using the following line will cause an infinite loop
    //jtms add r2
  }

}