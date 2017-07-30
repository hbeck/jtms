package engine

import jtms.JtmsDoyle
import jtms.core.{Atom, Program, Rule}
import org.scalatest.FunSuite

/**
  * Created by hb on 30.07.17.
  */
class JtmsDoyleTests extends FunSuite {

  val __ = Set[Atom]()
  def r(head:Atom,pos:Atom,neg:Atom) = Rule(head,Set(pos),Set(neg))
  def r(head:Atom,pos:Set[Atom],neg:Set[Atom]) = Rule(head,pos,neg)
  def r(head:Atom,pos:Atom,neg:Set[Atom]) = Rule(head,Set(pos),neg)
  def r(head:Atom,pos:Set[Atom],neg:Atom) = Rule(head,pos,Set(neg))

  test("ex 1") {

    val x = Atom("x")
    val y = Atom("y")

    val r1 = r(x, __, __) // x <-
    val r2 = r(y, x, __) // y <- x

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

    val r1 = r(a, b, __) //a <- b
    val r2 = r(b, __, c) //b <- not c
    val r3 = r(c, __, a) //c <- not a

    val jtms = JtmsDoyle()

    def model = jtms.getModel.get

    jtms add r1
    assert(model.isEmpty)

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

  test("ex 4-7, 11-12") {

    val a = Atom("a")
    val b = Atom("b")
    val c = Atom("c")
    val d = Atom("d")
    val e = Atom("e")

    val r1 = r(a, b, __) //a <- b
    val r2 = r(b, __, c) //b <- not c
    val r3 = r(a, d, __) //a <- d
    val r4 = r(d, c, __) //d <- c
    val r5 = r(c, d, __) //c <- d
    val r6 = r(c, __, e)  //c <- not e
    val r7 = r(e, __, __) // e <-

    val jtms = JtmsDoyle()

    def model = jtms.getModel.get

    jtms add r1
    assert(model.isEmpty)

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

    val r1 = r(x, __, x) //x <- not x

    val jtms = JtmsDoyle()
    def model = jtms.getModel.get

    jtms add r1
    assert(model equals Set(x)) //inadmissible

  }

  test("ex 10 (odd loop - non-termination)") {

    val a = Atom("a")
    val b = Atom("b")

    val r1 = r(a, __, b) //a <- not b
    val r2 = r(b, a, __) //b <- a

    val jtms = JtmsDoyle()
    def model = jtms.getModel.get

    jtms add r1
    assert(model equals Set(a))

    //using the following line will cause an infinite loop
    //jtms add r2
  }

}