package jtms.core

sealed abstract class Label

case object in extends Label

case object out extends Label

case object unknown extends Label
