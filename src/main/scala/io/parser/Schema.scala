package io.parser

enum FieldT(val value: String):
  
  case StringT extends FieldT("str")
  case NumT extends FieldT("num")
  
  def values: Seq[String] =
    Seq(StringT.value, NumT.value)

case class Name(value: String)
