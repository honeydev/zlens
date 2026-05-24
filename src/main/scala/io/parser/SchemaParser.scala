package io.parser

import cats.Monoid
import cats.Functor
import cats.Applicative
import cats.syntax.all._

case class Symbol(value: Char)
//case class Delimiter(value: Char) extends Element
//given Monoid[Symbol] = new Monoid[Symbol] {
//
//  override def empty: Symbol = Symbol("")
//
//  override def combine(x: Symbol, y: Symbol): Symbol = Symbol(
//    x.value ++ y.value
//  )
//}

case class Parser[A, B](
    parse: (A) => Either[String, B]
)

given functor[A]: Functor[[B] =>> Parser[A, B]] =
  new Functor[[B] =>> Parser[A, B]]:

    override def map[B, C](fa: Parser[A, B])(f: B => C): Parser[A, C] =
      Parser { input =>
        fa.parse(input) match {
          case Right(ch) => Right(f(ch))
          case Left(e)   => Left(e)
        }
      }

given applicative[I]: Applicative[[O] =>> Parser[I, O]] =
  new Applicative[[O] =>> Parser[I, O]] {

    def pure[A](a: A): Parser[I, A] = Parser { input =>
      Right(a)
    }
  }

case class Field(`type`: String, name: Symbol)
case class Schema(fields: List[Field])

//case class Parser(raw: String) {
//  def parse(schema: String) = {}
//}
