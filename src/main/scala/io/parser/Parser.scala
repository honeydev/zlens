package io.parser

import cats.syntax.all.*
import cats.{Applicative, Functor, Semigroup}
import io.parser.ParseError.*

enum ParseError:
  case UnexpectedCharacter(val value: String) extends ParseError
  case UnexpectedEndOfInput() extends ParseError
  case LogicalError(val value: String) extends ParseError

case class Parser[A, B](
    parse: (A) => Either[ParseError, (B, A)]
)

given functor[A]: Functor[[B] =>> Parser[A, B]] =
  new Functor[[B] =>> Parser[A, B]]:

    override def map[B, C](fa: Parser[A, B])(f: B => C): Parser[A, C] =
      Parser { input =>
        fa.parse(input) match {
          case Right((ch, tail)) => Right((f(ch), tail))
          case Left(e)           => Left(e)
        }
      }

given applicative[I]: Applicative[[O] =>> Parser[I, O]] =
  new Applicative[[O] =>> Parser[I, O]] {

    override def pure[A](x: A): Parser[I, A] = {
      Parser { input =>
        Right((x, input))
      }
    }

    override def ap[A, B](
        ff: Parser[I, A => B]
    )(fa: Parser[I, A]): Parser[I, B] = {
      Parser { input =>
        println(input)
        ff.parse(input) match {
          case Right((func, tail)) =>
            fa.parse(tail) match {
              case Right((v, tail2)) =>
                Right((func(v), tail2))
              case Left(e) => Left(e)
            }
          case Left(e) => Left(e)
        }
      }
    }
  }

class SatisfyParser(val predicate: (v: String) => Boolean)
    extends Parser(parse =
      (v: String) =>
        // println(v)
        v match {
          case "" => Left(UnexpectedEndOfInput())
          case _  =>
            val h = v.head.toString
            if (predicate(h)) {
              Right((h, v.tail))
            } else
              Left(UnexpectedCharacter(s"Unexpected token: $h"))
        }
    )

class FieldTypeParser
    extends Parser(parse = (v: String) => {

      val valuesParsers = FieldT.values.map { v => stringParser(v.value) }

      valuesParsers.tail
        .foldLeft(valuesParsers.head) { case (acc, p) =>
          orParser(acc, p)
        }
        .parse(v)
    })

def stringParser(s: String): Parser[String, String] = {
  val parsers = s.toList.map(charParser)

  parsers.tail.foldLeft(parsers.head) { (acc, p) =>
    acc.map2(p) { (a, b) => a |+| b }
  }
}

def charParser(ch: Char): Parser[String, String] = {
  val asStr = ch.toString
  SatisfyParser { v => v == asStr }
}

def anySymbol: Parser[String, String] = {
  SatisfyParser { v =>
    true
  }
}

def nParser[A: Semigroup](p: Parser[String, A], n: Int): Parser[String, A] = {
  Parser { input =>
    if (input.length != n) {
      Left(
        LogicalError(
          s"Invalid size, $input size: ${input.length} not equal input string size: $n"
        )
      )
    } else {
      (1 until n)
        .foldLeft(p) { case (p1, _) =>
          p1.map2(p) { case (a, b) => a |+| b }
        }
        .parse(input)
    }
  }
}

def gtParser[A: Semigroup](p: Parser[String, A], n: Int): Parser[String, A] = {
  Parser { input =>
    if (input.length <= n) {
      Left(
        LogicalError(
          s"Invalid size, $input size: ${input.length} should be gt than: $n"
        )
      )
    } else {
      (1 until input.length)
        .foldLeft(p) { case (p1, _) =>
          p1.map2(p) { case (a, b) =>
            a |+| b
          }
        }
        .parse(input)
    }
  }
}

def splitParser[A](
    splitSymbol: Char,
    partParser: Parser[String, A]
): Parser[String, List[A]] = {
  Parser { input =>
    input
      .split(splitSymbol.toString)
      .toList
      .traverse { p =>
        for {
          (parsed, tail) <- partParser.parse(p)
          parsed <-
            if (tail.nonEmpty)
              Left(
                LogicalError(
                  s"Split parser output should be empty after child parser apply, tail $tail"
                )
              )
            else Right(parsed)
        } yield parsed
      }
      .map((_, ""))
  }
}

def orParser[A](
    p1: Parser[String, A],
    p2: Parser[String, A]
): Parser[String, A] = {
  Parser { input =>
    p1
      .parse(input)
      .orElse(p2.parse(input))
  }
}
