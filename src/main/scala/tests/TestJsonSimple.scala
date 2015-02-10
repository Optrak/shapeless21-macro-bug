package tests

/**
 * Created by tim on 28/12/14.
 */

import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import shapeless._
import org.json4s.native.JsonMethods.{compact, render}
import shapeless.labelled._

trait JsonParser[A] {
  def parse(n: JValue): A
}


object JsonParser extends LabelledTypeClassCompanion[JsonParser] {
  implicit val typeClass: LabelledTypeClass[JsonParser] = new LabelledTypeClass[JsonParser] {

    def emptyProduct: JsonParser[HNil] =
      new JsonParser[HNil] {
        def parse(s: JValue) = HNil
      }

    def product[F, T <: HList](fieldName: String, scf: JsonParser[F], sct: JsonParser[T]) = 
      new JsonParser[F :: T] {
        def parse(jv: JValue): F :: T = {
          val child = jv \ fieldName
          println(s"parse product from $jv looking for $fieldName found as $child")
          val answer = { 
            val front = scf.parse(child)
            println(s"hcons front is $front")
            val back = sct.parse(jv)
            println(s"hcons back is $back")
            front :: back
          }
          println(s"HCons answer is $answer")
          answer
        }
      }

    implicit def emptyCoproduct: JsonParser[CNil] = new JsonParser[CNil] {
      def parse(s: JValue): CNil = throw new Exception("no coproduct found")
    }

    implicit def coproduct[L, R <: Coproduct](fieldName: String, scl: => JsonParser[L], scr: => JsonParser[R]) =
      new JsonParser[L :+: R] {
        override def parse(jv: JValue): L :+: R = {
          val child = jv \ fieldName
          println(s"parse for deriveCCons with $jv looking for $fieldName found as $child")
          val answer = child match {
            case JNothing =>
              println(s"trying cr.parse for $jv")
              Inr(scr.parse(jv))
              /*try {
                println(s"trying cr.parse for $jv")
                Inr(scr.parse(jv))
              } catch {
                case e: Exception => {
                  Inl(scl.parse(jv))
                }
              }*/
            case nn =>
              println(s"found coproduct $fieldName, now parse it")
              Inl(scl.parse(nn))
          }
          println(s"coProduct answer is $answer")
          answer
        }

      }

    implicit def project[F, G](instance: => JsonParser[G], to: F => G, from: G => F) =
      new JsonParser[F] {
        def parse(s: JValue): F = {
          println(s"project, parsing from $s")
          from(instance.parse(s))
        }
      }
  }
}


trait JsonSupport2 {

  implicit val IntParser = new JsonParser[Int] {
    def parse(n: JValue) = n match {
      case JInt(bi) => bi.toInt
      case JString(str) => str.toInt
      case xx => throw new Exception(s"unexpected thing where should be int $xx")
    }
  }
  implicit val StringParser = new JsonParser[String] {
    def parse(n: JValue) = n match {
      case JString(str) => str
      case xx => throw new Exception(s"unexpected thing where should be STRING $xx")
    }
  }


  class OptParser[A](implicit aParser: JsonParser[A]) extends JsonParser[Option[A]] {
    def parse(n: JValue): Option[A] =
      n match {
        case JNothing => None
        case x => Some(aParser.parse(x))
      }
  }

  class ListParser[A](implicit aParser: JsonParser[A]) extends JsonParser[List[A]] {
    def parse(n: JValue): List[A] = {
      println(s"in ListParser with node $n")
      n match {
        case ja: JArray => ja.arr.map(v => aParser.parse(v))
        case _ => ???
      }
    }
  }
  
  implicit def mkListParser[A](implicit aParser: JsonParser[A]): JsonParser[List[A]] = new ListParser[A]
  implicit def mkOptParser[A](implicit aParser: JsonParser[A]) = new OptParser[A]
}

object JsonSupport extends JsonSupport2

object TestJsonSimple extends App with JsonSupport2 {
  import Common._
  //import JsonSupport._

  /* Test Data */
  
  val singleWrapped = "DinnerIn" ->
    ("target" -> "me") ~
      ("menu" -> (
        ("first" -> "soup") ~
          ("mains" -> "beef") ~
          ("dessert" -> "ice-cream")
        ))

  /* Tests */
  val listParser = JsonParser[List[String]]
  val mkListParser = JsonParser[List[Int]]
  /*this doesn't compile if
    1. the enclosing object extends a trait that contains an implicit def that constructs a List parser
       out of an item parser. If it imports an object that contains an implicit, everything is fine
    2. the second list parser defined here has the same name as an implicit in the trait. If it comes first,
       or has a different name, everything compiles
   */
  val jsonParser = JsonParser[Msg]
  //mkListParser.parse(singleWrapped)
  jsonParser.parse(singleWrapped) match {
    case dinner: DinnerIn =>
      assert(dinner.target == "me", s"not me - ${dinner.target}")
      assert(dinner.menu.dessert == "ice-cream", s"not icecreaem ${dinner.menu.dessert}")
    case x => assert(false, s"oh dear got $x")
  }

  val m1 = Menu("soup", "beef", "ice-cream")
  val dinnerIn = DinnerIn("me", m1)

  println("success")
}