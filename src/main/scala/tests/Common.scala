package tests

/**
 * Created by tim on 30/12/14.
 */

object Common {
  case class Menu(first: String, mains: String, dessert: String)

  sealed trait Msg {
    def target: String
  }
  case class DinnerIn(target: String, menu: Menu) extends Msg

}

