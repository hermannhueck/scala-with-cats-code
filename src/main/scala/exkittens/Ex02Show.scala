package exkittens

import hutil.syntax.pipe._

import cats.implicits._, cats._, cats.derived._

object Ex02Show extends hutil.App {

  case class Address(street: String, city: String, state: String)
  case class ContactInfo(phoneNumber: String, address: Address)
  case class People(name: String, contactInfo: ContactInfo)

  val mike = People("Mike", ContactInfo("202-295-3928", Address("1 Main ST", "Chicago", "IL")))
  mike | println

  // existing Show instance for Address
  implicit val addressShow: Show[Address] =
    new Show[Address] {
      def show(a: Address) = s"${a.street}, ${a.city}, ${a.state}"
    }

  implicit val peopleShow: Show[People] = {
    import auto.show._
    semiauto.show
  } //auto derive Show for People

  println()
  mike.contactInfo.address.show | println

  println()
  mike.show | println
  show"$mike" | println
}
