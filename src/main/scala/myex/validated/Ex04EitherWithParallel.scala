package myex.validated

import cats.data.NonEmptyList

object Ex04EitherWithParallel extends hutil.App {

  import FormValidator._
  import cats.data.EitherNel
  import cats.instances.either._
  import cats.syntax.either._   // toEitherNel
  import cats.syntax.parallel._ // parMapN

  def validateForm(
      username: String,
      password: String,
      firstName: String,
      lastName: String,
      age: Int
  ): EitherNel[DomainValidation, RegistrationData] = {

    (
      validateUserName(username).toEitherNel,
      validatePassword(password).toEitherNel,
      validateFirstName(firstName).toEitherNel,
      validateLastName(lastName).toEitherNel,
      validateAge(age).toEitherNel
    ).parMapN(RegistrationData.apply)
  }

  import hutil.syntax.pipe._

  validateForm(
    username = "Joe",
    password = "Passw0r$1234",
    firstName = "John",
    lastName = "Doe",
    age = 21
  ) | println

  validateForm(
    username = "Joe%%%",
    password = "password",
    firstName = "John",
    lastName = "Doe",
    age = 15
  ) | println
}
