package myex.validated

object Ex03ValidatedApplicative extends hutil.App {

  import cats.syntax.apply._
  import FormValidatorNec._

  def validateForm(
      username: String,
      password: String,
      firstName: String,
      lastName: String,
      age: Int
  ): ValidationResult[RegistrationData] = {
    (
      validateUserName(username),
      validatePassword(password),
      validateFirstName(firstName),
      validateLastName(lastName),
      validateAge(age)
    ).mapN(RegistrationData)
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

  // convert Validated to Either

  validateForm(
    username = "Joe",
    password = "Passw0r$1234",
    firstName = "John",
    lastName = "Doe",
    age = 21
  ).toEither | println

  validateForm(
    username = "Joe%%%",
    password = "password",
    firstName = "John",
    lastName = "Doe",
    age = 15
  ).toEither | println
}
