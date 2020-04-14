package myex.validated

object Ex02ValidatedMonadic extends hutil.App {

  import cats.data._
  import cats.data.Validated._
  import cats.implicits._

  def validateUserName(userName: String): Validated[DomainValidation, String] =
    FormValidator.validateUserName(userName).toValidated

  def validatePassword(password: String): Validated[DomainValidation, String] =
    FormValidator.validatePassword(password).toValidated

  def validateFirstName(firstName: String): Validated[DomainValidation, String] =
    FormValidator.validateFirstName(firstName).toValidated

  def validateLastName(lastName: String): Validated[DomainValidation, String] =
    FormValidator.validateLastName(lastName).toValidated

  def validateAge(age: Int): Validated[DomainValidation, Int] = FormValidator.validateAge(age).toValidated

  // does not compile: flatMap is not a member of cats.data.Validated[myex.validated.DomainValidation,String]
  /*
  def validateForm(
      username: String,
      password: String,
      firstName: String,
      lastName: String,
      age: Int
  ): Either[DomainValidation, RegistrationData] = {

    for {
      validatedUserName  <- validateUserName(username)
      validatedPassword  <- validatePassword(password)
      validatedFirstName <- validateFirstName(firstName)
      validatedLastName  <- validateLastName(lastName)
      validatedAge       <- validateAge(age)
    } yield RegistrationData(
      validatedUserName,
      validatedPassword,
      validatedFirstName,
      validatedLastName,
      validatedAge
    )
  }

  import hutil.syntax.pipe._

  validateForm(
    username = "fakeUs3rname",
    password = "password",
    firstName = "John",
    lastName = "Doe",
    age = 15
  ) | println
 */
}
