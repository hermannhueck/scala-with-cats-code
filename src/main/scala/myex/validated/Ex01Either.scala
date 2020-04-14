package myex.validated

object Ex01Either extends hutil.App {

  def validateForm(
      username: String,
      password: String,
      firstName: String,
      lastName: String,
      age: Int
  ): Either[DomainValidation, RegistrationData] = {

    import FormValidator._

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
}
