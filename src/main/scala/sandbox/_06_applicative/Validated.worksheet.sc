// Validated
// see: https://typelevel.org/cats/datatypes/validated.html

case class RegistrationData(username: String, password: String, firstName: String, lastName: String, age: Int)

sealed trait DomainValidation {
  def errorMessage: String
}

case object UsernameHasSpecialCharacters extends DomainValidation {
  def errorMessage: String = "Username cannot contain special characters."
}

case object PasswordDoesNotMeetCriteria extends DomainValidation {
  def errorMessage: String =
    "Password must be at least 10 characters long, including an uppercase and a lowercase letter, one number and one special character."
}

case object FirstNameHasSpecialCharacters extends DomainValidation {
  def errorMessage: String = "First name cannot contain spaces, numbers or special characters."
}

case object LastNameHasSpecialCharacters extends DomainValidation {
  def errorMessage: String = "Last name cannot contain spaces, numbers or special characters."
}

case object AgeIsInvalid extends DomainValidation {
  def errorMessage: String = "You must be aged 18 and not older than 75 to use our services."
}

import cats.syntax.all._

// form validation with Either and for-comprehension/flatMap
sealed trait FormValidator {

  def validateUserName(userName: String): Either[DomainValidation, String] =
    Either.cond(
      userName.matches("^[a-zA-Z0-9]+$"),
      userName,
      UsernameHasSpecialCharacters
    )

  def validatePassword(password: String): Either[DomainValidation, String] =
    Either.cond(
      password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$"),
      password,
      PasswordDoesNotMeetCriteria
    )

  def validateFirstName(firstName: String): Either[DomainValidation, String] =
    Either.cond(
      firstName.matches("^[a-zA-Z]+$"),
      firstName,
      FirstNameHasSpecialCharacters
    )

  def validateLastName(lastName: String): Either[DomainValidation, String] =
    Either.cond(
      lastName.matches("^[a-zA-Z]+$"),
      lastName,
      LastNameHasSpecialCharacters
    )

  def validateAge(age: Int): Either[DomainValidation, Int] =
    Either.cond(
      age >= 18 && age <= 75,
      age,
      AgeIsInvalid
    )

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
    } yield RegistrationData(validatedUserName, validatedPassword, validatedFirstName, validatedLastName, validatedAge)
  }

}

object FormValidator extends FormValidator

// we get only the first error
FormValidator.validateForm(
  username = "fakeUs3rname",
  password = "password",
  firstName = "John",
  lastName = "Doe",
  age = 15
)

import cats.data._
import cats.data.Validated._
import cats.syntax.all._

def validateUserName(userName: String): Validated[DomainValidation, String] =
  FormValidator.validateUserName(userName).toValidated

def validatePassword(password: String): Validated[DomainValidation, String] =
  FormValidator.validatePassword(password).toValidated

def validateFirstName(firstName: String): Validated[DomainValidation, String] =
  FormValidator.validateFirstName(firstName).toValidated

def validateLastName(lastName: String): Validated[DomainValidation, String] =
  FormValidator.validateLastName(lastName).toValidated

def validateAge(age: Int): Validated[DomainValidation, Int] =
  FormValidator.validateAge(age).toValidated

// form validation with Validated and for-comprehension/flatMap
// does not compile
//
// def validateForm(
//     username: String,
//     password: String,
//     firstName: String,
//     lastName: String,
//     age: Int
// ): Validated[DomainValidation, RegistrationData] = {
//   for {
//     validatedUserName  <- validateUserName(username)
//     validatedPassword  <- validatePassword(password)
//     validatedFirstName <- validateFirstName(firstName)
//     validatedLastName  <- validateLastName(lastName)
//     validatedAge       <- validateAge(age)
//   } yield RegistrationData(validatedUserName, validatedPassword, validatedFirstName, validatedLastName, validatedAge)
// }
// error: value flatMap is not a member of cats.data.Validated[repl.MdocSession.App.Doma

// form validation with Validated and mapN
sealed trait FormValidatorNec {

  type ValidationResult[A] = ValidatedNec[DomainValidation, A]

  private def validateUserName(userName: String): ValidationResult[String] =
    if (userName.matches("^[a-zA-Z0-9]+$"))
      userName.validNec
    else
      UsernameHasSpecialCharacters.invalidNec

  private def validatePassword(password: String): ValidationResult[String] =
    if (password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$"))
      password.validNec
    else
      PasswordDoesNotMeetCriteria.invalidNec

  private def validateFirstName(firstName: String): ValidationResult[String] =
    if (firstName.matches("^[a-zA-Z]+$"))
      firstName.validNec
    else
      FirstNameHasSpecialCharacters.invalidNec

  private def validateLastName(lastName: String): ValidationResult[String] =
    if (lastName.matches("^[a-zA-Z]+$"))
      lastName.validNec
    else
      LastNameHasSpecialCharacters.invalidNec

  private def validateAge(age: Int): ValidationResult[Int] =
    if (age >= 18 && age <= 75)
      age.validNec
    else
      AgeIsInvalid.invalidNec

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
}

object FormValidatorNec extends FormValidatorNec

FormValidatorNec.validateForm(
  username = "Joe",
  password = "Passw0r$1234",
  firstName = "John",
  lastName = "Doe",
  age = 21
)

FormValidatorNec.validateForm(
  username = "Joe%%%",
  password = "password",
  firstName = "John",
  lastName = "Doe",
  age = 21
)

// Successful case
FormValidatorNec
  .validateForm(
    username = "Joe",
    password = "Passw0r$1234",
    firstName = "John",
    lastName = "Doe",
    age = 21
  )
  .toEither

// Invalid case
FormValidatorNec
  .validateForm(
    username = "Joe123#",
    password = "password",
    firstName = "John",
    lastName = "Doe",
    age = 5
  )
  .toEither

// form validation with Either and parMapN
sealed trait FormValidatorParallel {

  def validateUserName(userName: String): EitherNec[DomainValidation, String] =
    Either.cond(
      userName.matches("^[a-zA-Z0-9]+$"),
      userName,
      NonEmptyChain.one(UsernameHasSpecialCharacters)
    )

  def validatePassword(password: String): EitherNec[DomainValidation, String] =
    Either.cond(
      password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$"),
      password,
      NonEmptyChain.one(PasswordDoesNotMeetCriteria)
    )

  def validateFirstName(firstName: String): EitherNec[DomainValidation, String] =
    Either.cond(
      firstName.matches("^[a-zA-Z]+$"),
      firstName,
      NonEmptyChain.one(FirstNameHasSpecialCharacters)
    )

  def validateLastName(lastName: String): EitherNec[DomainValidation, String] =
    Either.cond(
      lastName.matches("^[a-zA-Z]+$"),
      lastName,
      NonEmptyChain.one(LastNameHasSpecialCharacters)
    )

  def validateAge(age: Int): EitherNec[DomainValidation, Int] =
    Either.cond(
      age >= 18 && age <= 75,
      age,
      NonEmptyChain.one(AgeIsInvalid)
    )

  import cats.Parallel
  import cats.syntax.parallel._

  def validateForm(
      username: String,
      password: String,
      firstName: String,
      lastName: String,
      age: Int
  ): EitherNec[DomainValidation, RegistrationData] =
    (
      validateUserName(username),
      validatePassword(password),
      validateFirstName(firstName),
      validateLastName(lastName),
      validateAge(age)
    ).parMapN(RegistrationData)

  // this alternative implementation is equivalent to the above
  // it demonstates what 'parMapN' is doing under the hood
  def validateForm2(
      username: String,
      password: String,
      firstName: String,
      lastName: String,
      age: Int
  ): EitherNec[DomainValidation, RegistrationData] = {

    def par[A](fa: EitherNec[DomainValidation, A]): ValidatedNec[DomainValidation, A] =
      Parallel[EitherNec[DomainValidation, *]].parallel(fa)
    def seq[A](fa: ValidatedNec[DomainValidation, A]): EitherNec[DomainValidation, A] =
      Parallel[EitherNec[DomainValidation, *]].sequential(fa)

    val validated: ValidatedNec[DomainValidation, RegistrationData] = (
      par(validateUserName(username)),
      par(validatePassword(password)),
      par(validateFirstName(firstName)),
      par(validateLastName(lastName)),
      par(validateAge(age))
    ).mapN(RegistrationData)

    val either: EitherNec[DomainValidation, RegistrationData] = seq(validated)
    either
  }

  // this alternative implementation is equivalent to the above
  // it demonstates what 'parMapN' is doing under the hood
  def validateForm3(
      username: String,
      password: String,
      firstName: String,
      lastName: String,
      age: Int
  ): EitherNec[DomainValidation, RegistrationData] = {

    def P[E] = Parallel[EitherNec[E, *]]

    implicit final class ParallelSyntax[E, A](private val fa: EitherNec[E, A])      {
      def par: ValidatedNec[E, A] = P.parallel(fa)
    }
    implicit final class SequentialSyntax[E, A](private val fa: ValidatedNec[E, A]) {
      def seq: EitherNec[E, A] = P.sequential(fa)
    }

    (
      validateUserName(username).par,
      validatePassword(password).par,
      validateFirstName(firstName).par,
      validateLastName(lastName).par,
      validateAge(age).par
    ).mapN(RegistrationData).seq
  }
}

object FormValidatorParallel extends FormValidatorParallel

// Successful case
FormValidatorParallel
  .validateForm(
    username = "Joe",
    password = "Passw0r$1234",
    firstName = "John",
    lastName = "Doe",
    age = 21
  )

// Invalid case
FormValidatorParallel
  .validateForm(
    username = "Joe123#",
    password = "password",
    firstName = "John",
    lastName = "Doe",
    age = 5
  )

// Successful case
FormValidatorParallel
  .validateForm2(
    username = "Joe",
    password = "Passw0r$1234",
    firstName = "John",
    lastName = "Doe",
    age = 21
  )

// Invalid case
FormValidatorParallel
  .validateForm2(
    username = "Joe123#",
    password = "password",
    firstName = "John",
    lastName = "Doe",
    age = 5
  )

// Successful case
FormValidatorParallel
  .validateForm3(
    username = "Joe",
    password = "Passw0r$1234",
    firstName = "John",
    lastName = "Doe",
    age = 21
  )

// Invalid case
FormValidatorParallel
  .validateForm3(
    username = "Joe123#",
    password = "password",
    firstName = "John",
    lastName = "Doe",
    age = 5
  )
