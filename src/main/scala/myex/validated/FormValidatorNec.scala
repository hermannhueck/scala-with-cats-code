package myex.validated

import cats.data.Validated
import cats.data.ValidatedNec
import cats.data.Validated._
import cats.syntax.validated._

sealed trait FormValidatorNec {

  type ValidationResult[A] = ValidatedNec[DomainValidation, A]

  def validateUserName(userName: String): ValidationResult[String] =
    if (userName.matches("^[a-zA-Z0-9]+$")) userName.validNec else UsernameHasSpecialCharacters.invalidNec

  def validatePassword(password: String): ValidationResult[String] =
    if (password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$")) password.validNec
    else PasswordDoesNotMeetCriteria.invalidNec

  def validateFirstName(firstName: String): ValidationResult[String] =
    if (firstName.matches("^[a-zA-Z]+$")) firstName.validNec else FirstNameHasSpecialCharacters.invalidNec

  def validateLastName(lastName: String): ValidationResult[String] =
    if (lastName.matches("^[a-zA-Z]+$")) lastName.validNec else LastNameHasSpecialCharacters.invalidNec

  def validateAge(age: Int): ValidationResult[Int] =
    if (age >= 18 && age <= 75) age.validNec else AgeIsInvalid.invalidNec

}

object FormValidatorNec extends FormValidatorNec
