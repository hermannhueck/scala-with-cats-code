package myex.validated.config

sealed abstract class ConfigError
final case class MissingConfig(field: String) extends ConfigError
final case class ParseError(field: String)    extends ConfigError
