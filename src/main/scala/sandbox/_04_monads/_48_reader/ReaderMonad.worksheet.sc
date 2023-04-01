// --- 4.8.1 Creating and Unpacking Readers

import cats.data.Reader

case class Cat(name: String, favoriteFood: String)

val catName = Reader[Cat, String](cat => cat.name)

catName.run(Cat("Garfield", "lasagne")) // same as ...
catName(Cat("Garfield", "lasagne"))

// --- 4.8.2 Composing Readers

val greetKitty: Reader[Cat, String] =
  catName.map(name => s"Hello $name")

greetKitty.run(Cat("Heathcliff", "junk food"))

val feedKitty: Reader[Cat, String] =
  Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greetKitty
    feed  <- feedKitty
  } yield s"$greet. $feed."

greetAndFeed(Cat("Garfield", "lasagne"))

greetAndFeed(Cat("Heathcliff", "junk food"))
