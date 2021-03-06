// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.essentials

import java.time.{Duration, LocalDate}

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.util.Try

object types {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // List all values of the type `Boolean`.
  //
  val BoolValues: List[Boolean] = List(true, false)
  val BoolValues1: List[Boolean] = true :: false :: Nil

  //
  // EXERCISE 2
  //
  // List all values of the type `Either[Unit, Boolean]`.
  //
  val EitherUnitBoolValues: List[Either[Unit, Boolean]] = List(Left(()), Right(true), Right(false))

  //
  // EXERCISE 3
  //
  // List all values of the type `(Boolean, Boolean)`.
  //
  val TupleBoolBoolValues: List[(Boolean, Boolean)] = List((true, true), (false, false), (true, false), (false, true))

  //
  // EXERCISE 4
  //
  // List all values of the type `Either[Either[Unit, Unit], Unit]`.
  //
  val EitherEitherUnitUnitUnitValues: List[Either[Either[Unit, Unit], Unit]] =
    List(Left(Left(())), Right(()), Left(Right(())), Right(Left(())))

  //
  // EXERCISE 5
  //
  // Create a product type of `Int` and `String`, representing the age and
  // name of a person.
  //
  type Person = (Int, String)

  //
  // EXERCISE 6
  //
  // Prove that `A * 1` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to1[A](t: (A, Unit)): A = t._1
  def from1[A](a: A): (A, Unit) = (a, ())

  //
  // EXERCISE 7
  //
  // Prove that `A * 0` is equivalent to `0` by implementing the following two
  // functions.
  //
  def to2[A](t: (A, Nothing)): Nothing = t._2
  def from2[A](n: A): (A, Nothing) = (n, ???)

  //
  // EXERCISE 8
  //
  // Create a sum type of `Int` and `String` representing the identifier of
  // a robot (a number) or a person (a name).
  //
  type Identifier = Either[Int, String]
  sealed trait Id
  case class RobotId(id: Int) extends Id
  case class PersonId(name: String) extends Id

  def toto(id: Id): String = id match {
    case RobotId(id) => id.toString
    case PersonId(name) => name
  }

  //
  // EXERCISE 9
  //
  // Prove that `A + 0` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to3[A](t: Either[A, Nothing]): A = t.left.get
  def from3[A](a: A): Either[A, Nothing] = Left(a)

  //
  // EXERCISE 10
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // credit card, which has a number, an expiration date, and a security code.
  //
  type CreditCard = (Int, LocalDate, String)

  //
  // EXERCISE 11
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // payment method, which could be a credit card, bank account, or
  // cryptocurrency.
  //
  type PaymentMethod = ???
  sealed trait PaymentMethod1
  case object Credit extends PaymentMethod1
  case object BankAccount extends PaymentMethod1
  case object CryptoCurrency extends PaymentMethod1

  //
  // EXERCISE 12
  //
  // Create either a sum type or a product type (as appropriate) to represent an
  // employee at a company, which has a title, salary, name, and employment date.
  //
  type Employee = (String, Double, String, LocalDate)

  //
  // EXERCISE 13
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // piece on a chess board, which could be a pawn, rook, bishop, knight,
  // queen, or king.
  //
  type ChessPiece = ???
  sealed trait Piece
  case object Pawn extends Piece
  case object Rook extends Piece
  case object Bishop extends Piece
  case object Knight extends Piece
  case object Queen extends Piece
  case object King extends Piece

  //
  // EXERCISE 14
  //
  // Create an ADT model of a game world, including a map, a player, non-player
  // characters, different classes of items, and character stats.
  //
  // Game characteristics:
  // --------------------
  //
  //  - Our game world is composed of realms and paths linking realms.
  //  - A realm has an identifier, an inventory which is a list of items, and has some characters present in it.
  //    It could be a plain, a dungeon or a cave.
  //  - A character an inventory. He could be a player or a non player.
  //  - A player has a name.
  //  - A non player can be a ogre, a troll or a wizard.
  //

  case class GameWorld(realms: List[Realm], paths: List[RealmPath])
  type Item = String

  final case class RealmPath(from: Realm, to: Realm)

  sealed trait RealmType
  final case object Plain extends RealmType
  final case object Dungeon extends RealmType
  final case object Cave extends RealmType

  final case class Realm(identifier: String, inventory: List[Item], characters: List[Character], realmType: RealmType)

  /*
    // OO
    def f(r: Realm): ??? = r match {
      case Plain(...) =>
      case Dungeon(...) =>
    }

    // FP
    def g(r: Realm): ??? = r.realmType match {
      case Plain => r.id
      case Dungeon =>
    }
   */

  final case class Character(inventory: List[Item], characterType: CharacterType)

  sealed trait CharacterType
  final case class Player(name: String)
  final case class NonPlayer(nonPlayerType: NonPlayerType)

  sealed trait NonPlayerType
  final case object Ogre extends NonPlayerType
  final case object Troll extends NonPlayerType
  final case object Wizard extends NonPlayerType

  /*
    def f(c: Character): ??? = c.match {
      case Character(_, Player(name)) => s"Hello I'm a player. My name is ${name}"
      case Character(_, NonPlayer(Ogre)) => "Hello I'm a ogre"
      case Character(_, NonPlayer(Troll)) => "Hello I'm a troll"
      case Character(_, NonPlayer(Wizard)) => "Hello I'm a wizard"
    }
   */
}

object functions {
  type ??? = Nothing


  //
  // EXERCISE 1
  //
  // Convert the following non-function into a function.
  //
  def parseInt1(s: String): Int = s.toInt
  def parseInt2(s: String): Try[Int] = Try(s.toInt)

  //
  // EXERCISE 2
  //
  // Convert the following non-function into a function.
  //
  def arrayUpdate1[A](arr: Array[A], i: Int, f: A => A): Unit =
    arr.update(i, f(arr(i)))

  def arrayUpdate2[A](arr: Array[A], i: Int, f: A => A): Array[A] = ???

  //
  // EXERCISE 3
  //
  // Convert the following non-function into a function.
  //
  def divide1(a: Int, b: Int): Int = a / b
  def divide2(a: Int, b: Int): Option[Int] =
    if (b != 0) Some(a / b)
    else None

  def divide3(a: Int, b: Int): Either[ArithmeticException, Int] =
    if (b != 0) Right(a / b)
    else Left(new ArithmeticException())

  //
  // EXERCISE 4
  //
  // Convert the following non-function into a function.
  //
  var id = 0
  def freshId1(): Int = {
    val newId = id
    id += 1
    newId
  }
  def freshId2(oldId: Int): (Int, Int) = (oldId, oldId + 1)

  //
  // EXERCISE 5
  //
  // Convert the following non-function into a function.
  //
  import java.time.LocalDateTime
  def afterOneHour1: LocalDateTime = LocalDateTime.now.plusHours(1)
  def afterOneHour2(now: LocalDateTime): LocalDateTime = now.plusHours(1)

  //
  // EXERCISE 6
  //
  // Convert the following non-function into function.
  //
  def head1[A](as: List[A]): A = {
    if (as.length == 0) println("Oh no, it's impossible!!!")
    as.head
  }
  def head2[A](as: List[A]): Option[A] = as.headOption

  //
  // EXERCISE 7
  //
  // Convert the following non-function into a function.
  //
  trait Account
  trait Processor {
    def charge(account: Account, amount: Double): Unit
  }
  case class Coffee() {
    val price = 3.14
  }
  def buyCoffee1(processor: Processor, account: Account): Coffee = {
    val coffee = Coffee()
    processor.charge(account, coffee.price)
    coffee
  }
  final case class Charge(account: Account, amount: Double)
  def buyCoffee2(account: Account): (Coffee, Charge) = {
    val coffee: Coffee = Coffee()
    (coffee, Charge(account, coffee.price))
  }

  //
  // EXERCISE 8
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def printLine(line: String): Unit = ()

  //
  // EXERCISE 9
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def readLine: String = ""
  def readLine1: String = "oui"

  //
  // EXERCISE 10
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def systemExit(code: Int): Unit = ()

  //
  // EXERCISE 11
  //
  // Rewrite the following non-function `printer1` into a pure function, which
  // could be used by pure or impure code.
  //
  def printer1(): Unit = {
    println("Welcome to the help page!")
    println("To list commands, type `commands`.")
    println("For help on a command, type `help <command>`")
    println("To exit the help page, type `exit`.")
  }
  def printer2[A](println: String => A, combine: (A, A) => A): A = {
//    combine(
//      combine(
//        println("Welcome to the help page!"),
//        println("To list commands, type `commands`.")
//      ),
//      combine(
//        println("For help on a command, type `help <command>`"),
//        println("To exit the help page, type `exit`.")
//      )
//    )
    List(
      "Welcome to the help page!",
      "To list commands, type `commands`.",
      "For help on a command, type `help <command>`",
      "To exit the help page, type `exit`."
    ).map(println).reduce(combine)
  }

  //
  // EXERCISE 12
  //
  // Create a purely-functional drawing library that is equivalent in
  // expressive power to the following procedural library. (draw2)
  //
  type Bitmap = List[List[Boolean]]

  trait Draw {
    def goLeft(): Unit
    def goRight(): Unit
    def goUp(): Unit
    def goDown(): Unit
    def draw(): Unit
    def finish(): Bitmap
  }

  trait Draw2 {
    def draw(canvas: Bitmap, x: Int, y: Int): Bitmap
    def finish(canvas: Bitmap): Bitmap
  }

  def draw1(size: Int): Draw = new Draw {
    val canvas: Array[Array[Boolean]] = Array.fill(size, size)(false)
    var x                             = 0
    var y                             = 0

    def goLeft(): Unit  = x -= 1
    def goRight(): Unit = x += 1
    def goUp(): Unit    = y += 1
    def goDown(): Unit  = y -= 1
    def draw(): Unit = {
      def wrap(x: Int): Int =
        if (x < 0) (size - 1) + ((x + 1) % size) else x % size

      val x2 = wrap(x)
      val y2 = wrap(y)

      canvas.updated(x2, canvas(x2).updated(y2, true))
    }
    def finish(): Bitmap = canvas.map(_.toList).toList
  }

  // Solution pas feature-complete (pas de go{...})
  def draw2(size: Int): Draw2 = new Draw2 {
    def draw(canvas: Bitmap, x: Int, y: Int): Bitmap = {
      def wrap(i: Int): Int = if (i < 0) (size - 1) + ((i + 1) % size) else i % size

      val x2 = wrap(x)
      val y2 = wrap(y)

      canvas.updated(x2, canvas(x2).updated(y2, true))
    }

    def finish(canvas: Bitmap): Bitmap = canvas.map(_.toList)
  }

  sealed trait Action
  case object GoLeft extends Action
  case object GoRight extends Action
  case object GoUp extends Action
  case object GoDown extends Action
  case object Draw extends Action

  final case class Abscissa(value: Int) extends AnyVal {
    def +(i: Int): Abscissa = copy(value + i)
    def -(i: Int): Abscissa = copy(value - i)
  }

  final case class Ordinate(value: Int) extends AnyVal {
    def +(i: Int): Ordinate = copy(value + i)
    def -(i: Int): Ordinate = copy(value - i)
  }

  type Coord = (Abscissa, Ordinate)

  def draw3(size: Int, actions: List[Action]): Bitmap = {
    @inline
    def wrap(x: Int): Int = if (x < 0) (size - 1) + ((x + 1) % size) else x % size

    actions.foldLeft(((Abscissa(0), Ordinate(0)), List.fill(size, size)(false)): (Coord, Bitmap)) {
      case (((x, y), bitmap), action) => action match {
        case GoLeft  => ((x - 1, y), bitmap)
        case GoRight => ((x + 1, y), bitmap)
        case GoUp    => ((x, y + 1), bitmap)
        case GoDown  => ((x, y - 1), bitmap)
        case Draw    =>
          val x2 = wrap(x.value)
          val y2 = wrap(y.value)

          ((Abscissa(x2), Ordinate(y2)), bitmap.updated(x2, bitmap(x2).updated(y2, true)))
      }
    }._2
  }
}

object higher_order {
  //
  // EXERCISE 1
  //
  // Implement the following higher-order function.
  //
  def fanout[A, B, C](f: A => B, g: A => C): A => (B, C) = { a: A => (f(a), g(a)) }

  //
  // EXERCISE 2
  //
  // Implement the following higher-order function.
  //
  def cross[A, B, C, D](f: A => B, g: C => D): (A, C) => (B, D) = { (a: A, c: C) => (f(a), g(c)) }

  //
  // EXERCISE 3
  //
  // Implement the following higher-order function.
  //
  def either[A, B, C](f: A => B, g: C => B): Either[A, C] => B = {
    case Left(v)  => f(v)
    case Right(v) => g(v)
  }

  //
  // EXERCISE 4
  //
  // Implement the following higher-order function.
  //
  def choice[A, B, C, D](f: A => B, g: C => D): Either[A, C] => Either[B, D] = {
    case Left(v)  => Left(f(v))
    case Right(v) => Right(g(v))
  }

  import cats.implicits._

  def choice2[A, B, C, D](f: A => B, g: C => D): Either[A, C] => Either[B, D] = _.bimap(f, g)

  //
  // EXERCISE 5
  //
  // Implement the following higher-order function.
  //
  def compose[A, B, C](f: B => C, g: A => B): A => C = { a: A => f(g(a)) }
  def compose2[A, B, C](f: B => C, g: A => B): A => C = { a: A => (g andThen f)(a) }
  def compose3[A, B, C](f: B => C, g: A => B): A => C = { a: A => (f compose g)(a) }


  // ------

  final case class Parser[+E, +A](run: String => Either[E, (String, A)])

  //
  // EXERCISE 6
  //
  // Implement the following higher-order function.
  //
  def fail[E](e: E): Parser[E, Nothing] = Parser(_ => Left(e))

  //
  // EXERCISE 7
  //
  // Implement the following higher-order function.
  //
  def point[A](a: => A): Parser[Nothing, A] = Parser(input => Right((input, a)))
  def point2[A](a: => A): Parser[Nothing, A] = Parser(input => Right(input -> a))

  //
  // EXERCISE 8
  //
  // Implement the following higher-order function.
  //
  def char[E](e: E): Parser[E, Char] = Parser(input => input.headOption match {
    case None    => Left(e)
    case Some(c) => Right(input.tail, c)
  })

  //
  // EXERCISE 9
  //
  // Implement the following higher-order function.
  //
  def alt[E1, E2, A, B](l: Parser[E1, A], r: Parser[E2, B]): Parser[E2, Either[A, B]] = Parser(input =>
    l.run(input) match {
      case Left(_) =>
        r.run(input) match {
          case Left(e2)              => Left(e2)
          case Right((rest, b)) => Right((rest, Right(b)))
        }

      case Right((rest, a)) => Right((rest, Left(a)))
  })
}

object poly_functions {
  //
  // EXERCISE 1
  //
  // Create a polymorphic function of two type parameters `A` and `B` called
  // `snd` that returns the second element out of any pair of `A` and `B`.
  //
  object snd {
    def apply[A, B](a: A, b: B): B = b
  }

  def snd[A, B](a: A, b: B): B = b
  // snd(1, "foo") // "foo"

  //
  // EXERCISE 2
  //
  // Create a polymorphic function called `repeat` that can take any
  // function `A => A`, and apply it repeatedly to a starting value
  // `A` the specified number of times.
  //
  object repeat {
    def apply[A](n: Int)(initialValue: A, f: A => A): A = {
      @tailrec
      def go(acc: Int, res: A): A = {
        if (acc >= n) res
        else go(acc + 1, f(res))
      }

      go(0, initialValue)
    }

    @tailrec
    def apply2[A](n: Int)(initialValue: A, f: A => A): A =
      if (n == 0) initialValue
      else apply2(n - 1)(f(initialValue), f)
  }
  // repeat[Int](100)(0, _ + 1) // 100
  // repeat[String](10)("", _ + "*") // "**********"

  //
  // EXERCISE 3
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample1[A, B](a: A, b: B): Either[A, B] = Left(a)
  def countExample11[A, B](a: A, b: B): Either[A, B] = Right(b)
  val countExample1Answer                           = 2

  //
  // EXERCISE 4
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample2[A, B](f: A => B, g: A => B, a: A): B = f(a)
  def countExample21[A, B](f: A => B, g: A => B, a: A): B = g(a)
  val countExample2Answer                                = 2

  //
  // EXERCISE 5
  //
  // Implement the function `groupBy`.
  //
  val Data =
  "poweroutage;2018-09-20;level=20" :: Nil
  val By: String => String =
    (data: String) => data.split(";")(1)
  val Reducer: (String, List[String]) => String =
    (date, events) =>
      "On date " +
        date + ", there were " +
        events.length + " power outages"
  val Expected =
    Map(
      "2018-09-20" ->
        "On date 2018-09-20, there were 1 power outages")

  def groupBy1(l: List[String], by: String => String)(reducer: (String, List[String]) => String): Map[String, String] =
    l.groupBy(by).map { case (day, lines) =>
      day -> reducer(day, lines)
    }

  // groupBy1(Data, By)(Reducer) == Expected

  //
  // EXERCISE 6
  //
  // Make the function `groupBy1` as polymorphic as possible and implement
  // the polymorphic function. Compare to the original.
  //
  def groupBy2[A, B, C](l: List[A], by: A => B)(reducer: (B, List[A]) => C): Map[B, C] =
    l.groupBy(by).map { case (b: B, c: C) =>
      b -> reducer(b, c)
    }
}

object higher_kinded {
  type ??          = Nothing
  type ???[A]      = Nothing
  type ????[A, B]  = Nothing
  type ?????[F[_]] = Nothing

  trait `* => *`[F[_]]
  trait `[*, *] => *`[F[_, _]]
  trait `(* => *) => *`[T[_[_]]]

  //
  // EXERCISE 1
  //
  // Identify a type constructor that takes one type parameter (i.e. has kind
  // `* => *`), and place your answer inside the square brackets.
  //
  type Answer1 = `* => *`[List]

  //
  // EXERCISE 2
  //
  // Identify a type constructor that takes two type parameters (i.e. has kind
  // `[*, *] => *`), and place your answer inside the square brackets.
  //
  type Answer2 = `[*, *] => *`[Tuple2]

  //
  // EXERCISE 3
  //
  // Create a trait with kind `*`.
  //
  trait Answer3

  //
  // EXERCISE 4
  //
  // Create a trait with kind `[*, *, *] => *`.
  //
  trait Answer4[A, B, C]

  //
  // EXERCISE 5
  //
  // Create a new type that has kind `(* => *) => *`.
  //
  type NewType1[List]
  type NewType2[F[_]]
//  type Answer5 = `(* => *) => *`[NewType1]

  //
  // EXERCISE 6
  //
  // Create a trait with kind `[* => *, (* => *) => *] => *`.
  //
  trait Answer6[F[_], G[_[_]]]
  trait Either[F, G]  // not higher-kinded
  //
  // EXERCISE 7
  //
  // Create an implementation of the trait `CollectionLike` for `List`.
  //
  trait CollectionLike[F[_]] {
    def empty[A]: F[A]

    def cons[A](a: A, as: F[A]): F[A]

    def uncons[A](as: F[A]): Option[(A, F[A])]

    final def singleton[A](a: A): F[A] =
      cons(a, empty[A])

    final def append[A](l: F[A], r: F[A]): F[A] =
      uncons(l) match {
        case Some((l, ls)) => append(ls, cons(l, r))
        case None          => r
      }

    final def filter[A](fa: F[A])(f: A => Boolean): F[A] =
      bind(fa)(a => if (f(a)) singleton(a) else empty[A])

    final def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
      uncons(fa) match {
        case Some((a, as)) => append(f(a), bind(as)(f))
        case None          => empty[B]
      }

    final def fmap[A, B](fa: F[A])(f: A => B): F[B] = {
      val single: B => F[B] = singleton[B](_)

      bind(fa)(f andThen single)
    }
  }
  val ListCollectionLike: CollectionLike[List] = new CollectionLike[List] {
    def empty[A]: List[A]                             = Nil
    def cons[A](a: A, as: List[A]): List[A]           = a :: as
    def uncons[A](as: List[A]): Option[(A, List[A])]  = as match {
      case Nil        => None
      case head :: tl => Some(head, tl)
    }
  }

  //
  // EXERCISE 8
  //
  // Implement `Sized` for `List`.
  //
  trait Sized[F[_]] {
    // This method will return the number of `A`s inside `fa`.
    def size[A](fa: F[A]): Int
  }

  val listSized: Sized[List] = new Sized[List] {
    override def size[A](fa: List[A]): Int = fa.size
  }

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to `String`.
  //
  val MapSized1: Sized[Map[String, ?]] = new Sized[Map[String, ?]] {
    override def size[A](fa: Map[String, A]): Int = fa.size
  }

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to a user-defined type parameter.
  //
  def MapSized2[K]: Sized[Map[K, ?]] = new Sized[Map[K, ?]] {
    override def size[A](fa: Map[K, A]): Int = fa.size
  }

  //
  // EXERCISE 10
  //
  // Implement `Sized` for `Tuple3`.
  //
  def Tuple3Sized[A, B]: Sized[(A, B, ?)] = new Sized[(A, B, ?)] {
    override def size[C](fa: (A, B, C)): Int = 1
  }
}

object typeclasses {

  /**
    * {{
    * Reflexivity:   a ==> equals(a, a)
    *
    * Transitivity:  equals(a, b) && equals(b, c) ==>
    *                equals(a, c)
    *
    * Symmetry:      equals(a, b) ==> equals(b, a)
    * }}
    */
  trait Eq[A] {
    def equals(l: A, r: A): Boolean
  }
  object Eq {
    def apply[A](implicit eq: Eq[A]): Eq[A] = eq

    implicit val EqInt: Eq[Int] =
      new Eq[Int] {
        def equals(l: Int, r: Int): Boolean = l == r
      }

    implicit def EqList[A: Eq]: Eq[List[A]] =
      new Eq[List[A]] {
        def equals(l: List[A], r: List[A]): Boolean =
          (l, r) match {
            case (Nil, Nil) => true
            case (Nil, _)   => false
            case (_, Nil)   => false
            case (l :: ls, r :: rs) =>
              Eq[A].equals(l, r) && equals(ls, rs)
          }
      }
  }
  implicit class EqSyntax[A](val l: A) {
    def ====(r: A)(implicit eq: Eq[A]): Boolean = eq.equals(l, r)
  }

  def g0[A](l: A, r: A)(eq: Eq[A]): Boolean              = eq.equals(l, r)
  def g1[A](l: A, r: A)(implicit eq: Eq[A]): Boolean     = eq.equals(l, r)
  def g2[A: Eq](l: A, r: A): Boolean = implicitly[Eq[A]].equals(l, r)
  def g3[A: Eq](l: A, r: A): Boolean = Eq[A].equals(l, r)
  def g4[A: Eq](l: A, r: A): Boolean = l ==== r

  //
  // Scalaz 7 Encoding
  //
  sealed trait Ordering
  case object EQUAL extends Ordering
  case object LT    extends Ordering
  case object GT    extends Ordering
  object Ordering {
    implicit val OrderingEq: Eq[Ordering] = new Eq[Ordering] {
      def equals(l: Ordering, r: Ordering): Boolean =
        (l, r) match {
          case (EQUAL, EQUAL) => true
          case (LT, LT)       => true
          case (GT, GT)       => true
          case _              => false
        }
    }
  }

  trait Ord[A] {
    def compare(l: A, r: A): Ordering
  }
  object Ord {
    def apply[A](implicit A: Ord[A]): Ord[A] = A

    implicit val OrdInt: Ord[Int] = new Ord[Int] {
      def compare(l: Int, r: Int): Ordering =
        if (l < r) LT else if (l > r) GT else EQUAL
    }
  }
  implicit class OrdSyntax[A](val l: A) extends AnyVal {
    def =?=(r: A)(implicit A: Ord[A]): Ordering =
      A.compare(l, r)

    def <(r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), LT)

    def <=(r: A)(implicit A: Ord[A]): Boolean =
      (l < r) || (this === r)

    def >(r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), GT)

    def >=(r: A)(implicit A: Ord[A]): Boolean =
      (l > r) || (this === r)

    def ===(r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), EQUAL)

    def !==(r: A)(implicit A: Ord[A]): Boolean =
      !Eq[Ordering].equals(A.compare(l, r), EQUAL)
  }

  case class Person(age: Int, name: String)

  object Person {
    implicit val OrdPerson: Ord[Person] = new Ord[Person] {
      def compare(l: Person, r: Person): Ordering =
        if (l.age < r.age) LT
        else if (l.age > r.age) GT
        else if (l.name < r.name) LT
        else if (l.name > r.name) GT
        else EQUAL
    }
    implicit val EqPerson: Eq[Person] = new Eq[Person] {
      def equals(l: Person, r: Person): Boolean =
        l == r
    }
  }

  //
  // EXERCISE 1
  //
  // Write a version of `sort1` called `sort2` that uses the polymorphic `List`
  // type constructor, and which uses the `Ord` type class, including the
  // compare syntax operator `=?=` to compare elements.
  //
  def sort1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)

      sort1(lessThan) ++ List(x) ++ sort1(notLessThan)
  }

  def sort2[A: Ord](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case x :: xs =>
        val (lessThan, moreThan) = xs.partition(_ < x)

        sort2(lessThan) ++ List(x) ++ sort2(moreThan)
    }

  //
  // EXERCISE 2
  //
  // Create an instance of `Ord` for the type `String`.
  //
  implicit val OrdString: Ord[String] = ???

  type ???[A] = Nothing

  import scalaz.{Monoid, Semigroup}

  //
  // EXERCISE 3
  //
  // Create an instance of the `Semigroup` type class for `java.time.Duration`.
  //
  implicit val SemigroupInstant: Semigroup[java.time.Duration] = new Semigroup[Duration] {
    override def append(f1: Duration, f2: => Duration): Duration = f1.plus(f2)
  }

  //
  // EXERCISE 4
  //
  // Create an instance of the `Semigroup` type class for `Int`.
  //
  implicit val SemigroupInt: Semigroup[Int] = new Semigroup[Int] {
    override def append(f1: Int, f2: => Int): Int = f1 + f2
  }

  //
  // EXERCISE 5
  //
  // Create an instance of the `Semigroup` type class for `Set[A]`.
  //
  implicit def SemigroupSet[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    override def append(f1: Set[A], f2: => Set[A]): Set[A] =
      f1 ++ f2
  }

  //
  // EXERCISE 6
  //
  // Create an instance of the `Semigroup` type class for `Map[K, ?]`. Hint:
  // you will need some constraint applied to the values.
  //
  implicit def SemigroupMap[K, V: Semigroup]: Semigroup[Map[K, V]] = new Semigroup[Map[K, V]] {
    override def append(f1: Map[K, V], f2: => Map[K, V]): Map[K, V] = f1 ++ f2
  }

  //
  // EXERCISE 7
  //
  // Create a type class `Monoid[A]` that implies `Semigroup[A]` (that is, every
  // `Monoid[A]` must be a `Semigroup[A]`), which adds a single operation called
  // `zero`, which satisfies additional laws.
  //

  trait MyMonoid[A] extends Semigroup[A] {
    def zero: A
  }

  //
  // EXERCISE 8
  //
  // Create an instance of the `Monoid` type class for `java.time.Duration`.
  //
  implicit val MonoidInstant: Monoid[java.time.Duration] = new Monoid[java.time.Duration] {
    override def zero: Duration = Duration.ZERO
    override def append(f1: Duration, f2: => Duration): Duration = f1.plus(f2)
  }

  //
  // EXERCISE 9
  //
  // Create an instance of the `Monoid` type class for `String`.
  //
  implicit val MonoidString: Monoid[String] = new Monoid[String] {
    override def zero: String = ""
    override def append(f1: String, f2: => String): String = f1 + f2
  }

  //
  // EXERCISE 10
  //
  // Create an instance of the `Monoid` type class for `List[A]`.
  //
  implicit def MonoidList[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def zero: List[A] = Nil
    override def append(f1: List[A], f2: => List[A]): List[A] = f1 ++ f2
  }

  //
  // EXERCISE 11
  //
  // Create an instance of the `Monoid` type class for `Int`.
  //
  implicit val MonoidInt: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0
    override def append(f1: Int, f2: => Int): Int = f1 + f2
  }

  //
  // EXERCISE 12
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the additive monoid, with addition as `append`, and 0 as
  // `zero`.
  //
  final case class Sum(run: Int) extends AnyVal
  implicit val MonoidSum: Monoid[Sum] = new Monoid[Sum] {
    override def zero: Sum = Sum(0)
    override def append(f1: Sum, f2: => Sum): Sum = Sum(f1.run + f2.run)
  }

  //
  // EXERCISE 13
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the multiplicative monoid, with multiplication as `append`,
  // and 1 as `zero`.
  //
  final case class Product(run: Int) extends AnyVal
  implicit val MonoidProduct: Monoid[Product] = new Monoid[Product] {
    override def zero: Product = Product(1)
    override def append(f1: Product, f2: => Product): Product = Product(f1.run * f2.run)
  }
}
