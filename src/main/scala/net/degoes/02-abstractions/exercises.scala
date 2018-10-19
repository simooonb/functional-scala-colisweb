// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.abstractions

import scalaz._
import Scalaz._

object algebra {
  //
  // EXERCISE 1
  //
  // Define a semigroup for `NotEmpty` below.
  //
  case class NotEmpty[+A](head: A, tail: Option[NotEmpty[A]])
  implicit def NotEmptySemigroup[A]: Semigroup[NotEmpty[A]] =
    ???
  val example1 = NotEmpty(1, None) |+| NotEmpty(2, None)

  //
  // EXERCISE 2
  //
  // Design a permission system for securing some resource, together with a
  // monoid for the permission data structure.
  //
  case class Permission(/* */)
  implicit val MonoidPermission: Monoid[Permission] = ???
  val example2 = mzero[Permission] |+| Permission()

  //
  // EXERCISE 3
  //
  // Define an instance of `Semigroup` for `(A, B)` when both `A` and
  // `B` form semigroups.
  //
  implicit def SemigroupTuple2[A: Semigroup, B: Semigroup]:
    Semigroup[(A, B)] = new Semigroup[(A, B)] {
      def append(l: (A, B), r: => (A, B)): (A, B) =
        (l._1 |+| r._1, l._2 |+| r._2)
    }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Monoid` for `NotEmpty` for any type `A`.
  //
  implicit def MonoidNotEmpty[A]: Monoid[NotEmpty[A]] = ???
}

object functor {
  //
  // EXERCISE 1
  //
  // Define an instance of `Functor` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val BTreeFunctor: Functor[BTree] =
    new Functor[BTree] {
      def map[A, B](fa: BTree[A])(f: A => B): BTree[B] =
        fa match {
          case Leaf(a) => Leaf(f(a))
          case Fork(left, right) => Fork(map(left)(f), map(right)(f))
        }
    }

  //
  // EXERCISE 2
  //
  // Define an instance of `Functor` for `Nothing`.
  //
  implicit val NothingFunctor: Functor[Nothing] = new Functor[Nothing] {
    override def map[A, B](fa: Nothing)(f: A => B): Nothing = fa
  }

  //
  // EXERCISE 3
  //
  // Define an instance of `Functor` for Parser[E, ?].
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def ParserFunctor[E]: Functor[Parser[E, ?]] =
    new Functor[Parser[E, ?]] {
      def map[A, B](fa: Parser[E, A])(f: A => B): Parser[E, B] = {
        Parser[E, B](str =>
          fa.run(str) match {
            case Left(e)       => Left(e)
            case Right((s, a)) => Right((s, f(a)))
          }
        )
      }
    }

  implicit def ParserFunctor2[E]: Functor[Parser[E, ?]] =
    new Functor[Parser[E, ?]] {
      def map[A, B](fa: Parser[E, A])(f: A => B): Parser[E, B] = {
        Parser[E, B](str =>
          fa.run(str).map {
            case (s, a) => (s, f(a))
          }
        )
      }
    }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Functor` for the following data type.
  //
  case class DataType[A](f: A => A)
  implicit val DataTypeFunctor: Functor[DataType] =
    new Functor[DataType] {
      def map[A, B](fa: DataType[A])(f: A => B): DataType[B] = {
        ???
      }
    }

  //
  // EXERCISE 5
  //
  // Define an instance of `Functor` for `FunctorProduct`.
  //
  case class FunctorProduct[F[_], G[_], A](l: F[A], r: G[A])
  implicit def FunctorProductFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorProduct[F, G, ?]] = new Functor[FunctorProduct[F, G, ?]] {
      def map[A, B](fa: FunctorProduct[F, G, A])(f: A => B): FunctorProduct[F, G, B] =
        FunctorProduct[F, G, B](fa.l.map(f), fa.r.map(f))
    }

  //
  // EXERCISE 6
  //
  // Define an instance of `Functor` for `FunctorSum`.
  //
  case class FunctorSum[F[_], G[_], A](run: Either[F[A], G[A]])
  implicit def FunctorSumFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorSum[F, G, ?]] = new Functor[FunctorSum[F, G, ?]] {
      def map[A, B](fa: FunctorSum[F, G, A])(f: A => B): FunctorSum[F, G, B] =
        FunctorSum(fa.run match {
          case Left(l)  => Left(l.map(f))
          case Right(r) => Right(r.map(f))
        })
    }

  //
  // EXERCISE 7
  //
  // Define an instance of `Functor` for `FunctorNest`.
  //
  case class FunctorNest[F[_], G[_], A](run: F[G[A]])
  implicit def FunctorNestFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorNest[F, G, ?]] = new Functor[FunctorNest[F, G, ?]] {
      def map[A, B](fa: FunctorNest[F, G, A])(f: A => B): FunctorNest[F, G, B] =
        FunctorNest(fa.run.map(_.map(f)))
    }

  def zipOption[A, B](l: Option[A], r: Option[B]): Option[(A, B)] =
    (l, r) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _ => None
    }

  def zipWith[A, B, C](l: Option[A], r: Option[B])(f: ((A, B)) => C): Option[C] =
    zipOption(l, r).map(f)

  def zipList1[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, bs) =>
        zipList1(as, bs) ++ bs.map(b => (a, b))
      case (Nil, bs) => Nil
    }
  def zipList2[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, b :: bs) => ((a, b)) :: zipList2(as, bs)
      case _ => Nil
    }

  //
  // EXERCISE 8
  //
  // Define `Applicative` for `Option`.
  //
  implicit val OptionApplicative: Applicative[Option] =
    new Applicative[Option] {
      def point[A](a: => A): Option[A] = Some(a)

      def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] =
        (fa, f) match {
          case (Some(a), Some(ff)) => Some(ff(a))
          case _                   => None
        }
    }

  //
  // EXERCISE 9
  //
  // Implement `zip` in terms of the applicative composition using `|@|`.
  //
  // Bonus: Implement `ap2` in terms of `zip`.
  //
  val example1 = (Option(3) |@| Option(5))((_, _))
  val example2 = zip(Option(3), Option("foo")) : Option[(Int, String)]

  def zip[F[_]: Applicative, A, B](l: F[A], r: F[B]): F[(A, B)] =
    (l |@| r)((_, _))

  def ap2[F[_]: Applicative, A, B](fa: F[A], fab: F[A => B]): F[B] =
    zip(fa, fab).map { case (a, f) => f(a) }

  //
  // EXERCISE 10
  //
  // Define an instance of `Applicative` for `Parser[E, ?]`.
  //
  implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
    new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E,A] =
        Parser[E, A](s => Right(s -> a))

      def ap[A, B](fa: => Parser[E,A])(f: => Parser[E, A => B]): Parser[E,B] =
        Parser[E, B] { s =>
          f.run(s) match {
            case Left(e)        => Left(e)
            case Right((s0, f)) =>
              fa.run(s0) match {
                case Left(e)        => Left(e)
                case Right((s1, a)) => Right((s1, f(a)))
              }
          }
        }
    }

  implicit def ApplicativeParserFlorian[E]: Applicative[Parser[E, ?]] =
    new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E,A] = Parser(s => Right((s, a)))

      def ap[A, B](fa: => Parser[E,A])(
        f: => Parser[E, A => B]): Parser[E,B] =
        Parser(
          (s: String) => zip(fa.run(s), f.run(s)) match {
            case Left(e) => Left(e)
            case Right(((s1, a), (s2, b))) => Right(/*s1 + s2*/s2, b(a))
          }
        )
    }

  //
  // EXERCISE 11
  //
  // Define an instance of `Monad` for `BTree`.
  //
  implicit val MonadBTree: Monad[BTree] =
    new Monad[BTree] {
      def point[A](a: => A): BTree[A] = Leaf(a)

      def bind[A, B](fa: BTree[A])(f: A => BTree[B]): BTree[B] =
        fa match {
          case Leaf(a)           => f(a)
          case Fork(left, right) => Fork(bind(left)(f), bind(right)(f))
        }
    }

  //
  // EXERCISE 12
  //
  // Define an instance of `Monad` for `Parser[E, ?]`.
  //
  implicit def MonadParser[E]: Monad[Parser[E, ?]] =
    new Monad[Parser[E, ?]] {
      def point[A](a: => A): Parser[E,A] = Parser[E, A](s => Right(s -> a))

      def bind[A, B](fa: Parser[E,A])(f: A => Parser[E,B]): Parser[E,B] =
        Parser[E, B](s =>
          fa.run(s) match {
            case Left(e)        => Left(e)
            case Right((s1, a)) => f(a).run(s1)
          }
        )
    }
}

object foldable {
  //
  // EXERCISE 1
  //
  // Define an instance of `Foldable` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val FoldableBTree: Foldable[BTree] =
    new Foldable[BTree] {
      def foldMap[A, B](fa: BTree[A])(f: A => B)(implicit F: Monoid[B]): B =
        ???

      def foldRight[A, B](fa: BTree[A], z: => B)(f: (A, => B) => B): B =
        ???
    }

  //
  // EXERCISE 2
  //
  // Try to define an instance of `Foldable` for `A => ?`.
  //
  implicit def FunctionFoldable[A]: Foldable[A => ?] =
    ???

  //
  // EXERCISE 3
  //
  // Define an instance of `Traverse` for `BTree`.
  //
  implicit val TraverseBTree: Traverse[BTree] =
    new Traverse[BTree] {
      def traverseImpl[G[_], A, B](fa: BTree[A])(f: A => G[B])(implicit F: Applicative[G]): G[BTree[B]] =
        fa match {
          case Leaf(a)           => f(a).map(Leaf(_))
          case Fork(left, right) => (traverseImpl(left)(f) |@| traverseImpl(right)(f))(Fork(_, _))
        }
    }

  implicit val TraverseBTree2: Traverse[BTree] =
    new Traverse[BTree] {
      def traverseImpl[G[_], A, B](fa: BTree[A])(f: A => G[B])(implicit F: Applicative[G]): G[BTree[B]] =
        fa match {
          case Leaf(a)           => f(a).map(Leaf(_))
          case Fork(left, right) => F.apply2(traverseImpl(left)(f), traverseImpl(right)(f))(Fork(_, _))
        }
    }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Traverse` for `Parser[E, ?]`.
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def TraverseParser[E]: Traverse[Parser[E, ?]] =
    new Traverse[Parser[E, ?]] {
      def traverseImpl[G[_], A, B](fa: Parser[E, A])(f: A => G[B])(implicit F: Applicative[G]): G[Parser[E, B]] =
        ???
//        Parser[E, B](input =>
//          fa.run(input) match {
//            case Left(e)  => Left(e)
//            case Right((s, a)) => f(a).map(b => Right(s, a))
//          }
//        )

      // Impossible
    }
}

object optics {
  sealed trait Country
  object Country {
    val usa: Prism[Country, Unit] =
      Prism[Country, Unit]({
        case USA => Some(())
        case _ => None
      }, _ => USA)
  }
  case object USA extends Country
  case object UK extends Country
  case object Poland extends Country

  case class Org(name: String, address: Address, site: Site)
  object Org {
    val site: Lens[Org, Site] =
      Lens[Org, Site](_.site, l => _.copy(site = l))
  }
  case class Address(
    number: String,
    street: String,
    postalCode: String,
    country: Country)
  case class Site(
    manager: Employee,
    address: Address,
    employees: Set[Employee])
  object Site {
    val manager: Lens[Site, Employee] =
      Lens[Site, Employee](_.manager, m => _.copy(manager = m))
  }
  case class Employee(
    name: String,
    dob: java.time.Instant,
    salary: BigDecimal,
    address: Address)
  object Employee {
    val salary: Lens[Employee, BigDecimal] =
      Lens[Employee, BigDecimal](_.salary, s => _.copy(salary = s))
  }

  lazy val org: Org = ???

  lazy val org2 =
    org.copy(site =
      org.site.copy(manager = org.site.manager.copy(
        salary = org.site.manager.salary * 0.95
      ))
    )

  //
  // EXERCISE 1
  //
  // Implement the `>>>` method of `Lens`.
  //
  final case class Lens[S, A](
    get: S => A,
    set: A => (S => S)
  ) { self =>
    def >>> [B](that: Lens[A, B]): Lens[S, B] =
      ???

    final def update(f: A => A): S => S =
      (s: S) => self.set(f(self.get(s)))(s)
  }

  //
  // EXERCISE 2
  //
  // Create a version of `org2` that uses lenses to update the salaries.
  //
  val org2_lens: Org =
    (Org.site >>> Site.manager >>> Employee.salary).
      update(_ * 0.95)(org)

  //
  // EXERCISE 3
  //
  // Implement `>>>` for `Prism`.
  //
  final case class Prism[S, A](
    get: S => Option[A],
    set: A => S) { self =>
    def >>> [B](that: Prism[A, B]): Prism[S, B] =
      ???

    final def select(implicit ev: Unit =:= A): S =
      set(ev(()))
  }

  //
  // EXERCISE 4
  //
  // Implement `_Left` and `_Right`.
  //
  def _Left[A, B]: Prism[Either[A, B], A] =
    ???
  def _Right[A, B]: Prism[Either[A, B], B] =
    ???
}
