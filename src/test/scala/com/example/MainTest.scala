package com.example

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import org.specs2.matcher.{AlwaysMatcher, Expectable, Matcher}

import scala.util.{Random, Try}

class MainTest extends SpecificationWithJUnit {

  trait Context extends Scope

  "specs2 basic matchers" >> {
    "use beEqualTo in order to check equality" in new Context {
      1 must beEqualTo(1)
      "some-string" must beEqualTo("some-string")
      ACaseClass(1, "Yuval") must beEqualTo(ACaseClass(1, "Yuval"))
    }

    "use beTrue and beFalse for booleans" in new Context {
      true must beTrue
      false must beFalse
    }

    "when we just want to assert the size of a collection" in new Context {
      List(1,2,3) must haveSize(3)
      Set(1,2,3,4) must haveLength(4) //(just an alias of haveSize)
      "1234" must haveLength(4)
    }

    "use available matchers for scala options" in new Context {
      Option(null) must beNone
      Option(ACaseClass(1, "whatever")) must beSome(ACaseClass(1, "whatever"))
    }

    "sometimes you only care about the type" in new Context {
      ACaseClass(1, "whatever") must beAnInstanceOf[ACaseClass]
    }

    "use 'not' when you want to negate the matcher" in new Context {
      true must not(beFalse)
      ACaseClass(1, "whatever") must not(beEqualTo(ACaseClass(1, "no")))
    }

    "when we expect an exception to be thrown" in new Context {
      val aCaseClass = ACaseClass(1, "lets throw exception")

      aCaseClass.throwException(true) must throwA[RuntimeException]
//      aCaseClass.throwException(true) must not(throwA[NullPointerException])
//      aCaseClass.aNothing() must beAnInstanceOf[Nothing]
    }

    "scala tries are also supported" in new Context {
      Try("some-string") must beSuccessfulTry.withValue("some-string")
      Try(throw new RuntimeException) must beFailedTry.withThrowable[RuntimeException]
    }

    "when we have collections that we know exactly what's inside" in new Context {
      List() must beEmpty
      List(1,2,3) must containTheSameElementsAs(List(1,2,3))
      Seq(1,2,3) must containTheSameElementsAs(Seq(1,3,2)) // order doesn't matter
      Set(1,2,3) must containTheSameElementsAs(Seq(1,3,2))
      Seq(1,2,3) must contain(1,2,3).inOrder.exactly // if we want it in order
    }

    "when we want only some of the data to exist in the collection" in new Context {
      List("JMock", "is", "amazing", "and", "not", "annoying") must
        contain("JMock", "is", "annoying").inOrder
    }

    "use custom equality when using containTheSameElementsAs" in new Context {
      val ls1 = List(ACaseClass(Random.nextInt(), "one"), ACaseClass(Random.nextInt(), "two"))
      val ls2 = List(ACaseClass(Random.nextInt(), "one"), ACaseClass(Random.nextInt(), "two"))

      ls1 must containTheSameElementsAs[ACaseClass](ls2, (elem1, elem2) => elem1.text == elem2.text)
    }
  }

  "matchers that receive matchers" >> {
    "use contains with matchers per value" in new Context {
      val ls1 = List(1, 2, 3)
      ls1 must contain(beGreaterThan(0), beGreaterThan(1), beGreaterThan(2))
      // atMost - elements in the asserted list should each have at least 1 corresponding successful matcher
      ls1 must contain(atMost(beGreaterThan(0), beGreaterThan(1), beGreaterThan(-1), beGreaterThan(2)))
      // atLeast - each matcher should have at least 1 corresponding successful value (same as contains(...))
      ls1 must contain(atLeast(beGreaterThan(0), beGreaterThan(1)))
      // exactly - each value from the list should have exactly 1 corresponding successful matcher
      ls1 must contain(exactly[Int](beGreaterThan(0), beGreaterThan(1), beEqualTo(3)))
//      ls1 must contain(exactly(beGreaterThan(0), beGreaterThan(1), beEqualTo(3)))
    }

    "contain with foreach and forall" in new Context {
      val ls1 = List(true, true, true)
      ls1 must forall(beTrue)
      ls1 must foreach(beTrue)

      //forall vs foreach???
 /*     val ls2 = List(1, 2, 3)
      ls2 must forall(beLessThan(2))
      ls2 must foreach(beLessThan(2))*/
    }

    "use matchers to match a successful try value" in new Context {
      Try("some-string") must beSuccessfulTry(startWith("some-"))
    }

    "use matchers to match an option" in new Context {
      Option("some-string") must beSome(endWith("-string"))
    }
  }

  "and + or" >> {
    "use 'and' when you want to verify more than one condition" in new Context {
      1 must beGreaterThan(0) and beLessThan(2) and beEqualTo(1)
    }

    "use 'or' to match successfully at least once" in new Context {
      1 must beEqualTo(0) or beEqualTo(2) or beEqualTo(1)
    }
  }

  "write your own matchers" >> {
    "use \'adapt\' matchers to create a new matcher" in new Context {
      def containACaseClassWithText(text: String): Matcher[Seq[ACaseClass]] = {
        contain[String](text) ^^ {(_:Seq[ACaseClass]).map(_.text)} and
        forall(beGreaterThan(0)) ^^ {(_:Seq[ACaseClass]).map(_.id)}
      }
      val classes = Seq(ACaseClass(2, "hello"), ACaseClass(4, "world"))

      classes must containACaseClassWithText("world")
    }

    "we can use aka to name values" in new Context {
      def haveTheFirstElementWithIdLessThan(number: Int): Matcher[Seq[ACaseClass]] = {
        val some = 3
        val bla = 4
        /*
        * .......
        * */
        beLessThan(number) ^^ {(_:Seq[ACaseClass]).head.id aka "super-cool-id"}
      }

      val classes = Seq(ACaseClass(2, "hello"), ACaseClass(4, "world"))

      classes must haveTheFirstElementWithIdLessThan(3)
//      classes must haveTheFirstElementWithIdLessThan(2)
    }

    "we can also get matchers from the outside" in new Context {
      def haveIdThat(isSomethingOnId: Matcher[Int]): Matcher[ACaseClass] = {
        isSomethingOnId ^^ { (_:ACaseClass).id }
      }

      ACaseClass(4, "world") must haveIdThat(beGreaterThan(3))
    }

    "we can use always matchers" in new Context {
      def beACaseClassThatHas(anIdThatIs: Matcher[Int] = AlwaysMatcher(),
                              aTextThatIs: Matcher[String] = AlwaysMatcher()): Matcher[ACaseClass] = {
        anIdThatIs ^^ { (_:ACaseClass).id } and
        aTextThatIs ^^ { (_:ACaseClass).text }
      }
      val aCaseClass = ACaseClass(4, ":)")

      aCaseClass must beACaseClassThatHas(anIdThatIs = equalTo(4))
      aCaseClass must beACaseClassThatHas(aTextThatIs = equalTo(":)"))
    }

    "sometimes aka is kaka" in new Context {
      def beACaseClassWithText(text: String): Matcher[ACaseClass] = {
        beEqualTo(text) ^^ {(_:ACaseClass).text/* aka "text"*/}
      }

      ACaseClass(4, "world") must beACaseClassWithText("world")
    }

    "so what is ^^^?" in new Context {
      def beMostlyEqualTo =
        (be_==(_:ACaseClass)) ^^^ ((_:ACaseClass).copy(id = 0))
      val caseClass1 = ACaseClass(2, "foo")
      val caseClass2 = ACaseClass(3, "foo")

      caseClass1 must beMostlyEqualTo(caseClass2)
    }

    "and if we just want to talk scala" in new Context {
      def startWithText(text: String): Matcher[String] = { s: String =>
        (
          s.startsWith(text) /*the check (boolean)*/,
          s+ s" starts with $text" /*the success message*/,
          s+ s" doesn't start with $text" /*the failure message*/
        )
      }

      "matchers are awesome" must startWithText("matchers are")
//      "matchers are awesome" must startWithText("bobby")
//      "matchers are awesome" must not(startWithText("matchers are"))
    }
  }
}

case class ACaseClass(id: Int, text: String) {
  def throwException(shouldThrow: Boolean): Unit =
    if (shouldThrow) throw new RuntimeException()

  def aNothing(): Nothing = throw new RuntimeException()
}
