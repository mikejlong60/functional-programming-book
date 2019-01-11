package chapter4

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scala.{Option => _, None => _, Some => _,  Either => _, _}


class OptionTest extends PropSpec with PropertyChecks with Matchers {

  property("Test Option map function for Ints") {
    forAll { x: Int =>
      val actual = Some(x).map(x => x + 1)
      val expected = Some(x + 1)
      actual should be (expected)
    }
  }

  property("Test Option flatmap function for Ints") {
    forAll { x: Int =>
      val actual = Some(x).flatMap(x => Some(x + 1))
      val expected = Some(x + 1)
      actual should be (expected)
    }
  }

  property("Test Option flatmap function for None") {
    forAll { x: Int =>
      val actual = None.flatMap(x => None)
      val expected = None
      actual should be (expected)
    }
  }

  property("Test Option map function for None") {
      val actual = None.map(x => 1)
      val expected = None
      actual should be (expected)
  }

  property("Test Option getOrElse function for None") {
    forAll { x: Int =>
      val actual = None.getOrElse(12)
      val expected = 12
      actual should be (expected)
    }
  }

  property("Test Option getOrElse function for Int") {
    forAll { x: Int =>
      val actual = Some(x).getOrElse(12)
      val expected = x
      actual should be (expected)
    }
  }

  property("Test Option orElse function for None") {
    forAll { x: Int =>
      val actual = None.orElse(Some(12))
      val expected = Some(12)
      actual should be (expected)
    }
  }

  property("Test Option orElse function for Int") {
    forAll { x: Int =>
      val actual = Some(x).orElse(Some(12))
      val expected = Some(x)
      actual should be (expected)
    }
  }

  property("Test Option filter function for None") {
    forAll { x: Int =>
      val actual = None.filter(x => true)//Some(12))
      val expected = None
      actual should be (expected)
    }
  }

  property("Test Option filter function for Int") {
    forAll { x: Int =>
      val actual = Some(x).filter(y => y == x)
      val expected = Some(x)
      actual should be (expected)
    }
  }

  property("Test Option not filter function for Int") {
    forAll { x: Int =>
      val actual = Some(x).filter(y => y != x)
      val expected = None
      actual should be (expected)
    }
  }

}

