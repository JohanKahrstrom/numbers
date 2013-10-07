package com.jk.numbers

import org.junit.runner.RunWith

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class NaturalNumberSuite extends FunSuite {
  trait TestNumbers {
    val zero = Zero
    val one = zero.successor
    val two = one.successor
    val three = two.successor
    val five = three.successor.successor
    val ten = zero.successor.successor.successor.successor.successor.successor.successor.successor.successor.successor
  }
  
  test("successor") {
    new TestNumbers {
      assert(Zero.successor == one)
      assert(Zero.successor.successor == two)
      assert(three.successor.successor == five)
    }
  }
  
  test("addition") {
    new TestNumbers {
      assert(zero + one == one)
      assert(two + three == five)
      assert(five + zero == five)
      assert(five + two != five)
      assert(five + five == ten)
    }
  }
  
  test("multiplication") {
    new TestNumbers {
      assert(zero * one == Zero)
      assert(one * five == five)
      assert(five * zero != five)
      assert(five * two == two * five)
      assert(five * two == ten)
    }
  }
  
  test("division") {
    new TestNumbers {
      assert(ten / five == two)
      assert(ten / three == three)
      assert(ten / two == five)
    }
  }
  
  test("toInt") {
    new TestNumbers {
      assert(ten.toInt == 10)
    }
  }
}