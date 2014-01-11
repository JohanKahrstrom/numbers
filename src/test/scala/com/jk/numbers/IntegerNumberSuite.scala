package com.jk.numbers

import org.junit.runner.RunWith

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class IntegerNumberSuite extends FunSuite {
  trait TestNumbers {
    val zero = IntegerNumber(0)
    val one = IntegerNumber(1)
    val two = IntegerNumber(2)
    val three = IntegerNumber(3)
    val five = IntegerNumber(5)
    val nine = IntegerNumber(9)
    val ten = IntegerNumber(10)
    val minusOne = IntegerNumber(-1)
    val minusThree = IntegerNumber(-3)
    val minusFifteen = IntegerNumber(-15)
  }
  
  test("addition") {
    new TestNumbers {
      assert(zero + one == one)
      assert(two + three == five)
      assert(five + zero == five)
      assert(five + two != five)
      assert(five + five == ten)
      assert(one + minusOne == zero)
      assert(one - minusOne == two)
      assert(minusOne + minusOne + minusOne == minusThree)
    }
  }
  
  test("multiplication") {
    new TestNumbers {
      assert(zero * one == zero)
      assert(one * five == five)
      assert(five * zero != five)
      assert(five * two == two * five)
      assert(five * two == ten)
      assert(minusOne * minusOne == one)
      assert(five * minusThree == minusFifteen)
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