// Ninety-Nine Scala Problems

// P01: find the last element of a list
//      assume list has at least one element
def last[A](lox: List[A]): A = {
  val len = lox.length
  len match {
    case 1 => lox.head
    case _ => lox(len - 1)
  }
}

// P02: find the last but one element of a list
def lastButOne[A](lox: List[A]): A = {
  val len = lox.length
  len match {
    case 1 => throw new NoSuchElementException
    case _ => lox(len - 2)
  }
}

// P04: find the number of elements in a list
def length[A](lox: List[A]): Int = {
  if (lox.isEmpty) return 0
  1 + length(lox.tail)
}

// P05: reverse a list
def reverse[A](lox: List[A]): List[A] = {
  lox match {
    case Nil => Nil
    case x :: tail => reverse(tail) :+ x
  }
}

// P22: create a list containing all integers within a given range
//      e.g. range(1,10) = [1..10]
//      Assume that start <= end
def range(start: Int, end: Int): List[Int] = {
  (start to end).toList
}

// P46: Define functions and, or, nand, nor, xor, impl, and equ
// (for logical equivalence) which return true or false according
// to the result of their respective operations;
// e.g. and(A, B) is true if and only if both A and B are true.
def and(b1: Boolean, b2: Boolean): Boolean = (b1, b2) match {
  case (true, true) => true
  case (_, _)       => false
}

def not(b: Boolean): Boolean = b match {
  case true => false
  case _    => true
}

def or(b1: Boolean, b2: Boolean): Boolean = (b1, b2) match {
  case (true, _) => true
  case (_, true) => true
  case (_, _)    => false
}

def nand(b1: Boolean, b2: Boolean): Boolean = not(and(b1, b2))
def nor(b1: Boolean, b2: Boolean): Boolean = not(or(b1, b2))
def xor(b1: Boolean, b2: Boolean): Boolean = not(or(b1, b2))
def equ(b1: Boolean, b2: Boolean): Boolean = and(and(b1, b2), and(not(b1), not(b2)))
def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)

def table2(fun: (Boolean, Boolean) => Boolean) = {
  println("A     B     fun(A, B)")
  for { a <- List(true, false); b <- List(true, false) } {
    print("%-5s %-5s %-5s\n", a, b, fun(a, b))
  }
}
