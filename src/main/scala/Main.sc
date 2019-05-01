import scala.annotation.tailrec
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

// Write a tail-recursive implementation of a function which
// generates the nth fibonacci number
def fib(n: Int): Int = {
  @tailrec
  def fibHelper(n: Int, prev: Int, curr: Int): Int =
    if (n <= 0) prev
    else fibHelper(n - 1, curr, prev + curr)
   fibHelper(n, 0, 1)
}

// Write a tail-recursive function which determines whether a list of
// Integers are sorted
def isSorted(lst: List[Int], ordering: (Int, Int) => Boolean): Boolean = {
  @tailrec
  def innerSortCheck(n: Int): Boolean =
    if (n >= lst.length - 1) true
    else if (ordering(lst(n), lst(n + 1))) false
    else innerSortCheck(n + 1)
  innerSortCheck(0)
}

// Implement filter
def filter[A](lst: List[A], predicate: A => Boolean): List[A] = {
  @tailrec
  def innerFilter(lst: List[A], result: List[A]): List[A] =
    if (lst.isEmpty) result
    else if (predicate(lst.head)) innerFilter(lst.tail, result ::: List(lst.head))
    else innerFilter(lst.tail, result)
  innerFilter(lst, Nil)
}

// Implement map
def map[A, B](lst: List[A], fun: A => B): List[B] = {
  @tailrec
  def innerMap(lst: List[A], result: List[B]): List[B] =
    if (lst.isEmpty) result
    else innerMap(lst.tail, result ::: List(fun(lst.head)))
  innerMap(lst, Nil)
}

// Implement foldr
def foldr[A, B](fun: (A, B) => B, base: B, lst: List[A]): B = {
  if (lst.isEmpty) base
  else fun(lst.head, foldr(fun, base, lst.tail))
}

// Implement foldl
// Basically tail-recursive foldr
def foldl[A, B](fun: (A, B) => B, base: B, lst: List[A]): B = {
  @tailrec
  def fold(lst: List[A], result: B): B =
    if (lst.isEmpty) result
    else fold(lst.tail, fun(lst.head, result))
  fold(lst, base)
}

// Implement andMap
def andMap[A](lst: List[A], pred: A => Boolean): Boolean = {
  @tailrec
  def innerAndMap(lst: List[A], result: Boolean): Boolean =
    if (lst.isEmpty) result
    else innerAndMap(lst.tail, pred(lst.head) && result)
  innerAndMap(lst, result=true)
}

// Implement orMap
def orMap[A](lst: List[A], pred: A => Boolean): Boolean = {
  @tailrec
  def innerOrMap(lst: List[A], result: Boolean): Boolean =
    if (lst.isEmpty) result
    else innerOrMap(lst.tail, pred(lst.head) || result)
  innerOrMap(lst, result=false)
}