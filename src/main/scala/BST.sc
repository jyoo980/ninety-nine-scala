import scala.annotation.tailrec

// Good for modelling immutable data structures
case class Node(key: Int, var left: Option[Node] = None, var right: Option[Node] = None)

class BinarySearchTree {
  var root: Option[Node] = None
  var left: Option[Node] = None
  var right: Option[Node] = None

  def has(i: Int): Boolean = {
    @tailrec
    def _has(target: Int, currNode: Option[Node]): Boolean = currNode match {
      case None => false
      case Some(node) if node.key == target => true
      case Some(node) if target < node.key  => _has(target, node.left)
      case Some(node)                       => _has(target, node.right)
    }
    _has(i, this.root)
  }

  def inOrder(root: Option[Node]): Unit = root match {
    case None =>
    case Some(node) =>
      inOrder(node.left)
      println("Node value: " + node.key)
      inOrder(node.right)
  }

  def preOrder(root: Option[Node]): Unit = root match {
    case None =>
    case Some(node) =>
      println("Node value: " + node.key)
      preOrder(node.left)
      preOrder(node.right)
  }

  def postOrder(root: Option[Node]): Unit = root match {
    case None =>
    case Some(node) =>
      postOrder(node.left)
      postOrder(node.right)
      println("Node value: " + node.key)
  }

}

