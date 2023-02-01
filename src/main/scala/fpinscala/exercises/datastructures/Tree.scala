package fpinscala.exercises.datastructures

import fpinscala.exercises.datastructures.Tree.Branch

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => 1 + l.depth.max(r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(a) => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  
  def sizeViaFold: Int =
    fold(_ => 1, _ + _ + 1)
  
  def depthViaFold: Int =
    fold(_ => 0, (l,r)=> 1 + (l max r))

  def mapViaFold[B](f: A => B): Tree[B] =
    fold(f.andThen(Leaf.apply), Branch(_, _))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int])
    def firstPositive: Int = t match
      case Leaf(value) => value
      case Branch(l, r) =>  if l.firstPositive > 0 then l.firstPositive else r.firstPositive

    def maximum: Int = t match
      case Leaf(value) => value
      case Branch(left, right) => left.maximum.max(right.maximum)

    def maximumViaFold: Int =
      t.fold(a => a, _ max _)
