package datastructures

import org.scalatest.{BeforeAndAfter, GivenWhenThen, FunSpec}

class Tree$Test extends FunSpec with GivenWhenThen with BeforeAndAfter {

  describe("Getting the size of a tree ...") {
    it("Should return correct size") {
      Given("a non-empty tree")
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

      When("size is invoked")
      val size = Tree.size(t)

      Then("the size should be correct")
      assert(size === 7)
    }
  }

  describe("max") {
    it("Should return the highest element") {
      Given("a non-empty tree")
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

      When("max is invoked")
      val max = Tree.max(t)

      Then("the max should be correct")
      assert(max === 4)
    }
  }

  describe("depth") {
    it("Should return the maximum path length from the root to of the tree to any leaf") {
      Given("a non-empty tree")
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))

      When("depth is invoked")
      val depth = Tree.depth(t)

      Then("the depth should be correct")
      assert(depth === 3)
    }
  }

  describe("map") {
    it("Should do a map on the leaves of the tree") {
      Given("a non-empty tree")
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

      When("map is invoked with a function that add one to every element")
      val tt = Tree.map(t)(_ + 1)

      Then("every leaves should be increased by 1")
      assert(tt === Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5))))
    }
  }

  describe("Getting the size_2 of a tree ...") {
    it("Should return correct size") {
      Given("a non-empty tree")
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

      When("size is invoked")
      val size = Tree.size_2(t)

      Then("the size should be correct")
      assert(size === 7)
    }
  }

  describe("max_2") {
    it("Should return the highest element") {
      Given("a non-empty tree")
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

      When("max is invoked")
      val max = Tree.max_2(t)

      Then("the max should be correct")
      assert(max === 4)
    }
  }

  describe("depth_2") {
    it("Should return the maximum path length from the root to of the tree to any leaf") {
      Given("a non-empty tree")
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))

      When("depth is invoked")
      val depth = Tree.depth_2(t)

      Then("the depth should be correct")
      assert(depth === 3)
    }
  }

  describe("map_2") {
    it("Should do a map on the leaves of the tree") {
      Given("a non-empty tree")
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

      When("map is invoked with a function that add one to every element")
      val tt = Tree.map_2(t)(_ + 1)

      Then("every leaves should be increased by 1")
      assert(tt === Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5))))
    }
  }

  describe("fold") {
    it("Should apply a function and accumulate") {
      Given("a non-empty tree")
      val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

      When("fold is invoked with a function that sum 2 elements")
      val sum = Tree.fold(t)(_ + 1)(_ + _)

      Then("we get the correct result")
      assert(sum === 14)
    }
  }
}
