object Day15 extends App {

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  val parseLine: String => Ingredient = line => {
    val regex = "(.*): capacity (.*), durability (.*), flavor (.*), texture (.*), calories (.*)".r
    line match {
      case regex(name, capacity, durability, flavor, texture, calories) => Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
      case _ => throw new Exception("failed")
    }
  }


  val inputIngredients =
    scala.io.Source.fromFile("src/input/Day15.txt")
    .mkString
    .split("\n")
    .toList
    .map(parseLine)


  def splitFourWays(i: Int) = for {
    a <- 0 to i
    b <- 0 to i - a
    c <- 0 to i - (a + b)
    d = i - (a + b + c)
  } yield Seq(a, b, c, d)


  def splitNWays(n :Int)(i: Int) = {

  }


  def getIngredientScore(ingredients: Seq[(Int, Ingredient)]) = {
    val capacity = ingredients.map { case (amount, ing) => amount * ing.capacity }.sum
    val durability = ingredients.map { case (amount, ing) => amount * ing.durability }.sum
    val flavor = ingredients.map { case (amount, ing) => amount * ing.flavor }.sum
    val texture = ingredients.map { case (amount, ing) => amount * ing.texture }.sum

    println(ingredients)
    println(capacity, durability, flavor, texture)

    capacity * durability * flavor * texture
  }

  val answer1 = splitFourWays(100)
    .slice(11, 12)
    .map(quantities => quantities.zip(inputIngredients)) // List[(num, Ingredient)]
    .map(getIngredientScore)
    .max


  println(answer1)

}