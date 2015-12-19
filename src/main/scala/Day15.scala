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


  def getIngredientScore(ingredients: Seq[(Int, Ingredient)]) = {
    val capacity = ingredients.map { case (amount, ing) => amount * ing.capacity }.sum
    val durability = ingredients.map { case (amount, ing) => amount * ing.durability }.sum
    val flavor = ingredients.map { case (amount, ing) => amount * ing.flavor }.sum
    val texture = ingredients.map { case (amount, ing) => amount * ing.texture }.sum

    if (capacity < 0 || durability < 0 || flavor < 0 || texture < 0)
      0
    else
      capacity * durability * flavor * texture
  }

  assert(
    getIngredientScore(
      Seq(
        (44, Ingredient("Butterscotch", -1, -2, 6, 3, 8)),
        (56, Ingredient("Cinnamon", 2, 3, -2, -1, 3))
      )
    ) == 62842880
  )


  val answer1 = splitFourWays(100)
    .map(quantities => quantities.zip(inputIngredients)) // List[(num, Ingredient)]
    .map(getIngredientScore)
    .max


  println("part1", answer1);

}