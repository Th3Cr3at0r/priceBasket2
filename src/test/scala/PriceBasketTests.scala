
import org.scalatest.funsuite.AnyFunSuite
import org.junit.Test
import org.scalatest.BeforeAndAfter

import scala.List
import scala.collection.immutable
import scala.collection.immutable.Nil.:::
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}
/** Contains all of the implementation tests to running the basket application
 *
 */
class PriceBasketTests extends AnyFunSuite with BeforeAndAfter {

  val soup2 = new Item("soup", 0.65, 0)
  val apples = new Item("apples", 1.00, .1)
  val milk = new Item("milk", 1.30, 0)
  val soup = new Item("soup", 0.65, 0)
  val bread = new Item("bread", 0.80, 0)
  val item4=new Item("soup", 0.65, 0)

  test("An instance of Item should never be null") {
    val item = new Item("apple", 1, .1)
    assert(item!=null)
  }
  test("The application should permit for an item to be added to the basket") {
    val items = List(apples)
    val basket = new Basket(items)
    assert(basket.items.size == 1)
  }
  test("The application should permit for multiple items to be added to the basket") {
    val items=List(apples,milk)
    val basket=new Basket(items)
    assert(basket.items.size==2)
  }

  test("The application should permit for an item to be added to the basket after a basket has been created") {
    val items = List(apples, milk)
    val basket = new Basket(items)
    val item=new Item("soup",0.65,0.5)
    basket.add(item)
    assert(basket.items.size == 3)
  }
  test("should allow for simple discounts") {
    val soup2 = new Item("soup", 0.65, 0)
    val apples = new Item("apples", 1.00, .1)
    val milk = new Item("milk", 1.30, 0)
    val soup = new Item("soup", 0.65, 0)
    val bread = new Item("bread", 0.80, 0)
    val item4 = new Item("soup", 0.65, 0)

    val items = List(apples, milk)
    val basket = new Basket(items)
    basket.add(soup2)
    val newTotal = basket.applyDiscount()
    assert(newTotal == 2.85)

  }
  test("should allow for grouped item discounts") {
    //item,item3,bread
    val items = List(soup2,soup,bread)
    val basket = new Basket(items)
  //  basket.add(item)
    val newTotal = basket.applyDiscount()
    assert(newTotal == 1.7)
  }
  test("should show no discount") {
    //item,item3,bread
    val items = List(milk, soup, bread)
    val basket = new Basket(items)
    //  basket.add(item)
    val newTotal = basket.applyDiscount()
  }

  test("should show the same results as the pdf document for Apples Milk Bread") {
    //item,item3,bread
    val items = List(apples, milk, bread)
    val basket = new Basket(items)
    //  basket.add(item)
    val newTotal = basket.applyDiscount()
    assert(newTotal == 3)


  }
  test ("should allow for multiple item to be added to tthe basket after a single item has been added") {
    val item = new Item("soup", 0.65, 0.5)
    val items = List(apples, milk)
    val basket = new Basket(List(item))
    basket.add(items)

    assert(basket.items.size == 3)

  }

  test("should allow creation from an empty list") {
    val basket = new Basket(List())
    basket.add(soup2)

    assert(basket.items.size == 1)

  }

}
/** Class for maintaining data about items
 *
 */
case class Item(name: String, var price: Double, discount: Double){}
/** Class for maintaining data about baskets
 *
 */
case class Basket(item2: List[Item]) {
  var output = ""
  var hasNoDiscount=true
  //var items=  Map.apply("1" -> new Item("apple",1.0,.1))
  var items: ListBuffer[Item] = item2.to(ListBuffer)
  var discountItems: ListBuffer[Item]=ListBuffer()

  /** Applies a discount to the basket if needed
   *@return The new total after the discount has been applied
   */
  def applyDiscount(): Double = {
    //output+="{\n"
    //items.foreach(tempItem=>items.contains(tempItem)
    //Total before discount
    var totalBeforeDiscounts = 0.0
    for (itemX <- items) {
      totalBeforeDiscounts = totalBeforeDiscounts + itemX.price
    }
    totalBeforeDiscounts="%06.2f".format(totalBeforeDiscounts)  .toDouble
    //println(totalBeforeDiscounts+"totalBeforeDiscounts"+items+"items")
    var tempItems = items.clone()
    var tempItemsPrevious = items.clone()
    val tempItems2 = items.filter(_.name != "soup")
    tempItems = items.filter(_.name == "soup")
    val tempItems3 =items.clone()
    //Checking if the basket has at list two soups
    var hasAtLeastTwoSoups=false
    //applying their simple discounts
    for(tempItem3<-items){
      //print(tempItem3+( tempItem3.price+"item price"+(1 - tempItem3.discount))+"Discount:") // Checking that the application is getting the priceses and discount amounts correct
      //write the logic to update the prices
      if( tempItem3.discount!=0){
        output+=tempItem3.name+":item has a "+tempItem3.discount*100+"% discount\n"
        hasNoDiscount=false
        tempItem3.price = tempItem3.price * (1 - tempItem3.discount)
      }
      //tempItem3.price = tempItem3.price * (1 - tempItem3.discount)
    }
    // Checking if there is two soups
    for(itemx<- tempItems3){
      if(itemx.name.equals("soup")){
        tempItemsPrevious -= itemx
        for (itemx <- tempItemsPrevious) {
          if (itemx.name.equals("soup")) {
            hasAtLeastTwoSoups=true
            //println(items+":baskets with two soups")
          }
        }

      }
    }

    var hasABread = false
    val numberOfDisccountedBreads = (tempItems.size / 2).floor.toInt

    //println(numberOfDisccountedBreads+":numberOfDisccountedBreads")
    //Applying grouped discount for soups and bread
    for (tempItem2 <- tempItems2) {
      if (tempItem2.name.equals("bread")) {
        //println("This basket has bread")
        hasABread = true
        breakable{
          break
        }

      }
    }
    //If a basket has bread and at least two soups then apply the special discount
    if (hasABread&&hasAtLeastTwoSoups) {
      //Remove all breads since we will update the backet with new discounted breads
      for(itemX<-tempItemsPrevious){
        if(itemX.name.equals("bread")){
          items -= itemX
        }
      }
      val range = 1 to  numberOfDisccountedBreads
      var discountedAmount = 0
      println()
      for (num <- range) {
        val discountedItem = new Item("bread", 0.40, 0)
        //remove the original bread
        add(discountedItem)
        //println(discountedItem+":Discounted item")
        //items=items.filter(_.name)
        //discountedAmount += discountedItem.price
      }
      output+="applied "+numberOfDisccountedBreads+" breads for discount\n"
      hasNoDiscount=false
    }
    // Noting whether this basket has any discounts
    if(hasNoDiscount){
      output="(No offers available)"
    }
    // getting the total
    var total: Double = 0.0
    //println(items+":items before final total")
    for (itemX2 <- items) {
      total = total + itemX2.price
      if(itemX2.name.equals("bread")) {
        //println(itemX2+":Item"+itemX2.price+":itemX.price"+total+"new total")
      }

    }
    //Producing the total
    val totalString=total+""
    total="%06.2f".format(total).toDouble
    val output2="{\nSubtotal:"+totalBeforeDiscounts+"\n"+output+"\nTotal price:"+total+"\n}"+items
    println(output2)
    total
  }

  /** Adds an items to the basket
   *
   * @param items the items that should be added to the basket
   */
  def add( items: List[Item] ): Unit = {
    val items2 =items.to(ListBuffer)
    items2.foreach(x=>add(x))
  }

  /** Adds a single item to the basket
   *
   * @param items the items that should be added to the basket
   */
  def add(item: Item): Unit = {
    //println(item+":: Items")
    val itemX= item
    val myVal= items ++ListBuffer(itemX)
    items= myVal
   // println(items+":: All Items")
  }

}