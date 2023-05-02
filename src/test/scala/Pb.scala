object Pb {
  def main(args: Array[String]): Unit = {
    val basket= Basket(List())
    for(argX<-args){
      val arg=argX.toLowerCase().trim
      if(arg.equals("soup")){
        basket.add(new Item("soup",0.65,0))
      }else if(arg.equals("bread")){
        basket.add(new Item("bread",0.80,0))
      } else if (arg.equals("milk")) {
        basket.add(new Item("milk",1.30,0))
      }
      else if (arg.equals("apples")) {
        basket.add(new Item("apples",1.0,0.1))
      }
    }
    basket.applyDiscount()
  }
}
