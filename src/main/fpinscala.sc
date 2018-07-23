import fpinscala.state.State.Candy
import fpinscala.state._

val s = Candy.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
s.run(Machine(true, 5, 10))
