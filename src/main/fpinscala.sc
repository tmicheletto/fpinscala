import fpinscala.laziness._

Stream(1,2,3,4,5).takeViaUnfold(2).toList