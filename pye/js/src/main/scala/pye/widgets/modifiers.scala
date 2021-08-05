package pye.widgets

import pye.CSS._

trait modifiers {

  object PyeS extends StyleSheet {

    object `pye:label` extends Block
    object `pye:input` extends Block
    object `pye:text-area` extends Block

    object `pye:radio-group` extends Block {
      object option extends Element {
        object first extends Modifier
        object last extends Modifier
        object selected extends Modifier
      }
    }

    object message extends Block {
      object info extends Modifier
      object error extends Modifier
    }

  }

}
object modifiers extends modifiers
