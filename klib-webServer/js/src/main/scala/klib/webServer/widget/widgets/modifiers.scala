package klib.webServer.widget.widgets

import klib.webServer.CSS._

trait modifiers {

  object KwsS extends StyleSheet {

    object `klws:label` extends Block
    object `klws:input` extends Block
    object `klws:text-area` extends Block

    object `kws:radio-group` extends Block {
      object option extends Element {
        object first extends Modifier
        object last extends Modifier
        object selected extends Modifier
      }
    }

  }

}
object modifiers extends modifiers
