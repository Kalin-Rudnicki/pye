package pye.widgets

import pye.CSS._

trait modifiers {

  object PyeS extends StyleSheet {

    object `pye:nav-bar` extends Block {

      object section extends Element {
        object wrap extends Modifier
        object expand extends Modifier
      }

      object item extends Element

    }

    object `pye:label` extends Block
    object `pye:input` extends Block
    object `pye:text-area` extends Block
    object `pye:file-input` extends Block {
      object file extends Element

      object `dragged-into` extends Modifier
    }

    object `pye:toggle-button` extends Block {
      object `true` extends Modifier
      object `false` extends Modifier
    }

    object `pye:radio-group` extends Block {
      object option extends Element {
        object first extends Modifier
        object last extends Modifier
        object selected extends Modifier
      }
    }

    object `pye:form-button` extends Block

    object message extends Block {
      object info extends Modifier
      object error extends Modifier
    }

  }

}
object modifiers extends modifiers
