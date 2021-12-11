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

    object `pye:date-picker` extends Block {
      object selected extends Element
      object editing extends Element
      object `editing-header` extends Element
      object `editing-under-header` extends Element
      object `year-month` extends Element
      object `arrow-button` extends Element
      object `day-of-week` extends Element
      object `day-button` extends Element {
        object `in-month` extends Modifier
        object selected extends Modifier
      }
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

    object `pye:modal` extends Block {

      object container extends Element

    }

    object `key-map-modal` extends Block {

      object section extends Element
      object table extends Element
      object header extends Element
      object text extends Element

    }

    object `pye:message` extends Block {
      object info extends Modifier
      object error extends Modifier
    }

  }

}
object modifiers extends modifiers
