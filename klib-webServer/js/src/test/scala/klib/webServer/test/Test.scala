package klib.webServer.test

import scalatags.JsDom.all._

import klib.Implicits._
import klib.fp.types._
import klib.webServer.CSS._

object Test {

  object S {

    object `primary-button` extends Block

    object centered extends Block

    object `nav-bar` extends Block {

      object section extends Element {
        object wrap extends Modifier
        object expand extends Modifier
      }

      object item extends Element

    }

    object `error-bar` extends Block {

      object error extends Element

    }

    object settings extends Block {

      object section extends Element

    }

    object `lesson-select` extends Block {

      object lesson extends Element

    }

    object `create-lesson` extends Block {

      object input extends Element

      object `text-area` extends Element

    }

    object `review-lesson` extends Block {

      object page extends Element
      object paragraph extends Element

    }

    object lesson extends Block {

      object page extends Element
      object paragraphs extends Element

    }

    object raw extends Block

    object word extends Block {

      object e extends Modifier
      object dne extends Modifier
      object `dont-mark` extends Modifier

      object unseen extends Modifier
      object `1` extends Modifier
      object `2` extends Modifier
      object `3` extends Modifier
      object `4` extends Modifier
      object known extends Modifier
      object ignored extends Modifier

    }

  }

  def main(args: Array[String]): Unit = {
    S.`primary-button`
  }

}
