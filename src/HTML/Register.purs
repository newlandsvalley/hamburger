module HTML.Register where

import Halogen.HTML as HH

register :: forall i p. HH.HTML i p
register =
  HH.div_
    [ HH.h2_
        [ HH.text "This is the Register page" ]
    ]
