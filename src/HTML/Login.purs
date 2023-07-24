module HTML.Login where

import Halogen.HTML as HH

login :: forall i p. HH.HTML i p
login =
  HH.div_
    [ HH.h2_
        [ HH.text "This is the Login page" ]
    ]