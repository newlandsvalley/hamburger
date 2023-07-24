module HTML.Home where

import Halogen.HTML as HH

home :: forall i p. HH.HTML i p
home =
  HH.div_
    [ HH.h2_
        [ HH.text "This is the Home page" ]
    ]