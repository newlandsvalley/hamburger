module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLElement (toNode)
import Navigation.Route (routeCodec)
import Navigation.Router as Router
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import AppM (Env, runAppM)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    bodyNode = toNode body
    

  -- Halogen only deals in Aff at the top level. We have to hoist our monad
  -- (which only adds Navigation to Aff) into Aff so Halogen can deal with it
  let
    baseURL = "foo"
    -- the basic environment
    environment :: Env
    environment = { baseURL }


    rootComponent :: H.Component Router.Query Router.Input Void Aff
    rootComponent = H.hoist (runAppM environment) Router.component

  halogenIO <- runUI rootComponent {} body

  -- The app is running. All that's left is to notify the router
  -- any time the location changes in the URL.
  --
  -- We're using hash-based routing, so we'll use the `matchesWith` function from `Routing.Hash` to
  -- listen for changes in the hash and parse the result (using our routing codec, `routeCodec`,
  -- along with the `parse` function from `Routing.Duplex`). Any time we parse a new location we'll
  -- trigger a `Navigate` query in the router.
  --
  -- See:
  --
  -- https://github.com/slamdata/purescript-routing/blob/v8.0.0/GUIDE.md
  -- https://github.com/natefaubion/purescript-routing-duplex/blob/v0.2.0/README.md
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ do 
        _response <- halogenIO.query $ H.mkTell $ Router.Navigate new
        pure unit