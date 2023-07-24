module Navigation.Router where

import Prelude
import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff.Class (class MonadAff)
import HTML.Header (header)
import HTML.Home (home)
import HTML.Login (login)
import HTML.Register (register)
import Halogen as H
import Halogen.HTML as HH
import Navigation.Navigate (class Navigate, navigate)
import Navigation.Route (Route(..), routeCodec)
import Navigation.RouterAction (Action(..))
import Navigation.Toggle (resetHamburgerMenu, toggleHamburgerMenu)
import Routing.Duplex as RD
import Routing.Hash (getHash)


-- | the Base URL of the downstream server providing the API
type BaseURL = String


type State =
  { route :: Maybe Route  }

type Input = 
  { }

data Query a
  = Navigate Route a

type ChildSlots = ()

component ::
    ∀ m r 
    . MonadAff m
    => MonadAsk { baseURL :: BaseURL | r } m
    => Navigate m
    => H.Component Query Input Void m
component =
  H.mkComponent
    { initialState: \input -> { route: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        , handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
      -- and, finally, we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute

    ToggleHamburgerMenu -> do 
      _ <- H.liftEffect toggleHamburgerMenu
      pure unit


  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      state <- H.get
      -- don't re-render unnecessarily if the state is unchanged.
      when ( (state.route /= Just dest)) do
         -- once a route is chosen, ensure that the hamburger menu is not visible, but the hamburger is available
         _ <- H.liftEffect resetHamburgerMenu
         H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = do
    let
      -- the route we give to the header defaults to Home which we use
      -- simply to highlight the menu options appropriately
      headerRoute = maybe Home identity state.route
    HH.div_
        [  header headerRoute
        ,  renderRoute state
        ]

  -- | Note - links are not well-typed.  Sproxy names must also match the
  -- | child slot names AND the route codec initial URI name.
  renderRoute :: State -> H.ComponentHTML Action ChildSlots m
  renderRoute state =
      case state.route of
        Just r -> case r of
          Home ->
            home
          Login ->
            login
          Register ->
            register

        Nothing ->
          HH.div_ [ HH.text "Oh no! That page wasn't found." ]