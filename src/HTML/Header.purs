module HTML.Header where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes.Color (Color(..))
import Navigation.Route (Route(..), routeCodec)
import Navigation.RouterAction (Action(..))
import Routing.Duplex (print)


header :: forall m. Route -> H.ComponentHTML Action () m
header route =
  HH.div_
      [
      HH.nav 
        [ css "nav"]
        [ HH.ul
          [ css "main-menu"]
          -- left hand large screen navigation menu
          [ HH.div
            [ css "navdiv"]
            [ navItem Home
            ]
            -- right hand large screen navigation menu
          , HH.div
            [ css "navdiv"]
            [ navItem Login
            , navItem Register
            ]
          ]
          -- hamburger navigation - small screens only
          , HH.ul 
            [ css "hamburger-nav"]
            -- hamburger app name
            [ HH.li
               [ css "hamburger-appname"]
               [ HH.text "hamburger test app"]
               -- hamburger icon
            , HH.li 
              [ css "hamburger-icon"] 
              [ HH.div 
                  [ css "hamburger-btn"
                  , HP.id "hamburger-btn" 
                  ]
                  [ HH.div
                    -- the hamburger lines indicate that the menu can be switched on
                    [ HE.onClick \_ -> ToggleHamburgerMenu ]
                    [ SE.svg 
                      [ SA.viewBox 0.0 0.0 80.0 80.0 
                      , SA.width 40.0 
                      , SA.height 20.0
                      , svgcss "visible"
                      , SA.id "hamburger-lines"
                      ]
                      [ SE.rect
                        [ SA.width 80.0 
                        , SA.height 10.0
                        , SA.fill white
                        ]
                      , SE.rect
                        [ SA.width 80.0 
                        , SA.height 10.0
                        , SA.fill white
                        , SA.y 30.0
                        ]
                      , SE.rect
                        [ SA.width 80.0 
                        , SA.height 10.0
                        , SA.fill white
                        , SA.y 60.0
                        ]
                      ]
                    ]                  
                    , HH.div
                    -- the hamburger cross indicate that the menu can be switched off
                      [ HE.onClick \_ -> ToggleHamburgerMenu]
                      [ SE.svg 
                        [ SA.viewBox 0.0 0.0 100.0 100.0 
                        , SA.width 40.0 
                        , SA.height 20.0
                        , svgcss "hidden"
                        , SA.id "hamburger-cross"
                        ]
                        [ SE.line
                          [ SA.x1 10.0 
                          , SA.x2 80.0
                          , SA.y1 10.0
                          , SA.y2 80.0
                          , SA.stroke white
                          , SA.strokeWidth 10.0
                          ]
                        , SE.line
                          [ SA.x1 80.0 
                          , SA.x2 10.0
                          , SA.y1 10.0
                          , SA.y2 80.0
                          , SA.stroke white
                          , SA.strokeWidth 10.0
                          ]
                        ]
                    ]
                  ]
                ]                  
              ]
            ]
          -- hamburger menu pops up when selected by the icom and disappears when deselected
          , HH.div 
             [ css "hamburger-menu"
             , HP.id "hamburger-menu"
             ]
             [ HH.ul 
               []
               [ 
                 mobnavItem Home
               , mobnavItem Login
               , mobnavItem Register
               ]
             ]
             
         ]

  where

  navItem :: forall i p. Route -> HH.HTML i p
  navItem r =
    item r "navitem"  

  mobnavItem :: forall i p. Route -> HH.HTML i p
  mobnavItem r =
    item r "mobnavitem"  

  -- | a navigation item available at any time
  item :: forall i p. Route -> String -> HH.HTML i p
  item r cssSelector =
      HH.li
        [ css cssSelector ]
        [ HH.a
          [ css "current"
          , safeHref r
          ]  
          [ HH.text (show r) ]
        ]


  -- | I get annoyed writing `class_ $ ClassName "..."` over and over again. This small utility saves
  -- | a few characters all over our HTML.
  css :: forall r i. String -> HH.IProp ( class :: String | r ) i
  css = HP.class_ <<< HH.ClassName   


  -- ditto for the SVG element
  svgcss :: forall r i. String -> HH.IProp ( class :: String | r ) i
  svgcss = SA.class_ <<< HH.ClassName   

  -- | We must provide a `String` to the "href" attribute, but we represent routes with the much
  -- | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.
  safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
  safeHref = HP.href <<< append "#" <<< print routeCodec

  white :: Color
  white = 
    RGB 255 255 255
      