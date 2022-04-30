module Main where


import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Routing.PushState (makeInterface, matches)
import Foreign (unsafeToForeign)
import Prelude
import Control.Alternative ((<|>))
import Data.Foldable (oneOf)

import Routing.Match (Match, lit, int, str, end, root)
main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

data Action = GoHome | GoAbout | OtherButton

data State = State MyRoute



component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = State Home

  render :: forall m. State -> H.ComponentHTML Action () m
  render state =
    let State route = state in
    HH.div_
      [ HH.button [  HE.onClick \_ -> GoHome
                  ] [ HH.text "Home" ]
      , HH.button [  HE.onClick \_ -> GoAbout
                  ] [ HH.text "About" ]
      , HH.button [  HE.onClick \_ -> OtherButton
                  ] [ HH.text "The is a big button that you should click on " ]
      , HH.div_ [ HH.text "Content", case route of
                     Home -> HH.text "This is my home page"
                     About -> HH.text "This is my about page"
                     _ -> HH.text "Well, I didn't implement this one yet"

                  ]
      ]
  handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    GoHome -> H.modify_ \state -> State Home
    GoAbout -> H.modify_ \state -> State About
    OtherButton -> pure unit

type PostId = Int

data MyRoute
  = Home
  | About
  | Blog Int

myRoute :: Match MyRoute
myRoute =
  root *> oneOf
    [ lit "home" *> pure Home
    , lit "about" *> pure About
    , Blog <$> (lit "blog" *> int)
    ] <* end

routingMain :: Effect Unit
routingMain = do
  nav <- makeInterface
  nav.pushState (unsafeToForeign {}) "blah"
  -- nav # matches myRoute \_ newRoute -> case newRoute of
  --   PostIndex -> nav.pushState (unsafeToForeign {}) "/about"
  --   Post postId -> pure unit
  --   PostEdit postId -> pure unit
  --   PostBrowse year month -> pure unit
