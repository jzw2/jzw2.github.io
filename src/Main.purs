module Main where


import Prelude

import Effect.Class (class MonadEffect)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Routing.Hash (matches, setHash)
import Prelude
import Control.Alternative ((<|>))
import Data.Foldable (oneOf)
import Data.Maybe
import Effect.Class.Console
import Routing.Match (Match, lit, int, str, end, root)
import Halogen.HTML.Properties as HP

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    halogenIO <- runUI component unit body
    H.liftEffect do
      matches myRoute \_ newRoute -> launchAff_ $ halogenIO.query $ H.mkTell (SetRoute newRoute)

data Action = GoHome | GoAbout | OtherButton | ChangeURL String

type State = { route :: MyRoute }


topBar = HH.div [ HP.class_ $ HH.ClassName "top-bar" ] [HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> ChangeURL "/home"
                    ] [ HH.text "Home" ]
        , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> ChangeURL "/about"
                    ] [ HH.text "About" ]
        , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> ChangeURL "/blog"
                    ] [ HH.text "Blog" ]
         ]

data Query a = SetRoute MyRoute a

component :: forall input output m. MonadEffect m => H.Component Query input output m
component =
  H.mkComponent { initialState , render , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery } }
  where
    initialState _ = { route: Other }

    render :: forall m. State -> H.ComponentHTML Action () m
    render state =
      let route = state.route in
      HH.div_
        [ topBar,
          HH.div_ [ HH.text "Content", case route of
                      Home -> HH.text "This is my home page"
                      About -> HH.text "This is my about page"
                      Blog n -> HH.text $ "This is my blog, on post " <>  show n
                      _ -> HH.text "Well, I didn't implement this one yet"

                    ]
        , HH.div_ [ HH.a [ HP.href "#/home" ] [ HH.text "this is a something" ]
                    ]
        , HH.div_ [ HH.a [ HP.href "#/about" ] [ HH.text "this is a something else" ]
                    ]
        ]


    handleQuery :: forall m o a. Query a -> H.HalogenM State Action () o m (Maybe a)
    handleQuery = case _ of
      SetRoute route a -> do
        H.modify_ (_ { route = route })
        pure (Just a)

    handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
    handleAction = case _ of
      ChangeURL url -> H.liftEffect $ setHash url
      _ -> pure unit

type PostId = Int

data MyRoute
  = Home
  | About
  | Blog Int
  | Other

myRoute :: Match MyRoute
myRoute =
  root *> oneOf
    [ lit "home" *> pure Home
    , lit "about" *> pure About
    , Blog <$> (lit "blog" *> int)
    ] <* end

