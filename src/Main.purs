module Main where


import Prelude

import Effect.Class (class MonadEffect)
import Effect (Effect)
import Effect.Aff (launchAff)
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


shrekUrl = "https://am21.mediaite.com/tms/cnt/uploads/2021/02/shrek.jpg"
main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    halogenIO <- runUI component unit body
    H.liftEffect do
      matches myRoute \_ newRoute -> void $ launchAff $ halogenIO.query $ H.mkTell (SetRoute newRoute)

data Action = ChangeURL String

type State = { route :: MyRoute }


topBar = HH.div [ HP.class_ $ HH.ClassName "top-bar" ] [HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> ChangeURL "/home"
                    ] [ HH.text "Home" ]
        , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> ChangeURL "/about"
                    ] [ HH.text "About" ]
        , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> ChangeURL "/blog"
                    ] [ HH.text "Blog" ]
         ]


homeHtml = HH.div [] [HH.img [HP.src shrekUrl], HH.text "welcome to the home page"]
aboutHtml = HH.text "Go away"


data Query a = SetRoute MyRoute a

component :: forall input output m. MonadEffect m => H.Component Query input output m
component =
  H.mkComponent { initialState , render , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery } }
  where
    initialState _ = { route: Home }

    render :: forall m. State -> H.ComponentHTML Action () m
    render state =
      let route = state.route in
      HH.div_
        [ topBar,
          HH.div [HP.class_ $ HH.ClassName "content"] [ HH.text "Content\n", case route of
                      Home -> homeHtml
                      About -> aboutHtml
                      Blog n -> HH.text $ "This is my blog, on post " <>  show n
                      BlogIndex -> HH.text $ "Welcome to the blog home"
                      _ -> HH.text "Well, I didn't implement this one yet"
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
  | BlogIndex
  | NotFound

myRoute :: Match MyRoute
myRoute =
  let normal = root *> oneOf [ lit "home" *> pure Home , lit "about" *> pure About , Blog <$> (lit "blog" *> int), lit "blog" *> pure BlogIndex] <* end in
  normal <|> pure NotFound
