module Main where

import Effect.Class (class MonadEffect)
import Effect (Effect)
import Effect.Aff (launchAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Routing.Hash (matches, setHash)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (*>), (<$>), (<*), (<>))
import Control.Alternative ((<|>))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (reverse, zip, (!!), (..))
import Data.Functor (map)
import Data.Tuple (Tuple(..))
import Routing.Match (Match, end, int, lit, root)
import Halogen.HTML.Properties as HP
import HomePage as HoPa
import Affjax.Web as Jx
import Affjax.ResponseFormat (string)
import Data.Ring ((-))

import BlogList (blogList)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  halogenIO <- runUI component unit body
  H.liftEffect do
    _ <- matches myRoute \_ newRoute -> void $ launchAff $ halogenIO.query $ H.mkTell (SetRoute newRoute)
    launchAff $ do
      resp <- Jx.get string "/blog_posts/"
      case resp of
         Right good -> halogenIO.query $ H.mkTell (SetRandom good.body)
         Left error  ->
           let message = case error of
                 Jx.RequestContentError _ -> "request error"
                 Jx.ResponseBodyError _ _ -> "request body"
                 Jx.TimeoutError -> "timeoout error"
                 Jx.RequestFailedError -> "erquest failed"
                 Jx.XHROtherError _ -> "wtf"
           in
           halogenIO.query $ H.mkTell (SetRandom ("we have bad news " <> message))

data Action = ChangeURL String

type State = { route :: MyRoute, randomInfo :: String}

topBar :: forall a. HH.HTML a Action
topBar = HH.div [ HP.class_ $ HH.ClassName "top-bar" ]
  [ HH.button
      [ HP.type_ HP.ButtonButton
      , HE.onClick \_ -> ChangeURL ""
      ]
      [ HH.text "Home" ]
  , HH.button
      [ HP.type_ HP.ButtonButton
      , HE.onClick \_ -> ChangeURL "/about"
      ]
      [ HH.text "About" ]
  , HH.button
      [ HP.type_ HP.ButtonButton
      , HE.onClick \_ -> ChangeURL "/blog"
      ]
      [ HH.text "Blog" ]
  ]

aboutHtml :: forall a b. HH.HTML a b
aboutHtml = HH.text "Go away"

data Query a = SetRoute MyRoute a | SetRandom String a

reversedBlog :: Array (Tuple (Tuple String String) Int)
reversedBlog = reverse $ zip blogList $ 1 .. 999 -- I don't know how to zip enumerate in haskell

sideBar :: forall x y. HH.HTML x y
sideBar = HH.div_ [
                  HH.div [HP.class_ $ HH.ClassName "blog_list" ] [
                   HH.ul_ $ map (\(Tuple (Tuple name _) index) -> HH.li_ [HH.a [HP.href $ "/#/blog/" <> show index] [ HH.text name]]) reversedBlog
                     ]
                  ]


component :: forall input output m. MonadEffect m => H.Component Query input output m
component =
  H.mkComponent { initialState, render, eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery } }
  where
  initialState _ = { route: Home, randomInfo: "initial test string" }

  -- render :: forall m. State -> H.ComponentHTML Action () m
  render state =
    let
      route = state.route
    in
      HH.div [ HP.class_ $ HH.ClassName "top_and_content" ]
        [ topBar
        , HH.div [ HP.class_ $ HH.ClassName "content" ]
            [ case route of
                Home -> HoPa.homeHtml
                About -> aboutHtml
                Blog n -> HH.div [HP.class_ $ HH.ClassName "side_and_main"] [sideBar,  case blogList !! ( n - 1 ) of
                                                                                                Just (Tuple _ page) -> HH.iframe [HP.src $ "/blog_posts/" <> page]
                                                                                                Nothing -> HH.text "Well, this page doesn't exist"

                                                                            ]
                BlogIndex -> sideBar

                _ -> HH.text "Well, I didn't implement this one yet"
            ]
        -- , HH.text state.randomInfo
          -- , HH.iframe [ HP.src "/blog_posts/my_first_post.html" ]
        ]

  handleQuery :: forall m1 o a. Query a -> H.HalogenM State Action () o m1 (Maybe a)
  handleQuery = case _ of
    SetRoute route a -> do
      H.modify_ (_ { route = route })
      pure (Just a)
    SetRandom randomInfo a -> do
      H.modify_ (_ { randomInfo = randomInfo })
      pure (Just a)

  -- handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    ChangeURL url -> H.liftEffect $ setHash url

type PostId = Int

data MyRoute
  = Home
  | About
  | Blog Int
  | BlogIndex
  | NotFound

myRoute :: Match MyRoute
myRoute =
  let
    normal = root *> oneOf [ lit "about" *> pure About, Blog <$> (lit "blog" *> int), lit "blog" *> pure BlogIndex
                           ] <* end
  in
    normal <|> ( end *> pure Home) <|> pure NotFound
