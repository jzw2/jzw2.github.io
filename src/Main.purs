module Main where


import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = GoHome | GoProject

data State = Home | Project



component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = Home

  render state =
    HH.div_
      [ HH.button [  HE.onClick \_ -> GoHome
                  ] [ HH.text "Home" ]
      , HH.button [  HE.onClick \_ -> GoProject
                  ] [ HH.text "My Project" ]
      , HH.div_ [ HH.text "Content", case state of
                     Home -> HH.text "This is my home page"
                     Project -> HH.text "This is my project  page"

                  ]
      ]

  handleAction = case _ of
    GoHome -> H.modify_ \state -> Home
    GoProject -> H.modify_ \state -> Project
