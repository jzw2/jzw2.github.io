module HomePage where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

shrekUrl :: String
shrekUrl = "https://am21.mediaite.com/tms/cnt/uploads/2021/02/shrek.jpg"



homeHtml = HH.div [] [ HH.img [ HP.src shrekUrl ], HH.br_, HH.text "welcome to the home page", HH.h1_ [HH.text "Why does your website look like doggy doo doo"], HH.text "Sorry, I'm bad at making websites" ]
