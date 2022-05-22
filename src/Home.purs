
module HomePage where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
shrekUrl = "https://am21.mediaite.com/tms/cnt/uploads/2021/02/shrek.jpg"
homeHtml = HH.div [] [HH.img [HP.src shrekUrl], HH.text "welcome to the home page"]
