{-# LANGUAGE OverloadedStrings #-}
import Data.Text.Lazy
import qualified Web.Scotty as Scotty
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text

import Shapes
import Render (render, renderColored,defaultWindow)

main = do 
    renderColored "drawing1.png" defaultWindow drawing1
    renderColored "drawing2.png" defaultWindow drawing2
    renderColored "drawing3.png" defaultWindow drawing3
    renderColored "drawing4.png" defaultWindow drawing4
    renderColored "drawing5.png" defaultWindow drawing5
    renderColored "drawing6.png" defaultWindow drawing6
    Scotty.scotty 3000 $ do
        Scotty.get "/" $ do
            Scotty.html $ response
        Scotty.get "/images/:image" $ do 
            image <- Scotty.param "image"
            Scotty.file ("./" ++ image ++ ".PNG")
        
response :: Text
response = do 
    renderHtml $ do
        Text.Blaze.Html5.style (".content { \n \
        \    max-width: 500px; \n\
        \    margin: auto; \n\
        \    text-align: center; \n\
        \} ")
        Text.Blaze.Html5.div ! class_ "content" $ do
            h1 ("Shape Server")
            h2 ("Image 1")
            img ! src "images/drawing1" ! width "50%"
            p "[(identity, circle, red)]"
            h2 ("Image 2")
            img ! src "images/drawing2" ! width "50%"
            p "[(identity, circle, red), (scale (point 2 2) <+> translate (point 5 5), square, blue), (translate (point (-3) 3), ellipse 16 9, green), (rotate 45 <+> translate (point 3 (-4)), rectangle 16 9, green)]"
            h2 ("Image 3")
            img ! src "images/drawing3" ! width "50%"
            p "\
            \[ \n \
            \(identity, convexPolygon [(point (-1) 5), (point 3 0), (point (-1) 0), (point (-1) 5)], yellow),\n \
            \(translate (point (-6) (-6)), convexPolygon [point (-2) 3.5, point 2 3.5, point 4 0, point 2 (-3.5), point (-2) (-3.5), point (-4) 0, point (-2) 3.5], pink),\n \
            \((translate (point 5 3)), convexPolygon [point (-6) 0, point (-2) 2, point 2 2, point 6 0, point (-6) 0], purple)\n \
            \]"
            h2 ("Image 4")
            img ! src "images/drawing4" ! width "50%"
            p "[ \
                \ (identity, ellipse 16 9, purple), \n \
                \((translate (point (-5) 0)), ellipse 9 16, orange), \n \
                \((rotate 5) <+> (translate (point 5 0)), ellipse 9 16, orange), \n \
                \((translate (point (5) 0)), ellipse 9 16, green), \n \
                \((rotate 5) <+> (translate (point (-5) 0)), ellipse 9 16, green) \n \
                \]"
            h2 ("Image 5")
            img ! src "images/drawing5" ! width "50%"
            p "mask 100 drawing4 drawing3"
            h2 ("Image 6")
            img ! src "images/drawing6" ! width "50%"
            p "mask 100 drawing5 drawing2"
        

drawing1 = [(identity, circle, red)]
drawing2 = [(identity, circle, red), (scale (point 2 2) <+> translate (point 5 5), square, blue), (translate (point (-3) 3), ellipse 16 9, green), (rotate 45 <+> translate (point 3 (-4)), rectangle 16 9, green)]      
drawing3 = [
    (identity, convexPolygon [(point (-1) 5), (point 3 0), (point (-1) 0), (point (-1) 5)], yellow),
    (translate (point (-6) (-6)), convexPolygon [point (-2) 3.5, point 2 3.5, point 4 0, point 2 (-3.5), point (-2) (-3.5), point (-4) 0, point (-2) 3.5], pink),
    ((translate (point 5 3)), convexPolygon [point (-6) 0, point (-2) 2, point 2 2, point 6 0, point (-6) 0], purple)
    ]
drawing4 = [
    (identity, ellipse 16 9, purple),
    ((translate (point (-5) 0)), ellipse 9 16, orange),
    ((rotate 5) <+> (translate (point 5 0)), ellipse 9 16, orange),
    ((translate (point (5) 0)), ellipse 9 16, green),
    ((rotate 5) <+> (translate (point (-5) 0)), ellipse 9 16, green)
    ]
drawing5 = mask 100 drawing4 drawing3 
drawing6 = mask 100 drawing5 drawing2
