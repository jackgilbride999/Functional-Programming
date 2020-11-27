{-# LANGUAGE OverloadedStrings #-}
import Data.Text.Lazy
import qualified Web.Scotty as Scotty
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text

import Shapes
import Render (render, renderColored,defaultWindow)

main = do 
    render "output.png" defaultWindow exampleDrawing
    render "mandelbrot.png" defaultWindow mandel
    renderColored "rectangle.png" defaultWindow colorDrawing
    renderColored "rectangle.png" defaultWindow maskedDrawing
    Scotty.scotty 3000 $ do
        Scotty.get "/" $ do
            Scotty.html $ response
        Scotty.get "/output.png" $ do 
            Scotty.file "./rectangle.PNG"
        
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
            img ! src "output.png" ! width "50%"
            p " [ (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle) ]"
        
        



exampleDrawing =  [ (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle) ]
mandel = mandelbrotDrawing (-5, -5) (5, 5)
rectangleDrawing = [
   (scale (point 0.2 0.2) <+> translate (point 0 0) , rectangle 16.0 9.0),
    (scale (point 0.2 0.2) <+> translate (point 5 0), square),
    (scale (point 0.2 0.2) <+> translate (point 0 5), ellipse 9.0 16.0),
    (scale (point 0.2 0.2) <+> translate (point (-5) 0), convexPolygon [point 0 5, point 0 0, point 5 0, point 0 5])
     ]

colorDrawing = [
   (translate (point 0 0) , rectangle 16.0 9.0, red),
    (translate (point 5 0), square, green),
    (translate (point 0 5), ellipse 9.0 16.0, green),
    (translate (point (-5) 0), convexPolygon [point 0 5, point 0 0, point 5 0, point 0 5], blue)
     ]

colorDrawing2 = [
   (translate (point 2 0) , rectangle 16.0 9.0, blue),
   (translate (point 2 6), square, green),
   (translate (point (-4) 5), ellipse 9.0 16.0, red),
    (translate (point (-5) (-3)), convexPolygon [point 0 5, point 0 0, point 5 0, point 0 5], color 255 255 0 1)
     ]
    
maskedDrawing = mask 120 colorDrawing colorDrawing2
