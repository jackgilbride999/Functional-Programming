{-# LANGUAGE OverloadedStrings #-}
import Data.Text.Lazy
import qualified Web.Scotty as Scotty
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text

import Shapes
import Render (render, renderColored,defaultWindow)

exampleDrawing =  [ (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle) ]
mandel = mandelbrotDrawing (-5, -5) (5, 5)
rectangleDrawing = [
   (scale (point 0.2 0.2) <+> translate (point 0 0) , rectangle 16.0 9.0),
    (scale (point 0.2 0.2) <+> translate (point 5 0), square),
    (scale (point 0.2 0.2) <+> translate (point 0 5), ellipse 9.0 16.0),
    (scale (point 0.2 0.2) <+> translate (point (-5) 0), convexPolygon [point 0 5, point 0 0, point 5 0, point 0 5])
     ]

colorDrawing = [
   (scale (point 0.2 0.2) <+> translate (point 0 0) , rectangle 16.0 9.0, red),
    (scale (point 0.2 0.2) <+> translate (point 5 0), square, blue),
    (scale (point 0.2 0.2) <+> translate (point 0 5), ellipse 9.0 16.0, green),
    (scale (point 0.2 0.2) <+> translate (point (-5) 0), convexPolygon [point 0 5, point 0 0, point 5 0, point 0 5], color 100 150 200 1)
     ]
    

main = do 
    render "output.png" defaultWindow exampleDrawing
    render "mandelbrot.png" defaultWindow mandel
    renderColored "rectangle.png" defaultWindow colorDrawing
    Scotty.scotty 3000 $ do
        Scotty.get "/" $ do
            Scotty.html $ response " world!"
        Scotty.get "/output.png" $ do 
            Scotty.file "./rectangle.PNG"
        
response :: Text -> Text
response n = do 
    renderHtml $ do
        h1 ("Hello" >> toHtml n)
        img ! src "output.png" ! width "50%"
        p " [ (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle) ]"
