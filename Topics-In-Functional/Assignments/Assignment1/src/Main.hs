{-# LANGUAGE OverloadedStrings #-}
import Data.Text.Lazy
import qualified Web.Scotty as Scotty
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text

import Shapes
import Render (render,defaultWindow)

main = 
 Scotty.scotty 3000 $ do
    Scotty.get "/" $ do
        Scotty.html $ response " world!"
    
    Scotty.get "/output.png" $ Scotty.file "./output.PNG"

    --Scotty.get "drawing/:shape" $
        


blaze = Scotty.html . renderHtml

response :: Text -> Text
response n = do 
    renderHtml $ do
        h1 ("Hello" >> toHtml n)
        img ! src "output.png"
        p " [ (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle) ]"
     --   img ! src "drawing/[ (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle) ]"



exampleDrawing =  [ (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle) ]

mandel = mandelbrotDrawing (-5, -5) (5, 5)

test = render "output.png" defaultWindow exampleDrawing >> render "mandelbrot.png" defaultWindow mandel
