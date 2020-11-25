{-# LANGUAGE OverloadedStrings #-}
import Data.Text.Lazy
import qualified Web.Scotty as Scotty
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text

main = Scotty.scotty 3000 $ do
    Scotty.get "/" $ do
        Scotty.html $ response " world!"

response :: Text -> Text
response n = do 
    renderHtml $ do
        h1 ("Hello" >> toHtml n)