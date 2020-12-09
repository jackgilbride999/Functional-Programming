{-
    Build a simple desktop calculator using ThreePenny GUI.
    It should handle four-function arithmetic (add, subtract, 
    divide, multiply) and display at least 8-bit integers. 
    Include a Clear button to reset the calculator. You don't 
    have to implement anything fancy, you can implement the 
    actual calculation any way you want (including just "read"-
    ing strings that are built up in the UI, or reusing the 
    simple expression evaluator we saw in class).
-}
import Graphics.UI.Threepenny


main = do
 startGUI defaultConfig setup
 
showMessage :: Window -> UI ()
showMessage window = do
 getBody window #+ [string "Hello, world!"]
 return ()

setup :: Window -> UI ()
setup w = do
    return w # set title "fadeIn - fadeOut"
    button <- button # set text "Click me to make me \
                                    \fade out and in!"
    getBody w #+ [column [string "Demonstration of \
                            \jQuery’s animate() function"
                            ,element button]]
    on click button $ \_ -> do
        fadeOut button 400 Swing $ do
            runUI w $ fadeIn button 400 Swing $ return ()