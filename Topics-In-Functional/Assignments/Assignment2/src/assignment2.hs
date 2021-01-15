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
 startGUI defaultConfig calculator

calculator :: Window -> UI ()
calculator window = do 
    return window # set title "Simple calculator"
    button1 <- button # set text "1"
    button2 <- button # set text "2"
    button3 <- button # set text "3"
    button4 <- button # set text "4"
    button5 <- button # set text "5"
    button6 <- button # set text "6"
    button7 <- button # set text "7"
    button8 <- button # set text "8"
    button9 <- button # set text "9"
    button0 <- button # set text "0"
    buttonAdd <- button # set text "+"
    buttonSub <- button # set text "-"
    buttonMul <- button # set text "X"
    buttonDiv <- button # set text "÷"
    buttonEquals <- button # set text "="
    buttonClear <- button # set text "C"
    getBody window #+ [
        grid [
            [element button1, element button2, element button3, element buttonAdd],
            [element button4, element button5, element button6, element buttonSub],
            [element button7, element button8, element button9, element buttonMul],
            [element buttonClear,element button0, element buttonEquals, element buttonDiv] 
        ]
        ]
    return ()



 
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