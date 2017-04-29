module GLLib.GLFW(
    simpleGLFWInit,
    isKeyPressed
) where

import Control.Monad
import qualified Graphics.UI.GLFW as GLFW

defaultWidth, defaultHeight :: Int
defaultWidth = 800
defaultHeight = 600

simpleGLFWInit :: IO GLFW.Window
simpleGLFWInit = do
    GLFW.setErrorCallback $ Just $ \err desc ->
        error $ show err ++ ": " ++ desc
    initSuccess <- GLFW.init
    case initSuccess of
        False -> error "Error initializing GLFW."
        True -> do
            GLFW.windowHint (GLFW.WindowHint'OpenGLDebugContext True)
            GLFW.windowHint (GLFW.WindowHint'sRGBCapable True)
            maybeWindow <- GLFW.createWindow defaultWidth defaultHeight "" Nothing Nothing
            case maybeWindow of
                Nothing -> error "Error creating GLFW window."
                Just window -> do
                    GLFW.makeContextCurrent (Just window)
                    GLFW.swapInterval 1
                    return window

isKeyPressed :: GLFW.Window -> GLFW.Key -> IO Bool
isKeyPressed window key = (== GLFW.KeyState'Pressed) `liftM` GLFW.getKey window key
