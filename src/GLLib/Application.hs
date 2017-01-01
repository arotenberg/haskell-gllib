{-# LANGUAGE ScopedTypeVariables #-}

module GLLib.Application(
    Scene(..),
    runGLApplication
) where

import Control.Monad
import Data.IORef(IORef, newIORef)
import Data.Maybe(fromMaybe)
import Data.Proxy(Proxy(..))
import Data.StateVar
import Data.Tagged(Tagged(..), proxy)
import qualified Graphics.UI.GLFW as GLFW
import Text.Printf(printf)

class Scene ss where
    sceneWindowTitle :: Tagged ss String
    initScene :: IO ss
    disposeScene :: ss -> IO ()
    sceneKeyCallback :: ss -> GLFW.KeyCallback
    updateScene :: ss -> GLFW.Window -> IO ()
    drawScene :: ss -> (Int, Int) -> IO ()
    
    sceneWindowTitle = Tagged "OpenGL Application"
    sceneKeyCallback _ss window key _scancode action _mods =
        when (action == GLFW.KeyState'Pressed || action == GLFW.KeyState'Repeating) $
            case key of
                GLFW.Key'Escape -> GLFW.setWindowShouldClose window True
                _ -> return ()

defaultWidth, defaultHeight :: Int
defaultWidth = 1366
defaultHeight = 768
secondsPerBatch :: Double
secondsPerBatch = 1.0

runGLApplication :: forall ss. Scene ss => Tagged ss (IO ())
runGLApplication = Tagged $ do
    GLFW.setErrorCallback $ Just $ \err desc ->
        putStrLn $ show err ++ ": " ++ desc
    initSuccess <- GLFW.init
    when initSuccess $ do
        GLFW.windowHint (GLFW.WindowHint'sRGBCapable True)
        let windowTitle = proxy sceneWindowTitle (Proxy :: Proxy ss)
        maybeWindow <- GLFW.createWindow defaultWidth defaultHeight windowTitle Nothing Nothing
        case maybeWindow of
            Nothing -> return ()
            Just window -> do
                GLFW.makeContextCurrent (Just window)
                GLFW.swapInterval 1
                gs <- mainInit window :: IO (GlobalState ss)
                GLFW.setKeyCallback window $ Just $ sceneKeyCallback $ gsSceneState gs
                mainLoop gs
                mainDispose gs
                GLFW.destroyWindow window
        GLFW.terminate

data GlobalState ss = GlobalState {
    gsWindow :: !GLFW.Window,
    gsFramesSinceBatchStartRef :: !(IORef Int),
    gsBatchStartTimeRef :: !(IORef Double),
    gsSceneState :: !ss
  }

mainInit :: Scene ss => GLFW.Window -> IO (GlobalState ss)
mainInit window = do
    framesSinceBatchStartRef <- newIORef 0
    currentTime <- getTime
    batchStartTimeRef <- newIORef currentTime
    
    sceneState <- initScene
    
    return GlobalState {
        gsWindow = window,
        gsFramesSinceBatchStartRef = framesSinceBatchStartRef,
        gsBatchStartTimeRef = batchStartTimeRef,
        gsSceneState = sceneState
      }

mainDispose :: Scene ss => GlobalState ss -> IO ()
mainDispose gs = disposeScene (gsSceneState gs)

mainLoop :: Scene ss => GlobalState ss -> IO ()
mainLoop gs = do
    shouldClose <- GLFW.windowShouldClose (gsWindow gs)
    unless shouldClose $ do
        printFPS gs
        
        updateScene (gsSceneState gs) (gsWindow gs)
        size <- GLFW.getFramebufferSize (gsWindow gs)
        drawScene (gsSceneState gs) size
        
        GLFW.swapBuffers (gsWindow gs)
        GLFW.pollEvents
        mainLoop gs

printFPS :: GlobalState ss -> IO ()
printFPS gs = do
    batchStartTime <- get (gsBatchStartTimeRef gs)
    currentTime <- getTime
    let batchTime = currentTime - batchStartTime
    when (batchTime >= secondsPerBatch) $ do
        framesSinceBatchStart <- get (gsFramesSinceBatchStartRef gs)
        printf "%.2f fps; %.2f ms per frame\n"
            (fromIntegral framesSinceBatchStart / batchTime)
            (1000 * batchTime / fromIntegral framesSinceBatchStart)
        gsFramesSinceBatchStartRef gs $= 0
        gsBatchStartTimeRef gs $= currentTime
    gsFramesSinceBatchStartRef gs $~! (+ 1)

getTime :: IO Double
getTime = do
    maybeElapsedTime <- GLFW.getTime
    -- If we get an error retrieving the elapsed time, there isn't really a lot we can do...
    return $ fromMaybe 0 maybeElapsedTime
