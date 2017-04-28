{-# LANGUAGE ScopedTypeVariables #-}

module GLLib.Utils where

import Control.Monad
import qualified Data.Set as Set
import Foreign.C.String(peekCAString, withCAString)
import Foreign.Marshal.Alloc(alloca, allocaBytes)
import Foreign.Ptr(Ptr, castPtr)
import Foreign.Storable
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW

-- | @allocaIn x f@ allocates a temporary block of memory, initializes it to a value of @x@,
-- executes the computation @f@ passing it a pointer to the temporary block, and finally frees the
-- allocated memory.
allocaIn :: Storable a => a -> (Ptr a -> IO b) -> IO b
allocaIn x f = alloca $ \ptr -> do
    poke ptr x
    f ptr

allocaInArray :: forall a b. Storable a => [a] -> (Ptr a -> IO b) -> IO b
allocaInArray xs f =
    allocaBytes (sizeOf (undefined :: a) * length xs) $ \ptr -> do
        forM_ (zip [0..] xs) $ uncurry (pokeElemOff ptr)
        f ptr

allocaInArrayWithLength :: Storable a => [a] -> (GLsizei -> Ptr a -> IO b) -> IO b
allocaInArrayWithLength xs f = allocaInArray xs $ f (fromIntegral (length xs))

-- | @allocaOut f@ allocates a temporary block of memory, executes the computation @f@ passing it a
-- pointer to the temporary block, pulls out the value now in the memory, and finally frees the
-- allocated memory and returns the value.
allocaOut :: Storable a => (Ptr a -> IO b) -> IO a
allocaOut f = alloca $ \ptr -> do
    f ptr
    peek ptr

allocaOutArray :: forall a b. Storable a => GLsizei -> (Ptr a -> IO b) -> IO [a]
allocaOutArray count f =
    allocaBytes (sizeOf (undefined :: a) * fromIntegral count) $ \ptr -> do
        f ptr
        forM [0..count - 1] (peekElemOff ptr . fromIntegral)

allocaOutArrayWithSize :: forall a b c. (Storable a, Num c) =>
    GLsizei -> (c -> Ptr a -> IO b) -> IO [a]
allocaOutArrayWithSize count f =
    let size = fromIntegral count * fromIntegral (sizeOf (undefined :: a))
    in allocaOutArray count (f size)

gen :: (GLsizei -> Ptr GLuint -> IO ()) -> IO GLuint
gen f = allocaOut (f 1)

del :: (GLsizei -> Ptr GLuint -> IO ()) -> [GLuint] -> IO ()
del = flip allocaInArrayWithLength

getLoc :: (Ptr GLchar -> IO GLint) -> String -> IO GLint
getLoc = flip withCAString

getSupportedOpenGLExtensions :: IO (Set.Set String)
getSupportedOpenGLExtensions = do
    numExts <- allocaOut (glGetIntegerv GL_NUM_EXTENSIONS)
    exts <- forM [0..fromIntegral numExts - 1] $ \index -> do
        namePtr <- glGetStringi GL_EXTENSIONS index
        peekCAString (castPtr namePtr)
    return $ Set.fromList exts

isKeyPressed :: GLFW.Window -> GLFW.Key -> IO Bool
isKeyPressed window key = (== GLFW.KeyState'Pressed) `liftM` GLFW.getKey window key
