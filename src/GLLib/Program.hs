{-# LANGUAGE TypeSynonymInstances #-}

module GLLib.Program (
    Program, programID, programUniformIDs, programAttribIDs,
    readProgram,
    GLValueType, GLSampler(..),
    ActiveProgram(..), usingProgram,
    setUniform, bindVertexAttribBuffer
) where

import Control.Monad(forM, forM_, when)
import qualified Data.ByteString as B
import Data.ByteString.Unsafe(unsafeUseAsCString)
import qualified Data.Map as Map
import Foreign.C.String(peekCAString)
import Foreign.Marshal.Alloc(allocaBytes)
import Foreign.Ptr(Ptr, nullPtr)
import Graphics.Rendering.OpenGL.Raw

import GLLib.Utils
import GLLib.Vec

data Program u a = Program {
    programID :: !GLuint,
    programUniformIDs :: !(Map.Map u GLint),
    -- | The type for functions that operate on uniforms is GLint, but it's GLuint for attributes,
    -- even though glGetAttribLocation returns a GLint. Because OpenGL hates you.
    programAttribIDs :: !(Map.Map a GLuint)
  } deriving (Show)

readProgram :: (Ord u, Ord a) => FilePath -> FilePath ->
    [(u, String)] -> [(a, String)] -> IO (Program u a)
readProgram vertexShaderFile fragmentShaderFile uniforms attribs = do
    vertexShaderSource <- B.readFile vertexShaderFile
    fragmentShaderSource <- B.readFile fragmentShaderFile
    pID <- createProgramFromSource vertexShaderSource fragmentShaderSource
    
    uniformIDs <- assembleIDArray pID uniforms glGetUniformLocation
    attribIDs <- assembleIDArray pID attribs glGetAttribLocation
    
    return Program {
        programID = pID,
        programUniformIDs = uniformIDs,
        programAttribIDs = attribIDs
      }

assembleIDArray :: (Ord k, Integral id) => GLuint -> [(k, String)] ->
    (GLuint -> Ptr GLchar -> IO GLint) -> IO (Map.Map k id)
assembleIDArray pID names glFunc = do
    idAList <- forM names $ \(key, name) -> do
        id <- getLoc (glFunc pID) name
        return (key, fromIntegral id)
    return $ Map.fromList idAList

createProgramFromSource :: B.ByteString -> B.ByteString -> IO GLuint
createProgramFromSource vertexShaderSource fragmentShaderSource = do
    vertexShaderID <- createShaderFromSource gl_VERTEX_SHADER vertexShaderSource
    fragmentShaderID <- createShaderFromSource gl_FRAGMENT_SHADER fragmentShaderSource
    
    programID <- glCreateProgram
    glAttachShader programID vertexShaderID
    glAttachShader programID fragmentShaderID
    glLinkProgram programID
    
    checkInfoLog programID glGetProgramiv glGetProgramInfoLog gl_LINK_STATUS
    
    glDeleteShader vertexShaderID
    glDeleteShader fragmentShaderID
    
    isProgram <- glIsProgram programID
    when (fromIntegral isProgram /= gl_TRUE) $
        error "Loading shader program failed in some unspecified manner..."
    
    return programID

createShaderFromSource :: GLenum -> B.ByteString -> IO GLuint
createShaderFromSource shaderType shaderSource = do
    shaderID <- glCreateShader shaderType
    
    unsafeUseAsCString shaderSource $ \cString ->
        allocaIn cString $ \cStringPtr ->
        allocaIn (fromIntegral (B.length shaderSource)) $ \lengthPtr -> do
            glShaderSource shaderID 1 cStringPtr lengthPtr
            -- glShaderSource is specified to copy the input string, so we can discard the original
            -- string immediately after that call.
    
    glCompileShader shaderID
    checkInfoLog shaderID glGetShaderiv glGetShaderInfoLog gl_COMPILE_STATUS
    return shaderID

checkInfoLog :: GLuint -> (GLuint -> GLenum -> Ptr GLint -> IO ()) ->
    (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GLenum -> IO ()
checkInfoLog objectID getiv getInfoLog statusType = do
    infoLogLength <- allocaOut $ getiv objectID gl_INFO_LOG_LENGTH
    -- gl_INFO_LOG_LENGTH includes the NUL character.
    when (infoLogLength > 1) $ do
        msg <- allocaBytes  (fromIntegral infoLogLength) $ \msgPtr -> do
            getInfoLog objectID infoLogLength nullPtr msgPtr
            peekCAString msgPtr
        compileStatus <- allocaOut $ getiv objectID statusType
        if fromIntegral compileStatus /= gl_TRUE then error msg else putStrLn msg

-- | @GLSampler textureType textureID textureUnitNum@
data GLSampler = GLSampler !GLenum !GLuint !GLint
    deriving (Show)

class GLValueType v where
    setUniformImpl :: GLint -> v -> IO ()
instance GLValueType GLfloat where
    setUniformImpl = glUniform1f
instance GLValueType Vec2 where
    setUniformImpl location (Vec2 x y) = glUniform2f location x y
instance GLValueType Vec3 where
    setUniformImpl location (Vec3 x y z) = glUniform3f location x y z
instance GLValueType Vec4 where
    setUniformImpl location (Vec4 x y z w) = glUniform4f location x y z w
instance GLValueType Mat2 where
    setUniformImpl location (Mat2 (Vec2 m11 m21) (Vec2 m12 m22)) =
        allocaInArray [
            -- Column-major order since transpose is false.
            m11, m21,
            m12, m22
          ] $ glUniformMatrix2fv location 1 (fromIntegral gl_FALSE)
instance GLValueType Mat3 where
    setUniformImpl location (Mat3 (Vec3 m11 m21 m31) (Vec3 m12 m22 m32) (Vec3 m13 m23 m33)) =
        allocaInArray [
            m11, m21, m31,
            m12, m22, m32,
            m13, m23, m33
          ] $ glUniformMatrix3fv location 1 (fromIntegral gl_FALSE)
instance GLValueType Mat4 where
    setUniformImpl location (Mat4 (Vec4 m11 m21 m31 m41) (Vec4 m12 m22 m32 m42)
            (Vec4 m13 m23 m33 m43) (Vec4 m14 m24 m34 m44)) =
        allocaInArray [
            m11, m21, m31, m41,
            m12, m22, m32, m42,
            m13, m23, m33, m43,
            m14, m24, m34, m44
          ] $ glUniformMatrix4fv location 1 (fromIntegral gl_FALSE)
instance GLValueType GLSampler where
    setUniformImpl location (GLSampler textureType textureID textureUnitNum) = do
        glActiveTexture (gl_TEXTURE0 + fromIntegral textureUnitNum)
        glBindTexture textureType textureID
        glUniform1i location textureUnitNum

newtype ActiveProgram u a = ActiveProgram (Program u a)
    deriving (Show)

usingProgram :: (Ord u, Ord a) => Program u a -> (ActiveProgram u a -> IO x) -> IO ()
usingProgram prog callback = do
    glUseProgram (programID prog)
    forM_ (Map.elems (programAttribIDs prog)) glEnableVertexAttribArray
    callback (ActiveProgram prog)
    forM_ (Map.elems (programAttribIDs prog)) glDisableVertexAttribArray

setUniform :: (Ord u, GLValueType v) => ActiveProgram u a -> u -> v -> IO ()
setUniform (ActiveProgram prog) key value = do
    let uniformID = programUniformIDs prog Map.! key
    setUniformImpl uniformID value

bindVertexAttribBuffer :: Ord a => ActiveProgram u a -> a -> GLuint -> GLint -> GLenum -> IO ()
bindVertexAttribBuffer (ActiveProgram prog) key bufferID dataSize dataType = do
    glBindBuffer gl_ARRAY_BUFFER bufferID
    let attribID = programAttribIDs prog Map.! key
    glVertexAttribPointer attribID dataSize dataType (fromIntegral gl_FALSE) 0 nullPtr
