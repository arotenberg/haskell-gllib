{-# LANGUAGE ScopedTypeVariables #-}

module GLLib.Mesh(
    Mesh, disposeMesh,
    ActiveMesh, bindMesh,
    drawMesh,
    MeshData(..), MeshDataArray(..),
    meshFromData, meshToData,
    StandardAttrib(..),
    planeMeshData
) where

import Control.Monad
import qualified Data.Map as Map
import Foreign.Ptr(nullPtr)
import Foreign.Storable(Storable, sizeOf)
import Graphics.GL

import GLLib.Program
import GLLib.Utils

data Mesh a = Mesh {
    meshVertexIndices :: !MeshBuffer,
    meshVertexAttribs :: !(Map.Map a MeshBuffer)
  } deriving (Show)
data MeshBuffer = MeshBuffer {
    meshBufferCount :: !GLsizei,
    meshBufferID :: !GLuint,
    meshBufferStride :: !GLint
  } deriving (Show)

disposeMesh :: Ord a => Mesh a -> IO ()
disposeMesh mesh =
    del glDeleteBuffers $ map meshBufferID $
        meshVertexIndices mesh : Map.elems (meshVertexAttribs mesh)

newtype ActiveMesh a = ActiveMesh (Mesh a)
    deriving (Show)

bindMesh :: Ord a => ActiveProgram u a -> Mesh a -> (ActiveMesh a -> IO ()) -> IO ()
bindMesh activeProg@(ActiveProgram prog) mesh callback = do
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER (meshBufferID $ meshVertexIndices $ mesh)
    forM_ (Map.keys $ programAttribIDs prog) $ \attribKey -> do
        let attribBuffer = meshVertexAttribs mesh Map.! attribKey
        bindVertexAttribBuffer activeProg attribKey (meshBufferID attribBuffer)
            (meshBufferStride attribBuffer) GL_FLOAT
    callback (ActiveMesh mesh)

drawMesh :: ActiveMesh a -> IO ()
drawMesh (ActiveMesh mesh) = do
    let vertexCount = meshBufferCount (meshVertexIndices mesh)
    glDrawElements GL_TRIANGLES vertexCount GL_UNSIGNED_INT nullPtr

data MeshData a = MeshData {
    meshDataVertexIndices :: [GLuint],
    meshDataVertexAttribs :: Map.Map a MeshDataArray
  } deriving (Show)
data MeshDataArray = MeshDataArray GLint [GLfloat]
    deriving (Show)

meshFromData :: Ord a => MeshData a -> IO (Mesh a)
meshFromData meshData = do
    indicesBuffer <- bufferData GL_ELEMENT_ARRAY_BUFFER 1 (meshDataVertexIndices meshData)
    attribBuffers <- forM (Map.toList (meshDataVertexAttribs meshData)) $
            \(key, MeshDataArray stride arrayData) -> do
        buffer <- bufferData GL_ARRAY_BUFFER stride arrayData
        return (key, buffer)
    return Mesh {
        meshVertexIndices = indicesBuffer,
        meshVertexAttribs = Map.fromList attribBuffers
      }

bufferData :: forall e. Storable e => GLenum -> GLint -> [e] -> IO MeshBuffer
bufferData target stride arrayData = do
    bufferID <- gen glGenBuffers
    glBindBuffer target bufferID
    let dataLength = length arrayData
        dataSize = fromIntegral dataLength * fromIntegral (sizeOf (undefined :: e)) :: GLsizeiptr
    allocaInArray arrayData $ \ptr ->
        glBufferData target dataSize ptr GL_STATIC_DRAW
    return MeshBuffer {
        meshBufferCount = fromIntegral dataLength,
        meshBufferID = bufferID,
        meshBufferStride = stride
      }

meshToData :: Ord a => Mesh a -> IO (MeshData a)
meshToData mesh = do
    vertexIndices <- unbufferData GL_ELEMENT_ARRAY_BUFFER (meshVertexIndices mesh)
    vertexAttribs <- forM (Map.toList (meshVertexAttribs mesh)) $ \(key, buffer) -> do
        let stride = meshBufferStride buffer
        arrayData <- unbufferData GL_ARRAY_BUFFER buffer
        return (key, MeshDataArray stride arrayData)
    return MeshData {
        meshDataVertexIndices = vertexIndices,
        meshDataVertexAttribs = Map.fromList vertexAttribs
      }

unbufferData :: forall e. Storable e => GLenum -> MeshBuffer -> IO [e]
unbufferData target meshBuffer = do
    glBindBuffer target (meshBufferID meshBuffer)
    let count = meshBufferCount meshBuffer
    allocaOutArrayWithSize count (glGetBufferSubData target 0) :: IO [e]

data StandardAttrib = Attrib_vertexPos | Attrib_vertexUV | Attrib_vertexNormal
    deriving (Show, Eq, Ord)

planeVertexIndices :: [GLuint]
planeVertexIndices = [1, 0, 2, 2, 3, 1]
planeVertexPositions :: [GLfloat]
planeVertexPositions = [-1, -1, -1, 1, 1, -1, 1, 1]

planeMeshData :: MeshData StandardAttrib
planeMeshData = MeshData planeVertexIndices vertexAttribs
  where vertexAttribs = Map.singleton Attrib_vertexPos (MeshDataArray 2 planeVertexPositions)
