{-# LANGUAGE ScopedTypeVariables #-}

module GLLib.Mesh (
    Mesh, disposeMesh,
    ActiveMesh, bindMesh,
    drawMesh,
    meshFromJSON, readMeshFromJSONFile,
    meshToJSON, writeMeshToJSONFile,
    StandardAttrib(..),
    meshFromOBJ, readMeshFromOBJFile
) where

import Control.Monad
import Data.Foldable(toList)
import Data.List(foldl')
import Data.List.Split(splitOn)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Foreign.Ptr(nullPtr)
import Foreign.Storable(Storable, sizeOf)
import Graphics.Rendering.OpenGL.Raw
import qualified Text.JSON as JSON

import GLLib.Program
import GLLib.Utils
import GLLib.Vec

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
    glBindBuffer gl_ELEMENT_ARRAY_BUFFER (meshBufferID $ meshVertexIndices $ mesh)
    forM_ (Map.keys $ programAttribIDs prog) $ \attribKey -> do
        let attribBuffer = meshVertexAttribs mesh Map.! attribKey
        bindVertexAttribBuffer activeProg attribKey (meshBufferID attribBuffer)
            (meshBufferStride attribBuffer) gl_FLOAT
    callback (ActiveMesh mesh)

drawMesh :: ActiveMesh a -> IO ()
drawMesh (ActiveMesh mesh) = do
    let vertexCount = meshBufferCount (meshVertexIndices mesh)
    glDrawElements gl_TRIANGLES vertexCount gl_UNSIGNED_INT nullPtr

readMeshFromJSONFile :: Ord a => [(a, String, GLint)] -> FilePath -> IO (Mesh a)
readMeshFromJSONFile attribNames = readFile >=> meshFromJSON attribNames

meshFromJSON :: Ord a => [(a, String, GLint)] -> String -> IO (Mesh a)
meshFromJSON attribNames jsonString =
    case JSON.decode jsonString :: JSON.Result (JSON.JSObject [Float]) of
        JSON.Error msg -> error msg
        JSON.Ok obj -> do
            let objAList = JSON.fromJSObject obj
            vertexIndices <-
                bufferJSONData objAList vertexIndicesField gl_ELEMENT_ARRAY_BUFFER UIntType 1
            attribBuffers <- forM attribNames $ \(attribKey, attribName, attribStride) -> do
                buffer <- bufferJSONData objAList attribName gl_ARRAY_BUFFER FloatType attribStride
                return (attribKey, buffer)
            return Mesh {
                meshVertexIndices = vertexIndices,
                meshVertexAttribs = Map.fromList attribBuffers
              }

vertexIndicesField :: String
vertexIndicesField = "vertexIndices"

data ArrayType = UIntType | FloatType
    deriving (Show, Eq)

bufferJSONData :: [(String, [Float])] -> String -> GLenum -> ArrayType -> GLint -> IO MeshBuffer
bufferJSONData objAList fieldName target arrayType stride =
    case lookup fieldName objAList of
        Nothing -> error $ "Missing field: " ++ fieldName
        Just fieldData ->
            case arrayType of
                UIntType -> do
                    let arrayData = map truncate fieldData :: [GLuint]
                    bufferData target stride arrayData
                FloatType -> do
                    let arrayData = map realToFrac fieldData :: [GLfloat]
                    bufferData target stride arrayData

bufferData :: forall a. Storable a => GLenum -> GLint -> [a] -> IO MeshBuffer
bufferData target stride arrayData = do
    bufferID <- gen glGenBuffers
    glBindBuffer target bufferID
    let dataLength = length arrayData
        dataSize = fromIntegral dataLength * fromIntegral (sizeOf (undefined :: a)) :: GLsizeiptr
    allocaInArray arrayData $ \ptr ->
        glBufferData target dataSize ptr gl_STATIC_DRAW
    return MeshBuffer {
        meshBufferCount = fromIntegral dataLength,
        meshBufferID = bufferID,
        meshBufferStride = stride
      }

writeMeshToJSONFile :: Ord a => FilePath -> [(a, String)] -> Mesh a -> IO ()
writeMeshToJSONFile filePath attribs = meshToJSON attribs >=> writeFile filePath

meshToJSON :: Ord a => [(a, String)] -> Mesh a -> IO String
meshToJSON attribs mesh = do
    vertexIndicesValue <- meshBufferToJSValue
        gl_ELEMENT_ARRAY_BUFFER UIntType (meshVertexIndices mesh)
    vertexAttribValues <- forM attribs $ \(attribKey, attribName) -> do
        let buffer = meshVertexAttribs mesh Map.! attribKey
        bufferValue <- meshBufferToJSValue gl_ARRAY_BUFFER FloatType buffer
        return (attribName, bufferValue)
    return $ JSON.encode $ JSON.toJSObject $
        (vertexIndicesField, vertexIndicesValue) : vertexAttribValues

meshBufferToJSValue :: GLenum -> ArrayType -> MeshBuffer -> IO JSON.JSValue
meshBufferToJSValue target arrayType buffer = do
    glBindBuffer target (meshBufferID buffer)
    let count = meshBufferCount buffer
    rationals <- case arrayType of
        UIntType -> map toRational `liftM`
            (allocaOutArrayWithSize count (glGetBufferSubData target 0) :: IO [GLuint])
        FloatType -> map toRational `liftM`
            (allocaOutArrayWithSize count (glGetBufferSubData target 0) :: IO [GLfloat])
    return $ JSON.JSArray (map (JSON.JSRational True) rationals)

data StandardAttrib = Attrib_vertexPos | Attrib_vertexUV | Attrib_vertexNormal
    deriving (Show, Eq, Ord)

readMeshFromOBJFile :: FilePath -> IO (Mesh StandardAttrib)
readMeshFromOBJFile = readFile >=> meshFromOBJ

meshFromOBJ :: String -> IO (Mesh StandardAttrib)
meshFromOBJ objString = reassembleLines $ foldl' parseOBJLine initialOBJState (lines objString)

parseOBJLine :: OBJState -> String -> OBJState
parseOBJLine st ('v':' ':line) = v `seq` st { objPositions = objPositions st Seq.|> v }
  where [xStr, yStr, zStr] = words line
        v = Vec3 (read xStr) (read yStr) (read zStr)
parseOBJLine st ('v':'t':' ':line) = v `seq` st { objUVs = objUVs st Seq.|> v }
  where [xStr, yStr] = words line
        v = Vec2 (read xStr) (read yStr)
parseOBJLine st ('v':'n':' ':line) = v `seq` st { objNormals = objNormals st Seq.|> v }
  where [xStr, yStr, zStr] = words line
        v = Vec3 (read xStr) (read yStr) (read zStr)
parseOBJLine st ('f':' ':line) = f `seq` st { objFaces = objFaces st Seq.|> f }
  where [vStr1, vStr2, vStr3] = words line
        f = OBJFace (readVertStr vStr1) (readVertStr vStr2) (readVertStr vStr3)
        readVertStr vertStr =
            let [posStr, uvStr, normalStr] = splitOn "/" vertStr
            in OBJVert (readIndex posStr) (readIndex uvStr) (readIndex normalStr)
        readIndex "" = -1
        readIndex str = read str - 1
parseOBJLine st _ = st

reassembleLines :: OBJState -> IO (Mesh StandardAttrib)
reassembleLines st = do
    vertexIndices <- bufferData gl_ELEMENT_ARRAY_BUFFER 1 indicesList
    attribs <- forM objAttribs $ \(attribName, attribStride, extractFunc) -> do
        attribBuffer <- bufferData gl_ARRAY_BUFFER attribStride $ concatMap (extractFunc st) $ toList vertsSet
        return (attribName, attribBuffer)
    return Mesh {
        meshVertexIndices = vertexIndices,
        meshVertexAttribs = Map.fromList attribs
      }
  where faceVertsList :: [OBJVert]
        faceVertsList = concatMap faceVerts $ toList $ objFaces st
        vertsSet = Set.fromList faceVertsList
        indicesList :: [GLuint]
        indicesList = map (fromIntegral . flip Set.findIndex vertsSet) faceVertsList

data OBJState = OBJState {
    objPositions :: !(Seq.Seq Vec3),
    objUVs :: !(Seq.Seq Vec2),
    objNormals :: !(Seq.Seq Vec3),
    objFaces :: !(Seq.Seq OBJFace)
  }
initialOBJState :: OBJState
initialOBJState = OBJState Seq.empty Seq.empty Seq.empty Seq.empty

data OBJFace = OBJFace !OBJVert !OBJVert !OBJVert
    deriving (Show)
faceVerts :: OBJFace -> [OBJVert]
faceVerts (OBJFace v1 v2 v3) = [v1, v2, v3]

data OBJVert = OBJVert !Int !Int !Int
    deriving (Show, Eq, Ord)

objAttribs :: [(StandardAttrib, GLint, OBJState -> OBJVert -> [GLfloat])]
objAttribs = [
    (Attrib_vertexPos, 3, \st (OBJVert i _ _) ->
        let Vec3 x y z = objPositions st `Seq.index` i in [x, y, z]),
    (Attrib_vertexUV, 2, \st (OBJVert _ i _) ->
        let Vec2 x y = objUVs st `Seq.index` i in [x, y]),
    (Attrib_vertexNormal, 3, \st (OBJVert _ _ i) ->
        let Vec3 x y z = objNormals st `Seq.index` i in [x, y, z])
  ]
