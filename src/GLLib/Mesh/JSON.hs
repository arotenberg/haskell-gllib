module GLLib.Mesh.JSON(
    meshDataFromJSON, readMeshFromJSONFile,
    meshDataToJSON, writeMeshToJSONFile
) where

import qualified Data.Map as Map
import Graphics.GL
import qualified Text.JSON as JSON

import GLLib.Mesh

readMeshFromJSONFile :: Ord a => [(a, String, GLint)] -> FilePath -> IO (Mesh a)
readMeshFromJSONFile attribNames filePath = do
    jsonString <- readFile filePath
    let meshData = meshDataFromJSON attribNames jsonString
    meshFromData meshData

meshDataFromJSON :: Ord a => [(a, String, GLint)] -> String -> MeshData a
meshDataFromJSON attribNames jsonString =
    case JSON.decode jsonString :: JSON.Result (JSON.JSObject [Float]) of
        JSON.Error msg -> error msg
        JSON.Ok obj -> meshDataFromJSONObjAList attribNames (JSON.fromJSObject obj)

meshDataFromJSONObjAList :: Ord a => [(a, String, GLint)] -> [(String, [Float])] -> MeshData a
meshDataFromJSONObjAList attribNames objAList = MeshData vertexIndices vertexAttribs
  where vertexIndices = extractJSONArrayData objAList vertexIndicesField
            (truncate :: Float -> GLuint)
        vertexAttribs = Map.fromList [(attribKey, MeshDataArray attribStride attribData) |
            (attribKey, attribName, attribStride) <- attribNames,
            let attribData = extractJSONArrayData objAList attribName
                    (realToFrac :: Float -> GLfloat)]

vertexIndicesField :: String
vertexIndicesField = "vertexIndices"

extractJSONArrayData :: [(String, [Float])] -> String -> (Float -> a) -> [a]
extractJSONArrayData objAList fieldName conversionFunc =
    case lookup fieldName objAList of
        Nothing -> error $ "Missing field: " ++ fieldName
        Just fieldData -> map conversionFunc fieldData

writeMeshToJSONFile :: Ord a => FilePath -> [(a, String)] -> Mesh a -> IO ()
writeMeshToJSONFile filePath attribs mesh = do
    meshData <- meshToData mesh
    let jsonString = meshDataToJSON attribs meshData
    writeFile filePath jsonString

meshDataToJSON :: Ord a => [(a, String)] -> MeshData a -> String
meshDataToJSON attribs meshData = JSON.encode $ JSON.toJSObject $
    (vertexIndicesField, toJSONDecimalArray vertexIndicesValue) : vertexAttribValues
  where vertexIndicesValue = meshDataVertexIndices meshData
        vertexAttribValues = [(attribName, toJSONDecimalArray arrayData) |
            (attribKey, attribName) <- attribs,
            let MeshDataArray _ arrayData = meshDataVertexAttribs meshData Map.! attribKey]

toJSONDecimalArray :: Real a => [a] -> JSON.JSValue
toJSONDecimalArray = JSON.JSArray . map (JSON.JSRational True . toRational)
