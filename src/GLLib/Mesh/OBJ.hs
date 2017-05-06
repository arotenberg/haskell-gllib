module GLLib.Mesh.OBJ(
    meshDataFromOBJ, readMeshFromOBJFile
) where

import Data.Foldable(toList)
import Data.List(foldl')
import Data.List.Split(splitOn)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Graphics.GL

import GLLib.Mesh
import GLLib.Vec

readMeshFromOBJFile :: FilePath -> IO (Mesh StandardAttrib)
readMeshFromOBJFile filePath = do
    objString <- readFile filePath
    let meshData = meshDataFromOBJ objString
    meshFromData meshData

meshDataFromOBJ :: String -> MeshData StandardAttrib
meshDataFromOBJ objString =
    reassembleOBJLines $ foldl' parseOBJLine initialOBJState (lines objString)

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

reassembleOBJLines :: OBJState -> MeshData StandardAttrib
reassembleOBJLines st = MeshData vertexIndices vertexAttribs
  where faceVertsList :: [OBJVert]
        faceVertsList = concatMap faceVerts $ toList $ objFaces st
        
        vertsSet :: Set.Set OBJVert
        vertsSet = Set.fromList faceVertsList
        
        vertexIndices :: [GLuint]
        vertexIndices = map (fromIntegral . flip Set.findIndex vertsSet) faceVertsList
        
        vertexAttribs :: Map.Map StandardAttrib MeshDataArray
        vertexAttribs = Map.fromList [(attribKey, MeshDataArray attribStride attribData) |
            (attribKey, attribStride, extractFunc) <- objAttribs,
            let attribData = concatMap (extractFunc st) (toList vertsSet)]

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
