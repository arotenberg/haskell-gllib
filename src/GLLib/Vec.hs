-- This module was originally based on the vector-space package but has been rewritten and
-- optimized for use with OpenGL.

{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeSynonymInstances #-}

module GLLib.Vec where

import Data.List(foldl')
import Graphics.GL

infixl 6 ^+^, ^-^
infixr 7 *^, ^/, <.>, ^@, ^.^
infixl 7 ^*

-- | Vector space @v@.
class VectorSpace v where
    -- | The zero element: identity for '(^+^)'
    zeroV :: v
    -- | Add vectors
    (^+^) :: v -> v -> v
    -- | Additive inverse
    negateV :: v -> v
    -- | Scale a vector
    (*^) :: GLfloat -> v -> v

-- | Group subtraction
{-# INLINABLE (^-^) #-}
(^-^) :: VectorSpace v => v -> v -> v
v ^-^ v' = v ^+^ negateV v'

-- | Sum over several vectors
{-# INLINABLE sumV #-}
sumV :: VectorSpace v => [v] -> v
sumV = foldl' (^+^) zeroV

-- | Vector multiplied by scalar
{-# INLINABLE (^*) #-}
(^*) :: VectorSpace v => v -> GLfloat -> v
(^*) = flip (*^)

-- | Vector divided by scalar
{-# INLINABLE (^/) #-}
(^/) :: VectorSpace v => v -> GLfloat -> v
v ^/ s = (1/s) *^ v

-- | Linear interpolation between @a@ (when @t==0@) and @b@ (when @t==1@).
{-# INLINABLE lerp #-}
lerp :: VectorSpace v => v -> v -> GLfloat -> v
lerp a b t = a ^+^ t *^ (b ^-^ a)

class VectorSpace v => GLVec v where
    -- | Inner/dot product
    (<.>) :: v -> v -> GLfloat
    minV :: v -> v -> v
    maxV :: v -> v -> v
    (^*^) :: v -> v -> v
    (^/^) :: v -> v -> v

-- | Square of the length of a vector.  Sometimes useful for efficiency.
-- See also 'magnitude'.
{-# INLINABLE magnitudeSq #-}
magnitudeSq :: GLVec v => v -> GLfloat
magnitudeSq v = v <.> v

-- | Length of a vector.   See also 'magnitudeSq'.
{-# INLINABLE magnitude #-}
magnitude :: GLVec v => v -> GLfloat
magnitude = sqrt . magnitudeSq

-- | Vector in same direction as given one but with length of one.
{-# INLINABLE normalized #-}
normalized :: GLVec v => v -> v
normalized v = v ^/ magnitude v

-- | @project u v@ computes the projection of @v@ onto @u@.
{-# INLINABLE project #-}
project :: GLVec v => v -> v -> v
project u v = (v <.> u') *^ u'
  where u' = normalized u

-- | @clamp minVal maxVal val@ - note that the argument order is different from GLSL!
{-# INLINABLE clamp #-}
clamp :: GLVec a => a -> a -> a -> a
clamp minVal maxVal val = minV maxVal (maxV minVal val)

class (VectorSpace m, GLVec (InOutVec m)) => GLSquareMat m where
    type InOutVec m
    -- | Transform (multiply) a vector with this matrix.
    (^@) :: m -> InOutVec m -> InOutVec m
    identityMatrix :: m
    -- | Compose (multiply) two matrices in the standard (right-to-left) order.
    (^.^) :: m -> m -> m

infinity :: GLfloat
infinity = 1/0

instance VectorSpace GLfloat where
    zeroV = 0
    (^+^) = (+)
    negateV = negate
    (*^) = (*)
instance GLVec GLfloat where
    (<.>) = (*)
    minV = min
    maxV = max
    (^*^) = (*)
    (^/^) = (/)

data Vec2 = Vec2
    {-# UNPACK #-} !GLfloat
    {-# UNPACK #-} !GLfloat
  deriving (Show)
instance VectorSpace Vec2 where
    zeroV = Vec2 0 0
    Vec2 x1 y1 ^+^ Vec2 x2 y2 = Vec2 (x1 + x2) (y1 + y2)
    negateV (Vec2 x y) = Vec2 (-x) (-y)
    s *^ Vec2 x y = Vec2 (s * x) (s * y)
instance GLVec Vec2 where
    Vec2 x1 y1 <.> Vec2 x2 y2 = x1 * x2 + y1 * y2
    minV (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (min x1 x2) (min y1 y2)
    maxV (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (max x1 x2) (max y1 y2)
    Vec2 x1 y1 ^*^ Vec2 x2 y2 = Vec2 (x1 * x2) (y1 * y2)
    Vec2 x1 y1 ^/^ Vec2 x2 y2 = Vec2 (x1 / x2) (y1 / y2)

data Vec3 = Vec3
    {-# UNPACK #-} !GLfloat
    {-# UNPACK #-} !GLfloat
    {-# UNPACK #-} !GLfloat
  deriving (Show)
instance VectorSpace Vec3 where
    zeroV = Vec3 0 0 0
    Vec3 x1 y1 z1 ^+^ Vec3 x2 y2 z2 = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
    negateV (Vec3 x y z) = Vec3 (-x) (-y) (-z)
    s *^ Vec3 x y z = Vec3 (s * x) (s * y) (s * z)
instance GLVec Vec3 where
    Vec3 x1 y1 z1 <.> Vec3 x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2
    minV (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (min x1 x2) (min y1 y2) (min z1 z2)
    maxV (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (max x1 x2) (max y1 y2) (max z1 z2)
    Vec3 x1 y1 z1 ^*^ Vec3 x2 y2 z2 = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)
    Vec3 x1 y1 z1 ^/^ Vec3 x2 y2 z2 = Vec3 (x1 / x2) (y1 / y2) (z1 / z2)

cross :: Vec3 -> Vec3 -> Vec3
Vec3 x1 y1 z1 `cross` Vec3 x2 y2 z2 =
    Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

data Vec4 = Vec4
    {-# UNPACK #-} !GLfloat
    {-# UNPACK #-} !GLfloat
    {-# UNPACK #-} !GLfloat
    {-# UNPACK #-} !GLfloat
  deriving (Show)
instance VectorSpace Vec4 where
    zeroV = Vec4 0 0 0 0
    Vec4 x1 y1 z1 w1 ^+^ Vec4 x2 y2 z2 w2 = Vec4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
    negateV (Vec4 x y z w) = Vec4 (-x) (-y) (-z) (-w)
    s *^ Vec4 x y z w = Vec4 (s * x) (s * y) (s * z) (s * w)
instance GLVec Vec4 where
    Vec4 x1 y1 z1 w1 <.> Vec4 x2 y2 z2 w2 = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2
    minV (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (min x1 x2) (min y1 y2) (min z1 z2) (min w1 w2)
    maxV (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (max x1 x2) (max y1 y2) (max z1 z2) (max w1 w2)
    Vec4 x1 y1 z1 w1 ^*^ Vec4 x2 y2 z2 w2 = Vec4 (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
    Vec4 x1 y1 z1 w1 ^/^ Vec4 x2 y2 z2 w2 = Vec4 (x1 / x2) (y1 / y2) (z1 / z2) (w1 / w2)

-- In matrices, each vector represents a column.

data Mat2 = Mat2
    {-# UNPACK #-} !Vec2 {-# UNPACK #-} !Vec2
  deriving (Show)
instance VectorSpace Mat2 where
    zeroV = Mat2 zeroV zeroV
    Mat2 x1 y1 ^+^ Mat2 x2 y2 = Mat2 (x1 ^+^ x2) (y1 ^+^ y2)
    negateV (Mat2 x y) = Mat2 (negateV x) (negateV y)
    s *^ Mat2 x y = Mat2 (s *^ x) (s *^ y)
instance GLSquareMat Mat2 where
    type InOutVec Mat2 = Vec2
    Mat2 (Vec2 m11 m21) (Vec2 m12 m22) ^@ Vec2 v1 v2 = Vec2
        (m11 * v1 + m12 * v2)
        (m21 * v1 + m22 * v2)
    identityMatrix = Mat2
        (Vec2 1 0)
        (Vec2 0 1)
    Mat2 (Vec2 a11 a21) (Vec2 a12 a22) ^.^ Mat2 (Vec2 b11 b21) (Vec2 b12 b22) = Mat2
        (Vec2
            (a11 * b11 + a12 * b21)
            (a21 * b11 + a22 * b21)
        )
        (Vec2
            (a11 * b12 + a12 * b22)
            (a21 * b12 + a22 * b22)
        )

data Mat3 = Mat3
    {-# UNPACK #-} !Vec3 {-# UNPACK #-} !Vec3 {-# UNPACK #-} !Vec3
  deriving (Show)
instance VectorSpace Mat3 where
    zeroV = Mat3 zeroV zeroV zeroV
    Mat3 x1 y1 z1 ^+^ Mat3 x2 y2 z2 = Mat3 (x1 ^+^ x2) (y1 ^+^ y2) (z1 ^+^ z2)
    negateV (Mat3 x y z) = Mat3 (negateV x) (negateV y) (negateV z)
    s *^ Mat3 x y z = Mat3 (s *^ x) (s *^ y) (s *^ z)
instance GLSquareMat Mat3 where
    type InOutVec Mat3 = Vec3
    Mat3 (Vec3 m11 m21 m31) (Vec3 m12 m22 m32) (Vec3 m13 m23 m33) ^@ Vec3 v1 v2 v3 = Vec3
        (m11 * v1 + m12 * v2 + m13 * v3)
        (m21 * v1 + m22 * v2 + m23 * v3)
        (m31 * v1 + m32 * v2 + m33 * v3)
    identityMatrix = Mat3
        (Vec3 1 0 0)
        (Vec3 0 1 0)
        (Vec3 0 0 1)
    Mat3 (Vec3 a11 a21 a31) (Vec3 a12 a22 a32) (Vec3 a13 a23 a33) ^.^
        Mat3 (Vec3 b11 b21 b31) (Vec3 b12 b22 b32) (Vec3 b13 b23 b33) = Mat3
        (Vec3
            (a11 * b11 + a12 * b21 + a13 * b31)
            (a21 * b11 + a22 * b21 + a23 * b31)
            (a31 * b11 + a32 * b21 + a33 * b31)
        )
        (Vec3
            (a11 * b12 + a12 * b22 + a13 * b32)
            (a21 * b12 + a22 * b22 + a23 * b32)
            (a31 * b12 + a32 * b22 + a33 * b32)
        )
        (Vec3
            (a11 * b13 + a12 * b23 + a13 * b33)
            (a21 * b13 + a22 * b23 + a23 * b33)
            (a31 * b13 + a32 * b23 + a33 * b33)
        )

data Mat4 = Mat4
    {-# UNPACK #-} !Vec4 {-# UNPACK #-} !Vec4 {-# UNPACK #-} !Vec4 {-# UNPACK #-} !Vec4
  deriving (Show)
instance VectorSpace Mat4 where
    zeroV = Mat4 zeroV zeroV zeroV zeroV
    Mat4 x1 y1 z1 w1 ^+^ Mat4 x2 y2 z2 w2 = Mat4 (x1 ^+^ x2) (y1 ^+^ y2) (z1 ^+^ z2) (w1 ^+^ w2)
    negateV (Mat4 x y z w) = Mat4 (negateV x) (negateV y) (negateV z) (negateV w)
    s *^ Mat4 x y z w = Mat4 (s *^ x) (s *^ y) (s *^ z) (s *^ w)
instance GLSquareMat Mat4 where
    type InOutVec Mat4 = Vec4
    Mat4 (Vec4 m11 m21 m31 m41) (Vec4 m12 m22 m32 m42) (Vec4 m13 m23 m33 m43) (Vec4 m14 m24 m34 m44) ^@
        Vec4 v1 v2 v3 v4 = Vec4
            (m11 * v1 + m12 * v2 + m13 * v3 + m14 * v4)
            (m21 * v1 + m22 * v2 + m23 * v3 + m24 * v4)
            (m31 * v1 + m32 * v2 + m33 * v3 + m34 * v4)
            (m41 * v1 + m42 * v2 + m43 * v3 + m44 * v4)
    identityMatrix = Mat4
        (Vec4 1 0 0 0)
        (Vec4 0 1 0 0)
        (Vec4 0 0 1 0)
        (Vec4 0 0 0 1)
    Mat4 (Vec4 a11 a21 a31 a41) (Vec4 a12 a22 a32 a42) (Vec4 a13 a23 a33 a43) (Vec4 a14 a24 a34 a44) ^.^
        Mat4 (Vec4 b11 b21 b31 b41) (Vec4 b12 b22 b32 b42) (Vec4 b13 b23 b33 b43) (Vec4 b14 b24 b34 b44) = Mat4
        (Vec4
            (a11 * b11 + a12 * b21 + a13 * b31 + a14 * b41)
            (a21 * b11 + a22 * b21 + a23 * b31 + a24 * b41)
            (a31 * b11 + a32 * b21 + a33 * b31 + a34 * b41)
            (a41 * b11 + a42 * b21 + a43 * b31 + a44 * b41)
        )
        (Vec4
            (a11 * b12 + a12 * b22 + a13 * b32 + a14 * b42)
            (a21 * b12 + a22 * b22 + a23 * b32 + a24 * b42)
            (a31 * b12 + a32 * b22 + a33 * b32 + a34 * b42)
            (a41 * b12 + a42 * b22 + a43 * b32 + a44 * b42)
        )
        (Vec4
            (a11 * b13 + a12 * b23 + a13 * b33 + a14 * b43)
            (a21 * b13 + a22 * b23 + a23 * b33 + a24 * b43)
            (a31 * b13 + a32 * b23 + a33 * b33 + a34 * b43)
            (a41 * b13 + a42 * b23 + a43 * b33 + a44 * b43)
        )
        (Vec4
            (a11 * b14 + a12 * b24 + a13 * b34 + a14 * b44)
            (a21 * b14 + a22 * b24 + a23 * b34 + a24 * b44)
            (a31 * b14 + a32 * b24 + a33 * b34 + a34 * b44)
            (a41 * b14 + a42 * b24 + a43 * b34 + a44 * b44)
        )

translationMatrix2 :: Vec2 -> Mat3
translationMatrix2 (Vec2 x y) = Mat3 (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 x y 1)

translationMatrix3 :: Vec3 -> Mat4
translationMatrix3 (Vec3 x y z) = Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 x y z 1)

stretchMatrix2 :: Vec2 -> Mat3
stretchMatrix2 (Vec2 x y) = Mat3 (Vec3 x 0 0) (Vec3 0 y 0) (Vec3 0 0 1)

stretchMatrix3 :: Vec3 -> Mat4
stretchMatrix3 (Vec3 x y z) = Mat4 (Vec4 x 0 0 0) (Vec4 0 y 0 0) (Vec4 0 0 z 0) (Vec4 0 0 0 1)

xRotationMatrix :: GLfloat -> Mat4
xRotationMatrix theta = Mat4 (Vec4 1 0 0 0) (Vec4 0 c s 0) (Vec4 0 (-s) c 0) (Vec4 0 0 0 1)
  where c = cos theta
        s = sin theta

yRotationMatrix :: GLfloat -> Mat4
yRotationMatrix theta = Mat4 (Vec4 c 0 (-s) 0) (Vec4 0 1 0 0) (Vec4 s 0 c 0) (Vec4 0 0 0 1)
  where c = cos theta
        s = sin theta

zRotationMatrix :: GLfloat -> Mat4
zRotationMatrix theta = Mat4 (Vec4 c s 0 0) (Vec4 (-s) c 0 0) (Vec4 0 0 1 0) (Vec4 0 0 0 1)
  where c = cos theta
        s = sin theta

lookAtMatrix :: Vec3 -> Vec3 -> Vec3 -> Mat4
lookAtMatrix eye center up =
    let f@(Vec3 fx fy fz) = normalized (eye ^-^ center)
        s@(Vec3 sx sy sz) = normalized (normalized up `cross` f)
        u@(Vec3 ux uy uz) = f `cross` s
    in Mat4 (Vec4 sx ux fx 0)
            (Vec4 sy uy fy 0)
            (Vec4 sz uz fz 0)
            (Vec4 (-s <.> eye) (-u <.> eye) (-f <.> eye) 1)

perspectiveMatrix :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Mat4
perspectiveMatrix fovy aspectRatio zNear zFar =
    let range  = tan (fovy / 2) * zNear
        left   = -range * aspectRatio
        right  =  range * aspectRatio
        bottom = -range
        top    =  range
    in Mat4 (Vec4 ((2 * zNear) / (right - left)) 0 0 0)
            (Vec4 0 ((2 * zNear) / (top - bottom)) 0 0)
            (Vec4 0 0 (-(zFar + zNear) / (zFar - zNear)) (-1))
            (Vec4 0 0 (-(2 * zFar * zNear) / (zFar - zNear)) 0)

orthoMatrix :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Mat4
orthoMatrix left right bottom top zNear zFar =
    let m00 = 2 / (right - left)
        m11 = 2 / (top - bottom)
        m22 = -2 / (zFar - zNear)
        m30 = -(right + left) / (right - left)
        m31 = -(top + bottom) / (top - bottom)
        m32 = -(zFar + zNear) / (zFar - zNear)
    in Mat4 (Vec4 m00 0 0 0)
            (Vec4 0 m11 0 0)
            (Vec4 0 0 m22 0)
            (Vec4 m30 m31 m32 1)
