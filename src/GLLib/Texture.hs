{-# LANGUAGE BangPatterns, RankNTypes #-}

module GLLib.Texture (
    TextureFormat, rgb8TextureFormat, rgba8TextureFormat,
    ColorSpace(..),
    readTexture,
    CubeMapFace(..),
    readCubeMapTexture, readMipmappedCubeMapTexture,
    writeActiveFramebufferAsPNG
) where

import Codec.Picture
import Control.Monad(forM_, when)
import Foreign.Marshal.Alloc(allocaBytes)
import Foreign.Ptr(Ptr)
import Foreign.Storable(peekByteOff, pokeByteOff)
import Graphics.GL

import GLLib.Utils

data TextureFormat p = TextureFormat {
    tfBytesPerPixel :: !Int,
    tfGLPixelFormat :: !GLenum,
    tfGLPixelInternalFormat :: ColorSpace -> GLenum,
    tfCreateImage :: Image p -> DynamicImage,
    tfExtractImage :: DynamicImage -> Maybe (Image p),
    tfReadPixel :: forall a. Ptr a -> Int -> IO p,
    tfWritePixel :: forall a. Ptr a -> Int -> p -> IO ()
  }

rgb8TextureFormat :: TextureFormat PixelRGB8
rgb8TextureFormat = TextureFormat {
    tfBytesPerPixel = 3,
    tfGLPixelFormat = GL_RGB,
    tfGLPixelInternalFormat = \cs -> case cs of { LinearColorSpace -> GL_RGB8; SRGBColorSpace -> GL_SRGB8 },
    tfCreateImage = ImageRGB8,
    tfExtractImage = \dynImg -> case dynImg of { ImageRGB8 img -> Just img; _ -> Nothing },
    tfReadPixel = \ptr baseOffset -> do
        r <- peekByteOff ptr baseOffset
        g <- peekByteOff ptr (baseOffset + 1)
        b <- peekByteOff ptr (baseOffset + 2)
        return $ PixelRGB8 r g b
    , tfWritePixel = \ptr baseOffset (PixelRGB8 r g b) -> do
        pokeByteOff ptr baseOffset r
        pokeByteOff ptr (baseOffset + 1) g
        pokeByteOff ptr (baseOffset + 2) b
  }

rgba8TextureFormat :: TextureFormat PixelRGBA8
rgba8TextureFormat = TextureFormat {
    tfBytesPerPixel = 4,
    tfGLPixelFormat = GL_RGBA,
    tfGLPixelInternalFormat = \cs -> case cs of { LinearColorSpace -> GL_RGBA8; SRGBColorSpace -> GL_SRGB8_ALPHA8 },
    tfCreateImage = ImageRGBA8,
    tfExtractImage = \dynImg -> case dynImg of { ImageRGBA8 img -> Just img; _ -> Nothing },
    tfReadPixel = \ptr baseOffset -> do
        r <- peekByteOff ptr baseOffset
        g <- peekByteOff ptr (baseOffset + 1)
        b <- peekByteOff ptr (baseOffset + 2)
        a <- peekByteOff ptr (baseOffset + 3)
        return $ PixelRGBA8 r g b a
    , tfWritePixel = \ptr baseOffset (PixelRGBA8 r g b a) -> do
        pokeByteOff ptr baseOffset r
        pokeByteOff ptr (baseOffset + 1) g
        pokeByteOff ptr (baseOffset + 2) b
        pokeByteOff ptr (baseOffset + 3) a
  }

data ColorSpace = LinearColorSpace | SRGBColorSpace
    deriving (Show, Eq, Ord)

readTexture :: Pixel p => FilePath -> TextureFormat p -> ColorSpace ->
    GLenum -> GLenum -> GLenum -> GLenum -> Bool -> IO GLuint
readTexture filePath tf colorSpace magFilter minFilter wrapS wrapT generateMipmap = do
    image <- readRequiredImage tf filePath
    
    textureID <- gen glGenTextures
    glBindTexture GL_TEXTURE_2D textureID
    
    uploadTexture tf colorSpace GL_TEXTURE_2D 0 image
    
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral magFilter)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral minFilter)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral wrapS)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral wrapT)
    when generateMipmap $
        glGenerateMipmap GL_TEXTURE_2D
    
    return textureID

data CubeMapFace = PosXFace | NegXFace | PosYFace | NegYFace | PosZFace | NegZFace
    deriving (Show, Eq, Ord, Bounded, Enum)

cubeMapFaceToGLTarget :: CubeMapFace -> GLenum
cubeMapFaceToGLTarget PosXFace = GL_TEXTURE_CUBE_MAP_POSITIVE_X
cubeMapFaceToGLTarget NegXFace = GL_TEXTURE_CUBE_MAP_NEGATIVE_X
cubeMapFaceToGLTarget PosYFace = GL_TEXTURE_CUBE_MAP_POSITIVE_Y
cubeMapFaceToGLTarget NegYFace = GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
cubeMapFaceToGLTarget PosZFace = GL_TEXTURE_CUBE_MAP_POSITIVE_Z
cubeMapFaceToGLTarget NegZFace = GL_TEXTURE_CUBE_MAP_NEGATIVE_Z

readCubeMapTexture :: Pixel p => [(CubeMapFace, FilePath)] -> TextureFormat p ->
    ColorSpace -> GLenum -> GLenum -> Bool -> IO GLuint
readCubeMapTexture filePaths tf colorSpace magFilter minFilter generateMipmap = do
    textureID <- gen glGenTextures
    glBindTexture GL_TEXTURE_CUBE_MAP textureID
    
    forM_ filePaths $ \(face, filePath) -> do
        image <- readRequiredImage tf filePath
        uploadTexture tf colorSpace (cubeMapFaceToGLTarget face) 0 image
    
    configureCubeMapTexParameters magFilter minFilter
    when generateMipmap $
        glGenerateMipmap GL_TEXTURE_CUBE_MAP
    return textureID

readMipmappedCubeMapTexture :: Pixel p => (CubeMapFace -> GLint -> FilePath) -> TextureFormat p ->
    ColorSpace -> GLenum -> GLenum -> GLint -> IO GLuint
readMipmappedCubeMapTexture filePathFunc tf colorSpace magFilter minFilter maxMipmapLevel = do
    textureID <- gen glGenTextures
    glBindTexture GL_TEXTURE_CUBE_MAP textureID
    
    forM_ [minBound..maxBound :: CubeMapFace] $ \face ->
        forM_ [0..maxMipmapLevel] $ \mipmapLevel -> do
            let filePath = filePathFunc face mipmapLevel
            image <- readRequiredImage tf filePath
            uploadTexture tf colorSpace (cubeMapFaceToGLTarget face) mipmapLevel image
    
    configureCubeMapTexParameters magFilter minFilter
    return textureID

configureCubeMapTexParameters :: GLenum -> GLenum -> IO ()
configureCubeMapTexParameters magFilter minFilter = do
    glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_MAG_FILTER (fromIntegral magFilter)
    glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_MIN_FILTER (fromIntegral minFilter)
    -- These are ignored if GL_TEXTURE_CUBE_MAP_SEAMLESS is on, but are important if it is off.
    glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_S (fromIntegral GL_CLAMP_TO_EDGE)
    glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_T (fromIntegral GL_CLAMP_TO_EDGE)
    glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_R (fromIntegral GL_CLAMP_TO_EDGE)

readRequiredImage :: TextureFormat p -> FilePath -> IO (Image p)
readRequiredImage tf filePath = do
    maybeImage <- readImage filePath
    case maybeImage of
        Left msg -> error msg
        Right imageData -> case tfExtractImage tf imageData of
            Nothing -> error "Unsupported image type."
            Just image -> return image

uploadTexture :: Pixel p => TextureFormat p -> ColorSpace -> GLenum -> GLint -> Image p -> IO ()
uploadTexture tf colorSpace target mipmapLevel image = do
    let width = imageWidth image
        height = imageHeight image
    allocaBytes (width * height * tfBytesPerPixel tf) $ \ptr -> do
        forM_ [0..height - 1] $ \destY -> do
            -- OpenGL treats (0,0) as being the lower-left corner of the image, but JuicyPixels
            -- treats (0,0) as being the upper-left corner. So we need to flip the Y coordinate on
            -- load so that everything sees the same image, just indexed differently.
            let !srcY = (height - 1) - destY
            forM_ [0..width - 1] $ \x -> do
                let pixel = pixelAt image x srcY
                    baseOffset = (destY * width + x) * tfBytesPerPixel tf
                tfWritePixel tf ptr baseOffset pixel
        glTexImage2D target mipmapLevel (fromIntegral (tfGLPixelInternalFormat tf colorSpace))
            (fromIntegral width) (fromIntegral height) 0 (tfGLPixelFormat tf) GL_UNSIGNED_BYTE ptr

writeActiveFramebufferAsPNG :: Pixel p => FilePath -> TextureFormat p -> GLsizei -> GLsizei -> IO ()
writeActiveFramebufferAsPNG filePath tf width height = do
    allocaBytes (fromIntegral width * fromIntegral height * tfBytesPerPixel tf) $ \ptr -> do
        glReadPixels 0 0 width height (tfGLPixelFormat tf) GL_UNSIGNED_BYTE ptr
        image <- withImage (fromIntegral width) (fromIntegral height) $ \x destY -> do
            let srcY = (fromIntegral height - 1) - destY
                baseOffset = (srcY * fromIntegral width + x) * tfBytesPerPixel tf
            tfReadPixel tf ptr baseOffset
        savePngImage filePath (tfCreateImage tf image)
