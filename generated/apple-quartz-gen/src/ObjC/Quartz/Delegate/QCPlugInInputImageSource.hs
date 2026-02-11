{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol QCPlugInInputImageSource@.
--
-- Usage:
--
-- @
-- delegate <- newQCPlugInInputImageSource defaultQCPlugInInputImageSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Quartz.Delegate.QCPlugInInputImageSource
  ( QCPlugInInputImageSourceOverrides(..)
  , defaultQCPlugInInputImageSourceOverrides
  , newQCPlugInInputImageSource
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol QCPlugInInputImageSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data QCPlugInInputImageSourceOverrides = QCPlugInInputImageSourceOverrides
  { _shouldColorMatch :: !(Maybe (IO Bool))
  , _bufferPixelsWide :: !(Maybe (IO Int))
  , _bufferPixelsHigh :: !(Maybe (IO Int))
  , _bufferPixelFormat :: !(Maybe (IO RawId))
  , _bufferBytesPerRow :: !(Maybe (IO Int))
  , _unlockBufferRepresentation :: !(Maybe (IO ()))
  , _texturePixelsWide :: !(Maybe (IO Int))
  , _texturePixelsHigh :: !(Maybe (IO Int))
  , _textureTarget :: !(Maybe (IO Int))
  , _textureName :: !(Maybe (IO Int))
  , _textureFlipped :: !(Maybe (IO Bool))
  , _textureMatrix :: !(Maybe (IO RawId))
  , _unlockTextureRepresentation :: !(Maybe (IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultQCPlugInInputImageSourceOverrides :: QCPlugInInputImageSourceOverrides
defaultQCPlugInInputImageSourceOverrides = QCPlugInInputImageSourceOverrides
  { _shouldColorMatch = Nothing
  , _bufferPixelsWide = Nothing
  , _bufferPixelsHigh = Nothing
  , _bufferPixelFormat = Nothing
  , _bufferBytesPerRow = Nothing
  , _unlockBufferRepresentation = Nothing
  , _texturePixelsWide = Nothing
  , _texturePixelsHigh = Nothing
  , _textureTarget = Nothing
  , _textureName = Nothing
  , _textureFlipped = Nothing
  , _textureMatrix = Nothing
  , _unlockTextureRepresentation = Nothing
  }

foreign import ccall "wrapper"
  wrap_I
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CUInt)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CUInt))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE qcPlugInInputImageSourceDelegateClass #-}
qcPlugInInputImageSourceDelegateClass :: Class
qcPlugInInputImageSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsQCPlugInInputImageSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_shouldColorMatch = unSelector (mkSelector "shouldColorMatch")
      sel_bufferPixelsWide = unSelector (mkSelector "bufferPixelsWide")
      sel_bufferPixelsHigh = unSelector (mkSelector "bufferPixelsHigh")
      sel_bufferPixelFormat = unSelector (mkSelector "bufferPixelFormat")
      sel_bufferBytesPerRow = unSelector (mkSelector "bufferBytesPerRow")
      sel_unlockBufferRepresentation = unSelector (mkSelector "unlockBufferRepresentation")
      sel_texturePixelsWide = unSelector (mkSelector "texturePixelsWide")
      sel_texturePixelsHigh = unSelector (mkSelector "texturePixelsHigh")
      sel_textureTarget = unSelector (mkSelector "textureTarget")
      sel_textureName = unSelector (mkSelector "textureName")
      sel_textureFlipped = unSelector (mkSelector "textureFlipped")
      sel_textureMatrix = unSelector (mkSelector "textureMatrix")
      sel_unlockTextureRepresentation = unSelector (mkSelector "unlockTextureRepresentation")
  -- shouldColorMatch
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _shouldColorMatch rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldColorMatch" "B@:" stub_0

  -- bufferPixelsWide
  stub_1 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _bufferPixelsWide rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "bufferPixelsWide" "Q@:" stub_1

  -- bufferPixelsHigh
  stub_2 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _bufferPixelsHigh rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "bufferPixelsHigh" "Q@:" stub_2

  -- bufferPixelFormat
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _bufferPixelFormat rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "bufferPixelFormat" "@@:" stub_3

  -- bufferBytesPerRow
  stub_4 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _bufferBytesPerRow rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "bufferBytesPerRow" "Q@:" stub_4

  -- unlockBufferRepresentation
  stub_5 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _unlockBufferRepresentation rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "unlockBufferRepresentation" "v@:" stub_5

  -- texturePixelsWide
  stub_6 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _texturePixelsWide rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "texturePixelsWide" "Q@:" stub_6

  -- texturePixelsHigh
  stub_7 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _texturePixelsHigh rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "texturePixelsHigh" "Q@:" stub_7

  -- textureTarget
  stub_8 <- wrap_I $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _textureTarget rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "textureTarget" "I@:" stub_8

  -- textureName
  stub_9 <- wrap_I $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _textureName rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "textureName" "I@:" stub_9

  -- textureFlipped
  stub_10 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _textureFlipped rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "textureFlipped" "B@:" stub_10

  -- textureMatrix
  stub_11 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _textureMatrix rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "textureMatrix" "@@:" stub_11

  -- unlockTextureRepresentation
  stub_12 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    case _unlockTextureRepresentation rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "unlockTextureRepresentation" "v@:" stub_12

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInInputImageSourceOverrides
    if queriedSel == sel_shouldColorMatch then pure (maybe 0 (const 1) (_shouldColorMatch rec_))
    else if queriedSel == sel_bufferPixelsWide then pure (maybe 0 (const 1) (_bufferPixelsWide rec_))
    else if queriedSel == sel_bufferPixelsHigh then pure (maybe 0 (const 1) (_bufferPixelsHigh rec_))
    else if queriedSel == sel_bufferPixelFormat then pure (maybe 0 (const 1) (_bufferPixelFormat rec_))
    else if queriedSel == sel_bufferBytesPerRow then pure (maybe 0 (const 1) (_bufferBytesPerRow rec_))
    else if queriedSel == sel_unlockBufferRepresentation then pure (maybe 0 (const 1) (_unlockBufferRepresentation rec_))
    else if queriedSel == sel_texturePixelsWide then pure (maybe 0 (const 1) (_texturePixelsWide rec_))
    else if queriedSel == sel_texturePixelsHigh then pure (maybe 0 (const 1) (_texturePixelsHigh rec_))
    else if queriedSel == sel_textureTarget then pure (maybe 0 (const 1) (_textureTarget rec_))
    else if queriedSel == sel_textureName then pure (maybe 0 (const 1) (_textureName rec_))
    else if queriedSel == sel_textureFlipped then pure (maybe 0 (const 1) (_textureFlipped rec_))
    else if queriedSel == sel_textureMatrix then pure (maybe 0 (const 1) (_textureMatrix rec_))
    else if queriedSel == sel_unlockTextureRepresentation then pure (maybe 0 (const 1) (_unlockTextureRepresentation rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newQCPlugInInputImageSource :: QCPlugInInputImageSourceOverrides -> IO RawId
newQCPlugInInputImageSource overrides = do
  inst <- class_createInstance qcPlugInInputImageSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
