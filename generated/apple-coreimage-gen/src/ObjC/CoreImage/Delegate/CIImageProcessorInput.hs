{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CIImageProcessorInput@.
--
-- Usage:
--
-- @
-- delegate <- newCIImageProcessorInput defaultCIImageProcessorInputOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreImage.Delegate.CIImageProcessorInput
  ( CIImageProcessorInputOverrides(..)
  , defaultCIImageProcessorInputOverrides
  , newCIImageProcessorInput
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

-- | Overrides record for @\@protocol CIImageProcessorInput@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CIImageProcessorInputOverrides = CIImageProcessorInputOverrides
  { _bytesPerRow :: !(Maybe (IO Int))
  , _format :: !(Maybe (IO Int))
  , _baseAddress :: !(Maybe (IO RawId))
  , _metalTexture :: !(Maybe (IO RawId))
  , _digest :: !(Maybe (IO Int))
  , _roiTileIndex :: !(Maybe (IO Int))
  , _roiTileCount :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultCIImageProcessorInputOverrides :: CIImageProcessorInputOverrides
defaultCIImageProcessorInputOverrides = CIImageProcessorInputOverrides
  { _bytesPerRow = Nothing
  , _format = Nothing
  , _baseAddress = Nothing
  , _metalTexture = Nothing
  , _digest = Nothing
  , _roiTileIndex = Nothing
  , _roiTileCount = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_i
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CInt)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CInt))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ciImageProcessorInputDelegateClass #-}
ciImageProcessorInputDelegateClass :: Class
ciImageProcessorInputDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCIImageProcessorInput" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_bytesPerRow = unSelector (mkSelector "bytesPerRow")
      sel_format = unSelector (mkSelector "format")
      sel_baseAddress = unSelector (mkSelector "baseAddress")
      sel_metalTexture = unSelector (mkSelector "metalTexture")
      sel_digest = unSelector (mkSelector "digest")
      sel_roiTileIndex = unSelector (mkSelector "roiTileIndex")
      sel_roiTileCount = unSelector (mkSelector "roiTileCount")
  -- bytesPerRow
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorInputOverrides
    case _bytesPerRow rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "bytesPerRow" "Q@:" stub_0

  -- format
  stub_1 <- wrap_i $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorInputOverrides
    case _format rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "format" "i@:" stub_1

  -- baseAddress
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorInputOverrides
    case _baseAddress rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "baseAddress" "@@:" stub_2

  -- metalTexture
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorInputOverrides
    case _metalTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "metalTexture" "@@:" stub_3

  -- digest
  stub_4 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorInputOverrides
    case _digest rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "digest" "Q@:" stub_4

  -- roiTileIndex
  stub_5 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorInputOverrides
    case _roiTileIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "roiTileIndex" "Q@:" stub_5

  -- roiTileCount
  stub_6 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorInputOverrides
    case _roiTileCount rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "roiTileCount" "Q@:" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorInputOverrides
    if queriedSel == sel_bytesPerRow then pure (maybe 0 (const 1) (_bytesPerRow rec_))
    else if queriedSel == sel_format then pure (maybe 0 (const 1) (_format rec_))
    else if queriedSel == sel_baseAddress then pure (maybe 0 (const 1) (_baseAddress rec_))
    else if queriedSel == sel_metalTexture then pure (maybe 0 (const 1) (_metalTexture rec_))
    else if queriedSel == sel_digest then pure (maybe 0 (const 1) (_digest rec_))
    else if queriedSel == sel_roiTileIndex then pure (maybe 0 (const 1) (_roiTileIndex rec_))
    else if queriedSel == sel_roiTileCount then pure (maybe 0 (const 1) (_roiTileCount rec_))
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
newCIImageProcessorInput :: CIImageProcessorInputOverrides -> IO RawId
newCIImageProcessorInput overrides = do
  inst <- class_createInstance ciImageProcessorInputDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
