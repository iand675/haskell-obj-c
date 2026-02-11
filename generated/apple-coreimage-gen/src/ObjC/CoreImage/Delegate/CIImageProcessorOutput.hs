{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CIImageProcessorOutput@.
--
-- Usage:
--
-- @
-- delegate <- newCIImageProcessorOutput defaultCIImageProcessorOutputOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreImage.Delegate.CIImageProcessorOutput
  ( CIImageProcessorOutputOverrides(..)
  , defaultCIImageProcessorOutputOverrides
  , newCIImageProcessorOutput
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

-- | Overrides record for @\@protocol CIImageProcessorOutput@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CIImageProcessorOutputOverrides = CIImageProcessorOutputOverrides
  { _bytesPerRow :: !(Maybe (IO Int))
  , _format :: !(Maybe (IO Int))
  , _baseAddress :: !(Maybe (IO RawId))
  , _metalTexture :: !(Maybe (IO RawId))
  , _metalCommandBuffer :: !(Maybe (IO RawId))
  , _digest :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultCIImageProcessorOutputOverrides :: CIImageProcessorOutputOverrides
defaultCIImageProcessorOutputOverrides = CIImageProcessorOutputOverrides
  { _bytesPerRow = Nothing
  , _format = Nothing
  , _baseAddress = Nothing
  , _metalTexture = Nothing
  , _metalCommandBuffer = Nothing
  , _digest = Nothing
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
{-# NOINLINE ciImageProcessorOutputDelegateClass #-}
ciImageProcessorOutputDelegateClass :: Class
ciImageProcessorOutputDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCIImageProcessorOutput" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_bytesPerRow = unSelector (mkSelector "bytesPerRow")
      sel_format = unSelector (mkSelector "format")
      sel_baseAddress = unSelector (mkSelector "baseAddress")
      sel_metalTexture = unSelector (mkSelector "metalTexture")
      sel_metalCommandBuffer = unSelector (mkSelector "metalCommandBuffer")
      sel_digest = unSelector (mkSelector "digest")
  -- bytesPerRow
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorOutputOverrides
    case _bytesPerRow rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "bytesPerRow" "Q@:" stub_0

  -- format
  stub_1 <- wrap_i $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorOutputOverrides
    case _format rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "format" "i@:" stub_1

  -- baseAddress
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorOutputOverrides
    case _baseAddress rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "baseAddress" "@@:" stub_2

  -- metalTexture
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorOutputOverrides
    case _metalTexture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "metalTexture" "@@:" stub_3

  -- metalCommandBuffer
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorOutputOverrides
    case _metalCommandBuffer rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "metalCommandBuffer" "@@:" stub_4

  -- digest
  stub_5 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorOutputOverrides
    case _digest rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "digest" "Q@:" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CIImageProcessorOutputOverrides
    if queriedSel == sel_bytesPerRow then pure (maybe 0 (const 1) (_bytesPerRow rec_))
    else if queriedSel == sel_format then pure (maybe 0 (const 1) (_format rec_))
    else if queriedSel == sel_baseAddress then pure (maybe 0 (const 1) (_baseAddress rec_))
    else if queriedSel == sel_metalTexture then pure (maybe 0 (const 1) (_metalTexture rec_))
    else if queriedSel == sel_metalCommandBuffer then pure (maybe 0 (const 1) (_metalCommandBuffer rec_))
    else if queriedSel == sel_digest then pure (maybe 0 (const 1) (_digest rec_))
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
newCIImageProcessorOutput :: CIImageProcessorOutputOverrides -> IO RawId
newCIImageProcessorOutput overrides = do
  inst <- class_createInstance ciImageProcessorOutputDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
