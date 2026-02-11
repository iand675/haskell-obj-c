{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MERAWProcessor@.
--
-- Usage:
--
-- @
-- delegate <- newMERAWProcessor defaultMERAWProcessorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MediaExtension.Delegate.MERAWProcessor
  ( MERAWProcessorOverrides(..)
  , defaultMERAWProcessorOverrides
  , newMERAWProcessor
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

-- | Overrides record for @\@protocol MERAWProcessor@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MERAWProcessorOverrides = MERAWProcessorOverrides
  { _metalDeviceRegistryID :: !(Maybe (IO Int))
  , _setMetalDeviceRegistryID :: !(Maybe (Int -> IO ()))
  , _outputColorAttachments :: !(Maybe (IO RawId))
  , _metadataForSidecarFile :: !(Maybe (IO RawId))
  , _processingParameters :: !(Maybe (IO RawId))
  , _readyForMoreMediaData :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultMERAWProcessorOverrides :: MERAWProcessorOverrides
defaultMERAWProcessorOverrides = MERAWProcessorOverrides
  { _metalDeviceRegistryID = Nothing
  , _setMetalDeviceRegistryID = Nothing
  , _outputColorAttachments = Nothing
  , _metadataForSidecarFile = Nothing
  , _processingParameters = Nothing
  , _readyForMoreMediaData = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE merawProcessorDelegateClass #-}
merawProcessorDelegateClass :: Class
merawProcessorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMERAWProcessor" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_metalDeviceRegistryID = unSelector (mkSelector "metalDeviceRegistryID")
      sel_setMetalDeviceRegistryID = unSelector (mkSelector "setMetalDeviceRegistryID:")
      sel_outputColorAttachments = unSelector (mkSelector "outputColorAttachments")
      sel_metadataForSidecarFile = unSelector (mkSelector "metadataForSidecarFile")
      sel_processingParameters = unSelector (mkSelector "processingParameters")
      sel_readyForMoreMediaData = unSelector (mkSelector "readyForMoreMediaData")
  -- metalDeviceRegistryID
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MERAWProcessorOverrides
    case _metalDeviceRegistryID rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "metalDeviceRegistryID" "Q@:" stub_0

  -- setMetalDeviceRegistryID:
  stub_1 <- wrap_Q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MERAWProcessorOverrides
    case _setMetalDeviceRegistryID rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setMetalDeviceRegistryID:" "v@:Q" stub_1

  -- outputColorAttachments
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MERAWProcessorOverrides
    case _outputColorAttachments rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outputColorAttachments" "@@:" stub_2

  -- metadataForSidecarFile
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MERAWProcessorOverrides
    case _metadataForSidecarFile rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "metadataForSidecarFile" "@@:" stub_3

  -- processingParameters
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MERAWProcessorOverrides
    case _processingParameters rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "processingParameters" "@@:" stub_4

  -- readyForMoreMediaData
  stub_5 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MERAWProcessorOverrides
    case _readyForMoreMediaData rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "readyForMoreMediaData" "B@:" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MERAWProcessorOverrides
    if queriedSel == sel_metalDeviceRegistryID then pure (maybe 0 (const 1) (_metalDeviceRegistryID rec_))
    else if queriedSel == sel_setMetalDeviceRegistryID then pure (maybe 0 (const 1) (_setMetalDeviceRegistryID rec_))
    else if queriedSel == sel_outputColorAttachments then pure (maybe 0 (const 1) (_outputColorAttachments rec_))
    else if queriedSel == sel_metadataForSidecarFile then pure (maybe 0 (const 1) (_metadataForSidecarFile rec_))
    else if queriedSel == sel_processingParameters then pure (maybe 0 (const 1) (_processingParameters rec_))
    else if queriedSel == sel_readyForMoreMediaData then pure (maybe 0 (const 1) (_readyForMoreMediaData rec_))
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
newMERAWProcessor :: MERAWProcessorOverrides -> IO RawId
newMERAWProcessor overrides = do
  inst <- class_createInstance merawProcessorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
