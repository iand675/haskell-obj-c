{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MESampleCursor@.
--
-- Usage:
--
-- @
-- delegate <- newMESampleCursor defaultMESampleCursorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MediaExtension.Delegate.MESampleCursor
  ( MESampleCursorOverrides(..)
  , defaultMESampleCursorOverrides
  , newMESampleCursor
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

-- | Overrides record for @\@protocol MESampleCursor@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MESampleCursorOverrides = MESampleCursorOverrides
  { _samplesWithEarlierDTSsMayHaveLaterPTSsThanCursor :: !(Maybe (RawId -> IO Bool))
  , _samplesWithLaterDTSsMayHaveEarlierPTSsThanCursor :: !(Maybe (RawId -> IO Bool))
  , _chunkDetailsReturningError :: !(Maybe (RawId -> IO RawId))
  , _sampleLocationReturningError :: !(Maybe (RawId -> IO RawId))
  , _estimatedSampleLocationReturningError :: !(Maybe (RawId -> IO RawId))
  , _loadPostDecodeProcessingMetadataWithCompletionHandler :: !(Maybe (RawId -> IO ()))
  , _currentSampleFormatDescription :: !(Maybe (IO RawId))
  , _hevcDependencyInfo :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMESampleCursorOverrides :: MESampleCursorOverrides
defaultMESampleCursorOverrides = MESampleCursorOverrides
  { _samplesWithEarlierDTSsMayHaveLaterPTSsThanCursor = Nothing
  , _samplesWithLaterDTSsMayHaveEarlierPTSsThanCursor = Nothing
  , _chunkDetailsReturningError = Nothing
  , _sampleLocationReturningError = Nothing
  , _estimatedSampleLocationReturningError = Nothing
  , _loadPostDecodeProcessingMetadataWithCompletionHandler = Nothing
  , _currentSampleFormatDescription = Nothing
  , _hevcDependencyInfo = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE meSampleCursorDelegateClass #-}
meSampleCursorDelegateClass :: Class
meSampleCursorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMESampleCursor" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_samplesWithEarlierDTSsMayHaveLaterPTSsThanCursor = unSelector (mkSelector "samplesWithEarlierDTSsMayHaveLaterPTSsThanCursor:")
      sel_samplesWithLaterDTSsMayHaveEarlierPTSsThanCursor = unSelector (mkSelector "samplesWithLaterDTSsMayHaveEarlierPTSsThanCursor:")
      sel_chunkDetailsReturningError = unSelector (mkSelector "chunkDetailsReturningError:")
      sel_sampleLocationReturningError = unSelector (mkSelector "sampleLocationReturningError:")
      sel_estimatedSampleLocationReturningError = unSelector (mkSelector "estimatedSampleLocationReturningError:")
      sel_loadPostDecodeProcessingMetadataWithCompletionHandler = unSelector (mkSelector "loadPostDecodeProcessingMetadataWithCompletionHandler:")
      sel_currentSampleFormatDescription = unSelector (mkSelector "currentSampleFormatDescription")
      sel_hevcDependencyInfo = unSelector (mkSelector "hevcDependencyInfo")
  -- samplesWithEarlierDTSsMayHaveLaterPTSsThanCursor:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MESampleCursorOverrides
    case _samplesWithEarlierDTSsMayHaveLaterPTSsThanCursor rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "samplesWithEarlierDTSsMayHaveLaterPTSsThanCursor:" "B@:@" stub_0

  -- samplesWithLaterDTSsMayHaveEarlierPTSsThanCursor:
  stub_1 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MESampleCursorOverrides
    case _samplesWithLaterDTSsMayHaveEarlierPTSsThanCursor rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "samplesWithLaterDTSsMayHaveEarlierPTSsThanCursor:" "B@:@" stub_1

  -- chunkDetailsReturningError:
  stub_2 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MESampleCursorOverrides
    case _chunkDetailsReturningError rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "chunkDetailsReturningError:" "@@:@" stub_2

  -- sampleLocationReturningError:
  stub_3 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MESampleCursorOverrides
    case _sampleLocationReturningError rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sampleLocationReturningError:" "@@:@" stub_3

  -- estimatedSampleLocationReturningError:
  stub_4 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MESampleCursorOverrides
    case _estimatedSampleLocationReturningError rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "estimatedSampleLocationReturningError:" "@@:@" stub_4

  -- loadPostDecodeProcessingMetadataWithCompletionHandler:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MESampleCursorOverrides
    case _loadPostDecodeProcessingMetadataWithCompletionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "loadPostDecodeProcessingMetadataWithCompletionHandler:" "v@:@" stub_5

  -- currentSampleFormatDescription
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MESampleCursorOverrides
    case _currentSampleFormatDescription rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "currentSampleFormatDescription" "@@:" stub_6

  -- hevcDependencyInfo
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MESampleCursorOverrides
    case _hevcDependencyInfo rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "hevcDependencyInfo" "@@:" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MESampleCursorOverrides
    if queriedSel == sel_samplesWithEarlierDTSsMayHaveLaterPTSsThanCursor then pure (maybe 0 (const 1) (_samplesWithEarlierDTSsMayHaveLaterPTSsThanCursor rec_))
    else if queriedSel == sel_samplesWithLaterDTSsMayHaveEarlierPTSsThanCursor then pure (maybe 0 (const 1) (_samplesWithLaterDTSsMayHaveEarlierPTSsThanCursor rec_))
    else if queriedSel == sel_chunkDetailsReturningError then pure (maybe 0 (const 1) (_chunkDetailsReturningError rec_))
    else if queriedSel == sel_sampleLocationReturningError then pure (maybe 0 (const 1) (_sampleLocationReturningError rec_))
    else if queriedSel == sel_estimatedSampleLocationReturningError then pure (maybe 0 (const 1) (_estimatedSampleLocationReturningError rec_))
    else if queriedSel == sel_loadPostDecodeProcessingMetadataWithCompletionHandler then pure (maybe 0 (const 1) (_loadPostDecodeProcessingMetadataWithCompletionHandler rec_))
    else if queriedSel == sel_currentSampleFormatDescription then pure (maybe 0 (const 1) (_currentSampleFormatDescription rec_))
    else if queriedSel == sel_hevcDependencyInfo then pure (maybe 0 (const 1) (_hevcDependencyInfo rec_))
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
newMESampleCursor :: MESampleCursorOverrides -> IO RawId
newMESampleCursor overrides = do
  inst <- class_createInstance meSampleCursorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
