{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol METrackReader@.
--
-- Usage:
--
-- @
-- delegate <- newMETrackReader defaultMETrackReaderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MediaExtension.Delegate.METrackReader
  ( METrackReaderOverrides(..)
  , defaultMETrackReaderOverrides
  , newMETrackReader
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

-- | Overrides record for @\@protocol METrackReader@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data METrackReaderOverrides = METrackReaderOverrides
  { _generateSampleCursorAtFirstSampleInDecodeOrderWithCompletionHandler :: !(Maybe (RawId -> IO ()))
  , _generateSampleCursorAtLastSampleInDecodeOrderWithCompletionHandler :: !(Maybe (RawId -> IO ()))
  , _loadMetadataWithCompletionHandler :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMETrackReaderOverrides :: METrackReaderOverrides
defaultMETrackReaderOverrides = METrackReaderOverrides
  { _generateSampleCursorAtFirstSampleInDecodeOrderWithCompletionHandler = Nothing
  , _generateSampleCursorAtLastSampleInDecodeOrderWithCompletionHandler = Nothing
  , _loadMetadataWithCompletionHandler = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE meTrackReaderDelegateClass #-}
meTrackReaderDelegateClass :: Class
meTrackReaderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMETrackReader" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_generateSampleCursorAtFirstSampleInDecodeOrderWithCompletionHandler = unSelector (mkSelector "generateSampleCursorAtFirstSampleInDecodeOrderWithCompletionHandler:")
      sel_generateSampleCursorAtLastSampleInDecodeOrderWithCompletionHandler = unSelector (mkSelector "generateSampleCursorAtLastSampleInDecodeOrderWithCompletionHandler:")
      sel_loadMetadataWithCompletionHandler = unSelector (mkSelector "loadMetadataWithCompletionHandler:")
  -- generateSampleCursorAtFirstSampleInDecodeOrderWithCompletionHandler:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO METrackReaderOverrides
    case _generateSampleCursorAtFirstSampleInDecodeOrderWithCompletionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "generateSampleCursorAtFirstSampleInDecodeOrderWithCompletionHandler:" "v@:@" stub_0

  -- generateSampleCursorAtLastSampleInDecodeOrderWithCompletionHandler:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO METrackReaderOverrides
    case _generateSampleCursorAtLastSampleInDecodeOrderWithCompletionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "generateSampleCursorAtLastSampleInDecodeOrderWithCompletionHandler:" "v@:@" stub_1

  -- loadMetadataWithCompletionHandler:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO METrackReaderOverrides
    case _loadMetadataWithCompletionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "loadMetadataWithCompletionHandler:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO METrackReaderOverrides
    if queriedSel == sel_generateSampleCursorAtFirstSampleInDecodeOrderWithCompletionHandler then pure (maybe 0 (const 1) (_generateSampleCursorAtFirstSampleInDecodeOrderWithCompletionHandler rec_))
    else if queriedSel == sel_generateSampleCursorAtLastSampleInDecodeOrderWithCompletionHandler then pure (maybe 0 (const 1) (_generateSampleCursorAtLastSampleInDecodeOrderWithCompletionHandler rec_))
    else if queriedSel == sel_loadMetadataWithCompletionHandler then pure (maybe 0 (const 1) (_loadMetadataWithCompletionHandler rec_))
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
newMETrackReader :: METrackReaderOverrides -> IO RawId
newMETrackReader overrides = do
  inst <- class_createInstance meTrackReaderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
