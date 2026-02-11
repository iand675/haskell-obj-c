{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MEFormatReader@.
--
-- Usage:
--
-- @
-- delegate <- newMEFormatReader defaultMEFormatReaderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MediaExtension.Delegate.MEFormatReader
  ( MEFormatReaderOverrides(..)
  , defaultMEFormatReaderOverrides
  , newMEFormatReader
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

-- | Overrides record for @\@protocol MEFormatReader@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MEFormatReaderOverrides = MEFormatReaderOverrides
  { _loadMetadataWithCompletionHandler :: !(Maybe (RawId -> IO ()))
  , _loadTrackReadersWithCompletionHandler :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMEFormatReaderOverrides :: MEFormatReaderOverrides
defaultMEFormatReaderOverrides = MEFormatReaderOverrides
  { _loadMetadataWithCompletionHandler = Nothing
  , _loadTrackReadersWithCompletionHandler = Nothing
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
{-# NOINLINE meFormatReaderDelegateClass #-}
meFormatReaderDelegateClass :: Class
meFormatReaderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMEFormatReader" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_loadMetadataWithCompletionHandler = unSelector (mkSelector "loadMetadataWithCompletionHandler:")
      sel_loadTrackReadersWithCompletionHandler = unSelector (mkSelector "loadTrackReadersWithCompletionHandler:")
  -- loadMetadataWithCompletionHandler:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEFormatReaderOverrides
    case _loadMetadataWithCompletionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "loadMetadataWithCompletionHandler:" "v@:@" stub_0

  -- loadTrackReadersWithCompletionHandler:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEFormatReaderOverrides
    case _loadTrackReadersWithCompletionHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "loadTrackReadersWithCompletionHandler:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEFormatReaderOverrides
    if queriedSel == sel_loadMetadataWithCompletionHandler then pure (maybe 0 (const 1) (_loadMetadataWithCompletionHandler rec_))
    else if queriedSel == sel_loadTrackReadersWithCompletionHandler then pure (maybe 0 (const 1) (_loadTrackReadersWithCompletionHandler rec_))
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
newMEFormatReader :: MEFormatReaderOverrides -> IO RawId
newMEFormatReader overrides = do
  inst <- class_createInstance meFormatReaderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
