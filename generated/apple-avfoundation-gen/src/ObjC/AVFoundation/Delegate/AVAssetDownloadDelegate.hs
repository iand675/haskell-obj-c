{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVAssetDownloadDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVAssetDownloadDelegate defaultAVAssetDownloadDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVAssetDownloadDelegate
  ( AVAssetDownloadDelegateOverrides(..)
  , defaultAVAssetDownloadDelegateOverrides
  , newAVAssetDownloadDelegate
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

-- | Overrides record for @\@protocol AVAssetDownloadDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVAssetDownloadDelegateOverrides = AVAssetDownloadDelegateOverrides
  { _urlSession_assetDownloadTask_didFinishDownloadingToURL :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlSession_assetDownloadTask_didResolveMediaSelection :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlSession_assetDownloadTask_willDownloadToURL :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlSession_aggregateAssetDownloadTask_willDownloadToURL :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlSession_aggregateAssetDownloadTask_didCompleteForMediaSelection :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlSession_assetDownloadTask_willDownloadVariants :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _urlSession_assetDownloadTask_didReceiveMetricEvent :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVAssetDownloadDelegateOverrides :: AVAssetDownloadDelegateOverrides
defaultAVAssetDownloadDelegateOverrides = AVAssetDownloadDelegateOverrides
  { _urlSession_assetDownloadTask_didFinishDownloadingToURL = Nothing
  , _urlSession_assetDownloadTask_didResolveMediaSelection = Nothing
  , _urlSession_assetDownloadTask_willDownloadToURL = Nothing
  , _urlSession_aggregateAssetDownloadTask_willDownloadToURL = Nothing
  , _urlSession_aggregateAssetDownloadTask_didCompleteForMediaSelection = Nothing
  , _urlSession_assetDownloadTask_willDownloadVariants = Nothing
  , _urlSession_assetDownloadTask_didReceiveMetricEvent = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avAssetDownloadDelegateDelegateClass #-}
avAssetDownloadDelegateDelegateClass :: Class
avAssetDownloadDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVAssetDownloadDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_urlSession_assetDownloadTask_didFinishDownloadingToURL = unSelector (mkSelector "URLSession:assetDownloadTask:didFinishDownloadingToURL:")
      sel_urlSession_assetDownloadTask_didResolveMediaSelection = unSelector (mkSelector "URLSession:assetDownloadTask:didResolveMediaSelection:")
      sel_urlSession_assetDownloadTask_willDownloadToURL = unSelector (mkSelector "URLSession:assetDownloadTask:willDownloadToURL:")
      sel_urlSession_aggregateAssetDownloadTask_willDownloadToURL = unSelector (mkSelector "URLSession:aggregateAssetDownloadTask:willDownloadToURL:")
      sel_urlSession_aggregateAssetDownloadTask_didCompleteForMediaSelection = unSelector (mkSelector "URLSession:aggregateAssetDownloadTask:didCompleteForMediaSelection:")
      sel_urlSession_assetDownloadTask_willDownloadVariants = unSelector (mkSelector "URLSession:assetDownloadTask:willDownloadVariants:")
      sel_urlSession_assetDownloadTask_didReceiveMetricEvent = unSelector (mkSelector "URLSession:assetDownloadTask:didReceiveMetricEvent:")
  -- URLSession:assetDownloadTask:didFinishDownloadingToURL:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetDownloadDelegateOverrides
    case _urlSession_assetDownloadTask_didFinishDownloadingToURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:assetDownloadTask:didFinishDownloadingToURL:" "v@:@@@" stub_0

  -- URLSession:assetDownloadTask:didResolveMediaSelection:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetDownloadDelegateOverrides
    case _urlSession_assetDownloadTask_didResolveMediaSelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:assetDownloadTask:didResolveMediaSelection:" "v@:@@@" stub_1

  -- URLSession:assetDownloadTask:willDownloadToURL:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetDownloadDelegateOverrides
    case _urlSession_assetDownloadTask_willDownloadToURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:assetDownloadTask:willDownloadToURL:" "v@:@@@" stub_2

  -- URLSession:aggregateAssetDownloadTask:willDownloadToURL:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetDownloadDelegateOverrides
    case _urlSession_aggregateAssetDownloadTask_willDownloadToURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:aggregateAssetDownloadTask:willDownloadToURL:" "v@:@@@" stub_3

  -- URLSession:aggregateAssetDownloadTask:didCompleteForMediaSelection:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetDownloadDelegateOverrides
    case _urlSession_aggregateAssetDownloadTask_didCompleteForMediaSelection rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:aggregateAssetDownloadTask:didCompleteForMediaSelection:" "v@:@@@" stub_4

  -- URLSession:assetDownloadTask:willDownloadVariants:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetDownloadDelegateOverrides
    case _urlSession_assetDownloadTask_willDownloadVariants rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:assetDownloadTask:willDownloadVariants:" "v@:@@@" stub_5

  -- URLSession:assetDownloadTask:didReceiveMetricEvent:
  stub_6 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetDownloadDelegateOverrides
    case _urlSession_assetDownloadTask_didReceiveMetricEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "URLSession:assetDownloadTask:didReceiveMetricEvent:" "v@:@@@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAssetDownloadDelegateOverrides
    if queriedSel == sel_urlSession_assetDownloadTask_didFinishDownloadingToURL then pure (maybe 0 (const 1) (_urlSession_assetDownloadTask_didFinishDownloadingToURL rec_))
    else if queriedSel == sel_urlSession_assetDownloadTask_didResolveMediaSelection then pure (maybe 0 (const 1) (_urlSession_assetDownloadTask_didResolveMediaSelection rec_))
    else if queriedSel == sel_urlSession_assetDownloadTask_willDownloadToURL then pure (maybe 0 (const 1) (_urlSession_assetDownloadTask_willDownloadToURL rec_))
    else if queriedSel == sel_urlSession_aggregateAssetDownloadTask_willDownloadToURL then pure (maybe 0 (const 1) (_urlSession_aggregateAssetDownloadTask_willDownloadToURL rec_))
    else if queriedSel == sel_urlSession_aggregateAssetDownloadTask_didCompleteForMediaSelection then pure (maybe 0 (const 1) (_urlSession_aggregateAssetDownloadTask_didCompleteForMediaSelection rec_))
    else if queriedSel == sel_urlSession_assetDownloadTask_willDownloadVariants then pure (maybe 0 (const 1) (_urlSession_assetDownloadTask_willDownloadVariants rec_))
    else if queriedSel == sel_urlSession_assetDownloadTask_didReceiveMetricEvent then pure (maybe 0 (const 1) (_urlSession_assetDownloadTask_didReceiveMetricEvent rec_))
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
newAVAssetDownloadDelegate :: AVAssetDownloadDelegateOverrides -> IO RawId
newAVAssetDownloadDelegate overrides = do
  inst <- class_createInstance avAssetDownloadDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
