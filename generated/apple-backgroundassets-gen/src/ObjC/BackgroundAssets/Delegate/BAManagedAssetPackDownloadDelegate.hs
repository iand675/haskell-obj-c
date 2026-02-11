{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol BAManagedAssetPackDownloadDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newBAManagedAssetPackDownloadDelegate defaultBAManagedAssetPackDownloadDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.BackgroundAssets.Delegate.BAManagedAssetPackDownloadDelegate
  ( BAManagedAssetPackDownloadDelegateOverrides(..)
  , defaultBAManagedAssetPackDownloadDelegateOverrides
  , newBAManagedAssetPackDownloadDelegate
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

-- | Overrides record for @\@protocol BAManagedAssetPackDownloadDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data BAManagedAssetPackDownloadDelegateOverrides = BAManagedAssetPackDownloadDelegateOverrides
  { _downloadOfAssetPackBegan :: !(Maybe (RawId -> IO ()))
  , _downloadOfAssetPackPaused :: !(Maybe (RawId -> IO ()))
  , _downloadOfAssetPack_hasProgress :: !(Maybe (RawId -> RawId -> IO ()))
  , _downloadOfAssetPackFinished :: !(Maybe (RawId -> IO ()))
  , _downloadOfAssetPack_failedWithError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultBAManagedAssetPackDownloadDelegateOverrides :: BAManagedAssetPackDownloadDelegateOverrides
defaultBAManagedAssetPackDownloadDelegateOverrides = BAManagedAssetPackDownloadDelegateOverrides
  { _downloadOfAssetPackBegan = Nothing
  , _downloadOfAssetPackPaused = Nothing
  , _downloadOfAssetPack_hasProgress = Nothing
  , _downloadOfAssetPackFinished = Nothing
  , _downloadOfAssetPack_failedWithError = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE baManagedAssetPackDownloadDelegateDelegateClass #-}
baManagedAssetPackDownloadDelegateDelegateClass :: Class
baManagedAssetPackDownloadDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsBAManagedAssetPackDownloadDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_downloadOfAssetPackBegan = unSelector (mkSelector "downloadOfAssetPackBegan:")
      sel_downloadOfAssetPackPaused = unSelector (mkSelector "downloadOfAssetPackPaused:")
      sel_downloadOfAssetPack_hasProgress = unSelector (mkSelector "downloadOfAssetPack:hasProgress:")
      sel_downloadOfAssetPackFinished = unSelector (mkSelector "downloadOfAssetPackFinished:")
      sel_downloadOfAssetPack_failedWithError = unSelector (mkSelector "downloadOfAssetPack:failedWithError:")
  -- downloadOfAssetPackBegan:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BAManagedAssetPackDownloadDelegateOverrides
    case _downloadOfAssetPackBegan rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "downloadOfAssetPackBegan:" "v@:@" stub_0

  -- downloadOfAssetPackPaused:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BAManagedAssetPackDownloadDelegateOverrides
    case _downloadOfAssetPackPaused rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "downloadOfAssetPackPaused:" "v@:@" stub_1

  -- downloadOfAssetPack:hasProgress:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BAManagedAssetPackDownloadDelegateOverrides
    case _downloadOfAssetPack_hasProgress rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "downloadOfAssetPack:hasProgress:" "v@:@@" stub_2

  -- downloadOfAssetPackFinished:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BAManagedAssetPackDownloadDelegateOverrides
    case _downloadOfAssetPackFinished rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "downloadOfAssetPackFinished:" "v@:@" stub_3

  -- downloadOfAssetPack:failedWithError:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BAManagedAssetPackDownloadDelegateOverrides
    case _downloadOfAssetPack_failedWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "downloadOfAssetPack:failedWithError:" "v@:@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO BAManagedAssetPackDownloadDelegateOverrides
    if queriedSel == sel_downloadOfAssetPackBegan then pure (maybe 0 (const 1) (_downloadOfAssetPackBegan rec_))
    else if queriedSel == sel_downloadOfAssetPackPaused then pure (maybe 0 (const 1) (_downloadOfAssetPackPaused rec_))
    else if queriedSel == sel_downloadOfAssetPack_hasProgress then pure (maybe 0 (const 1) (_downloadOfAssetPack_hasProgress rec_))
    else if queriedSel == sel_downloadOfAssetPackFinished then pure (maybe 0 (const 1) (_downloadOfAssetPackFinished rec_))
    else if queriedSel == sel_downloadOfAssetPack_failedWithError then pure (maybe 0 (const 1) (_downloadOfAssetPack_failedWithError rec_))
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
newBAManagedAssetPackDownloadDelegate :: BAManagedAssetPackDownloadDelegateOverrides -> IO RawId
newBAManagedAssetPackDownloadDelegate overrides = do
  inst <- class_createInstance baManagedAssetPackDownloadDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
