{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol FIFinderSync@.
--
-- Usage:
--
-- @
-- delegate <- newFIFinderSync defaultFIFinderSyncOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FinderSync.Delegate.FIFinderSync
  ( FIFinderSyncOverrides(..)
  , defaultFIFinderSyncOverrides
  , newFIFinderSync
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

-- | Overrides record for @\@protocol FIFinderSync@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data FIFinderSyncOverrides = FIFinderSyncOverrides
  { _beginObservingDirectoryAtURL :: !(Maybe (RawId -> IO ()))
  , _endObservingDirectoryAtURL :: !(Maybe (RawId -> IO ()))
  , _requestBadgeIdentifierForURL :: !(Maybe (RawId -> IO ()))
  , _supportedServiceNamesForItemWithURL :: !(Maybe (RawId -> IO RawId))
  , _makeListenerEndpointForServiceName_itemURL_andReturnError :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _valuesForAttributes_forItemWithURL_completion :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _toolbarItemName :: !(Maybe (IO RawId))
  , _toolbarItemImage :: !(Maybe (IO RawId))
  , _toolbarItemToolTip :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultFIFinderSyncOverrides :: FIFinderSyncOverrides
defaultFIFinderSyncOverrides = FIFinderSyncOverrides
  { _beginObservingDirectoryAtURL = Nothing
  , _endObservingDirectoryAtURL = Nothing
  , _requestBadgeIdentifierForURL = Nothing
  , _supportedServiceNamesForItemWithURL = Nothing
  , _makeListenerEndpointForServiceName_itemURL_andReturnError = Nothing
  , _valuesForAttributes_forItemWithURL_completion = Nothing
  , _toolbarItemName = Nothing
  , _toolbarItemImage = Nothing
  , _toolbarItemToolTip = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE fiFinderSyncDelegateClass #-}
fiFinderSyncDelegateClass :: Class
fiFinderSyncDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsFIFinderSync" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_beginObservingDirectoryAtURL = unSelector (mkSelector "beginObservingDirectoryAtURL:")
      sel_endObservingDirectoryAtURL = unSelector (mkSelector "endObservingDirectoryAtURL:")
      sel_requestBadgeIdentifierForURL = unSelector (mkSelector "requestBadgeIdentifierForURL:")
      sel_supportedServiceNamesForItemWithURL = unSelector (mkSelector "supportedServiceNamesForItemWithURL:")
      sel_makeListenerEndpointForServiceName_itemURL_andReturnError = unSelector (mkSelector "makeListenerEndpointForServiceName:itemURL:andReturnError:")
      sel_valuesForAttributes_forItemWithURL_completion = unSelector (mkSelector "valuesForAttributes:forItemWithURL:completion:")
      sel_toolbarItemName = unSelector (mkSelector "toolbarItemName")
      sel_toolbarItemImage = unSelector (mkSelector "toolbarItemImage")
      sel_toolbarItemToolTip = unSelector (mkSelector "toolbarItemToolTip")
  -- beginObservingDirectoryAtURL:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FIFinderSyncOverrides
    case _beginObservingDirectoryAtURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "beginObservingDirectoryAtURL:" "v@:@" stub_0

  -- endObservingDirectoryAtURL:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FIFinderSyncOverrides
    case _endObservingDirectoryAtURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "endObservingDirectoryAtURL:" "v@:@" stub_1

  -- requestBadgeIdentifierForURL:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FIFinderSyncOverrides
    case _requestBadgeIdentifierForURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "requestBadgeIdentifierForURL:" "v@:@" stub_2

  -- supportedServiceNamesForItemWithURL:
  stub_3 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FIFinderSyncOverrides
    case _supportedServiceNamesForItemWithURL rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedServiceNamesForItemWithURL:" "@@:@" stub_3

  -- makeListenerEndpointForServiceName:itemURL:andReturnError:
  stub_4 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FIFinderSyncOverrides
    case _makeListenerEndpointForServiceName_itemURL_andReturnError rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "makeListenerEndpointForServiceName:itemURL:andReturnError:" "@@:@@@" stub_4

  -- valuesForAttributes:forItemWithURL:completion:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FIFinderSyncOverrides
    case _valuesForAttributes_forItemWithURL_completion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "valuesForAttributes:forItemWithURL:completion:" "v@:@@@" stub_5

  -- toolbarItemName
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FIFinderSyncOverrides
    case _toolbarItemName rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "toolbarItemName" "@@:" stub_6

  -- toolbarItemImage
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FIFinderSyncOverrides
    case _toolbarItemImage rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "toolbarItemImage" "@@:" stub_7

  -- toolbarItemToolTip
  stub_8 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FIFinderSyncOverrides
    case _toolbarItemToolTip rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "toolbarItemToolTip" "@@:" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FIFinderSyncOverrides
    if queriedSel == sel_beginObservingDirectoryAtURL then pure (maybe 0 (const 1) (_beginObservingDirectoryAtURL rec_))
    else if queriedSel == sel_endObservingDirectoryAtURL then pure (maybe 0 (const 1) (_endObservingDirectoryAtURL rec_))
    else if queriedSel == sel_requestBadgeIdentifierForURL then pure (maybe 0 (const 1) (_requestBadgeIdentifierForURL rec_))
    else if queriedSel == sel_supportedServiceNamesForItemWithURL then pure (maybe 0 (const 1) (_supportedServiceNamesForItemWithURL rec_))
    else if queriedSel == sel_makeListenerEndpointForServiceName_itemURL_andReturnError then pure (maybe 0 (const 1) (_makeListenerEndpointForServiceName_itemURL_andReturnError rec_))
    else if queriedSel == sel_valuesForAttributes_forItemWithURL_completion then pure (maybe 0 (const 1) (_valuesForAttributes_forItemWithURL_completion rec_))
    else if queriedSel == sel_toolbarItemName then pure (maybe 0 (const 1) (_toolbarItemName rec_))
    else if queriedSel == sel_toolbarItemImage then pure (maybe 0 (const 1) (_toolbarItemImage rec_))
    else if queriedSel == sel_toolbarItemToolTip then pure (maybe 0 (const 1) (_toolbarItemToolTip rec_))
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
newFIFinderSync :: FIFinderSyncOverrides -> IO RawId
newFIFinderSync overrides = do
  inst <- class_createInstance fiFinderSyncDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
