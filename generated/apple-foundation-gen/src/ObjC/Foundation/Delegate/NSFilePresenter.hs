{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFilePresenter@.
--
-- Usage:
--
-- @
-- delegate <- newNSFilePresenter defaultNSFilePresenterOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSFilePresenter
  ( NSFilePresenterOverrides(..)
  , defaultNSFilePresenterOverrides
  , newNSFilePresenter
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

-- | Overrides record for @\@protocol NSFilePresenter@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFilePresenterOverrides = NSFilePresenterOverrides
  { _presentedItemDidMoveToURL :: !(Maybe (RawId -> IO ()))
  , _presentedItemDidChange :: !(Maybe (IO ()))
  , _presentedItemDidChangeUbiquityAttributes :: !(Maybe (RawId -> IO ()))
  , _presentedItemDidGainVersion :: !(Maybe (RawId -> IO ()))
  , _presentedItemDidLoseVersion :: !(Maybe (RawId -> IO ()))
  , _presentedItemDidResolveConflictVersion :: !(Maybe (RawId -> IO ()))
  , _presentedSubitemDidAppearAtURL :: !(Maybe (RawId -> IO ()))
  , _presentedSubitemAtURL_didMoveToURL :: !(Maybe (RawId -> RawId -> IO ()))
  , _presentedSubitemDidChangeAtURL :: !(Maybe (RawId -> IO ()))
  , _presentedSubitemAtURL_didGainVersion :: !(Maybe (RawId -> RawId -> IO ()))
  , _presentedSubitemAtURL_didLoseVersion :: !(Maybe (RawId -> RawId -> IO ()))
  , _presentedSubitemAtURL_didResolveConflictVersion :: !(Maybe (RawId -> RawId -> IO ()))
  , _presentedItemURL :: !(Maybe (IO RawId))
  , _presentedItemOperationQueue :: !(Maybe (IO RawId))
  , _primaryPresentedItemURL :: !(Maybe (IO RawId))
  , _observedPresentedItemUbiquityAttributes :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFilePresenterOverrides :: NSFilePresenterOverrides
defaultNSFilePresenterOverrides = NSFilePresenterOverrides
  { _presentedItemDidMoveToURL = Nothing
  , _presentedItemDidChange = Nothing
  , _presentedItemDidChangeUbiquityAttributes = Nothing
  , _presentedItemDidGainVersion = Nothing
  , _presentedItemDidLoseVersion = Nothing
  , _presentedItemDidResolveConflictVersion = Nothing
  , _presentedSubitemDidAppearAtURL = Nothing
  , _presentedSubitemAtURL_didMoveToURL = Nothing
  , _presentedSubitemDidChangeAtURL = Nothing
  , _presentedSubitemAtURL_didGainVersion = Nothing
  , _presentedSubitemAtURL_didLoseVersion = Nothing
  , _presentedSubitemAtURL_didResolveConflictVersion = Nothing
  , _presentedItemURL = Nothing
  , _presentedItemOperationQueue = Nothing
  , _primaryPresentedItemURL = Nothing
  , _observedPresentedItemUbiquityAttributes = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsFilePresenterDelegateClass #-}
nsFilePresenterDelegateClass :: Class
nsFilePresenterDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFilePresenter" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_presentedItemDidMoveToURL = unSelector (mkSelector "presentedItemDidMoveToURL:")
      sel_presentedItemDidChange = unSelector (mkSelector "presentedItemDidChange")
      sel_presentedItemDidChangeUbiquityAttributes = unSelector (mkSelector "presentedItemDidChangeUbiquityAttributes:")
      sel_presentedItemDidGainVersion = unSelector (mkSelector "presentedItemDidGainVersion:")
      sel_presentedItemDidLoseVersion = unSelector (mkSelector "presentedItemDidLoseVersion:")
      sel_presentedItemDidResolveConflictVersion = unSelector (mkSelector "presentedItemDidResolveConflictVersion:")
      sel_presentedSubitemDidAppearAtURL = unSelector (mkSelector "presentedSubitemDidAppearAtURL:")
      sel_presentedSubitemAtURL_didMoveToURL = unSelector (mkSelector "presentedSubitemAtURL:didMoveToURL:")
      sel_presentedSubitemDidChangeAtURL = unSelector (mkSelector "presentedSubitemDidChangeAtURL:")
      sel_presentedSubitemAtURL_didGainVersion = unSelector (mkSelector "presentedSubitemAtURL:didGainVersion:")
      sel_presentedSubitemAtURL_didLoseVersion = unSelector (mkSelector "presentedSubitemAtURL:didLoseVersion:")
      sel_presentedSubitemAtURL_didResolveConflictVersion = unSelector (mkSelector "presentedSubitemAtURL:didResolveConflictVersion:")
      sel_presentedItemURL = unSelector (mkSelector "presentedItemURL")
      sel_presentedItemOperationQueue = unSelector (mkSelector "presentedItemOperationQueue")
      sel_primaryPresentedItemURL = unSelector (mkSelector "primaryPresentedItemURL")
      sel_observedPresentedItemUbiquityAttributes = unSelector (mkSelector "observedPresentedItemUbiquityAttributes")
  -- presentedItemDidMoveToURL:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedItemDidMoveToURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "presentedItemDidMoveToURL:" "v@:@" stub_0

  -- presentedItemDidChange
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedItemDidChange rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "presentedItemDidChange" "v@:" stub_1

  -- presentedItemDidChangeUbiquityAttributes:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedItemDidChangeUbiquityAttributes rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "presentedItemDidChangeUbiquityAttributes:" "v@:@" stub_2

  -- presentedItemDidGainVersion:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedItemDidGainVersion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "presentedItemDidGainVersion:" "v@:@" stub_3

  -- presentedItemDidLoseVersion:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedItemDidLoseVersion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "presentedItemDidLoseVersion:" "v@:@" stub_4

  -- presentedItemDidResolveConflictVersion:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedItemDidResolveConflictVersion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "presentedItemDidResolveConflictVersion:" "v@:@" stub_5

  -- presentedSubitemDidAppearAtURL:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedSubitemDidAppearAtURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "presentedSubitemDidAppearAtURL:" "v@:@" stub_6

  -- presentedSubitemAtURL:didMoveToURL:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedSubitemAtURL_didMoveToURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "presentedSubitemAtURL:didMoveToURL:" "v@:@@" stub_7

  -- presentedSubitemDidChangeAtURL:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedSubitemDidChangeAtURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "presentedSubitemDidChangeAtURL:" "v@:@" stub_8

  -- presentedSubitemAtURL:didGainVersion:
  stub_9 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedSubitemAtURL_didGainVersion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "presentedSubitemAtURL:didGainVersion:" "v@:@@" stub_9

  -- presentedSubitemAtURL:didLoseVersion:
  stub_10 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedSubitemAtURL_didLoseVersion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "presentedSubitemAtURL:didLoseVersion:" "v@:@@" stub_10

  -- presentedSubitemAtURL:didResolveConflictVersion:
  stub_11 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedSubitemAtURL_didResolveConflictVersion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "presentedSubitemAtURL:didResolveConflictVersion:" "v@:@@" stub_11

  -- presentedItemURL
  stub_12 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedItemURL rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "presentedItemURL" "@@:" stub_12

  -- presentedItemOperationQueue
  stub_13 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _presentedItemOperationQueue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "presentedItemOperationQueue" "@@:" stub_13

  -- primaryPresentedItemURL
  stub_14 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _primaryPresentedItemURL rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "primaryPresentedItemURL" "@@:" stub_14

  -- observedPresentedItemUbiquityAttributes
  stub_15 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    case _observedPresentedItemUbiquityAttributes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "observedPresentedItemUbiquityAttributes" "@@:" stub_15

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFilePresenterOverrides
    if queriedSel == sel_presentedItemDidMoveToURL then pure (maybe 0 (const 1) (_presentedItemDidMoveToURL rec_))
    else if queriedSel == sel_presentedItemDidChange then pure (maybe 0 (const 1) (_presentedItemDidChange rec_))
    else if queriedSel == sel_presentedItemDidChangeUbiquityAttributes then pure (maybe 0 (const 1) (_presentedItemDidChangeUbiquityAttributes rec_))
    else if queriedSel == sel_presentedItemDidGainVersion then pure (maybe 0 (const 1) (_presentedItemDidGainVersion rec_))
    else if queriedSel == sel_presentedItemDidLoseVersion then pure (maybe 0 (const 1) (_presentedItemDidLoseVersion rec_))
    else if queriedSel == sel_presentedItemDidResolveConflictVersion then pure (maybe 0 (const 1) (_presentedItemDidResolveConflictVersion rec_))
    else if queriedSel == sel_presentedSubitemDidAppearAtURL then pure (maybe 0 (const 1) (_presentedSubitemDidAppearAtURL rec_))
    else if queriedSel == sel_presentedSubitemAtURL_didMoveToURL then pure (maybe 0 (const 1) (_presentedSubitemAtURL_didMoveToURL rec_))
    else if queriedSel == sel_presentedSubitemDidChangeAtURL then pure (maybe 0 (const 1) (_presentedSubitemDidChangeAtURL rec_))
    else if queriedSel == sel_presentedSubitemAtURL_didGainVersion then pure (maybe 0 (const 1) (_presentedSubitemAtURL_didGainVersion rec_))
    else if queriedSel == sel_presentedSubitemAtURL_didLoseVersion then pure (maybe 0 (const 1) (_presentedSubitemAtURL_didLoseVersion rec_))
    else if queriedSel == sel_presentedSubitemAtURL_didResolveConflictVersion then pure (maybe 0 (const 1) (_presentedSubitemAtURL_didResolveConflictVersion rec_))
    else if queriedSel == sel_presentedItemURL then pure (maybe 0 (const 1) (_presentedItemURL rec_))
    else if queriedSel == sel_presentedItemOperationQueue then pure (maybe 0 (const 1) (_presentedItemOperationQueue rec_))
    else if queriedSel == sel_primaryPresentedItemURL then pure (maybe 0 (const 1) (_primaryPresentedItemURL rec_))
    else if queriedSel == sel_observedPresentedItemUbiquityAttributes then pure (maybe 0 (const 1) (_observedPresentedItemUbiquityAttributes rec_))
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
newNSFilePresenter :: NSFilePresenterOverrides -> IO RawId
newNSFilePresenter overrides = do
  inst <- class_createInstance nsFilePresenterDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
