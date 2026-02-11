{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol WebDocumentRepresentation@.
--
-- Usage:
--
-- @
-- delegate <- newWebDocumentRepresentation defaultWebDocumentRepresentationOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.WebKit.Delegate.WebDocumentRepresentation
  ( WebDocumentRepresentationOverrides(..)
  , defaultWebDocumentRepresentationOverrides
  , newWebDocumentRepresentation
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

-- | Overrides record for @\@protocol WebDocumentRepresentation@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data WebDocumentRepresentationOverrides = WebDocumentRepresentationOverrides
  { _setDataSource :: !(Maybe (RawId -> IO ()))
  , _receivedData_withDataSource :: !(Maybe (RawId -> RawId -> IO ()))
  , _receivedError_withDataSource :: !(Maybe (RawId -> RawId -> IO ()))
  , _finishedLoadingWithDataSource :: !(Maybe (RawId -> IO ()))
  , _canProvideDocumentSource :: !(Maybe (IO Bool))
  , _documentSource :: !(Maybe (IO RawId))
  , _title :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultWebDocumentRepresentationOverrides :: WebDocumentRepresentationOverrides
defaultWebDocumentRepresentationOverrides = WebDocumentRepresentationOverrides
  { _setDataSource = Nothing
  , _receivedData_withDataSource = Nothing
  , _receivedError_withDataSource = Nothing
  , _finishedLoadingWithDataSource = Nothing
  , _canProvideDocumentSource = Nothing
  , _documentSource = Nothing
  , _title = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

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
{-# NOINLINE webDocumentRepresentationDelegateClass #-}
webDocumentRepresentationDelegateClass :: Class
webDocumentRepresentationDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsWebDocumentRepresentation" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_setDataSource = unSelector (mkSelector "setDataSource:")
      sel_receivedData_withDataSource = unSelector (mkSelector "receivedData:withDataSource:")
      sel_receivedError_withDataSource = unSelector (mkSelector "receivedError:withDataSource:")
      sel_finishedLoadingWithDataSource = unSelector (mkSelector "finishedLoadingWithDataSource:")
      sel_canProvideDocumentSource = unSelector (mkSelector "canProvideDocumentSource")
      sel_documentSource = unSelector (mkSelector "documentSource")
      sel_title = unSelector (mkSelector "title")
  -- setDataSource:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentRepresentationOverrides
    case _setDataSource rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setDataSource:" "v@:@" stub_0

  -- receivedData:withDataSource:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentRepresentationOverrides
    case _receivedData_withDataSource rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "receivedData:withDataSource:" "v@:@@" stub_1

  -- receivedError:withDataSource:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentRepresentationOverrides
    case _receivedError_withDataSource rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "receivedError:withDataSource:" "v@:@@" stub_2

  -- finishedLoadingWithDataSource:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentRepresentationOverrides
    case _finishedLoadingWithDataSource rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "finishedLoadingWithDataSource:" "v@:@" stub_3

  -- canProvideDocumentSource
  stub_4 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentRepresentationOverrides
    case _canProvideDocumentSource rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "canProvideDocumentSource" "B@:" stub_4

  -- documentSource
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentRepresentationOverrides
    case _documentSource rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "documentSource" "@@:" stub_5

  -- title
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentRepresentationOverrides
    case _title rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "title" "@@:" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentRepresentationOverrides
    if queriedSel == sel_setDataSource then pure (maybe 0 (const 1) (_setDataSource rec_))
    else if queriedSel == sel_receivedData_withDataSource then pure (maybe 0 (const 1) (_receivedData_withDataSource rec_))
    else if queriedSel == sel_receivedError_withDataSource then pure (maybe 0 (const 1) (_receivedError_withDataSource rec_))
    else if queriedSel == sel_finishedLoadingWithDataSource then pure (maybe 0 (const 1) (_finishedLoadingWithDataSource rec_))
    else if queriedSel == sel_canProvideDocumentSource then pure (maybe 0 (const 1) (_canProvideDocumentSource rec_))
    else if queriedSel == sel_documentSource then pure (maybe 0 (const 1) (_documentSource rec_))
    else if queriedSel == sel_title then pure (maybe 0 (const 1) (_title rec_))
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
newWebDocumentRepresentation :: WebDocumentRepresentationOverrides -> IO RawId
newWebDocumentRepresentation overrides = do
  inst <- class_createInstance webDocumentRepresentationDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
