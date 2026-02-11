{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol FSVolumeXattrOperations@.
--
-- Usage:
--
-- @
-- delegate <- newFSVolumeXattrOperations defaultFSVolumeXattrOperationsOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FSKit.Delegate.FSVolumeXattrOperations
  ( FSVolumeXattrOperationsOverrides(..)
  , defaultFSVolumeXattrOperationsOverrides
  , newFSVolumeXattrOperations
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

-- | Overrides record for @\@protocol FSVolumeXattrOperations@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data FSVolumeXattrOperationsOverrides = FSVolumeXattrOperationsOverrides
  { _supportedXattrNamesForItem :: !(Maybe (RawId -> IO RawId))
  , _listXattrsOfItem_replyHandler :: !(Maybe (RawId -> RawId -> IO ()))
  , _xattrOperationsInhibited :: !(Maybe (IO Bool))
  , _setXattrOperationsInhibited :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultFSVolumeXattrOperationsOverrides :: FSVolumeXattrOperationsOverrides
defaultFSVolumeXattrOperationsOverrides = FSVolumeXattrOperationsOverrides
  { _supportedXattrNamesForItem = Nothing
  , _listXattrsOfItem_replyHandler = Nothing
  , _xattrOperationsInhibited = Nothing
  , _setXattrOperationsInhibited = Nothing
  }

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE fsVolumeXattrOperationsDelegateClass #-}
fsVolumeXattrOperationsDelegateClass :: Class
fsVolumeXattrOperationsDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsFSVolumeXattrOperations" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_supportedXattrNamesForItem = unSelector (mkSelector "supportedXattrNamesForItem:")
      sel_listXattrsOfItem_replyHandler = unSelector (mkSelector "listXattrsOfItem:replyHandler:")
      sel_xattrOperationsInhibited = unSelector (mkSelector "xattrOperationsInhibited")
      sel_setXattrOperationsInhibited = unSelector (mkSelector "setXattrOperationsInhibited:")
  -- supportedXattrNamesForItem:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeXattrOperationsOverrides
    case _supportedXattrNamesForItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedXattrNamesForItem:" "@@:@" stub_0

  -- listXattrsOfItem:replyHandler:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeXattrOperationsOverrides
    case _listXattrsOfItem_replyHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "listXattrsOfItem:replyHandler:" "v@:@@" stub_1

  -- xattrOperationsInhibited
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeXattrOperationsOverrides
    case _xattrOperationsInhibited rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "xattrOperationsInhibited" "B@:" stub_2

  -- setXattrOperationsInhibited:
  stub_3 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeXattrOperationsOverrides
    case _setXattrOperationsInhibited rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setXattrOperationsInhibited:" "v@:B" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeXattrOperationsOverrides
    if queriedSel == sel_supportedXattrNamesForItem then pure (maybe 0 (const 1) (_supportedXattrNamesForItem rec_))
    else if queriedSel == sel_listXattrsOfItem_replyHandler then pure (maybe 0 (const 1) (_listXattrsOfItem_replyHandler rec_))
    else if queriedSel == sel_xattrOperationsInhibited then pure (maybe 0 (const 1) (_xattrOperationsInhibited rec_))
    else if queriedSel == sel_setXattrOperationsInhibited then pure (maybe 0 (const 1) (_setXattrOperationsInhibited rec_))
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
newFSVolumeXattrOperations :: FSVolumeXattrOperationsOverrides -> IO RawId
newFSVolumeXattrOperations overrides = do
  inst <- class_createInstance fsVolumeXattrOperationsDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
