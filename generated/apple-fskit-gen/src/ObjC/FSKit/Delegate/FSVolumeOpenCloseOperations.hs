{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol FSVolumeOpenCloseOperations@.
--
-- Usage:
--
-- @
-- delegate <- newFSVolumeOpenCloseOperations defaultFSVolumeOpenCloseOperationsOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FSKit.Delegate.FSVolumeOpenCloseOperations
  ( FSVolumeOpenCloseOperationsOverrides(..)
  , defaultFSVolumeOpenCloseOperationsOverrides
  , newFSVolumeOpenCloseOperations
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

-- | Overrides record for @\@protocol FSVolumeOpenCloseOperations@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data FSVolumeOpenCloseOperationsOverrides = FSVolumeOpenCloseOperationsOverrides
  { _openCloseInhibited :: !(Maybe (IO Bool))
  , _setOpenCloseInhibited :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultFSVolumeOpenCloseOperationsOverrides :: FSVolumeOpenCloseOperationsOverrides
defaultFSVolumeOpenCloseOperationsOverrides = FSVolumeOpenCloseOperationsOverrides
  { _openCloseInhibited = Nothing
  , _setOpenCloseInhibited = Nothing
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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE fsVolumeOpenCloseOperationsDelegateClass #-}
fsVolumeOpenCloseOperationsDelegateClass :: Class
fsVolumeOpenCloseOperationsDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsFSVolumeOpenCloseOperations" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_openCloseInhibited = unSelector (mkSelector "openCloseInhibited")
      sel_setOpenCloseInhibited = unSelector (mkSelector "setOpenCloseInhibited:")
  -- openCloseInhibited
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeOpenCloseOperationsOverrides
    case _openCloseInhibited rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "openCloseInhibited" "B@:" stub_0

  -- setOpenCloseInhibited:
  stub_1 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeOpenCloseOperationsOverrides
    case _setOpenCloseInhibited rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setOpenCloseInhibited:" "v@:B" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumeOpenCloseOperationsOverrides
    if queriedSel == sel_openCloseInhibited then pure (maybe 0 (const 1) (_openCloseInhibited rec_))
    else if queriedSel == sel_setOpenCloseInhibited then pure (maybe 0 (const 1) (_setOpenCloseInhibited rec_))
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
newFSVolumeOpenCloseOperations :: FSVolumeOpenCloseOperationsOverrides -> IO RawId
newFSVolumeOpenCloseOperations overrides = do
  inst <- class_createInstance fsVolumeOpenCloseOperationsDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
