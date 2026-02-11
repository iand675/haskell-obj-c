{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol EXHostViewControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newEXHostViewControllerDelegate defaultEXHostViewControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ExtensionKit.Delegate.EXHostViewControllerDelegate
  ( EXHostViewControllerDelegateOverrides(..)
  , defaultEXHostViewControllerDelegateOverrides
  , newEXHostViewControllerDelegate
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

-- | Overrides record for @\@protocol EXHostViewControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data EXHostViewControllerDelegateOverrides = EXHostViewControllerDelegateOverrides
  { _hostViewControllerDidActivate :: !(Maybe (RawId -> IO ()))
  , _hostViewControllerWillDeactivate_error :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultEXHostViewControllerDelegateOverrides :: EXHostViewControllerDelegateOverrides
defaultEXHostViewControllerDelegateOverrides = EXHostViewControllerDelegateOverrides
  { _hostViewControllerDidActivate = Nothing
  , _hostViewControllerWillDeactivate_error = Nothing
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
{-# NOINLINE exHostViewControllerDelegateDelegateClass #-}
exHostViewControllerDelegateDelegateClass :: Class
exHostViewControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsEXHostViewControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_hostViewControllerDidActivate = unSelector (mkSelector "hostViewControllerDidActivate:")
      sel_hostViewControllerWillDeactivate_error = unSelector (mkSelector "hostViewControllerWillDeactivate:error:")
  -- hostViewControllerDidActivate:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO EXHostViewControllerDelegateOverrides
    case _hostViewControllerDidActivate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "hostViewControllerDidActivate:" "v@:@" stub_0

  -- hostViewControllerWillDeactivate:error:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO EXHostViewControllerDelegateOverrides
    case _hostViewControllerWillDeactivate_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "hostViewControllerWillDeactivate:error:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO EXHostViewControllerDelegateOverrides
    if queriedSel == sel_hostViewControllerDidActivate then pure (maybe 0 (const 1) (_hostViewControllerDidActivate rec_))
    else if queriedSel == sel_hostViewControllerWillDeactivate_error then pure (maybe 0 (const 1) (_hostViewControllerWillDeactivate_error rec_))
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
newEXHostViewControllerDelegate :: EXHostViewControllerDelegateOverrides -> IO RawId
newEXHostViewControllerDelegate overrides = do
  inst <- class_createInstance exHostViewControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
