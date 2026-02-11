{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSUserActivityDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSUserActivityDelegate defaultNSUserActivityDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSUserActivityDelegate
  ( NSUserActivityDelegateOverrides(..)
  , defaultNSUserActivityDelegateOverrides
  , newNSUserActivityDelegate
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

-- | Overrides record for @\@protocol NSUserActivityDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSUserActivityDelegateOverrides = NSUserActivityDelegateOverrides
  { _userActivityWillSave :: !(Maybe (RawId -> IO ()))
  , _userActivityWasContinued :: !(Maybe (RawId -> IO ()))
  , _userActivity_didReceiveInputStream_outputStream :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSUserActivityDelegateOverrides :: NSUserActivityDelegateOverrides
defaultNSUserActivityDelegateOverrides = NSUserActivityDelegateOverrides
  { _userActivityWillSave = Nothing
  , _userActivityWasContinued = Nothing
  , _userActivity_didReceiveInputStream_outputStream = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsUserActivityDelegateDelegateClass #-}
nsUserActivityDelegateDelegateClass :: Class
nsUserActivityDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSUserActivityDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_userActivityWillSave = unSelector (mkSelector "userActivityWillSave:")
      sel_userActivityWasContinued = unSelector (mkSelector "userActivityWasContinued:")
      sel_userActivity_didReceiveInputStream_outputStream = unSelector (mkSelector "userActivity:didReceiveInputStream:outputStream:")
  -- userActivityWillSave:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserActivityDelegateOverrides
    case _userActivityWillSave rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "userActivityWillSave:" "v@:@" stub_0

  -- userActivityWasContinued:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserActivityDelegateOverrides
    case _userActivityWasContinued rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "userActivityWasContinued:" "v@:@" stub_1

  -- userActivity:didReceiveInputStream:outputStream:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserActivityDelegateOverrides
    case _userActivity_didReceiveInputStream_outputStream rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "userActivity:didReceiveInputStream:outputStream:" "v@:@@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserActivityDelegateOverrides
    if queriedSel == sel_userActivityWillSave then pure (maybe 0 (const 1) (_userActivityWillSave rec_))
    else if queriedSel == sel_userActivityWasContinued then pure (maybe 0 (const 1) (_userActivityWasContinued rec_))
    else if queriedSel == sel_userActivity_didReceiveInputStream_outputStream then pure (maybe 0 (const 1) (_userActivity_didReceiveInputStream_outputStream rec_))
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
newNSUserActivityDelegate :: NSUserActivityDelegateOverrides -> IO RawId
newNSUserActivityDelegate overrides = do
  inst <- class_createInstance nsUserActivityDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
