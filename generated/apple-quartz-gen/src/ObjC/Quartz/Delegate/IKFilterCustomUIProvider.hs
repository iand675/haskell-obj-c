{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IKFilterCustomUIProvider@.
--
-- Usage:
--
-- @
-- delegate <- newIKFilterCustomUIProvider defaultIKFilterCustomUIProviderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Quartz.Delegate.IKFilterCustomUIProvider
  ( IKFilterCustomUIProviderOverrides(..)
  , defaultIKFilterCustomUIProviderOverrides
  , newIKFilterCustomUIProvider
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

-- | Overrides record for @\@protocol IKFilterCustomUIProvider@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IKFilterCustomUIProviderOverrides = IKFilterCustomUIProviderOverrides
  { _provideViewForUIConfiguration_excludedKeys :: !(Maybe (RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultIKFilterCustomUIProviderOverrides :: IKFilterCustomUIProviderOverrides
defaultIKFilterCustomUIProviderOverrides = IKFilterCustomUIProviderOverrides
  { _provideViewForUIConfiguration_excludedKeys = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ikFilterCustomUIProviderDelegateClass #-}
ikFilterCustomUIProviderDelegateClass :: Class
ikFilterCustomUIProviderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIKFilterCustomUIProvider" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_provideViewForUIConfiguration_excludedKeys = unSelector (mkSelector "provideViewForUIConfiguration:excludedKeys:")
  -- provideViewForUIConfiguration:excludedKeys:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKFilterCustomUIProviderOverrides
    case _provideViewForUIConfiguration_excludedKeys rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "provideViewForUIConfiguration:excludedKeys:" "@@:@@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKFilterCustomUIProviderOverrides
    if queriedSel == sel_provideViewForUIConfiguration_excludedKeys then pure (maybe 0 (const 1) (_provideViewForUIConfiguration_excludedKeys rec_))
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
newIKFilterCustomUIProvider :: IKFilterCustomUIProviderOverrides -> IO RawId
newIKFilterCustomUIProvider overrides = do
  inst <- class_createInstance ikFilterCustomUIProviderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
