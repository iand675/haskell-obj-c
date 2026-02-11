{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFileProviderReplicatedExtension@.
--
-- Usage:
--
-- @
-- delegate <- newNSFileProviderReplicatedExtension defaultNSFileProviderReplicatedExtensionOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FileProvider.Delegate.NSFileProviderReplicatedExtension
  ( NSFileProviderReplicatedExtensionOverrides(..)
  , defaultNSFileProviderReplicatedExtensionOverrides
  , newNSFileProviderReplicatedExtension
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

-- | Overrides record for @\@protocol NSFileProviderReplicatedExtension@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFileProviderReplicatedExtensionOverrides = NSFileProviderReplicatedExtensionOverrides
  { _initWithDomain :: !(Maybe (RawId -> IO RawId))
  , _invalidate :: !(Maybe (IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFileProviderReplicatedExtensionOverrides :: NSFileProviderReplicatedExtensionOverrides
defaultNSFileProviderReplicatedExtensionOverrides = NSFileProviderReplicatedExtensionOverrides
  { _initWithDomain = Nothing
  , _invalidate = Nothing
  }

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsFileProviderReplicatedExtensionDelegateClass #-}
nsFileProviderReplicatedExtensionDelegateClass :: Class
nsFileProviderReplicatedExtensionDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFileProviderReplicatedExtension" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_initWithDomain = unSelector (mkSelector "initWithDomain:")
      sel_invalidate = unSelector (mkSelector "invalidate")
  -- initWithDomain:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderReplicatedExtensionOverrides
    case _initWithDomain rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "initWithDomain:" "@@:@" stub_0

  -- invalidate
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderReplicatedExtensionOverrides
    case _invalidate rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "invalidate" "v@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderReplicatedExtensionOverrides
    if queriedSel == sel_initWithDomain then pure (maybe 0 (const 1) (_initWithDomain rec_))
    else if queriedSel == sel_invalidate then pure (maybe 0 (const 1) (_invalidate rec_))
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
newNSFileProviderReplicatedExtension :: NSFileProviderReplicatedExtensionOverrides -> IO RawId
newNSFileProviderReplicatedExtension overrides = do
  inst <- class_createInstance nsFileProviderReplicatedExtensionDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
