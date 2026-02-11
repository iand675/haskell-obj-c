{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MDLAssetResolver@.
--
-- Usage:
--
-- @
-- delegate <- newMDLAssetResolver defaultMDLAssetResolverOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ModelIO.Delegate.MDLAssetResolver
  ( MDLAssetResolverOverrides(..)
  , defaultMDLAssetResolverOverrides
  , newMDLAssetResolver
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

-- | Overrides record for @\@protocol MDLAssetResolver@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MDLAssetResolverOverrides = MDLAssetResolverOverrides
  { _canResolveAssetNamed :: !(Maybe (RawId -> IO Bool))
  , _resolveAssetNamed :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMDLAssetResolverOverrides :: MDLAssetResolverOverrides
defaultMDLAssetResolverOverrides = MDLAssetResolverOverrides
  { _canResolveAssetNamed = Nothing
  , _resolveAssetNamed = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mdlAssetResolverDelegateClass #-}
mdlAssetResolverDelegateClass :: Class
mdlAssetResolverDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMDLAssetResolver" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_canResolveAssetNamed = unSelector (mkSelector "canResolveAssetNamed:")
      sel_resolveAssetNamed = unSelector (mkSelector "resolveAssetNamed:")
  -- canResolveAssetNamed:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLAssetResolverOverrides
    case _canResolveAssetNamed rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "canResolveAssetNamed:" "B@:@" stub_0

  -- resolveAssetNamed:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLAssetResolverOverrides
    case _resolveAssetNamed rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "resolveAssetNamed:" "@@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLAssetResolverOverrides
    if queriedSel == sel_canResolveAssetNamed then pure (maybe 0 (const 1) (_canResolveAssetNamed rec_))
    else if queriedSel == sel_resolveAssetNamed then pure (maybe 0 (const 1) (_resolveAssetNamed rec_))
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
newMDLAssetResolver :: MDLAssetResolverOverrides -> IO RawId
newMDLAssetResolver overrides = do
  inst <- class_createInstance mdlAssetResolverDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
