{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CALayoutManager@.
--
-- Usage:
--
-- @
-- delegate <- newCALayoutManager defaultCALayoutManagerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.QuartzCore.Delegate.CALayoutManager
  ( CALayoutManagerOverrides(..)
  , defaultCALayoutManagerOverrides
  , newCALayoutManager
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

-- | Overrides record for @\@protocol CALayoutManager@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CALayoutManagerOverrides = CALayoutManagerOverrides
  { _invalidateLayoutOfLayer :: !(Maybe (RawId -> IO ()))
  , _layoutSublayersOfLayer :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCALayoutManagerOverrides :: CALayoutManagerOverrides
defaultCALayoutManagerOverrides = CALayoutManagerOverrides
  { _invalidateLayoutOfLayer = Nothing
  , _layoutSublayersOfLayer = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE caLayoutManagerDelegateClass #-}
caLayoutManagerDelegateClass :: Class
caLayoutManagerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCALayoutManager" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_invalidateLayoutOfLayer = unSelector (mkSelector "invalidateLayoutOfLayer:")
      sel_layoutSublayersOfLayer = unSelector (mkSelector "layoutSublayersOfLayer:")
  -- invalidateLayoutOfLayer:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CALayoutManagerOverrides
    case _invalidateLayoutOfLayer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "invalidateLayoutOfLayer:" "v@:@" stub_0

  -- layoutSublayersOfLayer:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CALayoutManagerOverrides
    case _layoutSublayersOfLayer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "layoutSublayersOfLayer:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CALayoutManagerOverrides
    if queriedSel == sel_invalidateLayoutOfLayer then pure (maybe 0 (const 1) (_invalidateLayoutOfLayer rec_))
    else if queriedSel == sel_layoutSublayersOfLayer then pure (maybe 0 (const 1) (_layoutSublayersOfLayer rec_))
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
newCALayoutManager :: CALayoutManagerOverrides -> IO RawId
newCALayoutManager overrides = do
  inst <- class_createInstance caLayoutManagerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
