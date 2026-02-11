{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SKViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSKViewDelegate defaultSKViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SpriteKit.Delegate.SKViewDelegate
  ( SKViewDelegateOverrides(..)
  , defaultSKViewDelegateOverrides
  , newSKViewDelegate
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

-- | Overrides record for @\@protocol SKViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SKViewDelegateOverrides = SKViewDelegateOverrides
  { _view_shouldRenderAtTime :: !(Maybe (RawId -> Double -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultSKViewDelegateOverrides :: SKViewDelegateOverrides
defaultSKViewDelegateOverrides = SKViewDelegateOverrides
  { _view_shouldRenderAtTime = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_d_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE skViewDelegateDelegateClass #-}
skViewDelegateDelegateClass :: Class
skViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSKViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_view_shouldRenderAtTime = unSelector (mkSelector "view:shouldRenderAtTime:")
  -- view:shouldRenderAtTime:
  stub_0 <- wrap_at_d_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKViewDelegateOverrides
    case _view_shouldRenderAtTime rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (realToFrac arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "view:shouldRenderAtTime:" "B@:@d" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKViewDelegateOverrides
    if queriedSel == sel_view_shouldRenderAtTime then pure (maybe 0 (const 1) (_view_shouldRenderAtTime rec_))
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
newSKViewDelegate :: SKViewDelegateOverrides -> IO RawId
newSKViewDelegate overrides = do
  inst <- class_createInstance skViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
