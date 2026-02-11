{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CAAnimationDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newCAAnimationDelegate defaultCAAnimationDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.QuartzCore.Delegate.CAAnimationDelegate
  ( CAAnimationDelegateOverrides(..)
  , defaultCAAnimationDelegateOverrides
  , newCAAnimationDelegate
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

-- | Overrides record for @\@protocol CAAnimationDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CAAnimationDelegateOverrides = CAAnimationDelegateOverrides
  { _animationDidStart :: !(Maybe (RawId -> IO ()))
  , _animationDidStop_finished :: !(Maybe (RawId -> Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCAAnimationDelegateOverrides :: CAAnimationDelegateOverrides
defaultCAAnimationDelegateOverrides = CAAnimationDelegateOverrides
  { _animationDidStart = Nothing
  , _animationDidStop_finished = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE caAnimationDelegateDelegateClass #-}
caAnimationDelegateDelegateClass :: Class
caAnimationDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCAAnimationDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_animationDidStart = unSelector (mkSelector "animationDidStart:")
      sel_animationDidStop_finished = unSelector (mkSelector "animationDidStop:finished:")
  -- animationDidStart:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAAnimationDelegateOverrides
    case _animationDidStart rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "animationDidStart:" "v@:@" stub_0

  -- animationDidStop:finished:
  stub_1 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAAnimationDelegateOverrides
    case _animationDidStop_finished rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "animationDidStop:finished:" "v@:@B" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAAnimationDelegateOverrides
    if queriedSel == sel_animationDidStart then pure (maybe 0 (const 1) (_animationDidStart rec_))
    else if queriedSel == sel_animationDidStop_finished then pure (maybe 0 (const 1) (_animationDidStop_finished rec_))
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
newCAAnimationDelegate :: CAAnimationDelegateOverrides -> IO RawId
newCAAnimationDelegate overrides = do
  inst <- class_createInstance caAnimationDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
