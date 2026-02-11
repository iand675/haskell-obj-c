{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAnimatablePropertyContainer@.
--
-- Usage:
--
-- @
-- delegate <- newNSAnimatablePropertyContainer defaultNSAnimatablePropertyContainerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAnimatablePropertyContainer
  ( NSAnimatablePropertyContainerOverrides(..)
  , defaultNSAnimatablePropertyContainerOverrides
  , newNSAnimatablePropertyContainer
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

-- | Overrides record for @\@protocol NSAnimatablePropertyContainer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAnimatablePropertyContainerOverrides = NSAnimatablePropertyContainerOverrides
  { _animator :: !(Maybe (IO RawId))
  , _animationForKey :: !(Maybe (RawId -> IO RawId))
  , _animations :: !(Maybe (IO RawId))
  , _setAnimations :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAnimatablePropertyContainerOverrides :: NSAnimatablePropertyContainerOverrides
defaultNSAnimatablePropertyContainerOverrides = NSAnimatablePropertyContainerOverrides
  { _animator = Nothing
  , _animationForKey = Nothing
  , _animations = Nothing
  , _setAnimations = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAnimatablePropertyContainerDelegateClass #-}
nsAnimatablePropertyContainerDelegateClass :: Class
nsAnimatablePropertyContainerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAnimatablePropertyContainer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_animator = unSelector (mkSelector "animator")
      sel_animationForKey = unSelector (mkSelector "animationForKey:")
      sel_animations = unSelector (mkSelector "animations")
      sel_setAnimations = unSelector (mkSelector "setAnimations:")
  -- animator
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAnimatablePropertyContainerOverrides
    case _animator rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "animator" "@@:" stub_0

  -- animationForKey:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAnimatablePropertyContainerOverrides
    case _animationForKey rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "animationForKey:" "@@:@" stub_1

  -- animations
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAnimatablePropertyContainerOverrides
    case _animations rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "animations" "@@:" stub_2

  -- setAnimations:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAnimatablePropertyContainerOverrides
    case _setAnimations rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAnimations:" "v@:@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAnimatablePropertyContainerOverrides
    if queriedSel == sel_animator then pure (maybe 0 (const 1) (_animator rec_))
    else if queriedSel == sel_animationForKey then pure (maybe 0 (const 1) (_animationForKey rec_))
    else if queriedSel == sel_animations then pure (maybe 0 (const 1) (_animations rec_))
    else if queriedSel == sel_setAnimations then pure (maybe 0 (const 1) (_setAnimations rec_))
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
newNSAnimatablePropertyContainer :: NSAnimatablePropertyContainerOverrides -> IO RawId
newNSAnimatablePropertyContainer overrides = do
  inst <- class_createInstance nsAnimatablePropertyContainerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
