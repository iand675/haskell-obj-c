{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSDrawerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSDrawerDelegate defaultNSDrawerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSDrawerDelegate
  ( NSDrawerDelegateOverrides(..)
  , defaultNSDrawerDelegateOverrides
  , newNSDrawerDelegate
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

-- | Overrides record for @\@protocol NSDrawerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSDrawerDelegateOverrides = NSDrawerDelegateOverrides
  { _drawerShouldOpen :: !(Maybe (RawId -> IO Bool))
  , _drawerShouldClose :: !(Maybe (RawId -> IO Bool))
  , _drawerWillOpen :: !(Maybe (RawId -> IO ()))
  , _drawerDidOpen :: !(Maybe (RawId -> IO ()))
  , _drawerWillClose :: !(Maybe (RawId -> IO ()))
  , _drawerDidClose :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSDrawerDelegateOverrides :: NSDrawerDelegateOverrides
defaultNSDrawerDelegateOverrides = NSDrawerDelegateOverrides
  { _drawerShouldOpen = Nothing
  , _drawerShouldClose = Nothing
  , _drawerWillOpen = Nothing
  , _drawerDidOpen = Nothing
  , _drawerWillClose = Nothing
  , _drawerDidClose = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsDrawerDelegateDelegateClass #-}
nsDrawerDelegateDelegateClass :: Class
nsDrawerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSDrawerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_drawerShouldOpen = unSelector (mkSelector "drawerShouldOpen:")
      sel_drawerShouldClose = unSelector (mkSelector "drawerShouldClose:")
      sel_drawerWillOpen = unSelector (mkSelector "drawerWillOpen:")
      sel_drawerDidOpen = unSelector (mkSelector "drawerDidOpen:")
      sel_drawerWillClose = unSelector (mkSelector "drawerWillClose:")
      sel_drawerDidClose = unSelector (mkSelector "drawerDidClose:")
  -- drawerShouldOpen:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDrawerDelegateOverrides
    case _drawerShouldOpen rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "drawerShouldOpen:" "B@:@" stub_0

  -- drawerShouldClose:
  stub_1 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDrawerDelegateOverrides
    case _drawerShouldClose rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "drawerShouldClose:" "B@:@" stub_1

  -- drawerWillOpen:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDrawerDelegateOverrides
    case _drawerWillOpen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "drawerWillOpen:" "v@:@" stub_2

  -- drawerDidOpen:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDrawerDelegateOverrides
    case _drawerDidOpen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "drawerDidOpen:" "v@:@" stub_3

  -- drawerWillClose:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDrawerDelegateOverrides
    case _drawerWillClose rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "drawerWillClose:" "v@:@" stub_4

  -- drawerDidClose:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDrawerDelegateOverrides
    case _drawerDidClose rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "drawerDidClose:" "v@:@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDrawerDelegateOverrides
    if queriedSel == sel_drawerShouldOpen then pure (maybe 0 (const 1) (_drawerShouldOpen rec_))
    else if queriedSel == sel_drawerShouldClose then pure (maybe 0 (const 1) (_drawerShouldClose rec_))
    else if queriedSel == sel_drawerWillOpen then pure (maybe 0 (const 1) (_drawerWillOpen rec_))
    else if queriedSel == sel_drawerDidOpen then pure (maybe 0 (const 1) (_drawerDidOpen rec_))
    else if queriedSel == sel_drawerWillClose then pure (maybe 0 (const 1) (_drawerWillClose rec_))
    else if queriedSel == sel_drawerDidClose then pure (maybe 0 (const 1) (_drawerDidClose rec_))
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
newNSDrawerDelegate :: NSDrawerDelegateOverrides -> IO RawId
newNSDrawerDelegate overrides = do
  inst <- class_createInstance nsDrawerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
