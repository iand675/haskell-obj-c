{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSPopoverDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSPopoverDelegate defaultNSPopoverDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSPopoverDelegate
  ( NSPopoverDelegateOverrides(..)
  , defaultNSPopoverDelegateOverrides
  , newNSPopoverDelegate
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

-- | Overrides record for @\@protocol NSPopoverDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSPopoverDelegateOverrides = NSPopoverDelegateOverrides
  { _popoverShouldClose :: !(Maybe (RawId -> IO Bool))
  , _popoverShouldDetach :: !(Maybe (RawId -> IO Bool))
  , _popoverDidDetach :: !(Maybe (RawId -> IO ()))
  , _detachableWindowForPopover :: !(Maybe (RawId -> IO RawId))
  , _popoverWillShow :: !(Maybe (RawId -> IO ()))
  , _popoverDidShow :: !(Maybe (RawId -> IO ()))
  , _popoverWillClose :: !(Maybe (RawId -> IO ()))
  , _popoverDidClose :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSPopoverDelegateOverrides :: NSPopoverDelegateOverrides
defaultNSPopoverDelegateOverrides = NSPopoverDelegateOverrides
  { _popoverShouldClose = Nothing
  , _popoverShouldDetach = Nothing
  , _popoverDidDetach = Nothing
  , _detachableWindowForPopover = Nothing
  , _popoverWillShow = Nothing
  , _popoverDidShow = Nothing
  , _popoverWillClose = Nothing
  , _popoverDidClose = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

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
{-# NOINLINE nsPopoverDelegateDelegateClass #-}
nsPopoverDelegateDelegateClass :: Class
nsPopoverDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSPopoverDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_popoverShouldClose = unSelector (mkSelector "popoverShouldClose:")
      sel_popoverShouldDetach = unSelector (mkSelector "popoverShouldDetach:")
      sel_popoverDidDetach = unSelector (mkSelector "popoverDidDetach:")
      sel_detachableWindowForPopover = unSelector (mkSelector "detachableWindowForPopover:")
      sel_popoverWillShow = unSelector (mkSelector "popoverWillShow:")
      sel_popoverDidShow = unSelector (mkSelector "popoverDidShow:")
      sel_popoverWillClose = unSelector (mkSelector "popoverWillClose:")
      sel_popoverDidClose = unSelector (mkSelector "popoverDidClose:")
  -- popoverShouldClose:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPopoverDelegateOverrides
    case _popoverShouldClose rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "popoverShouldClose:" "B@:@" stub_0

  -- popoverShouldDetach:
  stub_1 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPopoverDelegateOverrides
    case _popoverShouldDetach rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "popoverShouldDetach:" "B@:@" stub_1

  -- popoverDidDetach:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPopoverDelegateOverrides
    case _popoverDidDetach rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "popoverDidDetach:" "v@:@" stub_2

  -- detachableWindowForPopover:
  stub_3 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPopoverDelegateOverrides
    case _detachableWindowForPopover rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "detachableWindowForPopover:" "@@:@" stub_3

  -- popoverWillShow:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPopoverDelegateOverrides
    case _popoverWillShow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "popoverWillShow:" "v@:@" stub_4

  -- popoverDidShow:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPopoverDelegateOverrides
    case _popoverDidShow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "popoverDidShow:" "v@:@" stub_5

  -- popoverWillClose:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPopoverDelegateOverrides
    case _popoverWillClose rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "popoverWillClose:" "v@:@" stub_6

  -- popoverDidClose:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPopoverDelegateOverrides
    case _popoverDidClose rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "popoverDidClose:" "v@:@" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPopoverDelegateOverrides
    if queriedSel == sel_popoverShouldClose then pure (maybe 0 (const 1) (_popoverShouldClose rec_))
    else if queriedSel == sel_popoverShouldDetach then pure (maybe 0 (const 1) (_popoverShouldDetach rec_))
    else if queriedSel == sel_popoverDidDetach then pure (maybe 0 (const 1) (_popoverDidDetach rec_))
    else if queriedSel == sel_detachableWindowForPopover then pure (maybe 0 (const 1) (_detachableWindowForPopover rec_))
    else if queriedSel == sel_popoverWillShow then pure (maybe 0 (const 1) (_popoverWillShow rec_))
    else if queriedSel == sel_popoverDidShow then pure (maybe 0 (const 1) (_popoverDidShow rec_))
    else if queriedSel == sel_popoverWillClose then pure (maybe 0 (const 1) (_popoverWillClose rec_))
    else if queriedSel == sel_popoverDidClose then pure (maybe 0 (const 1) (_popoverDidClose rec_))
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
newNSPopoverDelegate :: NSPopoverDelegateOverrides -> IO RawId
newNSPopoverDelegate overrides = do
  inst <- class_createInstance nsPopoverDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
