{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSPathControlDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSPathControlDelegate defaultNSPathControlDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSPathControlDelegate
  ( NSPathControlDelegateOverrides(..)
  , defaultNSPathControlDelegateOverrides
  , newNSPathControlDelegate
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

-- | Overrides record for @\@protocol NSPathControlDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSPathControlDelegateOverrides = NSPathControlDelegateOverrides
  { _pathControl_shouldDragItem_withPasteboard :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _pathControl_shouldDragPathComponentCell_withPasteboard :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _pathControl_acceptDrop :: !(Maybe (RawId -> RawId -> IO Bool))
  , _pathControl_willDisplayOpenPanel :: !(Maybe (RawId -> RawId -> IO ()))
  , _pathControl_willPopUpMenu :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSPathControlDelegateOverrides :: NSPathControlDelegateOverrides
defaultNSPathControlDelegateOverrides = NSPathControlDelegateOverrides
  { _pathControl_shouldDragItem_withPasteboard = Nothing
  , _pathControl_shouldDragPathComponentCell_withPasteboard = Nothing
  , _pathControl_acceptDrop = Nothing
  , _pathControl_willDisplayOpenPanel = Nothing
  , _pathControl_willPopUpMenu = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsPathControlDelegateDelegateClass #-}
nsPathControlDelegateDelegateClass :: Class
nsPathControlDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSPathControlDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pathControl_shouldDragItem_withPasteboard = unSelector (mkSelector "pathControl:shouldDragItem:withPasteboard:")
      sel_pathControl_shouldDragPathComponentCell_withPasteboard = unSelector (mkSelector "pathControl:shouldDragPathComponentCell:withPasteboard:")
      sel_pathControl_acceptDrop = unSelector (mkSelector "pathControl:acceptDrop:")
      sel_pathControl_willDisplayOpenPanel = unSelector (mkSelector "pathControl:willDisplayOpenPanel:")
      sel_pathControl_willPopUpMenu = unSelector (mkSelector "pathControl:willPopUpMenu:")
  -- pathControl:shouldDragItem:withPasteboard:
  stub_0 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPathControlDelegateOverrides
    case _pathControl_shouldDragItem_withPasteboard rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "pathControl:shouldDragItem:withPasteboard:" "B@:@@@" stub_0

  -- pathControl:shouldDragPathComponentCell:withPasteboard:
  stub_1 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPathControlDelegateOverrides
    case _pathControl_shouldDragPathComponentCell_withPasteboard rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "pathControl:shouldDragPathComponentCell:withPasteboard:" "B@:@@@" stub_1

  -- pathControl:acceptDrop:
  stub_2 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPathControlDelegateOverrides
    case _pathControl_acceptDrop rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "pathControl:acceptDrop:" "B@:@@" stub_2

  -- pathControl:willDisplayOpenPanel:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPathControlDelegateOverrides
    case _pathControl_willDisplayOpenPanel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "pathControl:willDisplayOpenPanel:" "v@:@@" stub_3

  -- pathControl:willPopUpMenu:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPathControlDelegateOverrides
    case _pathControl_willPopUpMenu rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "pathControl:willPopUpMenu:" "v@:@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPathControlDelegateOverrides
    if queriedSel == sel_pathControl_shouldDragItem_withPasteboard then pure (maybe 0 (const 1) (_pathControl_shouldDragItem_withPasteboard rec_))
    else if queriedSel == sel_pathControl_shouldDragPathComponentCell_withPasteboard then pure (maybe 0 (const 1) (_pathControl_shouldDragPathComponentCell_withPasteboard rec_))
    else if queriedSel == sel_pathControl_acceptDrop then pure (maybe 0 (const 1) (_pathControl_acceptDrop rec_))
    else if queriedSel == sel_pathControl_willDisplayOpenPanel then pure (maybe 0 (const 1) (_pathControl_willDisplayOpenPanel rec_))
    else if queriedSel == sel_pathControl_willPopUpMenu then pure (maybe 0 (const 1) (_pathControl_willPopUpMenu rec_))
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
newNSPathControlDelegate :: NSPathControlDelegateOverrides -> IO RawId
newNSPathControlDelegate overrides = do
  inst <- class_createInstance nsPathControlDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
