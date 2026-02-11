{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTextViewportLayoutControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSTextViewportLayoutControllerDelegate defaultNSTextViewportLayoutControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTextViewportLayoutControllerDelegate
  ( NSTextViewportLayoutControllerDelegateOverrides(..)
  , defaultNSTextViewportLayoutControllerDelegateOverrides
  , newNSTextViewportLayoutControllerDelegate
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

-- | Overrides record for @\@protocol NSTextViewportLayoutControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTextViewportLayoutControllerDelegateOverrides = NSTextViewportLayoutControllerDelegateOverrides
  { _textViewportLayoutController_configureRenderingSurfaceForTextLayoutFragment :: !(Maybe (RawId -> RawId -> IO ()))
  , _textViewportLayoutControllerWillLayout :: !(Maybe (RawId -> IO ()))
  , _textViewportLayoutControllerDidLayout :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTextViewportLayoutControllerDelegateOverrides :: NSTextViewportLayoutControllerDelegateOverrides
defaultNSTextViewportLayoutControllerDelegateOverrides = NSTextViewportLayoutControllerDelegateOverrides
  { _textViewportLayoutController_configureRenderingSurfaceForTextLayoutFragment = Nothing
  , _textViewportLayoutControllerWillLayout = Nothing
  , _textViewportLayoutControllerDidLayout = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTextViewportLayoutControllerDelegateDelegateClass #-}
nsTextViewportLayoutControllerDelegateDelegateClass :: Class
nsTextViewportLayoutControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTextViewportLayoutControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_textViewportLayoutController_configureRenderingSurfaceForTextLayoutFragment = unSelector (mkSelector "textViewportLayoutController:configureRenderingSurfaceForTextLayoutFragment:")
      sel_textViewportLayoutControllerWillLayout = unSelector (mkSelector "textViewportLayoutControllerWillLayout:")
      sel_textViewportLayoutControllerDidLayout = unSelector (mkSelector "textViewportLayoutControllerDidLayout:")
  -- textViewportLayoutController:configureRenderingSurfaceForTextLayoutFragment:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewportLayoutControllerDelegateOverrides
    case _textViewportLayoutController_configureRenderingSurfaceForTextLayoutFragment rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "textViewportLayoutController:configureRenderingSurfaceForTextLayoutFragment:" "v@:@@" stub_0

  -- textViewportLayoutControllerWillLayout:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewportLayoutControllerDelegateOverrides
    case _textViewportLayoutControllerWillLayout rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "textViewportLayoutControllerWillLayout:" "v@:@" stub_1

  -- textViewportLayoutControllerDidLayout:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewportLayoutControllerDelegateOverrides
    case _textViewportLayoutControllerDidLayout rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "textViewportLayoutControllerDidLayout:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextViewportLayoutControllerDelegateOverrides
    if queriedSel == sel_textViewportLayoutController_configureRenderingSurfaceForTextLayoutFragment then pure (maybe 0 (const 1) (_textViewportLayoutController_configureRenderingSurfaceForTextLayoutFragment rec_))
    else if queriedSel == sel_textViewportLayoutControllerWillLayout then pure (maybe 0 (const 1) (_textViewportLayoutControllerWillLayout rec_))
    else if queriedSel == sel_textViewportLayoutControllerDidLayout then pure (maybe 0 (const 1) (_textViewportLayoutControllerDidLayout rec_))
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
newNSTextViewportLayoutControllerDelegate :: NSTextViewportLayoutControllerDelegateOverrides -> IO RawId
newNSTextViewportLayoutControllerDelegate overrides = do
  inst <- class_createInstance nsTextViewportLayoutControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
