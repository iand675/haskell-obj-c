{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSStackViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSStackViewDelegate defaultNSStackViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSStackViewDelegate
  ( NSStackViewDelegateOverrides(..)
  , defaultNSStackViewDelegateOverrides
  , newNSStackViewDelegate
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

-- | Overrides record for @\@protocol NSStackViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSStackViewDelegateOverrides = NSStackViewDelegateOverrides
  { _stackView_willDetachViews :: !(Maybe (RawId -> RawId -> IO ()))
  , _stackView_didReattachViews :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSStackViewDelegateOverrides :: NSStackViewDelegateOverrides
defaultNSStackViewDelegateOverrides = NSStackViewDelegateOverrides
  { _stackView_willDetachViews = Nothing
  , _stackView_didReattachViews = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsStackViewDelegateDelegateClass #-}
nsStackViewDelegateDelegateClass :: Class
nsStackViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSStackViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_stackView_willDetachViews = unSelector (mkSelector "stackView:willDetachViews:")
      sel_stackView_didReattachViews = unSelector (mkSelector "stackView:didReattachViews:")
  -- stackView:willDetachViews:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStackViewDelegateOverrides
    case _stackView_willDetachViews rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "stackView:willDetachViews:" "v@:@@" stub_0

  -- stackView:didReattachViews:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStackViewDelegateOverrides
    case _stackView_didReattachViews rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "stackView:didReattachViews:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSStackViewDelegateOverrides
    if queriedSel == sel_stackView_willDetachViews then pure (maybe 0 (const 1) (_stackView_willDetachViews rec_))
    else if queriedSel == sel_stackView_didReattachViews then pure (maybe 0 (const 1) (_stackView_didReattachViews rec_))
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
newNSStackViewDelegate :: NSStackViewDelegateOverrides -> IO RawId
newNSStackViewDelegate overrides = do
  inst <- class_createInstance nsStackViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
