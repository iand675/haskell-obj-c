{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSControlTextEditingDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSControlTextEditingDelegate defaultNSControlTextEditingDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSControlTextEditingDelegate
  ( NSControlTextEditingDelegateOverrides(..)
  , defaultNSControlTextEditingDelegateOverrides
  , newNSControlTextEditingDelegate
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

-- | Overrides record for @\@protocol NSControlTextEditingDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSControlTextEditingDelegateOverrides = NSControlTextEditingDelegateOverrides
  { _controlTextDidBeginEditing :: !(Maybe (RawId -> IO ()))
  , _controlTextDidEndEditing :: !(Maybe (RawId -> IO ()))
  , _controlTextDidChange :: !(Maybe (RawId -> IO ()))
  , _control_textShouldBeginEditing :: !(Maybe (RawId -> RawId -> IO Bool))
  , _control_textShouldEndEditing :: !(Maybe (RawId -> RawId -> IO Bool))
  , _control_didFailToFormatString_errorDescription :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _control_didFailToValidatePartialString_errorDescription :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _control_isValidObject :: !(Maybe (RawId -> RawId -> IO Bool))
  , _control_textView_doCommandBySelector :: !(Maybe (RawId -> RawId -> Sel -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSControlTextEditingDelegateOverrides :: NSControlTextEditingDelegateOverrides
defaultNSControlTextEditingDelegateOverrides = NSControlTextEditingDelegateOverrides
  { _controlTextDidBeginEditing = Nothing
  , _controlTextDidEndEditing = Nothing
  , _controlTextDidChange = Nothing
  , _control_textShouldBeginEditing = Nothing
  , _control_textShouldEndEditing = Nothing
  , _control_didFailToFormatString_errorDescription = Nothing
  , _control_didFailToValidatePartialString_errorDescription = Nothing
  , _control_isValidObject = Nothing
  , _control_textView_doCommandBySelector = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_sel_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsControlTextEditingDelegateDelegateClass #-}
nsControlTextEditingDelegateDelegateClass :: Class
nsControlTextEditingDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSControlTextEditingDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_controlTextDidBeginEditing = unSelector (mkSelector "controlTextDidBeginEditing:")
      sel_controlTextDidEndEditing = unSelector (mkSelector "controlTextDidEndEditing:")
      sel_controlTextDidChange = unSelector (mkSelector "controlTextDidChange:")
      sel_control_textShouldBeginEditing = unSelector (mkSelector "control:textShouldBeginEditing:")
      sel_control_textShouldEndEditing = unSelector (mkSelector "control:textShouldEndEditing:")
      sel_control_didFailToFormatString_errorDescription = unSelector (mkSelector "control:didFailToFormatString:errorDescription:")
      sel_control_didFailToValidatePartialString_errorDescription = unSelector (mkSelector "control:didFailToValidatePartialString:errorDescription:")
      sel_control_isValidObject = unSelector (mkSelector "control:isValidObject:")
      sel_control_textView_doCommandBySelector = unSelector (mkSelector "control:textView:doCommandBySelector:")
  -- controlTextDidBeginEditing:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSControlTextEditingDelegateOverrides
    case _controlTextDidBeginEditing rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "controlTextDidBeginEditing:" "v@:@" stub_0

  -- controlTextDidEndEditing:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSControlTextEditingDelegateOverrides
    case _controlTextDidEndEditing rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "controlTextDidEndEditing:" "v@:@" stub_1

  -- controlTextDidChange:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSControlTextEditingDelegateOverrides
    case _controlTextDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "controlTextDidChange:" "v@:@" stub_2

  -- control:textShouldBeginEditing:
  stub_3 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSControlTextEditingDelegateOverrides
    case _control_textShouldBeginEditing rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "control:textShouldBeginEditing:" "B@:@@" stub_3

  -- control:textShouldEndEditing:
  stub_4 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSControlTextEditingDelegateOverrides
    case _control_textShouldEndEditing rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "control:textShouldEndEditing:" "B@:@@" stub_4

  -- control:didFailToFormatString:errorDescription:
  stub_5 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSControlTextEditingDelegateOverrides
    case _control_didFailToFormatString_errorDescription rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "control:didFailToFormatString:errorDescription:" "B@:@@@" stub_5

  -- control:didFailToValidatePartialString:errorDescription:
  stub_6 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSControlTextEditingDelegateOverrides
    case _control_didFailToValidatePartialString_errorDescription rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "control:didFailToValidatePartialString:errorDescription:" "v@:@@@" stub_6

  -- control:isValidObject:
  stub_7 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSControlTextEditingDelegateOverrides
    case _control_isValidObject rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "control:isValidObject:" "B@:@@" stub_7

  -- control:textView:doCommandBySelector:
  stub_8 <- wrap_at_at_sel_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSControlTextEditingDelegateOverrides
    case _control_textView_doCommandBySelector rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (Selector arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "control:textView:doCommandBySelector:" "B@:@@:" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSControlTextEditingDelegateOverrides
    if queriedSel == sel_controlTextDidBeginEditing then pure (maybe 0 (const 1) (_controlTextDidBeginEditing rec_))
    else if queriedSel == sel_controlTextDidEndEditing then pure (maybe 0 (const 1) (_controlTextDidEndEditing rec_))
    else if queriedSel == sel_controlTextDidChange then pure (maybe 0 (const 1) (_controlTextDidChange rec_))
    else if queriedSel == sel_control_textShouldBeginEditing then pure (maybe 0 (const 1) (_control_textShouldBeginEditing rec_))
    else if queriedSel == sel_control_textShouldEndEditing then pure (maybe 0 (const 1) (_control_textShouldEndEditing rec_))
    else if queriedSel == sel_control_didFailToFormatString_errorDescription then pure (maybe 0 (const 1) (_control_didFailToFormatString_errorDescription rec_))
    else if queriedSel == sel_control_didFailToValidatePartialString_errorDescription then pure (maybe 0 (const 1) (_control_didFailToValidatePartialString_errorDescription rec_))
    else if queriedSel == sel_control_isValidObject then pure (maybe 0 (const 1) (_control_isValidObject rec_))
    else if queriedSel == sel_control_textView_doCommandBySelector then pure (maybe 0 (const 1) (_control_textView_doCommandBySelector rec_))
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
newNSControlTextEditingDelegate :: NSControlTextEditingDelegateOverrides -> IO RawId
newNSControlTextEditingDelegate overrides = do
  inst <- class_createInstance nsControlTextEditingDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
