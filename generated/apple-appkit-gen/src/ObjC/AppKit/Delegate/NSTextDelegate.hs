{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTextDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSTextDelegate defaultNSTextDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTextDelegate
  ( NSTextDelegateOverrides(..)
  , defaultNSTextDelegateOverrides
  , newNSTextDelegate
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

-- | Overrides record for @\@protocol NSTextDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTextDelegateOverrides = NSTextDelegateOverrides
  { _textShouldBeginEditing :: !(Maybe (RawId -> IO Bool))
  , _textShouldEndEditing :: !(Maybe (RawId -> IO Bool))
  , _textDidBeginEditing :: !(Maybe (RawId -> IO ()))
  , _textDidEndEditing :: !(Maybe (RawId -> IO ()))
  , _textDidChange :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTextDelegateOverrides :: NSTextDelegateOverrides
defaultNSTextDelegateOverrides = NSTextDelegateOverrides
  { _textShouldBeginEditing = Nothing
  , _textShouldEndEditing = Nothing
  , _textDidBeginEditing = Nothing
  , _textDidEndEditing = Nothing
  , _textDidChange = Nothing
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
{-# NOINLINE nsTextDelegateDelegateClass #-}
nsTextDelegateDelegateClass :: Class
nsTextDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTextDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_textShouldBeginEditing = unSelector (mkSelector "textShouldBeginEditing:")
      sel_textShouldEndEditing = unSelector (mkSelector "textShouldEndEditing:")
      sel_textDidBeginEditing = unSelector (mkSelector "textDidBeginEditing:")
      sel_textDidEndEditing = unSelector (mkSelector "textDidEndEditing:")
      sel_textDidChange = unSelector (mkSelector "textDidChange:")
  -- textShouldBeginEditing:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextDelegateOverrides
    case _textShouldBeginEditing rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "textShouldBeginEditing:" "B@:@" stub_0

  -- textShouldEndEditing:
  stub_1 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextDelegateOverrides
    case _textShouldEndEditing rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "textShouldEndEditing:" "B@:@" stub_1

  -- textDidBeginEditing:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextDelegateOverrides
    case _textDidBeginEditing rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "textDidBeginEditing:" "v@:@" stub_2

  -- textDidEndEditing:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextDelegateOverrides
    case _textDidEndEditing rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "textDidEndEditing:" "v@:@" stub_3

  -- textDidChange:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextDelegateOverrides
    case _textDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "textDidChange:" "v@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextDelegateOverrides
    if queriedSel == sel_textShouldBeginEditing then pure (maybe 0 (const 1) (_textShouldBeginEditing rec_))
    else if queriedSel == sel_textShouldEndEditing then pure (maybe 0 (const 1) (_textShouldEndEditing rec_))
    else if queriedSel == sel_textDidBeginEditing then pure (maybe 0 (const 1) (_textDidBeginEditing rec_))
    else if queriedSel == sel_textDidEndEditing then pure (maybe 0 (const 1) (_textDidEndEditing rec_))
    else if queriedSel == sel_textDidChange then pure (maybe 0 (const 1) (_textDidChange rec_))
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
newNSTextDelegate :: NSTextDelegateOverrides -> IO RawId
newNSTextDelegate overrides = do
  inst <- class_createInstance nsTextDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
