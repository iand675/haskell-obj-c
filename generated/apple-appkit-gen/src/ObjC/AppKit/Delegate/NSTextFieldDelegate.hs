{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTextFieldDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSTextFieldDelegate defaultNSTextFieldDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTextFieldDelegate
  ( NSTextFieldDelegateOverrides(..)
  , defaultNSTextFieldDelegateOverrides
  , newNSTextFieldDelegate
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

-- | Overrides record for @\@protocol NSTextFieldDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTextFieldDelegateOverrides = NSTextFieldDelegateOverrides
  { _textField_textView_shouldSelectCandidateAtIndex :: !(Maybe (RawId -> RawId -> Int -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTextFieldDelegateOverrides :: NSTextFieldDelegateOverrides
defaultNSTextFieldDelegateOverrides = NSTextFieldDelegateOverrides
  { _textField_textView_shouldSelectCandidateAtIndex = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_Q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTextFieldDelegateDelegateClass #-}
nsTextFieldDelegateDelegateClass :: Class
nsTextFieldDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTextFieldDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_textField_textView_shouldSelectCandidateAtIndex = unSelector (mkSelector "textField:textView:shouldSelectCandidateAtIndex:")
  -- textField:textView:shouldSelectCandidateAtIndex:
  stub_0 <- wrap_at_at_Q_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFieldDelegateOverrides
    case _textField_textView_shouldSelectCandidateAtIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "textField:textView:shouldSelectCandidateAtIndex:" "B@:@@Q" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFieldDelegateOverrides
    if queriedSel == sel_textField_textView_shouldSelectCandidateAtIndex then pure (maybe 0 (const 1) (_textField_textView_shouldSelectCandidateAtIndex rec_))
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
newNSTextFieldDelegate :: NSTextFieldDelegateOverrides -> IO RawId
newNSTextFieldDelegate overrides = do
  inst <- class_createInstance nsTextFieldDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
