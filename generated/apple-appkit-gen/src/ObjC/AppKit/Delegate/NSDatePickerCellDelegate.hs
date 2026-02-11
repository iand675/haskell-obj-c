{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSDatePickerCellDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSDatePickerCellDelegate defaultNSDatePickerCellDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSDatePickerCellDelegate
  ( NSDatePickerCellDelegateOverrides(..)
  , defaultNSDatePickerCellDelegateOverrides
  , newNSDatePickerCellDelegate
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

-- | Overrides record for @\@protocol NSDatePickerCellDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSDatePickerCellDelegateOverrides = NSDatePickerCellDelegateOverrides
  { _datePickerCell_validateProposedDateValue_timeInterval :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSDatePickerCellDelegateOverrides :: NSDatePickerCellDelegateOverrides
defaultNSDatePickerCellDelegateOverrides = NSDatePickerCellDelegateOverrides
  { _datePickerCell_validateProposedDateValue_timeInterval = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsDatePickerCellDelegateDelegateClass #-}
nsDatePickerCellDelegateDelegateClass :: Class
nsDatePickerCellDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSDatePickerCellDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_datePickerCell_validateProposedDateValue_timeInterval = unSelector (mkSelector "datePickerCell:validateProposedDateValue:timeInterval:")
  -- datePickerCell:validateProposedDateValue:timeInterval:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDatePickerCellDelegateOverrides
    case _datePickerCell_validateProposedDateValue_timeInterval rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "datePickerCell:validateProposedDateValue:timeInterval:" "v@:@@@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDatePickerCellDelegateOverrides
    if queriedSel == sel_datePickerCell_validateProposedDateValue_timeInterval then pure (maybe 0 (const 1) (_datePickerCell_validateProposedDateValue_timeInterval rec_))
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
newNSDatePickerCellDelegate :: NSDatePickerCellDelegateOverrides -> IO RawId
newNSDatePickerCellDelegate overrides = do
  inst <- class_createInstance nsDatePickerCellDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
