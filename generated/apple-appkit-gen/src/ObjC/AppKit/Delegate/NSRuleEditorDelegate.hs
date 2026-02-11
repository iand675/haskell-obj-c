{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSRuleEditorDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSRuleEditorDelegate defaultNSRuleEditorDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSRuleEditorDelegate
  ( NSRuleEditorDelegateOverrides(..)
  , defaultNSRuleEditorDelegateOverrides
  , newNSRuleEditorDelegate
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

-- | Overrides record for @\@protocol NSRuleEditorDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSRuleEditorDelegateOverrides = NSRuleEditorDelegateOverrides
  { _ruleEditor_displayValueForCriterion_inRow :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  , _ruleEditor_predicatePartsForCriterion_withDisplayValue_inRow :: !(Maybe (RawId -> RawId -> RawId -> Int -> IO RawId))
  , _ruleEditorRowsDidChange :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSRuleEditorDelegateOverrides :: NSRuleEditorDelegateOverrides
defaultNSRuleEditorDelegateOverrides = NSRuleEditorDelegateOverrides
  { _ruleEditor_displayValueForCriterion_inRow = Nothing
  , _ruleEditor_predicatePartsForCriterion_withDisplayValue_inRow = Nothing
  , _ruleEditorRowsDidChange = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsRuleEditorDelegateDelegateClass #-}
nsRuleEditorDelegateDelegateClass :: Class
nsRuleEditorDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSRuleEditorDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_ruleEditor_displayValueForCriterion_inRow = unSelector (mkSelector "ruleEditor:displayValueForCriterion:inRow:")
      sel_ruleEditor_predicatePartsForCriterion_withDisplayValue_inRow = unSelector (mkSelector "ruleEditor:predicatePartsForCriterion:withDisplayValue:inRow:")
      sel_ruleEditorRowsDidChange = unSelector (mkSelector "ruleEditorRowsDidChange:")
  -- ruleEditor:displayValueForCriterion:inRow:
  stub_0 <- wrap_at_at_q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSRuleEditorDelegateOverrides
    case _ruleEditor_displayValueForCriterion_inRow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "ruleEditor:displayValueForCriterion:inRow:" "@@:@@q" stub_0

  -- ruleEditor:predicatePartsForCriterion:withDisplayValue:inRow:
  stub_1 <- wrap_at_at_at_q_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSRuleEditorDelegateOverrides
    case _ruleEditor_predicatePartsForCriterion_withDisplayValue_inRow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (fromIntegral arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "ruleEditor:predicatePartsForCriterion:withDisplayValue:inRow:" "@@:@@@q" stub_1

  -- ruleEditorRowsDidChange:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSRuleEditorDelegateOverrides
    case _ruleEditorRowsDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "ruleEditorRowsDidChange:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSRuleEditorDelegateOverrides
    if queriedSel == sel_ruleEditor_displayValueForCriterion_inRow then pure (maybe 0 (const 1) (_ruleEditor_displayValueForCriterion_inRow rec_))
    else if queriedSel == sel_ruleEditor_predicatePartsForCriterion_withDisplayValue_inRow then pure (maybe 0 (const 1) (_ruleEditor_predicatePartsForCriterion_withDisplayValue_inRow rec_))
    else if queriedSel == sel_ruleEditorRowsDidChange then pure (maybe 0 (const 1) (_ruleEditorRowsDidChange rec_))
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
newNSRuleEditorDelegate :: NSRuleEditorDelegateOverrides -> IO RawId
newNSRuleEditorDelegate overrides = do
  inst <- class_createInstance nsRuleEditorDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
