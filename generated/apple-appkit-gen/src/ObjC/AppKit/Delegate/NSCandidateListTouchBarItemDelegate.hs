{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSCandidateListTouchBarItemDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSCandidateListTouchBarItemDelegate defaultNSCandidateListTouchBarItemDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSCandidateListTouchBarItemDelegate
  ( NSCandidateListTouchBarItemDelegateOverrides(..)
  , defaultNSCandidateListTouchBarItemDelegateOverrides
  , newNSCandidateListTouchBarItemDelegate
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

-- | Overrides record for @\@protocol NSCandidateListTouchBarItemDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSCandidateListTouchBarItemDelegateOverrides = NSCandidateListTouchBarItemDelegateOverrides
  { _candidateListTouchBarItem_beginSelectingCandidateAtIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _candidateListTouchBarItem_changeSelectionFromCandidateAtIndex_toIndex :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _candidateListTouchBarItem_endSelectingCandidateAtIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _candidateListTouchBarItem_changedCandidateListVisibility :: !(Maybe (RawId -> Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSCandidateListTouchBarItemDelegateOverrides :: NSCandidateListTouchBarItemDelegateOverrides
defaultNSCandidateListTouchBarItemDelegateOverrides = NSCandidateListTouchBarItemDelegateOverrides
  { _candidateListTouchBarItem_beginSelectingCandidateAtIndex = Nothing
  , _candidateListTouchBarItem_changeSelectionFromCandidateAtIndex_toIndex = Nothing
  , _candidateListTouchBarItem_endSelectingCandidateAtIndex = Nothing
  , _candidateListTouchBarItem_changedCandidateListVisibility = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsCandidateListTouchBarItemDelegateDelegateClass #-}
nsCandidateListTouchBarItemDelegateDelegateClass :: Class
nsCandidateListTouchBarItemDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSCandidateListTouchBarItemDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_candidateListTouchBarItem_beginSelectingCandidateAtIndex = unSelector (mkSelector "candidateListTouchBarItem:beginSelectingCandidateAtIndex:")
      sel_candidateListTouchBarItem_changeSelectionFromCandidateAtIndex_toIndex = unSelector (mkSelector "candidateListTouchBarItem:changeSelectionFromCandidateAtIndex:toIndex:")
      sel_candidateListTouchBarItem_endSelectingCandidateAtIndex = unSelector (mkSelector "candidateListTouchBarItem:endSelectingCandidateAtIndex:")
      sel_candidateListTouchBarItem_changedCandidateListVisibility = unSelector (mkSelector "candidateListTouchBarItem:changedCandidateListVisibility:")
  -- candidateListTouchBarItem:beginSelectingCandidateAtIndex:
  stub_0 <- wrap_at_q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCandidateListTouchBarItemDelegateOverrides
    case _candidateListTouchBarItem_beginSelectingCandidateAtIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "candidateListTouchBarItem:beginSelectingCandidateAtIndex:" "v@:@q" stub_0

  -- candidateListTouchBarItem:changeSelectionFromCandidateAtIndex:toIndex:
  stub_1 <- wrap_at_q_q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCandidateListTouchBarItemDelegateOverrides
    case _candidateListTouchBarItem_changeSelectionFromCandidateAtIndex_toIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "candidateListTouchBarItem:changeSelectionFromCandidateAtIndex:toIndex:" "v@:@qq" stub_1

  -- candidateListTouchBarItem:endSelectingCandidateAtIndex:
  stub_2 <- wrap_at_q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCandidateListTouchBarItemDelegateOverrides
    case _candidateListTouchBarItem_endSelectingCandidateAtIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "candidateListTouchBarItem:endSelectingCandidateAtIndex:" "v@:@q" stub_2

  -- candidateListTouchBarItem:changedCandidateListVisibility:
  stub_3 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCandidateListTouchBarItemDelegateOverrides
    case _candidateListTouchBarItem_changedCandidateListVisibility rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "candidateListTouchBarItem:changedCandidateListVisibility:" "v@:@B" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCandidateListTouchBarItemDelegateOverrides
    if queriedSel == sel_candidateListTouchBarItem_beginSelectingCandidateAtIndex then pure (maybe 0 (const 1) (_candidateListTouchBarItem_beginSelectingCandidateAtIndex rec_))
    else if queriedSel == sel_candidateListTouchBarItem_changeSelectionFromCandidateAtIndex_toIndex then pure (maybe 0 (const 1) (_candidateListTouchBarItem_changeSelectionFromCandidateAtIndex_toIndex rec_))
    else if queriedSel == sel_candidateListTouchBarItem_endSelectingCandidateAtIndex then pure (maybe 0 (const 1) (_candidateListTouchBarItem_endSelectingCandidateAtIndex rec_))
    else if queriedSel == sel_candidateListTouchBarItem_changedCandidateListVisibility then pure (maybe 0 (const 1) (_candidateListTouchBarItem_changedCandidateListVisibility rec_))
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
newNSCandidateListTouchBarItemDelegate :: NSCandidateListTouchBarItemDelegateOverrides -> IO RawId
newNSCandidateListTouchBarItemDelegate overrides = do
  inst <- class_createInstance nsCandidateListTouchBarItemDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
