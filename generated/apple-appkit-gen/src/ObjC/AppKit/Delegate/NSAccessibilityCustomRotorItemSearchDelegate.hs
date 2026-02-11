{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAccessibilityCustomRotorItemSearchDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSAccessibilityCustomRotorItemSearchDelegate defaultNSAccessibilityCustomRotorItemSearchDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAccessibilityCustomRotorItemSearchDelegate
  ( NSAccessibilityCustomRotorItemSearchDelegateOverrides(..)
  , defaultNSAccessibilityCustomRotorItemSearchDelegateOverrides
  , newNSAccessibilityCustomRotorItemSearchDelegate
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

-- | Overrides record for @\@protocol NSAccessibilityCustomRotorItemSearchDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAccessibilityCustomRotorItemSearchDelegateOverrides = NSAccessibilityCustomRotorItemSearchDelegateOverrides
  { _rotor_resultForSearchParameters :: !(Maybe (RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAccessibilityCustomRotorItemSearchDelegateOverrides :: NSAccessibilityCustomRotorItemSearchDelegateOverrides
defaultNSAccessibilityCustomRotorItemSearchDelegateOverrides = NSAccessibilityCustomRotorItemSearchDelegateOverrides
  { _rotor_resultForSearchParameters = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAccessibilityCustomRotorItemSearchDelegateDelegateClass #-}
nsAccessibilityCustomRotorItemSearchDelegateDelegateClass :: Class
nsAccessibilityCustomRotorItemSearchDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAccessibilityCustomRotorItemSearchDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_rotor_resultForSearchParameters = unSelector (mkSelector "rotor:resultForSearchParameters:")
  -- rotor:resultForSearchParameters:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityCustomRotorItemSearchDelegateOverrides
    case _rotor_resultForSearchParameters rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "rotor:resultForSearchParameters:" "@@:@@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityCustomRotorItemSearchDelegateOverrides
    if queriedSel == sel_rotor_resultForSearchParameters then pure (maybe 0 (const 1) (_rotor_resultForSearchParameters rec_))
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
newNSAccessibilityCustomRotorItemSearchDelegate :: NSAccessibilityCustomRotorItemSearchDelegateOverrides -> IO RawId
newNSAccessibilityCustomRotorItemSearchDelegate overrides = do
  inst <- class_createInstance nsAccessibilityCustomRotorItemSearchDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
