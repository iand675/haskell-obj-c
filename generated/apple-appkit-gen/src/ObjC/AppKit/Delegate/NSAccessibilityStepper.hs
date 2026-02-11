{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAccessibilityStepper@.
--
-- Usage:
--
-- @
-- delegate <- newNSAccessibilityStepper defaultNSAccessibilityStepperOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAccessibilityStepper
  ( NSAccessibilityStepperOverrides(..)
  , defaultNSAccessibilityStepperOverrides
  , newNSAccessibilityStepper
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

-- | Overrides record for @\@protocol NSAccessibilityStepper@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAccessibilityStepperOverrides = NSAccessibilityStepperOverrides
  { _accessibilityLabel :: !(Maybe (IO RawId))
  , _accessibilityPerformIncrement :: !(Maybe (IO Bool))
  , _accessibilityPerformDecrement :: !(Maybe (IO Bool))
  , _accessibilityValue :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAccessibilityStepperOverrides :: NSAccessibilityStepperOverrides
defaultNSAccessibilityStepperOverrides = NSAccessibilityStepperOverrides
  { _accessibilityLabel = Nothing
  , _accessibilityPerformIncrement = Nothing
  , _accessibilityPerformDecrement = Nothing
  , _accessibilityValue = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAccessibilityStepperDelegateClass #-}
nsAccessibilityStepperDelegateClass :: Class
nsAccessibilityStepperDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAccessibilityStepper" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityLabel = unSelector (mkSelector "accessibilityLabel")
      sel_accessibilityPerformIncrement = unSelector (mkSelector "accessibilityPerformIncrement")
      sel_accessibilityPerformDecrement = unSelector (mkSelector "accessibilityPerformDecrement")
      sel_accessibilityValue = unSelector (mkSelector "accessibilityValue")
  -- accessibilityLabel
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityStepperOverrides
    case _accessibilityLabel rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityLabel" "@@:" stub_0

  -- accessibilityPerformIncrement
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityStepperOverrides
    case _accessibilityPerformIncrement rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformIncrement" "B@:" stub_1

  -- accessibilityPerformDecrement
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityStepperOverrides
    case _accessibilityPerformDecrement rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformDecrement" "B@:" stub_2

  -- accessibilityValue
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityStepperOverrides
    case _accessibilityValue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityValue" "@@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityStepperOverrides
    if queriedSel == sel_accessibilityLabel then pure (maybe 0 (const 1) (_accessibilityLabel rec_))
    else if queriedSel == sel_accessibilityPerformIncrement then pure (maybe 0 (const 1) (_accessibilityPerformIncrement rec_))
    else if queriedSel == sel_accessibilityPerformDecrement then pure (maybe 0 (const 1) (_accessibilityPerformDecrement rec_))
    else if queriedSel == sel_accessibilityValue then pure (maybe 0 (const 1) (_accessibilityValue rec_))
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
newNSAccessibilityStepper :: NSAccessibilityStepperOverrides -> IO RawId
newNSAccessibilityStepper overrides = do
  inst <- class_createInstance nsAccessibilityStepperDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
