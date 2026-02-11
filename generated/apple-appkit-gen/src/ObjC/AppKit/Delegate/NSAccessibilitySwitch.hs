{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAccessibilitySwitch@.
--
-- Usage:
--
-- @
-- delegate <- newNSAccessibilitySwitch defaultNSAccessibilitySwitchOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAccessibilitySwitch
  ( NSAccessibilitySwitchOverrides(..)
  , defaultNSAccessibilitySwitchOverrides
  , newNSAccessibilitySwitch
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

-- | Overrides record for @\@protocol NSAccessibilitySwitch@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAccessibilitySwitchOverrides = NSAccessibilitySwitchOverrides
  { _accessibilityValue :: !(Maybe (IO RawId))
  , _accessibilityPerformIncrement :: !(Maybe (IO Bool))
  , _accessibilityPerformDecrement :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAccessibilitySwitchOverrides :: NSAccessibilitySwitchOverrides
defaultNSAccessibilitySwitchOverrides = NSAccessibilitySwitchOverrides
  { _accessibilityValue = Nothing
  , _accessibilityPerformIncrement = Nothing
  , _accessibilityPerformDecrement = Nothing
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
{-# NOINLINE nsAccessibilitySwitchDelegateClass #-}
nsAccessibilitySwitchDelegateClass :: Class
nsAccessibilitySwitchDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAccessibilitySwitch" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityValue = unSelector (mkSelector "accessibilityValue")
      sel_accessibilityPerformIncrement = unSelector (mkSelector "accessibilityPerformIncrement")
      sel_accessibilityPerformDecrement = unSelector (mkSelector "accessibilityPerformDecrement")
  -- accessibilityValue
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilitySwitchOverrides
    case _accessibilityValue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityValue" "@@:" stub_0

  -- accessibilityPerformIncrement
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilitySwitchOverrides
    case _accessibilityPerformIncrement rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformIncrement" "B@:" stub_1

  -- accessibilityPerformDecrement
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilitySwitchOverrides
    case _accessibilityPerformDecrement rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformDecrement" "B@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilitySwitchOverrides
    if queriedSel == sel_accessibilityValue then pure (maybe 0 (const 1) (_accessibilityValue rec_))
    else if queriedSel == sel_accessibilityPerformIncrement then pure (maybe 0 (const 1) (_accessibilityPerformIncrement rec_))
    else if queriedSel == sel_accessibilityPerformDecrement then pure (maybe 0 (const 1) (_accessibilityPerformDecrement rec_))
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
newNSAccessibilitySwitch :: NSAccessibilitySwitchOverrides -> IO RawId
newNSAccessibilitySwitch overrides = do
  inst <- class_createInstance nsAccessibilitySwitchDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
