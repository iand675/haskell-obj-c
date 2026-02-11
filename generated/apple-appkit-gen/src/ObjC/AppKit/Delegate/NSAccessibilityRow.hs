{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAccessibilityRow@.
--
-- Usage:
--
-- @
-- delegate <- newNSAccessibilityRow defaultNSAccessibilityRowOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAccessibilityRow
  ( NSAccessibilityRowOverrides(..)
  , defaultNSAccessibilityRowOverrides
  , newNSAccessibilityRow
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

-- | Overrides record for @\@protocol NSAccessibilityRow@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAccessibilityRowOverrides = NSAccessibilityRowOverrides
  { _accessibilityIndex :: !(Maybe (IO Int))
  , _accessibilityDisclosureLevel :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAccessibilityRowOverrides :: NSAccessibilityRowOverrides
defaultNSAccessibilityRowOverrides = NSAccessibilityRowOverrides
  { _accessibilityIndex = Nothing
  , _accessibilityDisclosureLevel = Nothing
  }

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAccessibilityRowDelegateClass #-}
nsAccessibilityRowDelegateClass :: Class
nsAccessibilityRowDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAccessibilityRow" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityIndex = unSelector (mkSelector "accessibilityIndex")
      sel_accessibilityDisclosureLevel = unSelector (mkSelector "accessibilityDisclosureLevel")
  -- accessibilityIndex
  stub_0 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityRowOverrides
    case _accessibilityIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "accessibilityIndex" "q@:" stub_0

  -- accessibilityDisclosureLevel
  stub_1 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityRowOverrides
    case _accessibilityDisclosureLevel rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "accessibilityDisclosureLevel" "q@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityRowOverrides
    if queriedSel == sel_accessibilityIndex then pure (maybe 0 (const 1) (_accessibilityIndex rec_))
    else if queriedSel == sel_accessibilityDisclosureLevel then pure (maybe 0 (const 1) (_accessibilityDisclosureLevel rec_))
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
newNSAccessibilityRow :: NSAccessibilityRowOverrides -> IO RawId
newNSAccessibilityRow overrides = do
  inst <- class_createInstance nsAccessibilityRowDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
