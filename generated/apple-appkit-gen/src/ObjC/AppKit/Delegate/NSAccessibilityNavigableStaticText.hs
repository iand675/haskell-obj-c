{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAccessibilityNavigableStaticText@.
--
-- Usage:
--
-- @
-- delegate <- newNSAccessibilityNavigableStaticText defaultNSAccessibilityNavigableStaticTextOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAccessibilityNavigableStaticText
  ( NSAccessibilityNavigableStaticTextOverrides(..)
  , defaultNSAccessibilityNavigableStaticTextOverrides
  , newNSAccessibilityNavigableStaticText
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

-- | Overrides record for @\@protocol NSAccessibilityNavigableStaticText@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAccessibilityNavigableStaticTextOverrides = NSAccessibilityNavigableStaticTextOverrides
  { _accessibilityLineForIndex :: !(Maybe (Int -> IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAccessibilityNavigableStaticTextOverrides :: NSAccessibilityNavigableStaticTextOverrides
defaultNSAccessibilityNavigableStaticTextOverrides = NSAccessibilityNavigableStaticTextOverrides
  { _accessibilityLineForIndex = Nothing
  }

foreign import ccall "wrapper"
  wrap_q_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAccessibilityNavigableStaticTextDelegateClass #-}
nsAccessibilityNavigableStaticTextDelegateClass :: Class
nsAccessibilityNavigableStaticTextDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAccessibilityNavigableStaticText" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityLineForIndex = unSelector (mkSelector "accessibilityLineForIndex:")
  -- accessibilityLineForIndex:
  stub_0 <- wrap_q_q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityNavigableStaticTextOverrides
    case _accessibilityLineForIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (fromIntegral r)
  addObjCMethod cls "accessibilityLineForIndex:" "q@:q" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityNavigableStaticTextOverrides
    if queriedSel == sel_accessibilityLineForIndex then pure (maybe 0 (const 1) (_accessibilityLineForIndex rec_))
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
newNSAccessibilityNavigableStaticText :: NSAccessibilityNavigableStaticTextOverrides -> IO RawId
newNSAccessibilityNavigableStaticText overrides = do
  inst <- class_createInstance nsAccessibilityNavigableStaticTextDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
