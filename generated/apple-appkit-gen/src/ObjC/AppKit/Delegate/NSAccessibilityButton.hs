{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAccessibilityButton@.
--
-- Usage:
--
-- @
-- delegate <- newNSAccessibilityButton defaultNSAccessibilityButtonOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAccessibilityButton
  ( NSAccessibilityButtonOverrides(..)
  , defaultNSAccessibilityButtonOverrides
  , newNSAccessibilityButton
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

-- | Overrides record for @\@protocol NSAccessibilityButton@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAccessibilityButtonOverrides = NSAccessibilityButtonOverrides
  { _accessibilityLabel :: !(Maybe (IO RawId))
  , _accessibilityPerformPress :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAccessibilityButtonOverrides :: NSAccessibilityButtonOverrides
defaultNSAccessibilityButtonOverrides = NSAccessibilityButtonOverrides
  { _accessibilityLabel = Nothing
  , _accessibilityPerformPress = Nothing
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
{-# NOINLINE nsAccessibilityButtonDelegateClass #-}
nsAccessibilityButtonDelegateClass :: Class
nsAccessibilityButtonDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAccessibilityButton" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityLabel = unSelector (mkSelector "accessibilityLabel")
      sel_accessibilityPerformPress = unSelector (mkSelector "accessibilityPerformPress")
  -- accessibilityLabel
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityButtonOverrides
    case _accessibilityLabel rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityLabel" "@@:" stub_0

  -- accessibilityPerformPress
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityButtonOverrides
    case _accessibilityPerformPress rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "accessibilityPerformPress" "B@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityButtonOverrides
    if queriedSel == sel_accessibilityLabel then pure (maybe 0 (const 1) (_accessibilityLabel rec_))
    else if queriedSel == sel_accessibilityPerformPress then pure (maybe 0 (const 1) (_accessibilityPerformPress rec_))
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
newNSAccessibilityButton :: NSAccessibilityButtonOverrides -> IO RawId
newNSAccessibilityButton overrides = do
  inst <- class_createInstance nsAccessibilityButtonDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
