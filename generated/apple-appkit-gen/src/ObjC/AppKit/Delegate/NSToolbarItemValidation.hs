{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSToolbarItemValidation@.
--
-- Usage:
--
-- @
-- delegate <- newNSToolbarItemValidation defaultNSToolbarItemValidationOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSToolbarItemValidation
  ( NSToolbarItemValidationOverrides(..)
  , defaultNSToolbarItemValidationOverrides
  , newNSToolbarItemValidation
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

-- | Overrides record for @\@protocol NSToolbarItemValidation@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSToolbarItemValidationOverrides = NSToolbarItemValidationOverrides
  { _validateToolbarItem :: !(Maybe (RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSToolbarItemValidationOverrides :: NSToolbarItemValidationOverrides
defaultNSToolbarItemValidationOverrides = NSToolbarItemValidationOverrides
  { _validateToolbarItem = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsToolbarItemValidationDelegateClass #-}
nsToolbarItemValidationDelegateClass :: Class
nsToolbarItemValidationDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSToolbarItemValidation" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_validateToolbarItem = unSelector (mkSelector "validateToolbarItem:")
  -- validateToolbarItem:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSToolbarItemValidationOverrides
    case _validateToolbarItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "validateToolbarItem:" "B@:@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSToolbarItemValidationOverrides
    if queriedSel == sel_validateToolbarItem then pure (maybe 0 (const 1) (_validateToolbarItem rec_))
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
newNSToolbarItemValidation :: NSToolbarItemValidationOverrides -> IO RawId
newNSToolbarItemValidation overrides = do
  inst <- class_createInstance nsToolbarItemValidationDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
