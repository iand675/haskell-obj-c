{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAppearanceCustomization@.
--
-- Usage:
--
-- @
-- delegate <- newNSAppearanceCustomization defaultNSAppearanceCustomizationOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAppearanceCustomization
  ( NSAppearanceCustomizationOverrides(..)
  , defaultNSAppearanceCustomizationOverrides
  , newNSAppearanceCustomization
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

-- | Overrides record for @\@protocol NSAppearanceCustomization@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAppearanceCustomizationOverrides = NSAppearanceCustomizationOverrides
  { _appearance :: !(Maybe (IO RawId))
  , _setAppearance :: !(Maybe (RawId -> IO ()))
  , _effectiveAppearance :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAppearanceCustomizationOverrides :: NSAppearanceCustomizationOverrides
defaultNSAppearanceCustomizationOverrides = NSAppearanceCustomizationOverrides
  { _appearance = Nothing
  , _setAppearance = Nothing
  , _effectiveAppearance = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAppearanceCustomizationDelegateClass #-}
nsAppearanceCustomizationDelegateClass :: Class
nsAppearanceCustomizationDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAppearanceCustomization" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_appearance = unSelector (mkSelector "appearance")
      sel_setAppearance = unSelector (mkSelector "setAppearance:")
      sel_effectiveAppearance = unSelector (mkSelector "effectiveAppearance")
  -- appearance
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAppearanceCustomizationOverrides
    case _appearance rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "appearance" "@@:" stub_0

  -- setAppearance:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAppearanceCustomizationOverrides
    case _setAppearance rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAppearance:" "v@:@" stub_1

  -- effectiveAppearance
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAppearanceCustomizationOverrides
    case _effectiveAppearance rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "effectiveAppearance" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAppearanceCustomizationOverrides
    if queriedSel == sel_appearance then pure (maybe 0 (const 1) (_appearance rec_))
    else if queriedSel == sel_setAppearance then pure (maybe 0 (const 1) (_setAppearance rec_))
    else if queriedSel == sel_effectiveAppearance then pure (maybe 0 (const 1) (_effectiveAppearance rec_))
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
newNSAppearanceCustomization :: NSAppearanceCustomizationOverrides -> IO RawId
newNSAppearanceCustomization overrides = do
  inst <- class_createInstance nsAppearanceCustomizationDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
