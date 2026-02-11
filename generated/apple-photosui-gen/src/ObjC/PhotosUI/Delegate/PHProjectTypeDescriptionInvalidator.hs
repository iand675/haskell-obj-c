{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PHProjectTypeDescriptionInvalidator@.
--
-- Usage:
--
-- @
-- delegate <- newPHProjectTypeDescriptionInvalidator defaultPHProjectTypeDescriptionInvalidatorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PhotosUI.Delegate.PHProjectTypeDescriptionInvalidator
  ( PHProjectTypeDescriptionInvalidatorOverrides(..)
  , defaultPHProjectTypeDescriptionInvalidatorOverrides
  , newPHProjectTypeDescriptionInvalidator
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

-- | Overrides record for @\@protocol PHProjectTypeDescriptionInvalidator@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PHProjectTypeDescriptionInvalidatorOverrides = PHProjectTypeDescriptionInvalidatorOverrides
  { _invalidateTypeDescriptionForProjectType :: !(Maybe (RawId -> IO ()))
  , _invalidateFooterTextForSubtypesOfProjectType :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultPHProjectTypeDescriptionInvalidatorOverrides :: PHProjectTypeDescriptionInvalidatorOverrides
defaultPHProjectTypeDescriptionInvalidatorOverrides = PHProjectTypeDescriptionInvalidatorOverrides
  { _invalidateTypeDescriptionForProjectType = Nothing
  , _invalidateFooterTextForSubtypesOfProjectType = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE phProjectTypeDescriptionInvalidatorDelegateClass #-}
phProjectTypeDescriptionInvalidatorDelegateClass :: Class
phProjectTypeDescriptionInvalidatorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPHProjectTypeDescriptionInvalidator" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_invalidateTypeDescriptionForProjectType = unSelector (mkSelector "invalidateTypeDescriptionForProjectType:")
      sel_invalidateFooterTextForSubtypesOfProjectType = unSelector (mkSelector "invalidateFooterTextForSubtypesOfProjectType:")
  -- invalidateTypeDescriptionForProjectType:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHProjectTypeDescriptionInvalidatorOverrides
    case _invalidateTypeDescriptionForProjectType rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "invalidateTypeDescriptionForProjectType:" "v@:@" stub_0

  -- invalidateFooterTextForSubtypesOfProjectType:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHProjectTypeDescriptionInvalidatorOverrides
    case _invalidateFooterTextForSubtypesOfProjectType rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "invalidateFooterTextForSubtypesOfProjectType:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHProjectTypeDescriptionInvalidatorOverrides
    if queriedSel == sel_invalidateTypeDescriptionForProjectType then pure (maybe 0 (const 1) (_invalidateTypeDescriptionForProjectType rec_))
    else if queriedSel == sel_invalidateFooterTextForSubtypesOfProjectType then pure (maybe 0 (const 1) (_invalidateFooterTextForSubtypesOfProjectType rec_))
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
newPHProjectTypeDescriptionInvalidator :: PHProjectTypeDescriptionInvalidatorOverrides -> IO RawId
newPHProjectTypeDescriptionInvalidator overrides = do
  inst <- class_createInstance phProjectTypeDescriptionInvalidatorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
