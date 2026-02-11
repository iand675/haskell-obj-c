{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PHProjectTypeDescriptionDataSource@.
--
-- Usage:
--
-- @
-- delegate <- newPHProjectTypeDescriptionDataSource defaultPHProjectTypeDescriptionDataSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PhotosUI.Delegate.PHProjectTypeDescriptionDataSource
  ( PHProjectTypeDescriptionDataSourceOverrides(..)
  , defaultPHProjectTypeDescriptionDataSourceOverrides
  , newPHProjectTypeDescriptionDataSource
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

-- | Overrides record for @\@protocol PHProjectTypeDescriptionDataSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PHProjectTypeDescriptionDataSourceOverrides = PHProjectTypeDescriptionDataSourceOverrides
  { _subtypesForProjectType :: !(Maybe (RawId -> IO RawId))
  , _typeDescriptionForProjectType :: !(Maybe (RawId -> IO RawId))
  , _footerTextForSubtypesOfProjectType :: !(Maybe (RawId -> IO RawId))
  , _extensionWillDiscardDataSource :: !(Maybe (IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultPHProjectTypeDescriptionDataSourceOverrides :: PHProjectTypeDescriptionDataSourceOverrides
defaultPHProjectTypeDescriptionDataSourceOverrides = PHProjectTypeDescriptionDataSourceOverrides
  { _subtypesForProjectType = Nothing
  , _typeDescriptionForProjectType = Nothing
  , _footerTextForSubtypesOfProjectType = Nothing
  , _extensionWillDiscardDataSource = Nothing
  }

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE phProjectTypeDescriptionDataSourceDelegateClass #-}
phProjectTypeDescriptionDataSourceDelegateClass :: Class
phProjectTypeDescriptionDataSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPHProjectTypeDescriptionDataSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_subtypesForProjectType = unSelector (mkSelector "subtypesForProjectType:")
      sel_typeDescriptionForProjectType = unSelector (mkSelector "typeDescriptionForProjectType:")
      sel_footerTextForSubtypesOfProjectType = unSelector (mkSelector "footerTextForSubtypesOfProjectType:")
      sel_extensionWillDiscardDataSource = unSelector (mkSelector "extensionWillDiscardDataSource")
  -- subtypesForProjectType:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHProjectTypeDescriptionDataSourceOverrides
    case _subtypesForProjectType rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "subtypesForProjectType:" "@@:@" stub_0

  -- typeDescriptionForProjectType:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHProjectTypeDescriptionDataSourceOverrides
    case _typeDescriptionForProjectType rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "typeDescriptionForProjectType:" "@@:@" stub_1

  -- footerTextForSubtypesOfProjectType:
  stub_2 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHProjectTypeDescriptionDataSourceOverrides
    case _footerTextForSubtypesOfProjectType rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "footerTextForSubtypesOfProjectType:" "@@:@" stub_2

  -- extensionWillDiscardDataSource
  stub_3 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHProjectTypeDescriptionDataSourceOverrides
    case _extensionWillDiscardDataSource rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "extensionWillDiscardDataSource" "v@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHProjectTypeDescriptionDataSourceOverrides
    if queriedSel == sel_subtypesForProjectType then pure (maybe 0 (const 1) (_subtypesForProjectType rec_))
    else if queriedSel == sel_typeDescriptionForProjectType then pure (maybe 0 (const 1) (_typeDescriptionForProjectType rec_))
    else if queriedSel == sel_footerTextForSubtypesOfProjectType then pure (maybe 0 (const 1) (_footerTextForSubtypesOfProjectType rec_))
    else if queriedSel == sel_extensionWillDiscardDataSource then pure (maybe 0 (const 1) (_extensionWillDiscardDataSource rec_))
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
newPHProjectTypeDescriptionDataSource :: PHProjectTypeDescriptionDataSourceOverrides -> IO RawId
newPHProjectTypeDescriptionDataSource overrides = do
  inst <- class_createInstance phProjectTypeDescriptionDataSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
