{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSPathCellDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSPathCellDelegate defaultNSPathCellDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSPathCellDelegate
  ( NSPathCellDelegateOverrides(..)
  , defaultNSPathCellDelegateOverrides
  , newNSPathCellDelegate
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

-- | Overrides record for @\@protocol NSPathCellDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSPathCellDelegateOverrides = NSPathCellDelegateOverrides
  { _pathCell_willDisplayOpenPanel :: !(Maybe (RawId -> RawId -> IO ()))
  , _pathCell_willPopUpMenu :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSPathCellDelegateOverrides :: NSPathCellDelegateOverrides
defaultNSPathCellDelegateOverrides = NSPathCellDelegateOverrides
  { _pathCell_willDisplayOpenPanel = Nothing
  , _pathCell_willPopUpMenu = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsPathCellDelegateDelegateClass #-}
nsPathCellDelegateDelegateClass :: Class
nsPathCellDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSPathCellDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pathCell_willDisplayOpenPanel = unSelector (mkSelector "pathCell:willDisplayOpenPanel:")
      sel_pathCell_willPopUpMenu = unSelector (mkSelector "pathCell:willPopUpMenu:")
  -- pathCell:willDisplayOpenPanel:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPathCellDelegateOverrides
    case _pathCell_willDisplayOpenPanel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "pathCell:willDisplayOpenPanel:" "v@:@@" stub_0

  -- pathCell:willPopUpMenu:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPathCellDelegateOverrides
    case _pathCell_willPopUpMenu rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "pathCell:willPopUpMenu:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPathCellDelegateOverrides
    if queriedSel == sel_pathCell_willDisplayOpenPanel then pure (maybe 0 (const 1) (_pathCell_willDisplayOpenPanel rec_))
    else if queriedSel == sel_pathCell_willPopUpMenu then pure (maybe 0 (const 1) (_pathCell_willPopUpMenu rec_))
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
newNSPathCellDelegate :: NSPathCellDelegateOverrides -> IO RawId
newNSPathCellDelegate overrides = do
  inst <- class_createInstance nsPathCellDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
