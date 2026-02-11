{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol WebDocumentText@.
--
-- Usage:
--
-- @
-- delegate <- newWebDocumentText defaultWebDocumentTextOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.WebKit.Delegate.WebDocumentText
  ( WebDocumentTextOverrides(..)
  , defaultWebDocumentTextOverrides
  , newWebDocumentText
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

-- | Overrides record for @\@protocol WebDocumentText@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data WebDocumentTextOverrides = WebDocumentTextOverrides
  { _supportsTextEncoding :: !(Maybe (IO Bool))
  , _string :: !(Maybe (IO RawId))
  , _attributedString :: !(Maybe (IO RawId))
  , _selectedString :: !(Maybe (IO RawId))
  , _selectedAttributedString :: !(Maybe (IO RawId))
  , _selectAll :: !(Maybe (IO ()))
  , _deselectAll :: !(Maybe (IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultWebDocumentTextOverrides :: WebDocumentTextOverrides
defaultWebDocumentTextOverrides = WebDocumentTextOverrides
  { _supportsTextEncoding = Nothing
  , _string = Nothing
  , _attributedString = Nothing
  , _selectedString = Nothing
  , _selectedAttributedString = Nothing
  , _selectAll = Nothing
  , _deselectAll = Nothing
  }

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE webDocumentTextDelegateClass #-}
webDocumentTextDelegateClass :: Class
webDocumentTextDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsWebDocumentText" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_supportsTextEncoding = unSelector (mkSelector "supportsTextEncoding")
      sel_string = unSelector (mkSelector "string")
      sel_attributedString = unSelector (mkSelector "attributedString")
      sel_selectedString = unSelector (mkSelector "selectedString")
      sel_selectedAttributedString = unSelector (mkSelector "selectedAttributedString")
      sel_selectAll = unSelector (mkSelector "selectAll")
      sel_deselectAll = unSelector (mkSelector "deselectAll")
  -- supportsTextEncoding
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentTextOverrides
    case _supportsTextEncoding rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "supportsTextEncoding" "B@:" stub_0

  -- string
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentTextOverrides
    case _string rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "string" "@@:" stub_1

  -- attributedString
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentTextOverrides
    case _attributedString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "attributedString" "@@:" stub_2

  -- selectedString
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentTextOverrides
    case _selectedString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "selectedString" "@@:" stub_3

  -- selectedAttributedString
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentTextOverrides
    case _selectedAttributedString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "selectedAttributedString" "@@:" stub_4

  -- selectAll
  stub_5 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentTextOverrides
    case _selectAll rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "selectAll" "v@:" stub_5

  -- deselectAll
  stub_6 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentTextOverrides
    case _deselectAll rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "deselectAll" "v@:" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentTextOverrides
    if queriedSel == sel_supportsTextEncoding then pure (maybe 0 (const 1) (_supportsTextEncoding rec_))
    else if queriedSel == sel_string then pure (maybe 0 (const 1) (_string rec_))
    else if queriedSel == sel_attributedString then pure (maybe 0 (const 1) (_attributedString rec_))
    else if queriedSel == sel_selectedString then pure (maybe 0 (const 1) (_selectedString rec_))
    else if queriedSel == sel_selectedAttributedString then pure (maybe 0 (const 1) (_selectedAttributedString rec_))
    else if queriedSel == sel_selectAll then pure (maybe 0 (const 1) (_selectAll rec_))
    else if queriedSel == sel_deselectAll then pure (maybe 0 (const 1) (_deselectAll rec_))
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
newWebDocumentText :: WebDocumentTextOverrides -> IO RawId
newWebDocumentText overrides = do
  inst <- class_createInstance webDocumentTextDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
