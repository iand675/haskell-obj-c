{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSGlyphStorage@.
--
-- Usage:
--
-- @
-- delegate <- newNSGlyphStorage defaultNSGlyphStorageOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSGlyphStorage
  ( NSGlyphStorageOverrides(..)
  , defaultNSGlyphStorageOverrides
  , newNSGlyphStorage
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

-- | Overrides record for @\@protocol NSGlyphStorage@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSGlyphStorageOverrides = NSGlyphStorageOverrides
  { _insertGlyphs_length_forStartingGlyphAtIndex_characterIndex :: !(Maybe (RawId -> Int -> Int -> Int -> IO ()))
  , _setIntAttribute_value_forGlyphAtIndex :: !(Maybe (Int -> Int -> Int -> IO ()))
  , _attributedString :: !(Maybe (IO RawId))
  , _layoutOptions :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultNSGlyphStorageOverrides :: NSGlyphStorageOverrides
defaultNSGlyphStorageOverrides = NSGlyphStorageOverrides
  { _insertGlyphs_length_forStartingGlyphAtIndex_characterIndex = Nothing
  , _setIntAttribute_value_forGlyphAtIndex = Nothing
  , _attributedString = Nothing
  , _layoutOptions = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_q_q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> CLong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> CLong -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsGlyphStorageDelegateClass #-}
nsGlyphStorageDelegateClass :: Class
nsGlyphStorageDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSGlyphStorage" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_insertGlyphs_length_forStartingGlyphAtIndex_characterIndex = unSelector (mkSelector "insertGlyphs:length:forStartingGlyphAtIndex:characterIndex:")
      sel_setIntAttribute_value_forGlyphAtIndex = unSelector (mkSelector "setIntAttribute:value:forGlyphAtIndex:")
      sel_attributedString = unSelector (mkSelector "attributedString")
      sel_layoutOptions = unSelector (mkSelector "layoutOptions")
  -- insertGlyphs:length:forStartingGlyphAtIndex:characterIndex:
  stub_0 <- wrap_at_Q_Q_Q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGlyphStorageOverrides
    case _insertGlyphs_length_forStartingGlyphAtIndex_characterIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2) (fromIntegral arg3)
  addObjCMethod cls "insertGlyphs:length:forStartingGlyphAtIndex:characterIndex:" "v@:@QQQ" stub_0

  -- setIntAttribute:value:forGlyphAtIndex:
  stub_1 <- wrap_q_q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGlyphStorageOverrides
    case _setIntAttribute_value_forGlyphAtIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setIntAttribute:value:forGlyphAtIndex:" "v@:qqQ" stub_1

  -- attributedString
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGlyphStorageOverrides
    case _attributedString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "attributedString" "@@:" stub_2

  -- layoutOptions
  stub_3 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGlyphStorageOverrides
    case _layoutOptions rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "layoutOptions" "Q@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSGlyphStorageOverrides
    if queriedSel == sel_insertGlyphs_length_forStartingGlyphAtIndex_characterIndex then pure (maybe 0 (const 1) (_insertGlyphs_length_forStartingGlyphAtIndex_characterIndex rec_))
    else if queriedSel == sel_setIntAttribute_value_forGlyphAtIndex then pure (maybe 0 (const 1) (_setIntAttribute_value_forGlyphAtIndex rec_))
    else if queriedSel == sel_attributedString then pure (maybe 0 (const 1) (_attributedString rec_))
    else if queriedSel == sel_layoutOptions then pure (maybe 0 (const 1) (_layoutOptions rec_))
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
newNSGlyphStorage :: NSGlyphStorageOverrides -> IO RawId
newNSGlyphStorage overrides = do
  inst <- class_createInstance nsGlyphStorageDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
