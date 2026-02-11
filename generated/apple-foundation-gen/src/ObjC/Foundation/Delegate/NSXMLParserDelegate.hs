{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSXMLParserDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSXMLParserDelegate defaultNSXMLParserDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSXMLParserDelegate
  ( NSXMLParserDelegateOverrides(..)
  , defaultNSXMLParserDelegateOverrides
  , newNSXMLParserDelegate
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

-- | Overrides record for @\@protocol NSXMLParserDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSXMLParserDelegateOverrides = NSXMLParserDelegateOverrides
  { _parserDidStartDocument :: !(Maybe (RawId -> IO ()))
  , _parserDidEndDocument :: !(Maybe (RawId -> IO ()))
  , _parser_foundNotationDeclarationWithName_publicID_systemID :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _parser_foundUnparsedEntityDeclarationWithName_publicID_systemID_notationName :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> IO ()))
  , _parser_foundAttributeDeclarationWithName_forElement_type_defaultValue :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> IO ()))
  , _parser_foundElementDeclarationWithName_model :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _parser_foundInternalEntityDeclarationWithName_value :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _parser_foundExternalEntityDeclarationWithName_publicID_systemID :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _parser_didStartElement_namespaceURI_qualifiedName_attributes :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> IO ()))
  , _parser_didEndElement_namespaceURI_qualifiedName :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _parser_didStartMappingPrefix_toURI :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _parser_didEndMappingPrefix :: !(Maybe (RawId -> RawId -> IO ()))
  , _parser_foundCharacters :: !(Maybe (RawId -> RawId -> IO ()))
  , _parser_foundIgnorableWhitespace :: !(Maybe (RawId -> RawId -> IO ()))
  , _parser_foundProcessingInstructionWithTarget_data :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _parser_foundComment :: !(Maybe (RawId -> RawId -> IO ()))
  , _parser_foundCDATA :: !(Maybe (RawId -> RawId -> IO ()))
  , _parser_resolveExternalEntityName_systemID :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _parser_parseErrorOccurred :: !(Maybe (RawId -> RawId -> IO ()))
  , _parser_validationErrorOccurred :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSXMLParserDelegateOverrides :: NSXMLParserDelegateOverrides
defaultNSXMLParserDelegateOverrides = NSXMLParserDelegateOverrides
  { _parserDidStartDocument = Nothing
  , _parserDidEndDocument = Nothing
  , _parser_foundNotationDeclarationWithName_publicID_systemID = Nothing
  , _parser_foundUnparsedEntityDeclarationWithName_publicID_systemID_notationName = Nothing
  , _parser_foundAttributeDeclarationWithName_forElement_type_defaultValue = Nothing
  , _parser_foundElementDeclarationWithName_model = Nothing
  , _parser_foundInternalEntityDeclarationWithName_value = Nothing
  , _parser_foundExternalEntityDeclarationWithName_publicID_systemID = Nothing
  , _parser_didStartElement_namespaceURI_qualifiedName_attributes = Nothing
  , _parser_didEndElement_namespaceURI_qualifiedName = Nothing
  , _parser_didStartMappingPrefix_toURI = Nothing
  , _parser_didEndMappingPrefix = Nothing
  , _parser_foundCharacters = Nothing
  , _parser_foundIgnorableWhitespace = Nothing
  , _parser_foundProcessingInstructionWithTarget_data = Nothing
  , _parser_foundComment = Nothing
  , _parser_foundCDATA = Nothing
  , _parser_resolveExternalEntityName_systemID = Nothing
  , _parser_parseErrorOccurred = Nothing
  , _parser_validationErrorOccurred = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsxmlParserDelegateDelegateClass #-}
nsxmlParserDelegateDelegateClass :: Class
nsxmlParserDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSXMLParserDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_parserDidStartDocument = unSelector (mkSelector "parserDidStartDocument:")
      sel_parserDidEndDocument = unSelector (mkSelector "parserDidEndDocument:")
      sel_parser_foundNotationDeclarationWithName_publicID_systemID = unSelector (mkSelector "parser:foundNotationDeclarationWithName:publicID:systemID:")
      sel_parser_foundUnparsedEntityDeclarationWithName_publicID_systemID_notationName = unSelector (mkSelector "parser:foundUnparsedEntityDeclarationWithName:publicID:systemID:notationName:")
      sel_parser_foundAttributeDeclarationWithName_forElement_type_defaultValue = unSelector (mkSelector "parser:foundAttributeDeclarationWithName:forElement:type:defaultValue:")
      sel_parser_foundElementDeclarationWithName_model = unSelector (mkSelector "parser:foundElementDeclarationWithName:model:")
      sel_parser_foundInternalEntityDeclarationWithName_value = unSelector (mkSelector "parser:foundInternalEntityDeclarationWithName:value:")
      sel_parser_foundExternalEntityDeclarationWithName_publicID_systemID = unSelector (mkSelector "parser:foundExternalEntityDeclarationWithName:publicID:systemID:")
      sel_parser_didStartElement_namespaceURI_qualifiedName_attributes = unSelector (mkSelector "parser:didStartElement:namespaceURI:qualifiedName:attributes:")
      sel_parser_didEndElement_namespaceURI_qualifiedName = unSelector (mkSelector "parser:didEndElement:namespaceURI:qualifiedName:")
      sel_parser_didStartMappingPrefix_toURI = unSelector (mkSelector "parser:didStartMappingPrefix:toURI:")
      sel_parser_didEndMappingPrefix = unSelector (mkSelector "parser:didEndMappingPrefix:")
      sel_parser_foundCharacters = unSelector (mkSelector "parser:foundCharacters:")
      sel_parser_foundIgnorableWhitespace = unSelector (mkSelector "parser:foundIgnorableWhitespace:")
      sel_parser_foundProcessingInstructionWithTarget_data = unSelector (mkSelector "parser:foundProcessingInstructionWithTarget:data:")
      sel_parser_foundComment = unSelector (mkSelector "parser:foundComment:")
      sel_parser_foundCDATA = unSelector (mkSelector "parser:foundCDATA:")
      sel_parser_resolveExternalEntityName_systemID = unSelector (mkSelector "parser:resolveExternalEntityName:systemID:")
      sel_parser_parseErrorOccurred = unSelector (mkSelector "parser:parseErrorOccurred:")
      sel_parser_validationErrorOccurred = unSelector (mkSelector "parser:validationErrorOccurred:")
  -- parserDidStartDocument:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parserDidStartDocument rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "parserDidStartDocument:" "v@:@" stub_0

  -- parserDidEndDocument:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parserDidEndDocument rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "parserDidEndDocument:" "v@:@" stub_1

  -- parser:foundNotationDeclarationWithName:publicID:systemID:
  stub_2 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_foundNotationDeclarationWithName_publicID_systemID rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "parser:foundNotationDeclarationWithName:publicID:systemID:" "v@:@@@@" stub_2

  -- parser:foundUnparsedEntityDeclarationWithName:publicID:systemID:notationName:
  stub_3 <- wrap_at_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_foundUnparsedEntityDeclarationWithName_publicID_systemID_notationName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4)
  addObjCMethod cls "parser:foundUnparsedEntityDeclarationWithName:publicID:systemID:notationName:" "v@:@@@@@" stub_3

  -- parser:foundAttributeDeclarationWithName:forElement:type:defaultValue:
  stub_4 <- wrap_at_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_foundAttributeDeclarationWithName_forElement_type_defaultValue rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4)
  addObjCMethod cls "parser:foundAttributeDeclarationWithName:forElement:type:defaultValue:" "v@:@@@@@" stub_4

  -- parser:foundElementDeclarationWithName:model:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_foundElementDeclarationWithName_model rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "parser:foundElementDeclarationWithName:model:" "v@:@@@" stub_5

  -- parser:foundInternalEntityDeclarationWithName:value:
  stub_6 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_foundInternalEntityDeclarationWithName_value rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "parser:foundInternalEntityDeclarationWithName:value:" "v@:@@@" stub_6

  -- parser:foundExternalEntityDeclarationWithName:publicID:systemID:
  stub_7 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_foundExternalEntityDeclarationWithName_publicID_systemID rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "parser:foundExternalEntityDeclarationWithName:publicID:systemID:" "v@:@@@@" stub_7

  -- parser:didStartElement:namespaceURI:qualifiedName:attributes:
  stub_8 <- wrap_at_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_didStartElement_namespaceURI_qualifiedName_attributes rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4)
  addObjCMethod cls "parser:didStartElement:namespaceURI:qualifiedName:attributes:" "v@:@@@@@" stub_8

  -- parser:didEndElement:namespaceURI:qualifiedName:
  stub_9 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_didEndElement_namespaceURI_qualifiedName rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "parser:didEndElement:namespaceURI:qualifiedName:" "v@:@@@@" stub_9

  -- parser:didStartMappingPrefix:toURI:
  stub_10 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_didStartMappingPrefix_toURI rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "parser:didStartMappingPrefix:toURI:" "v@:@@@" stub_10

  -- parser:didEndMappingPrefix:
  stub_11 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_didEndMappingPrefix rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "parser:didEndMappingPrefix:" "v@:@@" stub_11

  -- parser:foundCharacters:
  stub_12 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_foundCharacters rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "parser:foundCharacters:" "v@:@@" stub_12

  -- parser:foundIgnorableWhitespace:
  stub_13 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_foundIgnorableWhitespace rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "parser:foundIgnorableWhitespace:" "v@:@@" stub_13

  -- parser:foundProcessingInstructionWithTarget:data:
  stub_14 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_foundProcessingInstructionWithTarget_data rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "parser:foundProcessingInstructionWithTarget:data:" "v@:@@@" stub_14

  -- parser:foundComment:
  stub_15 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_foundComment rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "parser:foundComment:" "v@:@@" stub_15

  -- parser:foundCDATA:
  stub_16 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_foundCDATA rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "parser:foundCDATA:" "v@:@@" stub_16

  -- parser:resolveExternalEntityName:systemID:
  stub_17 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_resolveExternalEntityName_systemID rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "parser:resolveExternalEntityName:systemID:" "@@:@@@" stub_17

  -- parser:parseErrorOccurred:
  stub_18 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_parseErrorOccurred rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "parser:parseErrorOccurred:" "v@:@@" stub_18

  -- parser:validationErrorOccurred:
  stub_19 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    case _parser_validationErrorOccurred rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "parser:validationErrorOccurred:" "v@:@@" stub_19

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSXMLParserDelegateOverrides
    if queriedSel == sel_parserDidStartDocument then pure (maybe 0 (const 1) (_parserDidStartDocument rec_))
    else if queriedSel == sel_parserDidEndDocument then pure (maybe 0 (const 1) (_parserDidEndDocument rec_))
    else if queriedSel == sel_parser_foundNotationDeclarationWithName_publicID_systemID then pure (maybe 0 (const 1) (_parser_foundNotationDeclarationWithName_publicID_systemID rec_))
    else if queriedSel == sel_parser_foundUnparsedEntityDeclarationWithName_publicID_systemID_notationName then pure (maybe 0 (const 1) (_parser_foundUnparsedEntityDeclarationWithName_publicID_systemID_notationName rec_))
    else if queriedSel == sel_parser_foundAttributeDeclarationWithName_forElement_type_defaultValue then pure (maybe 0 (const 1) (_parser_foundAttributeDeclarationWithName_forElement_type_defaultValue rec_))
    else if queriedSel == sel_parser_foundElementDeclarationWithName_model then pure (maybe 0 (const 1) (_parser_foundElementDeclarationWithName_model rec_))
    else if queriedSel == sel_parser_foundInternalEntityDeclarationWithName_value then pure (maybe 0 (const 1) (_parser_foundInternalEntityDeclarationWithName_value rec_))
    else if queriedSel == sel_parser_foundExternalEntityDeclarationWithName_publicID_systemID then pure (maybe 0 (const 1) (_parser_foundExternalEntityDeclarationWithName_publicID_systemID rec_))
    else if queriedSel == sel_parser_didStartElement_namespaceURI_qualifiedName_attributes then pure (maybe 0 (const 1) (_parser_didStartElement_namespaceURI_qualifiedName_attributes rec_))
    else if queriedSel == sel_parser_didEndElement_namespaceURI_qualifiedName then pure (maybe 0 (const 1) (_parser_didEndElement_namespaceURI_qualifiedName rec_))
    else if queriedSel == sel_parser_didStartMappingPrefix_toURI then pure (maybe 0 (const 1) (_parser_didStartMappingPrefix_toURI rec_))
    else if queriedSel == sel_parser_didEndMappingPrefix then pure (maybe 0 (const 1) (_parser_didEndMappingPrefix rec_))
    else if queriedSel == sel_parser_foundCharacters then pure (maybe 0 (const 1) (_parser_foundCharacters rec_))
    else if queriedSel == sel_parser_foundIgnorableWhitespace then pure (maybe 0 (const 1) (_parser_foundIgnorableWhitespace rec_))
    else if queriedSel == sel_parser_foundProcessingInstructionWithTarget_data then pure (maybe 0 (const 1) (_parser_foundProcessingInstructionWithTarget_data rec_))
    else if queriedSel == sel_parser_foundComment then pure (maybe 0 (const 1) (_parser_foundComment rec_))
    else if queriedSel == sel_parser_foundCDATA then pure (maybe 0 (const 1) (_parser_foundCDATA rec_))
    else if queriedSel == sel_parser_resolveExternalEntityName_systemID then pure (maybe 0 (const 1) (_parser_resolveExternalEntityName_systemID rec_))
    else if queriedSel == sel_parser_parseErrorOccurred then pure (maybe 0 (const 1) (_parser_parseErrorOccurred rec_))
    else if queriedSel == sel_parser_validationErrorOccurred then pure (maybe 0 (const 1) (_parser_validationErrorOccurred rec_))
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
newNSXMLParserDelegate :: NSXMLParserDelegateOverrides -> IO RawId
newNSXMLParserDelegate overrides = do
  inst <- class_createInstance nsxmlParserDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
