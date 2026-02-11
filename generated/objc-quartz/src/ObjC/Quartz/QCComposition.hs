{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCComposition@.
module ObjC.Quartz.QCComposition
  ( QCComposition
  , IsQCComposition(..)
  , compositionWithFile
  , compositionWithData
  , protocols
  , attributes
  , inputKeys
  , outputKeys
  , identifier
  , compositionWithFileSelector
  , compositionWithDataSelector
  , protocolsSelector
  , attributesSelector
  , inputKeysSelector
  , outputKeysSelector
  , identifierSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ compositionWithFile:@
compositionWithFile :: IsNSString path => path -> IO (Id QCComposition)
compositionWithFile path =
  do
    cls' <- getRequiredClass "QCComposition"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "compositionWithFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @+ compositionWithData:@
compositionWithData :: IsNSData data_ => data_ -> IO (Id QCComposition)
compositionWithData data_ =
  do
    cls' <- getRequiredClass "QCComposition"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "compositionWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- protocols@
protocols :: IsQCComposition qcComposition => qcComposition -> IO (Id NSArray)
protocols qcComposition  =
  sendMsg qcComposition (mkSelector "protocols") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributes@
attributes :: IsQCComposition qcComposition => qcComposition -> IO (Id NSDictionary)
attributes qcComposition  =
  sendMsg qcComposition (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- inputKeys@
inputKeys :: IsQCComposition qcComposition => qcComposition -> IO (Id NSArray)
inputKeys qcComposition  =
  sendMsg qcComposition (mkSelector "inputKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- outputKeys@
outputKeys :: IsQCComposition qcComposition => qcComposition -> IO (Id NSArray)
outputKeys qcComposition  =
  sendMsg qcComposition (mkSelector "outputKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsQCComposition qcComposition => qcComposition -> IO (Id NSString)
identifier qcComposition  =
  sendMsg qcComposition (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compositionWithFile:@
compositionWithFileSelector :: Selector
compositionWithFileSelector = mkSelector "compositionWithFile:"

-- | @Selector@ for @compositionWithData:@
compositionWithDataSelector :: Selector
compositionWithDataSelector = mkSelector "compositionWithData:"

-- | @Selector@ for @protocols@
protocolsSelector :: Selector
protocolsSelector = mkSelector "protocols"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @inputKeys@
inputKeysSelector :: Selector
inputKeysSelector = mkSelector "inputKeys"

-- | @Selector@ for @outputKeys@
outputKeysSelector :: Selector
outputKeysSelector = mkSelector "outputKeys"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

