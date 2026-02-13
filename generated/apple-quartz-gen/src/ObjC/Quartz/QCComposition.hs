{-# LANGUAGE DataKinds #-}
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
  , attributesSelector
  , compositionWithDataSelector
  , compositionWithFileSelector
  , identifierSelector
  , inputKeysSelector
  , outputKeysSelector
  , protocolsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ compositionWithFile:@
compositionWithFile :: IsNSString path => path -> IO (Id QCComposition)
compositionWithFile path =
  do
    cls' <- getRequiredClass "QCComposition"
    sendClassMessage cls' compositionWithFileSelector (toNSString path)

-- | @+ compositionWithData:@
compositionWithData :: IsNSData data_ => data_ -> IO (Id QCComposition)
compositionWithData data_ =
  do
    cls' <- getRequiredClass "QCComposition"
    sendClassMessage cls' compositionWithDataSelector (toNSData data_)

-- | @- protocols@
protocols :: IsQCComposition qcComposition => qcComposition -> IO (Id NSArray)
protocols qcComposition =
  sendMessage qcComposition protocolsSelector

-- | @- attributes@
attributes :: IsQCComposition qcComposition => qcComposition -> IO (Id NSDictionary)
attributes qcComposition =
  sendMessage qcComposition attributesSelector

-- | @- inputKeys@
inputKeys :: IsQCComposition qcComposition => qcComposition -> IO (Id NSArray)
inputKeys qcComposition =
  sendMessage qcComposition inputKeysSelector

-- | @- outputKeys@
outputKeys :: IsQCComposition qcComposition => qcComposition -> IO (Id NSArray)
outputKeys qcComposition =
  sendMessage qcComposition outputKeysSelector

-- | @- identifier@
identifier :: IsQCComposition qcComposition => qcComposition -> IO (Id NSString)
identifier qcComposition =
  sendMessage qcComposition identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compositionWithFile:@
compositionWithFileSelector :: Selector '[Id NSString] (Id QCComposition)
compositionWithFileSelector = mkSelector "compositionWithFile:"

-- | @Selector@ for @compositionWithData:@
compositionWithDataSelector :: Selector '[Id NSData] (Id QCComposition)
compositionWithDataSelector = mkSelector "compositionWithData:"

-- | @Selector@ for @protocols@
protocolsSelector :: Selector '[] (Id NSArray)
protocolsSelector = mkSelector "protocols"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSDictionary)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @inputKeys@
inputKeysSelector :: Selector '[] (Id NSArray)
inputKeysSelector = mkSelector "inputKeys"

-- | @Selector@ for @outputKeys@
outputKeysSelector :: Selector '[] (Id NSArray)
outputKeysSelector = mkSelector "outputKeys"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

