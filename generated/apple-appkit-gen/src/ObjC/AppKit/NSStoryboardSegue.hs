{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStoryboardSegue@.
module ObjC.AppKit.NSStoryboardSegue
  ( NSStoryboardSegue
  , IsNSStoryboardSegue(..)
  , segueWithIdentifier_source_destination_performHandler
  , initWithIdentifier_source_destination
  , perform
  , identifier
  , sourceController
  , destinationController
  , destinationControllerSelector
  , identifierSelector
  , initWithIdentifier_source_destinationSelector
  , performSelector
  , segueWithIdentifier_source_destination_performHandlerSelector
  , sourceControllerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ segueWithIdentifier:source:destination:performHandler:@
segueWithIdentifier_source_destination_performHandler :: IsNSString identifier => identifier -> RawId -> RawId -> Ptr () -> IO (Id NSStoryboardSegue)
segueWithIdentifier_source_destination_performHandler identifier sourceController destinationController performHandler =
  do
    cls' <- getRequiredClass "NSStoryboardSegue"
    sendClassMessage cls' segueWithIdentifier_source_destination_performHandlerSelector (toNSString identifier) sourceController destinationController performHandler

-- | @- initWithIdentifier:source:destination:@
initWithIdentifier_source_destination :: (IsNSStoryboardSegue nsStoryboardSegue, IsNSString identifier) => nsStoryboardSegue -> identifier -> RawId -> RawId -> IO (Id NSStoryboardSegue)
initWithIdentifier_source_destination nsStoryboardSegue identifier sourceController destinationController =
  sendOwnedMessage nsStoryboardSegue initWithIdentifier_source_destinationSelector (toNSString identifier) sourceController destinationController

-- | @- perform@
perform :: IsNSStoryboardSegue nsStoryboardSegue => nsStoryboardSegue -> IO ()
perform nsStoryboardSegue =
  sendMessage nsStoryboardSegue performSelector

-- | @- identifier@
identifier :: IsNSStoryboardSegue nsStoryboardSegue => nsStoryboardSegue -> IO (Id NSString)
identifier nsStoryboardSegue =
  sendMessage nsStoryboardSegue identifierSelector

-- | @- sourceController@
sourceController :: IsNSStoryboardSegue nsStoryboardSegue => nsStoryboardSegue -> IO RawId
sourceController nsStoryboardSegue =
  sendMessage nsStoryboardSegue sourceControllerSelector

-- | @- destinationController@
destinationController :: IsNSStoryboardSegue nsStoryboardSegue => nsStoryboardSegue -> IO RawId
destinationController nsStoryboardSegue =
  sendMessage nsStoryboardSegue destinationControllerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @segueWithIdentifier:source:destination:performHandler:@
segueWithIdentifier_source_destination_performHandlerSelector :: Selector '[Id NSString, RawId, RawId, Ptr ()] (Id NSStoryboardSegue)
segueWithIdentifier_source_destination_performHandlerSelector = mkSelector "segueWithIdentifier:source:destination:performHandler:"

-- | @Selector@ for @initWithIdentifier:source:destination:@
initWithIdentifier_source_destinationSelector :: Selector '[Id NSString, RawId, RawId] (Id NSStoryboardSegue)
initWithIdentifier_source_destinationSelector = mkSelector "initWithIdentifier:source:destination:"

-- | @Selector@ for @perform@
performSelector :: Selector '[] ()
performSelector = mkSelector "perform"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @sourceController@
sourceControllerSelector :: Selector '[] RawId
sourceControllerSelector = mkSelector "sourceController"

-- | @Selector@ for @destinationController@
destinationControllerSelector :: Selector '[] RawId
destinationControllerSelector = mkSelector "destinationController"

