{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INInteraction@.
module ObjC.Intents.INInteraction
  ( INInteraction
  , IsINInteraction(..)
  , init_
  , initWithIntent_response
  , donateInteractionWithCompletion
  , deleteAllInteractionsWithCompletion
  , deleteInteractionsWithIdentifiers_completion
  , deleteInteractionsWithGroupIdentifier_completion
  , parameterValueForParameter
  , intent
  , intentResponse
  , intentHandlingStatus
  , direction
  , setDirection
  , dateInterval
  , setDateInterval
  , identifier
  , setIdentifier
  , groupIdentifier
  , setGroupIdentifier
  , initSelector
  , initWithIntent_responseSelector
  , donateInteractionWithCompletionSelector
  , deleteAllInteractionsWithCompletionSelector
  , deleteInteractionsWithIdentifiers_completionSelector
  , deleteInteractionsWithGroupIdentifier_completionSelector
  , parameterValueForParameterSelector
  , intentSelector
  , intentResponseSelector
  , intentHandlingStatusSelector
  , directionSelector
  , setDirectionSelector
  , dateIntervalSelector
  , setDateIntervalSelector
  , identifierSelector
  , setIdentifierSelector
  , groupIdentifierSelector
  , setGroupIdentifierSelector

  -- * Enum types
  , INIntentHandlingStatus(INIntentHandlingStatus)
  , pattern INIntentHandlingStatusUnspecified
  , pattern INIntentHandlingStatusReady
  , pattern INIntentHandlingStatusInProgress
  , pattern INIntentHandlingStatusSuccess
  , pattern INIntentHandlingStatusFailure
  , pattern INIntentHandlingStatusDeferredToApplication
  , pattern INIntentHandlingStatusUserConfirmationRequired
  , INInteractionDirection(INInteractionDirection)
  , pattern INInteractionDirectionUnspecified
  , pattern INInteractionDirectionOutgoing
  , pattern INInteractionDirectionIncoming

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINInteraction inInteraction => inInteraction -> IO (Id INInteraction)
init_ inInteraction  =
  sendMsg inInteraction (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithIntent:response:@
initWithIntent_response :: (IsINInteraction inInteraction, IsINIntent intent, IsINIntentResponse response) => inInteraction -> intent -> response -> IO (Id INInteraction)
initWithIntent_response inInteraction  intent response =
withObjCPtr intent $ \raw_intent ->
  withObjCPtr response $ \raw_response ->
      sendMsg inInteraction (mkSelector "initWithIntent:response:") (retPtr retVoid) [argPtr (castPtr raw_intent :: Ptr ()), argPtr (castPtr raw_response :: Ptr ())] >>= ownedObject . castPtr

-- | @- donateInteractionWithCompletion:@
donateInteractionWithCompletion :: IsINInteraction inInteraction => inInteraction -> Ptr () -> IO ()
donateInteractionWithCompletion inInteraction  completion =
  sendMsg inInteraction (mkSelector "donateInteractionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @+ deleteAllInteractionsWithCompletion:@
deleteAllInteractionsWithCompletion :: Ptr () -> IO ()
deleteAllInteractionsWithCompletion completion =
  do
    cls' <- getRequiredClass "INInteraction"
    sendClassMsg cls' (mkSelector "deleteAllInteractionsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @+ deleteInteractionsWithIdentifiers:completion:@
deleteInteractionsWithIdentifiers_completion :: IsNSArray identifiers => identifiers -> Ptr () -> IO ()
deleteInteractionsWithIdentifiers_completion identifiers completion =
  do
    cls' <- getRequiredClass "INInteraction"
    withObjCPtr identifiers $ \raw_identifiers ->
      sendClassMsg cls' (mkSelector "deleteInteractionsWithIdentifiers:completion:") retVoid [argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @+ deleteInteractionsWithGroupIdentifier:completion:@
deleteInteractionsWithGroupIdentifier_completion :: IsNSString groupIdentifier => groupIdentifier -> Ptr () -> IO ()
deleteInteractionsWithGroupIdentifier_completion groupIdentifier completion =
  do
    cls' <- getRequiredClass "INInteraction"
    withObjCPtr groupIdentifier $ \raw_groupIdentifier ->
      sendClassMsg cls' (mkSelector "deleteInteractionsWithGroupIdentifier:completion:") retVoid [argPtr (castPtr raw_groupIdentifier :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- parameterValueForParameter:@
parameterValueForParameter :: (IsINInteraction inInteraction, IsINParameter parameter) => inInteraction -> parameter -> IO RawId
parameterValueForParameter inInteraction  parameter =
withObjCPtr parameter $ \raw_parameter ->
    fmap (RawId . castPtr) $ sendMsg inInteraction (mkSelector "parameterValueForParameter:") (retPtr retVoid) [argPtr (castPtr raw_parameter :: Ptr ())]

-- | @- intent@
intent :: IsINInteraction inInteraction => inInteraction -> IO (Id INIntent)
intent inInteraction  =
  sendMsg inInteraction (mkSelector "intent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- intentResponse@
intentResponse :: IsINInteraction inInteraction => inInteraction -> IO (Id INIntentResponse)
intentResponse inInteraction  =
  sendMsg inInteraction (mkSelector "intentResponse") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- intentHandlingStatus@
intentHandlingStatus :: IsINInteraction inInteraction => inInteraction -> IO INIntentHandlingStatus
intentHandlingStatus inInteraction  =
  fmap (coerce :: CLong -> INIntentHandlingStatus) $ sendMsg inInteraction (mkSelector "intentHandlingStatus") retCLong []

-- | @- direction@
direction :: IsINInteraction inInteraction => inInteraction -> IO INInteractionDirection
direction inInteraction  =
  fmap (coerce :: CLong -> INInteractionDirection) $ sendMsg inInteraction (mkSelector "direction") retCLong []

-- | @- setDirection:@
setDirection :: IsINInteraction inInteraction => inInteraction -> INInteractionDirection -> IO ()
setDirection inInteraction  value =
  sendMsg inInteraction (mkSelector "setDirection:") retVoid [argCLong (coerce value)]

-- | @- dateInterval@
dateInterval :: IsINInteraction inInteraction => inInteraction -> IO (Id NSDateInterval)
dateInterval inInteraction  =
  sendMsg inInteraction (mkSelector "dateInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDateInterval:@
setDateInterval :: (IsINInteraction inInteraction, IsNSDateInterval value) => inInteraction -> value -> IO ()
setDateInterval inInteraction  value =
withObjCPtr value $ \raw_value ->
    sendMsg inInteraction (mkSelector "setDateInterval:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- identifier@
identifier :: IsINInteraction inInteraction => inInteraction -> IO (Id NSString)
identifier inInteraction  =
  sendMsg inInteraction (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsINInteraction inInteraction, IsNSString value) => inInteraction -> value -> IO ()
setIdentifier inInteraction  value =
withObjCPtr value $ \raw_value ->
    sendMsg inInteraction (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupIdentifier@
groupIdentifier :: IsINInteraction inInteraction => inInteraction -> IO (Id NSString)
groupIdentifier inInteraction  =
  sendMsg inInteraction (mkSelector "groupIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupIdentifier:@
setGroupIdentifier :: (IsINInteraction inInteraction, IsNSString value) => inInteraction -> value -> IO ()
setGroupIdentifier inInteraction  value =
withObjCPtr value $ \raw_value ->
    sendMsg inInteraction (mkSelector "setGroupIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIntent:response:@
initWithIntent_responseSelector :: Selector
initWithIntent_responseSelector = mkSelector "initWithIntent:response:"

-- | @Selector@ for @donateInteractionWithCompletion:@
donateInteractionWithCompletionSelector :: Selector
donateInteractionWithCompletionSelector = mkSelector "donateInteractionWithCompletion:"

-- | @Selector@ for @deleteAllInteractionsWithCompletion:@
deleteAllInteractionsWithCompletionSelector :: Selector
deleteAllInteractionsWithCompletionSelector = mkSelector "deleteAllInteractionsWithCompletion:"

-- | @Selector@ for @deleteInteractionsWithIdentifiers:completion:@
deleteInteractionsWithIdentifiers_completionSelector :: Selector
deleteInteractionsWithIdentifiers_completionSelector = mkSelector "deleteInteractionsWithIdentifiers:completion:"

-- | @Selector@ for @deleteInteractionsWithGroupIdentifier:completion:@
deleteInteractionsWithGroupIdentifier_completionSelector :: Selector
deleteInteractionsWithGroupIdentifier_completionSelector = mkSelector "deleteInteractionsWithGroupIdentifier:completion:"

-- | @Selector@ for @parameterValueForParameter:@
parameterValueForParameterSelector :: Selector
parameterValueForParameterSelector = mkSelector "parameterValueForParameter:"

-- | @Selector@ for @intent@
intentSelector :: Selector
intentSelector = mkSelector "intent"

-- | @Selector@ for @intentResponse@
intentResponseSelector :: Selector
intentResponseSelector = mkSelector "intentResponse"

-- | @Selector@ for @intentHandlingStatus@
intentHandlingStatusSelector :: Selector
intentHandlingStatusSelector = mkSelector "intentHandlingStatus"

-- | @Selector@ for @direction@
directionSelector :: Selector
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @dateInterval@
dateIntervalSelector :: Selector
dateIntervalSelector = mkSelector "dateInterval"

-- | @Selector@ for @setDateInterval:@
setDateIntervalSelector :: Selector
setDateIntervalSelector = mkSelector "setDateInterval:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector
groupIdentifierSelector = mkSelector "groupIdentifier"

-- | @Selector@ for @setGroupIdentifier:@
setGroupIdentifierSelector :: Selector
setGroupIdentifierSelector = mkSelector "setGroupIdentifier:"

