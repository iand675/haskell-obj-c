{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , dateIntervalSelector
  , deleteAllInteractionsWithCompletionSelector
  , deleteInteractionsWithGroupIdentifier_completionSelector
  , deleteInteractionsWithIdentifiers_completionSelector
  , directionSelector
  , donateInteractionWithCompletionSelector
  , groupIdentifierSelector
  , identifierSelector
  , initSelector
  , initWithIntent_responseSelector
  , intentHandlingStatusSelector
  , intentResponseSelector
  , intentSelector
  , parameterValueForParameterSelector
  , setDateIntervalSelector
  , setDirectionSelector
  , setGroupIdentifierSelector
  , setIdentifierSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINInteraction inInteraction => inInteraction -> IO (Id INInteraction)
init_ inInteraction =
  sendOwnedMessage inInteraction initSelector

-- | @- initWithIntent:response:@
initWithIntent_response :: (IsINInteraction inInteraction, IsINIntent intent, IsINIntentResponse response) => inInteraction -> intent -> response -> IO (Id INInteraction)
initWithIntent_response inInteraction intent response =
  sendOwnedMessage inInteraction initWithIntent_responseSelector (toINIntent intent) (toINIntentResponse response)

-- | @- donateInteractionWithCompletion:@
donateInteractionWithCompletion :: IsINInteraction inInteraction => inInteraction -> Ptr () -> IO ()
donateInteractionWithCompletion inInteraction completion =
  sendMessage inInteraction donateInteractionWithCompletionSelector completion

-- | @+ deleteAllInteractionsWithCompletion:@
deleteAllInteractionsWithCompletion :: Ptr () -> IO ()
deleteAllInteractionsWithCompletion completion =
  do
    cls' <- getRequiredClass "INInteraction"
    sendClassMessage cls' deleteAllInteractionsWithCompletionSelector completion

-- | @+ deleteInteractionsWithIdentifiers:completion:@
deleteInteractionsWithIdentifiers_completion :: IsNSArray identifiers => identifiers -> Ptr () -> IO ()
deleteInteractionsWithIdentifiers_completion identifiers completion =
  do
    cls' <- getRequiredClass "INInteraction"
    sendClassMessage cls' deleteInteractionsWithIdentifiers_completionSelector (toNSArray identifiers) completion

-- | @+ deleteInteractionsWithGroupIdentifier:completion:@
deleteInteractionsWithGroupIdentifier_completion :: IsNSString groupIdentifier => groupIdentifier -> Ptr () -> IO ()
deleteInteractionsWithGroupIdentifier_completion groupIdentifier completion =
  do
    cls' <- getRequiredClass "INInteraction"
    sendClassMessage cls' deleteInteractionsWithGroupIdentifier_completionSelector (toNSString groupIdentifier) completion

-- | @- parameterValueForParameter:@
parameterValueForParameter :: (IsINInteraction inInteraction, IsINParameter parameter) => inInteraction -> parameter -> IO RawId
parameterValueForParameter inInteraction parameter =
  sendMessage inInteraction parameterValueForParameterSelector (toINParameter parameter)

-- | @- intent@
intent :: IsINInteraction inInteraction => inInteraction -> IO (Id INIntent)
intent inInteraction =
  sendMessage inInteraction intentSelector

-- | @- intentResponse@
intentResponse :: IsINInteraction inInteraction => inInteraction -> IO (Id INIntentResponse)
intentResponse inInteraction =
  sendMessage inInteraction intentResponseSelector

-- | @- intentHandlingStatus@
intentHandlingStatus :: IsINInteraction inInteraction => inInteraction -> IO INIntentHandlingStatus
intentHandlingStatus inInteraction =
  sendMessage inInteraction intentHandlingStatusSelector

-- | @- direction@
direction :: IsINInteraction inInteraction => inInteraction -> IO INInteractionDirection
direction inInteraction =
  sendMessage inInteraction directionSelector

-- | @- setDirection:@
setDirection :: IsINInteraction inInteraction => inInteraction -> INInteractionDirection -> IO ()
setDirection inInteraction value =
  sendMessage inInteraction setDirectionSelector value

-- | @- dateInterval@
dateInterval :: IsINInteraction inInteraction => inInteraction -> IO (Id NSDateInterval)
dateInterval inInteraction =
  sendMessage inInteraction dateIntervalSelector

-- | @- setDateInterval:@
setDateInterval :: (IsINInteraction inInteraction, IsNSDateInterval value) => inInteraction -> value -> IO ()
setDateInterval inInteraction value =
  sendMessage inInteraction setDateIntervalSelector (toNSDateInterval value)

-- | @- identifier@
identifier :: IsINInteraction inInteraction => inInteraction -> IO (Id NSString)
identifier inInteraction =
  sendMessage inInteraction identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsINInteraction inInteraction, IsNSString value) => inInteraction -> value -> IO ()
setIdentifier inInteraction value =
  sendMessage inInteraction setIdentifierSelector (toNSString value)

-- | @- groupIdentifier@
groupIdentifier :: IsINInteraction inInteraction => inInteraction -> IO (Id NSString)
groupIdentifier inInteraction =
  sendMessage inInteraction groupIdentifierSelector

-- | @- setGroupIdentifier:@
setGroupIdentifier :: (IsINInteraction inInteraction, IsNSString value) => inInteraction -> value -> IO ()
setGroupIdentifier inInteraction value =
  sendMessage inInteraction setGroupIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INInteraction)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIntent:response:@
initWithIntent_responseSelector :: Selector '[Id INIntent, Id INIntentResponse] (Id INInteraction)
initWithIntent_responseSelector = mkSelector "initWithIntent:response:"

-- | @Selector@ for @donateInteractionWithCompletion:@
donateInteractionWithCompletionSelector :: Selector '[Ptr ()] ()
donateInteractionWithCompletionSelector = mkSelector "donateInteractionWithCompletion:"

-- | @Selector@ for @deleteAllInteractionsWithCompletion:@
deleteAllInteractionsWithCompletionSelector :: Selector '[Ptr ()] ()
deleteAllInteractionsWithCompletionSelector = mkSelector "deleteAllInteractionsWithCompletion:"

-- | @Selector@ for @deleteInteractionsWithIdentifiers:completion:@
deleteInteractionsWithIdentifiers_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
deleteInteractionsWithIdentifiers_completionSelector = mkSelector "deleteInteractionsWithIdentifiers:completion:"

-- | @Selector@ for @deleteInteractionsWithGroupIdentifier:completion:@
deleteInteractionsWithGroupIdentifier_completionSelector :: Selector '[Id NSString, Ptr ()] ()
deleteInteractionsWithGroupIdentifier_completionSelector = mkSelector "deleteInteractionsWithGroupIdentifier:completion:"

-- | @Selector@ for @parameterValueForParameter:@
parameterValueForParameterSelector :: Selector '[Id INParameter] RawId
parameterValueForParameterSelector = mkSelector "parameterValueForParameter:"

-- | @Selector@ for @intent@
intentSelector :: Selector '[] (Id INIntent)
intentSelector = mkSelector "intent"

-- | @Selector@ for @intentResponse@
intentResponseSelector :: Selector '[] (Id INIntentResponse)
intentResponseSelector = mkSelector "intentResponse"

-- | @Selector@ for @intentHandlingStatus@
intentHandlingStatusSelector :: Selector '[] INIntentHandlingStatus
intentHandlingStatusSelector = mkSelector "intentHandlingStatus"

-- | @Selector@ for @direction@
directionSelector :: Selector '[] INInteractionDirection
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector '[INInteractionDirection] ()
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @dateInterval@
dateIntervalSelector :: Selector '[] (Id NSDateInterval)
dateIntervalSelector = mkSelector "dateInterval"

-- | @Selector@ for @setDateInterval:@
setDateIntervalSelector :: Selector '[Id NSDateInterval] ()
setDateIntervalSelector = mkSelector "setDateInterval:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector '[] (Id NSString)
groupIdentifierSelector = mkSelector "groupIdentifier"

-- | @Selector@ for @setGroupIdentifier:@
setGroupIdentifierSelector :: Selector '[Id NSString] ()
setGroupIdentifierSelector = mkSelector "setGroupIdentifier:"

