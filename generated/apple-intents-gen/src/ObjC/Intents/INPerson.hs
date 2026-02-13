{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPerson@.
module ObjC.Intents.INPerson
  ( INPerson
  , IsINPerson(..)
  , init_
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationship
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionType
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionType
  , initWithHandle_nameComponents_contactIdentifier
  , initWithHandle_displayName_contactIdentifier
  , initWithHandle_nameComponents_displayName_image_contactIdentifier
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionType
  , personHandle
  , nameComponents
  , displayName
  , image
  , contactIdentifier
  , customIdentifier
  , relationship
  , contactSuggestion
  , handle
  , siriMatches
  , isMe
  , aliases
  , suggestionType
  , aliasesSelector
  , contactIdentifierSelector
  , contactSuggestionSelector
  , customIdentifierSelector
  , displayNameSelector
  , handleSelector
  , imageSelector
  , initSelector
  , initWithHandle_displayName_contactIdentifierSelector
  , initWithHandle_nameComponents_contactIdentifierSelector
  , initWithHandle_nameComponents_displayName_image_contactIdentifierSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifierSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionTypeSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionTypeSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMeSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionTypeSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationshipSelector
  , isMeSelector
  , nameComponentsSelector
  , personHandleSelector
  , relationshipSelector
  , siriMatchesSelector
  , suggestionTypeSelector

  -- * Enum types
  , INPersonSuggestionType(INPersonSuggestionType)
  , pattern INPersonSuggestionTypeNone
  , pattern INPersonSuggestionTypeSocialProfile
  , pattern INPersonSuggestionTypeInstantMessageAddress

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
init_ :: IsINPerson inPerson => inPerson -> IO (Id INPerson)
init_ inPerson =
  sendOwnedMessage inPerson initSelector

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier inPerson personHandle nameComponents displayName image contactIdentifier customIdentifier =
  sendOwnedMessage inPerson initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifierSelector (toINPersonHandle personHandle) (toNSPersonNameComponents nameComponents) (toNSString displayName) (toINImage image) (toNSString contactIdentifier) (toNSString customIdentifier)

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:relationship:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationship :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier, IsNSString relationship) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> relationship -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationship inPerson personHandle nameComponents displayName image contactIdentifier customIdentifier relationship =
  sendOwnedMessage inPerson initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationshipSelector (toINPersonHandle personHandle) (toNSPersonNameComponents nameComponents) (toNSString displayName) (toINImage image) (toNSString contactIdentifier) (toNSString customIdentifier) (toNSString relationship)

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> Bool -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe inPerson personHandle nameComponents displayName image contactIdentifier customIdentifier isMe =
  sendOwnedMessage inPerson initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMeSelector (toINPersonHandle personHandle) (toNSPersonNameComponents nameComponents) (toNSString displayName) (toINImage image) (toNSString contactIdentifier) (toNSString customIdentifier) isMe

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionType :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> Bool -> INPersonSuggestionType -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionType inPerson personHandle nameComponents displayName image contactIdentifier customIdentifier isMe suggestionType =
  sendOwnedMessage inPerson initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionTypeSelector (toINPersonHandle personHandle) (toNSPersonNameComponents nameComponents) (toNSString displayName) (toINImage image) (toNSString contactIdentifier) (toNSString customIdentifier) isMe suggestionType

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isContactSuggestion:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionType :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> Bool -> INPersonSuggestionType -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionType inPerson personHandle nameComponents displayName image contactIdentifier customIdentifier isContactSuggestion suggestionType =
  sendOwnedMessage inPerson initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionTypeSelector (toINPersonHandle personHandle) (toNSPersonNameComponents nameComponents) (toNSString displayName) (toINImage image) (toNSString contactIdentifier) (toNSString customIdentifier) isContactSuggestion suggestionType

-- | @- initWithHandle:nameComponents:contactIdentifier:@
initWithHandle_nameComponents_contactIdentifier :: (IsINPerson inPerson, IsNSString handle, IsNSPersonNameComponents nameComponents, IsNSString contactIdentifier) => inPerson -> handle -> nameComponents -> contactIdentifier -> IO (Id INPerson)
initWithHandle_nameComponents_contactIdentifier inPerson handle nameComponents contactIdentifier =
  sendOwnedMessage inPerson initWithHandle_nameComponents_contactIdentifierSelector (toNSString handle) (toNSPersonNameComponents nameComponents) (toNSString contactIdentifier)

-- | @- initWithHandle:displayName:contactIdentifier:@
initWithHandle_displayName_contactIdentifier :: (IsINPerson inPerson, IsNSString handle, IsNSString displayName, IsNSString contactIdentifier) => inPerson -> handle -> displayName -> contactIdentifier -> IO (Id INPerson)
initWithHandle_displayName_contactIdentifier inPerson handle displayName contactIdentifier =
  sendOwnedMessage inPerson initWithHandle_displayName_contactIdentifierSelector (toNSString handle) (toNSString displayName) (toNSString contactIdentifier)

-- | @- initWithHandle:nameComponents:displayName:image:contactIdentifier:@
initWithHandle_nameComponents_displayName_image_contactIdentifier :: (IsINPerson inPerson, IsNSString handle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier) => inPerson -> handle -> nameComponents -> displayName -> image -> contactIdentifier -> IO (Id INPerson)
initWithHandle_nameComponents_displayName_image_contactIdentifier inPerson handle nameComponents displayName image contactIdentifier =
  sendOwnedMessage inPerson initWithHandle_nameComponents_displayName_image_contactIdentifierSelector (toNSString handle) (toNSPersonNameComponents nameComponents) (toNSString displayName) (toINImage image) (toNSString contactIdentifier)

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:aliases:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionType :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier, IsNSArray aliases) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> aliases -> INPersonSuggestionType -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionType inPerson personHandle nameComponents displayName image contactIdentifier customIdentifier aliases suggestionType =
  sendOwnedMessage inPerson initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionTypeSelector (toINPersonHandle personHandle) (toNSPersonNameComponents nameComponents) (toNSString displayName) (toINImage image) (toNSString contactIdentifier) (toNSString customIdentifier) (toNSArray aliases) suggestionType

-- | @- personHandle@
personHandle :: IsINPerson inPerson => inPerson -> IO (Id INPersonHandle)
personHandle inPerson =
  sendMessage inPerson personHandleSelector

-- | @- nameComponents@
nameComponents :: IsINPerson inPerson => inPerson -> IO (Id NSPersonNameComponents)
nameComponents inPerson =
  sendMessage inPerson nameComponentsSelector

-- | @- displayName@
displayName :: IsINPerson inPerson => inPerson -> IO (Id NSString)
displayName inPerson =
  sendMessage inPerson displayNameSelector

-- | @- image@
image :: IsINPerson inPerson => inPerson -> IO (Id INImage)
image inPerson =
  sendMessage inPerson imageSelector

-- | @- contactIdentifier@
contactIdentifier :: IsINPerson inPerson => inPerson -> IO (Id NSString)
contactIdentifier inPerson =
  sendMessage inPerson contactIdentifierSelector

-- | @- customIdentifier@
customIdentifier :: IsINPerson inPerson => inPerson -> IO (Id NSString)
customIdentifier inPerson =
  sendMessage inPerson customIdentifierSelector

-- | @- relationship@
relationship :: IsINPerson inPerson => inPerson -> IO (Id NSString)
relationship inPerson =
  sendMessage inPerson relationshipSelector

-- | @- contactSuggestion@
contactSuggestion :: IsINPerson inPerson => inPerson -> IO Bool
contactSuggestion inPerson =
  sendMessage inPerson contactSuggestionSelector

-- | @- handle@
handle :: IsINPerson inPerson => inPerson -> IO (Id NSString)
handle inPerson =
  sendMessage inPerson handleSelector

-- | @- siriMatches@
siriMatches :: IsINPerson inPerson => inPerson -> IO (Id NSArray)
siriMatches inPerson =
  sendMessage inPerson siriMatchesSelector

-- | @- isMe@
isMe :: IsINPerson inPerson => inPerson -> IO Bool
isMe inPerson =
  sendMessage inPerson isMeSelector

-- | @- aliases@
aliases :: IsINPerson inPerson => inPerson -> IO (Id NSArray)
aliases inPerson =
  sendMessage inPerson aliasesSelector

-- | @- suggestionType@
suggestionType :: IsINPerson inPerson => inPerson -> IO INPersonSuggestionType
suggestionType inPerson =
  sendMessage inPerson suggestionTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INPerson)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifierSelector :: Selector '[Id INPersonHandle, Id NSPersonNameComponents, Id NSString, Id INImage, Id NSString, Id NSString] (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifierSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:relationship:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationshipSelector :: Selector '[Id INPersonHandle, Id NSPersonNameComponents, Id NSString, Id INImage, Id NSString, Id NSString, Id NSString] (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationshipSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:relationship:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMeSelector :: Selector '[Id INPersonHandle, Id NSPersonNameComponents, Id NSString, Id INImage, Id NSString, Id NSString, Bool] (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMeSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionTypeSelector :: Selector '[Id INPersonHandle, Id NSPersonNameComponents, Id NSString, Id INImage, Id NSString, Id NSString, Bool, INPersonSuggestionType] (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionTypeSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:suggestionType:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isContactSuggestion:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionTypeSelector :: Selector '[Id INPersonHandle, Id NSPersonNameComponents, Id NSString, Id INImage, Id NSString, Id NSString, Bool, INPersonSuggestionType] (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionTypeSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isContactSuggestion:suggestionType:"

-- | @Selector@ for @initWithHandle:nameComponents:contactIdentifier:@
initWithHandle_nameComponents_contactIdentifierSelector :: Selector '[Id NSString, Id NSPersonNameComponents, Id NSString] (Id INPerson)
initWithHandle_nameComponents_contactIdentifierSelector = mkSelector "initWithHandle:nameComponents:contactIdentifier:"

-- | @Selector@ for @initWithHandle:displayName:contactIdentifier:@
initWithHandle_displayName_contactIdentifierSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id INPerson)
initWithHandle_displayName_contactIdentifierSelector = mkSelector "initWithHandle:displayName:contactIdentifier:"

-- | @Selector@ for @initWithHandle:nameComponents:displayName:image:contactIdentifier:@
initWithHandle_nameComponents_displayName_image_contactIdentifierSelector :: Selector '[Id NSString, Id NSPersonNameComponents, Id NSString, Id INImage, Id NSString] (Id INPerson)
initWithHandle_nameComponents_displayName_image_contactIdentifierSelector = mkSelector "initWithHandle:nameComponents:displayName:image:contactIdentifier:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:aliases:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionTypeSelector :: Selector '[Id INPersonHandle, Id NSPersonNameComponents, Id NSString, Id INImage, Id NSString, Id NSString, Id NSArray, INPersonSuggestionType] (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionTypeSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:aliases:suggestionType:"

-- | @Selector@ for @personHandle@
personHandleSelector :: Selector '[] (Id INPersonHandle)
personHandleSelector = mkSelector "personHandle"

-- | @Selector@ for @nameComponents@
nameComponentsSelector :: Selector '[] (Id NSPersonNameComponents)
nameComponentsSelector = mkSelector "nameComponents"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id INImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @contactIdentifier@
contactIdentifierSelector :: Selector '[] (Id NSString)
contactIdentifierSelector = mkSelector "contactIdentifier"

-- | @Selector@ for @customIdentifier@
customIdentifierSelector :: Selector '[] (Id NSString)
customIdentifierSelector = mkSelector "customIdentifier"

-- | @Selector@ for @relationship@
relationshipSelector :: Selector '[] (Id NSString)
relationshipSelector = mkSelector "relationship"

-- | @Selector@ for @contactSuggestion@
contactSuggestionSelector :: Selector '[] Bool
contactSuggestionSelector = mkSelector "contactSuggestion"

-- | @Selector@ for @handle@
handleSelector :: Selector '[] (Id NSString)
handleSelector = mkSelector "handle"

-- | @Selector@ for @siriMatches@
siriMatchesSelector :: Selector '[] (Id NSArray)
siriMatchesSelector = mkSelector "siriMatches"

-- | @Selector@ for @isMe@
isMeSelector :: Selector '[] Bool
isMeSelector = mkSelector "isMe"

-- | @Selector@ for @aliases@
aliasesSelector :: Selector '[] (Id NSArray)
aliasesSelector = mkSelector "aliases"

-- | @Selector@ for @suggestionType@
suggestionTypeSelector :: Selector '[] INPersonSuggestionType
suggestionTypeSelector = mkSelector "suggestionType"

