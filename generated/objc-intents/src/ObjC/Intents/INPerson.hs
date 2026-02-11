{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifierSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationshipSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMeSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionTypeSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionTypeSelector
  , initWithHandle_nameComponents_contactIdentifierSelector
  , initWithHandle_displayName_contactIdentifierSelector
  , initWithHandle_nameComponents_displayName_image_contactIdentifierSelector
  , initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionTypeSelector
  , personHandleSelector
  , nameComponentsSelector
  , displayNameSelector
  , imageSelector
  , contactIdentifierSelector
  , customIdentifierSelector
  , relationshipSelector
  , contactSuggestionSelector
  , handleSelector
  , siriMatchesSelector
  , isMeSelector
  , aliasesSelector
  , suggestionTypeSelector

  -- * Enum types
  , INPersonSuggestionType(INPersonSuggestionType)
  , pattern INPersonSuggestionTypeNone
  , pattern INPersonSuggestionTypeSocialProfile
  , pattern INPersonSuggestionTypeInstantMessageAddress

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
init_ :: IsINPerson inPerson => inPerson -> IO (Id INPerson)
init_ inPerson  =
  sendMsg inPerson (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier inPerson  personHandle nameComponents displayName image contactIdentifier customIdentifier =
withObjCPtr personHandle $ \raw_personHandle ->
  withObjCPtr nameComponents $ \raw_nameComponents ->
    withObjCPtr displayName $ \raw_displayName ->
      withObjCPtr image $ \raw_image ->
        withObjCPtr contactIdentifier $ \raw_contactIdentifier ->
          withObjCPtr customIdentifier $ \raw_customIdentifier ->
              sendMsg inPerson (mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_personHandle :: Ptr ()), argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_contactIdentifier :: Ptr ()), argPtr (castPtr raw_customIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:relationship:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationship :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier, IsNSString relationship) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> relationship -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationship inPerson  personHandle nameComponents displayName image contactIdentifier customIdentifier relationship =
withObjCPtr personHandle $ \raw_personHandle ->
  withObjCPtr nameComponents $ \raw_nameComponents ->
    withObjCPtr displayName $ \raw_displayName ->
      withObjCPtr image $ \raw_image ->
        withObjCPtr contactIdentifier $ \raw_contactIdentifier ->
          withObjCPtr customIdentifier $ \raw_customIdentifier ->
            withObjCPtr relationship $ \raw_relationship ->
                sendMsg inPerson (mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:relationship:") (retPtr retVoid) [argPtr (castPtr raw_personHandle :: Ptr ()), argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_contactIdentifier :: Ptr ()), argPtr (castPtr raw_customIdentifier :: Ptr ()), argPtr (castPtr raw_relationship :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> Bool -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe inPerson  personHandle nameComponents displayName image contactIdentifier customIdentifier isMe =
withObjCPtr personHandle $ \raw_personHandle ->
  withObjCPtr nameComponents $ \raw_nameComponents ->
    withObjCPtr displayName $ \raw_displayName ->
      withObjCPtr image $ \raw_image ->
        withObjCPtr contactIdentifier $ \raw_contactIdentifier ->
          withObjCPtr customIdentifier $ \raw_customIdentifier ->
              sendMsg inPerson (mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:") (retPtr retVoid) [argPtr (castPtr raw_personHandle :: Ptr ()), argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_contactIdentifier :: Ptr ()), argPtr (castPtr raw_customIdentifier :: Ptr ()), argCULong (if isMe then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionType :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> Bool -> INPersonSuggestionType -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionType inPerson  personHandle nameComponents displayName image contactIdentifier customIdentifier isMe suggestionType =
withObjCPtr personHandle $ \raw_personHandle ->
  withObjCPtr nameComponents $ \raw_nameComponents ->
    withObjCPtr displayName $ \raw_displayName ->
      withObjCPtr image $ \raw_image ->
        withObjCPtr contactIdentifier $ \raw_contactIdentifier ->
          withObjCPtr customIdentifier $ \raw_customIdentifier ->
              sendMsg inPerson (mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:suggestionType:") (retPtr retVoid) [argPtr (castPtr raw_personHandle :: Ptr ()), argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_contactIdentifier :: Ptr ()), argPtr (castPtr raw_customIdentifier :: Ptr ()), argCULong (if isMe then 1 else 0), argCLong (coerce suggestionType)] >>= ownedObject . castPtr

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isContactSuggestion:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionType :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> Bool -> INPersonSuggestionType -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionType inPerson  personHandle nameComponents displayName image contactIdentifier customIdentifier isContactSuggestion suggestionType =
withObjCPtr personHandle $ \raw_personHandle ->
  withObjCPtr nameComponents $ \raw_nameComponents ->
    withObjCPtr displayName $ \raw_displayName ->
      withObjCPtr image $ \raw_image ->
        withObjCPtr contactIdentifier $ \raw_contactIdentifier ->
          withObjCPtr customIdentifier $ \raw_customIdentifier ->
              sendMsg inPerson (mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isContactSuggestion:suggestionType:") (retPtr retVoid) [argPtr (castPtr raw_personHandle :: Ptr ()), argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_contactIdentifier :: Ptr ()), argPtr (castPtr raw_customIdentifier :: Ptr ()), argCULong (if isContactSuggestion then 1 else 0), argCLong (coerce suggestionType)] >>= ownedObject . castPtr

-- | @- initWithHandle:nameComponents:contactIdentifier:@
initWithHandle_nameComponents_contactIdentifier :: (IsINPerson inPerson, IsNSString handle, IsNSPersonNameComponents nameComponents, IsNSString contactIdentifier) => inPerson -> handle -> nameComponents -> contactIdentifier -> IO (Id INPerson)
initWithHandle_nameComponents_contactIdentifier inPerson  handle nameComponents contactIdentifier =
withObjCPtr handle $ \raw_handle ->
  withObjCPtr nameComponents $ \raw_nameComponents ->
    withObjCPtr contactIdentifier $ \raw_contactIdentifier ->
        sendMsg inPerson (mkSelector "initWithHandle:nameComponents:contactIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_handle :: Ptr ()), argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_contactIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithHandle:displayName:contactIdentifier:@
initWithHandle_displayName_contactIdentifier :: (IsINPerson inPerson, IsNSString handle, IsNSString displayName, IsNSString contactIdentifier) => inPerson -> handle -> displayName -> contactIdentifier -> IO (Id INPerson)
initWithHandle_displayName_contactIdentifier inPerson  handle displayName contactIdentifier =
withObjCPtr handle $ \raw_handle ->
  withObjCPtr displayName $ \raw_displayName ->
    withObjCPtr contactIdentifier $ \raw_contactIdentifier ->
        sendMsg inPerson (mkSelector "initWithHandle:displayName:contactIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_handle :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_contactIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithHandle:nameComponents:displayName:image:contactIdentifier:@
initWithHandle_nameComponents_displayName_image_contactIdentifier :: (IsINPerson inPerson, IsNSString handle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier) => inPerson -> handle -> nameComponents -> displayName -> image -> contactIdentifier -> IO (Id INPerson)
initWithHandle_nameComponents_displayName_image_contactIdentifier inPerson  handle nameComponents displayName image contactIdentifier =
withObjCPtr handle $ \raw_handle ->
  withObjCPtr nameComponents $ \raw_nameComponents ->
    withObjCPtr displayName $ \raw_displayName ->
      withObjCPtr image $ \raw_image ->
        withObjCPtr contactIdentifier $ \raw_contactIdentifier ->
            sendMsg inPerson (mkSelector "initWithHandle:nameComponents:displayName:image:contactIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_handle :: Ptr ()), argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_contactIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:aliases:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionType :: (IsINPerson inPerson, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString contactIdentifier, IsNSString customIdentifier, IsNSArray aliases) => inPerson -> personHandle -> nameComponents -> displayName -> image -> contactIdentifier -> customIdentifier -> aliases -> INPersonSuggestionType -> IO (Id INPerson)
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionType inPerson  personHandle nameComponents displayName image contactIdentifier customIdentifier aliases suggestionType =
withObjCPtr personHandle $ \raw_personHandle ->
  withObjCPtr nameComponents $ \raw_nameComponents ->
    withObjCPtr displayName $ \raw_displayName ->
      withObjCPtr image $ \raw_image ->
        withObjCPtr contactIdentifier $ \raw_contactIdentifier ->
          withObjCPtr customIdentifier $ \raw_customIdentifier ->
            withObjCPtr aliases $ \raw_aliases ->
                sendMsg inPerson (mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:aliases:suggestionType:") (retPtr retVoid) [argPtr (castPtr raw_personHandle :: Ptr ()), argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_contactIdentifier :: Ptr ()), argPtr (castPtr raw_customIdentifier :: Ptr ()), argPtr (castPtr raw_aliases :: Ptr ()), argCLong (coerce suggestionType)] >>= ownedObject . castPtr

-- | @- personHandle@
personHandle :: IsINPerson inPerson => inPerson -> IO (Id INPersonHandle)
personHandle inPerson  =
  sendMsg inPerson (mkSelector "personHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nameComponents@
nameComponents :: IsINPerson inPerson => inPerson -> IO (Id NSPersonNameComponents)
nameComponents inPerson  =
  sendMsg inPerson (mkSelector "nameComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displayName@
displayName :: IsINPerson inPerson => inPerson -> IO (Id NSString)
displayName inPerson  =
  sendMsg inPerson (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- image@
image :: IsINPerson inPerson => inPerson -> IO (Id INImage)
image inPerson  =
  sendMsg inPerson (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contactIdentifier@
contactIdentifier :: IsINPerson inPerson => inPerson -> IO (Id NSString)
contactIdentifier inPerson  =
  sendMsg inPerson (mkSelector "contactIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- customIdentifier@
customIdentifier :: IsINPerson inPerson => inPerson -> IO (Id NSString)
customIdentifier inPerson  =
  sendMsg inPerson (mkSelector "customIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- relationship@
relationship :: IsINPerson inPerson => inPerson -> IO (Id NSString)
relationship inPerson  =
  sendMsg inPerson (mkSelector "relationship") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contactSuggestion@
contactSuggestion :: IsINPerson inPerson => inPerson -> IO Bool
contactSuggestion inPerson  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inPerson (mkSelector "contactSuggestion") retCULong []

-- | @- handle@
handle :: IsINPerson inPerson => inPerson -> IO (Id NSString)
handle inPerson  =
  sendMsg inPerson (mkSelector "handle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- siriMatches@
siriMatches :: IsINPerson inPerson => inPerson -> IO (Id NSArray)
siriMatches inPerson  =
  sendMsg inPerson (mkSelector "siriMatches") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isMe@
isMe :: IsINPerson inPerson => inPerson -> IO Bool
isMe inPerson  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inPerson (mkSelector "isMe") retCULong []

-- | @- aliases@
aliases :: IsINPerson inPerson => inPerson -> IO (Id NSArray)
aliases inPerson  =
  sendMsg inPerson (mkSelector "aliases") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- suggestionType@
suggestionType :: IsINPerson inPerson => inPerson -> IO INPersonSuggestionType
suggestionType inPerson  =
  fmap (coerce :: CLong -> INPersonSuggestionType) $ sendMsg inPerson (mkSelector "suggestionType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifierSelector :: Selector
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifierSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:relationship:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationshipSelector :: Selector
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_relationshipSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:relationship:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMeSelector :: Selector
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMeSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionTypeSelector :: Selector
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isMe_suggestionTypeSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:suggestionType:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isContactSuggestion:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionTypeSelector :: Selector
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_isContactSuggestion_suggestionTypeSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isContactSuggestion:suggestionType:"

-- | @Selector@ for @initWithHandle:nameComponents:contactIdentifier:@
initWithHandle_nameComponents_contactIdentifierSelector :: Selector
initWithHandle_nameComponents_contactIdentifierSelector = mkSelector "initWithHandle:nameComponents:contactIdentifier:"

-- | @Selector@ for @initWithHandle:displayName:contactIdentifier:@
initWithHandle_displayName_contactIdentifierSelector :: Selector
initWithHandle_displayName_contactIdentifierSelector = mkSelector "initWithHandle:displayName:contactIdentifier:"

-- | @Selector@ for @initWithHandle:nameComponents:displayName:image:contactIdentifier:@
initWithHandle_nameComponents_displayName_image_contactIdentifierSelector :: Selector
initWithHandle_nameComponents_displayName_image_contactIdentifierSelector = mkSelector "initWithHandle:nameComponents:displayName:image:contactIdentifier:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:aliases:suggestionType:@
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionTypeSelector :: Selector
initWithPersonHandle_nameComponents_displayName_image_contactIdentifier_customIdentifier_aliases_suggestionTypeSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:aliases:suggestionType:"

-- | @Selector@ for @personHandle@
personHandleSelector :: Selector
personHandleSelector = mkSelector "personHandle"

-- | @Selector@ for @nameComponents@
nameComponentsSelector :: Selector
nameComponentsSelector = mkSelector "nameComponents"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @contactIdentifier@
contactIdentifierSelector :: Selector
contactIdentifierSelector = mkSelector "contactIdentifier"

-- | @Selector@ for @customIdentifier@
customIdentifierSelector :: Selector
customIdentifierSelector = mkSelector "customIdentifier"

-- | @Selector@ for @relationship@
relationshipSelector :: Selector
relationshipSelector = mkSelector "relationship"

-- | @Selector@ for @contactSuggestion@
contactSuggestionSelector :: Selector
contactSuggestionSelector = mkSelector "contactSuggestion"

-- | @Selector@ for @handle@
handleSelector :: Selector
handleSelector = mkSelector "handle"

-- | @Selector@ for @siriMatches@
siriMatchesSelector :: Selector
siriMatchesSelector = mkSelector "siriMatches"

-- | @Selector@ for @isMe@
isMeSelector :: Selector
isMeSelector = mkSelector "isMe"

-- | @Selector@ for @aliases@
aliasesSelector :: Selector
aliasesSelector = mkSelector "aliases"

-- | @Selector@ for @suggestionType@
suggestionTypeSelector :: Selector
suggestionTypeSelector = mkSelector "suggestionType"

