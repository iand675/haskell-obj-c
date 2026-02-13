{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWCollaborationMetadata@.
module ObjC.SharedWithYouCore.SWCollaborationMetadata
  ( SWCollaborationMetadata
  , IsSWCollaborationMetadata(..)
  , initWithLocalIdentifier
  , initWithCollaborationIdentifier
  , init_
  , new
  , collaborationIdentifier
  , localIdentifier
  , title
  , setTitle
  , defaultShareOptions
  , setDefaultShareOptions
  , userSelectedShareOptions
  , setUserSelectedShareOptions
  , initiatorHandle
  , setInitiatorHandle
  , initiatorNameComponents
  , setInitiatorNameComponents
  , collaborationIdentifierSelector
  , defaultShareOptionsSelector
  , initSelector
  , initWithCollaborationIdentifierSelector
  , initWithLocalIdentifierSelector
  , initiatorHandleSelector
  , initiatorNameComponentsSelector
  , localIdentifierSelector
  , newSelector
  , setDefaultShareOptionsSelector
  , setInitiatorHandleSelector
  , setInitiatorNameComponentsSelector
  , setTitleSelector
  , setUserSelectedShareOptionsSelector
  , titleSelector
  , userSelectedShareOptionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLocalIdentifier:@
initWithLocalIdentifier :: (IsSWCollaborationMetadata swCollaborationMetadata, IsNSString localIdentifier) => swCollaborationMetadata -> localIdentifier -> IO (Id SWCollaborationMetadata)
initWithLocalIdentifier swCollaborationMetadata localIdentifier =
  sendOwnedMessage swCollaborationMetadata initWithLocalIdentifierSelector (toNSString localIdentifier)

-- | @- initWithCollaborationIdentifier:@
initWithCollaborationIdentifier :: (IsSWCollaborationMetadata swCollaborationMetadata, IsNSString collaborationIdentifier) => swCollaborationMetadata -> collaborationIdentifier -> IO (Id SWCollaborationMetadata)
initWithCollaborationIdentifier swCollaborationMetadata collaborationIdentifier =
  sendOwnedMessage swCollaborationMetadata initWithCollaborationIdentifierSelector (toNSString collaborationIdentifier)

-- | @- init@
init_ :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id SWCollaborationMetadata)
init_ swCollaborationMetadata =
  sendOwnedMessage swCollaborationMetadata initSelector

-- | @+ new@
new :: IO (Id SWCollaborationMetadata)
new  =
  do
    cls' <- getRequiredClass "SWCollaborationMetadata"
    sendOwnedClassMessage cls' newSelector

-- | Globally unique identifier for the item represented by this metadata.
--
-- This identifier is unique across platforms and shares of the same item.
--
-- ObjC selector: @- collaborationIdentifier@
collaborationIdentifier :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id NSString)
collaborationIdentifier swCollaborationMetadata =
  sendMessage swCollaborationMetadata collaborationIdentifierSelector

-- | Locally unique identifier for the item represented by this metadata.
--
-- Use this identifier to uniquely identify this metadata before a collaborationIdentifier can be created
--
-- ObjC selector: @- localIdentifier@
localIdentifier :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id NSString)
localIdentifier swCollaborationMetadata =
  sendMessage swCollaborationMetadata localIdentifierSelector

-- | Title of the content.
--
-- Title of the collaboration if provided by the app which owns the collaboration item.
--
-- ObjC selector: @- title@
title :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id NSString)
title swCollaborationMetadata =
  sendMessage swCollaborationMetadata titleSelector

-- | Title of the content.
--
-- Title of the collaboration if provided by the app which owns the collaboration item.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsSWCollaborationMetadata swCollaborationMetadata, IsNSString value) => swCollaborationMetadata -> value -> IO ()
setTitle swCollaborationMetadata value =
  sendMessage swCollaborationMetadata setTitleSelector (toNSString value)

-- | The collaboration options that this content supports (updated).
--
-- ObjC selector: @- defaultShareOptions@
defaultShareOptions :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id SWCollaborationShareOptions)
defaultShareOptions swCollaborationMetadata =
  sendMessage swCollaborationMetadata defaultShareOptionsSelector

-- | The collaboration options that this content supports (updated).
--
-- ObjC selector: @- setDefaultShareOptions:@
setDefaultShareOptions :: (IsSWCollaborationMetadata swCollaborationMetadata, IsSWCollaborationShareOptions value) => swCollaborationMetadata -> value -> IO ()
setDefaultShareOptions swCollaborationMetadata value =
  sendMessage swCollaborationMetadata setDefaultShareOptionsSelector (toSWCollaborationShareOptions value)

-- | The collaboration options that the user selected when sending the invite (updated).
--
-- ObjC selector: @- userSelectedShareOptions@
userSelectedShareOptions :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id SWCollaborationShareOptions)
userSelectedShareOptions swCollaborationMetadata =
  sendMessage swCollaborationMetadata userSelectedShareOptionsSelector

-- | The collaboration options that the user selected when sending the invite (updated).
--
-- ObjC selector: @- setUserSelectedShareOptions:@
setUserSelectedShareOptions :: (IsSWCollaborationMetadata swCollaborationMetadata, IsSWCollaborationShareOptions value) => swCollaborationMetadata -> value -> IO ()
setUserSelectedShareOptions swCollaborationMetadata value =
  sendMessage swCollaborationMetadata setUserSelectedShareOptionsSelector (toSWCollaborationShareOptions value)

-- | The handle of the person initiating the collaboration, e.g. an email address or phone number.
--
-- Set by the initiating application to allow the user to confirm the handle being used before beginning collaboration. Value will not be transmitted to recipients, and will be nil when not initiating collaboration.
--
-- ObjC selector: @- initiatorHandle@
initiatorHandle :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id NSString)
initiatorHandle swCollaborationMetadata =
  sendOwnedMessage swCollaborationMetadata initiatorHandleSelector

-- | The handle of the person initiating the collaboration, e.g. an email address or phone number.
--
-- Set by the initiating application to allow the user to confirm the handle being used before beginning collaboration. Value will not be transmitted to recipients, and will be nil when not initiating collaboration.
--
-- ObjC selector: @- setInitiatorHandle:@
setInitiatorHandle :: (IsSWCollaborationMetadata swCollaborationMetadata, IsNSString value) => swCollaborationMetadata -> value -> IO ()
setInitiatorHandle swCollaborationMetadata value =
  sendMessage swCollaborationMetadata setInitiatorHandleSelector (toNSString value)

-- | The name of the person initiating the collaboration.
--
-- Set by the initiating application to allow the user to confirm the name being used before beginning collaboration. Value will not be transmitted to recipients, and will be nil when not initiating collaboration.
--
-- ObjC selector: @- initiatorNameComponents@
initiatorNameComponents :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id NSPersonNameComponents)
initiatorNameComponents swCollaborationMetadata =
  sendOwnedMessage swCollaborationMetadata initiatorNameComponentsSelector

-- | The name of the person initiating the collaboration.
--
-- Set by the initiating application to allow the user to confirm the name being used before beginning collaboration. Value will not be transmitted to recipients, and will be nil when not initiating collaboration.
--
-- ObjC selector: @- setInitiatorNameComponents:@
setInitiatorNameComponents :: (IsSWCollaborationMetadata swCollaborationMetadata, IsNSPersonNameComponents value) => swCollaborationMetadata -> value -> IO ()
setInitiatorNameComponents swCollaborationMetadata value =
  sendMessage swCollaborationMetadata setInitiatorNameComponentsSelector (toNSPersonNameComponents value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocalIdentifier:@
initWithLocalIdentifierSelector :: Selector '[Id NSString] (Id SWCollaborationMetadata)
initWithLocalIdentifierSelector = mkSelector "initWithLocalIdentifier:"

-- | @Selector@ for @initWithCollaborationIdentifier:@
initWithCollaborationIdentifierSelector :: Selector '[Id NSString] (Id SWCollaborationMetadata)
initWithCollaborationIdentifierSelector = mkSelector "initWithCollaborationIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWCollaborationMetadata)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWCollaborationMetadata)
newSelector = mkSelector "new"

-- | @Selector@ for @collaborationIdentifier@
collaborationIdentifierSelector :: Selector '[] (Id NSString)
collaborationIdentifierSelector = mkSelector "collaborationIdentifier"

-- | @Selector@ for @localIdentifier@
localIdentifierSelector :: Selector '[] (Id NSString)
localIdentifierSelector = mkSelector "localIdentifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @defaultShareOptions@
defaultShareOptionsSelector :: Selector '[] (Id SWCollaborationShareOptions)
defaultShareOptionsSelector = mkSelector "defaultShareOptions"

-- | @Selector@ for @setDefaultShareOptions:@
setDefaultShareOptionsSelector :: Selector '[Id SWCollaborationShareOptions] ()
setDefaultShareOptionsSelector = mkSelector "setDefaultShareOptions:"

-- | @Selector@ for @userSelectedShareOptions@
userSelectedShareOptionsSelector :: Selector '[] (Id SWCollaborationShareOptions)
userSelectedShareOptionsSelector = mkSelector "userSelectedShareOptions"

-- | @Selector@ for @setUserSelectedShareOptions:@
setUserSelectedShareOptionsSelector :: Selector '[Id SWCollaborationShareOptions] ()
setUserSelectedShareOptionsSelector = mkSelector "setUserSelectedShareOptions:"

-- | @Selector@ for @initiatorHandle@
initiatorHandleSelector :: Selector '[] (Id NSString)
initiatorHandleSelector = mkSelector "initiatorHandle"

-- | @Selector@ for @setInitiatorHandle:@
setInitiatorHandleSelector :: Selector '[Id NSString] ()
setInitiatorHandleSelector = mkSelector "setInitiatorHandle:"

-- | @Selector@ for @initiatorNameComponents@
initiatorNameComponentsSelector :: Selector '[] (Id NSPersonNameComponents)
initiatorNameComponentsSelector = mkSelector "initiatorNameComponents"

-- | @Selector@ for @setInitiatorNameComponents:@
setInitiatorNameComponentsSelector :: Selector '[Id NSPersonNameComponents] ()
setInitiatorNameComponentsSelector = mkSelector "setInitiatorNameComponents:"

