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
  , initWithLocalIdentifierSelector
  , initWithCollaborationIdentifierSelector
  , initSelector
  , newSelector
  , collaborationIdentifierSelector
  , localIdentifierSelector
  , titleSelector
  , setTitleSelector
  , defaultShareOptionsSelector
  , setDefaultShareOptionsSelector
  , userSelectedShareOptionsSelector
  , setUserSelectedShareOptionsSelector
  , initiatorHandleSelector
  , setInitiatorHandleSelector
  , initiatorNameComponentsSelector
  , setInitiatorNameComponentsSelector


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

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLocalIdentifier:@
initWithLocalIdentifier :: (IsSWCollaborationMetadata swCollaborationMetadata, IsNSString localIdentifier) => swCollaborationMetadata -> localIdentifier -> IO (Id SWCollaborationMetadata)
initWithLocalIdentifier swCollaborationMetadata  localIdentifier =
withObjCPtr localIdentifier $ \raw_localIdentifier ->
    sendMsg swCollaborationMetadata (mkSelector "initWithLocalIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_localIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCollaborationIdentifier:@
initWithCollaborationIdentifier :: (IsSWCollaborationMetadata swCollaborationMetadata, IsNSString collaborationIdentifier) => swCollaborationMetadata -> collaborationIdentifier -> IO (Id SWCollaborationMetadata)
initWithCollaborationIdentifier swCollaborationMetadata  collaborationIdentifier =
withObjCPtr collaborationIdentifier $ \raw_collaborationIdentifier ->
    sendMsg swCollaborationMetadata (mkSelector "initWithCollaborationIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_collaborationIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id SWCollaborationMetadata)
init_ swCollaborationMetadata  =
  sendMsg swCollaborationMetadata (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWCollaborationMetadata)
new  =
  do
    cls' <- getRequiredClass "SWCollaborationMetadata"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Globally unique identifier for the item represented by this metadata.
--
-- This identifier is unique across platforms and shares of the same item.
--
-- ObjC selector: @- collaborationIdentifier@
collaborationIdentifier :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id NSString)
collaborationIdentifier swCollaborationMetadata  =
  sendMsg swCollaborationMetadata (mkSelector "collaborationIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Locally unique identifier for the item represented by this metadata.
--
-- Use this identifier to uniquely identify this metadata before a collaborationIdentifier can be created
--
-- ObjC selector: @- localIdentifier@
localIdentifier :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id NSString)
localIdentifier swCollaborationMetadata  =
  sendMsg swCollaborationMetadata (mkSelector "localIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Title of the content.
--
-- Title of the collaboration if provided by the app which owns the collaboration item.
--
-- ObjC selector: @- title@
title :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id NSString)
title swCollaborationMetadata  =
  sendMsg swCollaborationMetadata (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Title of the content.
--
-- Title of the collaboration if provided by the app which owns the collaboration item.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsSWCollaborationMetadata swCollaborationMetadata, IsNSString value) => swCollaborationMetadata -> value -> IO ()
setTitle swCollaborationMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationMetadata (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The collaboration options that this content supports (updated).
--
-- ObjC selector: @- defaultShareOptions@
defaultShareOptions :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id SWCollaborationShareOptions)
defaultShareOptions swCollaborationMetadata  =
  sendMsg swCollaborationMetadata (mkSelector "defaultShareOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The collaboration options that this content supports (updated).
--
-- ObjC selector: @- setDefaultShareOptions:@
setDefaultShareOptions :: (IsSWCollaborationMetadata swCollaborationMetadata, IsSWCollaborationShareOptions value) => swCollaborationMetadata -> value -> IO ()
setDefaultShareOptions swCollaborationMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationMetadata (mkSelector "setDefaultShareOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The collaboration options that the user selected when sending the invite (updated).
--
-- ObjC selector: @- userSelectedShareOptions@
userSelectedShareOptions :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id SWCollaborationShareOptions)
userSelectedShareOptions swCollaborationMetadata  =
  sendMsg swCollaborationMetadata (mkSelector "userSelectedShareOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The collaboration options that the user selected when sending the invite (updated).
--
-- ObjC selector: @- setUserSelectedShareOptions:@
setUserSelectedShareOptions :: (IsSWCollaborationMetadata swCollaborationMetadata, IsSWCollaborationShareOptions value) => swCollaborationMetadata -> value -> IO ()
setUserSelectedShareOptions swCollaborationMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationMetadata (mkSelector "setUserSelectedShareOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The handle of the person initiating the collaboration, e.g. an email address or phone number.
--
-- Set by the initiating application to allow the user to confirm the handle being used before beginning collaboration. Value will not be transmitted to recipients, and will be nil when not initiating collaboration.
--
-- ObjC selector: @- initiatorHandle@
initiatorHandle :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id NSString)
initiatorHandle swCollaborationMetadata  =
  sendMsg swCollaborationMetadata (mkSelector "initiatorHandle") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The handle of the person initiating the collaboration, e.g. an email address or phone number.
--
-- Set by the initiating application to allow the user to confirm the handle being used before beginning collaboration. Value will not be transmitted to recipients, and will be nil when not initiating collaboration.
--
-- ObjC selector: @- setInitiatorHandle:@
setInitiatorHandle :: (IsSWCollaborationMetadata swCollaborationMetadata, IsNSString value) => swCollaborationMetadata -> value -> IO ()
setInitiatorHandle swCollaborationMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationMetadata (mkSelector "setInitiatorHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The name of the person initiating the collaboration.
--
-- Set by the initiating application to allow the user to confirm the name being used before beginning collaboration. Value will not be transmitted to recipients, and will be nil when not initiating collaboration.
--
-- ObjC selector: @- initiatorNameComponents@
initiatorNameComponents :: IsSWCollaborationMetadata swCollaborationMetadata => swCollaborationMetadata -> IO (Id NSPersonNameComponents)
initiatorNameComponents swCollaborationMetadata  =
  sendMsg swCollaborationMetadata (mkSelector "initiatorNameComponents") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The name of the person initiating the collaboration.
--
-- Set by the initiating application to allow the user to confirm the name being used before beginning collaboration. Value will not be transmitted to recipients, and will be nil when not initiating collaboration.
--
-- ObjC selector: @- setInitiatorNameComponents:@
setInitiatorNameComponents :: (IsSWCollaborationMetadata swCollaborationMetadata, IsNSPersonNameComponents value) => swCollaborationMetadata -> value -> IO ()
setInitiatorNameComponents swCollaborationMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationMetadata (mkSelector "setInitiatorNameComponents:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocalIdentifier:@
initWithLocalIdentifierSelector :: Selector
initWithLocalIdentifierSelector = mkSelector "initWithLocalIdentifier:"

-- | @Selector@ for @initWithCollaborationIdentifier:@
initWithCollaborationIdentifierSelector :: Selector
initWithCollaborationIdentifierSelector = mkSelector "initWithCollaborationIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @collaborationIdentifier@
collaborationIdentifierSelector :: Selector
collaborationIdentifierSelector = mkSelector "collaborationIdentifier"

-- | @Selector@ for @localIdentifier@
localIdentifierSelector :: Selector
localIdentifierSelector = mkSelector "localIdentifier"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @defaultShareOptions@
defaultShareOptionsSelector :: Selector
defaultShareOptionsSelector = mkSelector "defaultShareOptions"

-- | @Selector@ for @setDefaultShareOptions:@
setDefaultShareOptionsSelector :: Selector
setDefaultShareOptionsSelector = mkSelector "setDefaultShareOptions:"

-- | @Selector@ for @userSelectedShareOptions@
userSelectedShareOptionsSelector :: Selector
userSelectedShareOptionsSelector = mkSelector "userSelectedShareOptions"

-- | @Selector@ for @setUserSelectedShareOptions:@
setUserSelectedShareOptionsSelector :: Selector
setUserSelectedShareOptionsSelector = mkSelector "setUserSelectedShareOptions:"

-- | @Selector@ for @initiatorHandle@
initiatorHandleSelector :: Selector
initiatorHandleSelector = mkSelector "initiatorHandle"

-- | @Selector@ for @setInitiatorHandle:@
setInitiatorHandleSelector :: Selector
setInitiatorHandleSelector = mkSelector "setInitiatorHandle:"

-- | @Selector@ for @initiatorNameComponents@
initiatorNameComponentsSelector :: Selector
initiatorNameComponentsSelector = mkSelector "initiatorNameComponents"

-- | @Selector@ for @setInitiatorNameComponents:@
setInitiatorNameComponentsSelector :: Selector
setInitiatorNameComponentsSelector = mkSelector "setInitiatorNameComponents:"

