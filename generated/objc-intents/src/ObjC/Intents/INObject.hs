{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INObject@.
module ObjC.Intents.INObject
  ( INObject
  , IsINObject(..)
  , init_
  , initWithIdentifier_displayString_pronunciationHint
  , initWithIdentifier_displayString
  , initWithIdentifier_displayString_subtitleString_displayImage
  , initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImage
  , identifier
  , displayString
  , pronunciationHint
  , subtitleString
  , setSubtitleString
  , displayImage
  , setDisplayImage
  , alternativeSpeakableMatches
  , setAlternativeSpeakableMatches
  , initSelector
  , initWithIdentifier_displayString_pronunciationHintSelector
  , initWithIdentifier_displayStringSelector
  , initWithIdentifier_displayString_subtitleString_displayImageSelector
  , initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImageSelector
  , identifierSelector
  , displayStringSelector
  , pronunciationHintSelector
  , subtitleStringSelector
  , setSubtitleStringSelector
  , displayImageSelector
  , setDisplayImageSelector
  , alternativeSpeakableMatchesSelector
  , setAlternativeSpeakableMatchesSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINObject inObject => inObject -> IO (Id INObject)
init_ inObject  =
  sendMsg inObject (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithIdentifier:displayString:pronunciationHint:@
initWithIdentifier_displayString_pronunciationHint :: (IsINObject inObject, IsNSString identifier, IsNSString displayString, IsNSString pronunciationHint) => inObject -> identifier -> displayString -> pronunciationHint -> IO (Id INObject)
initWithIdentifier_displayString_pronunciationHint inObject  identifier displayString pronunciationHint =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr displayString $ \raw_displayString ->
    withObjCPtr pronunciationHint $ \raw_pronunciationHint ->
        sendMsg inObject (mkSelector "initWithIdentifier:displayString:pronunciationHint:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_displayString :: Ptr ()), argPtr (castPtr raw_pronunciationHint :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:displayString:@
initWithIdentifier_displayString :: (IsINObject inObject, IsNSString identifier, IsNSString displayString) => inObject -> identifier -> displayString -> IO (Id INObject)
initWithIdentifier_displayString inObject  identifier displayString =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr displayString $ \raw_displayString ->
      sendMsg inObject (mkSelector "initWithIdentifier:displayString:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_displayString :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:displayString:subtitleString:displayImage:@
initWithIdentifier_displayString_subtitleString_displayImage :: (IsINObject inObject, IsNSString identifier, IsNSString displayString, IsNSString subtitleString, IsINImage displayImage) => inObject -> identifier -> displayString -> subtitleString -> displayImage -> IO (Id INObject)
initWithIdentifier_displayString_subtitleString_displayImage inObject  identifier displayString subtitleString displayImage =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr displayString $ \raw_displayString ->
    withObjCPtr subtitleString $ \raw_subtitleString ->
      withObjCPtr displayImage $ \raw_displayImage ->
          sendMsg inObject (mkSelector "initWithIdentifier:displayString:subtitleString:displayImage:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_displayString :: Ptr ()), argPtr (castPtr raw_subtitleString :: Ptr ()), argPtr (castPtr raw_displayImage :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:displayString:pronunciationHint:subtitleString:displayImage:@
initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImage :: (IsINObject inObject, IsNSString identifier, IsNSString displayString, IsNSString pronunciationHint, IsNSString subtitleString, IsINImage displayImage) => inObject -> identifier -> displayString -> pronunciationHint -> subtitleString -> displayImage -> IO (Id INObject)
initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImage inObject  identifier displayString pronunciationHint subtitleString displayImage =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr displayString $ \raw_displayString ->
    withObjCPtr pronunciationHint $ \raw_pronunciationHint ->
      withObjCPtr subtitleString $ \raw_subtitleString ->
        withObjCPtr displayImage $ \raw_displayImage ->
            sendMsg inObject (mkSelector "initWithIdentifier:displayString:pronunciationHint:subtitleString:displayImage:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_displayString :: Ptr ()), argPtr (castPtr raw_pronunciationHint :: Ptr ()), argPtr (castPtr raw_subtitleString :: Ptr ()), argPtr (castPtr raw_displayImage :: Ptr ())] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsINObject inObject => inObject -> IO (Id NSString)
identifier inObject  =
  sendMsg inObject (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displayString@
displayString :: IsINObject inObject => inObject -> IO (Id NSString)
displayString inObject  =
  sendMsg inObject (mkSelector "displayString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pronunciationHint@
pronunciationHint :: IsINObject inObject => inObject -> IO (Id NSString)
pronunciationHint inObject  =
  sendMsg inObject (mkSelector "pronunciationHint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subtitleString@
subtitleString :: IsINObject inObject => inObject -> IO (Id NSString)
subtitleString inObject  =
  sendMsg inObject (mkSelector "subtitleString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubtitleString:@
setSubtitleString :: (IsINObject inObject, IsNSString value) => inObject -> value -> IO ()
setSubtitleString inObject  value =
withObjCPtr value $ \raw_value ->
    sendMsg inObject (mkSelector "setSubtitleString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- displayImage@
displayImage :: IsINObject inObject => inObject -> IO (Id INImage)
displayImage inObject  =
  sendMsg inObject (mkSelector "displayImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDisplayImage:@
setDisplayImage :: (IsINObject inObject, IsINImage value) => inObject -> value -> IO ()
setDisplayImage inObject  value =
withObjCPtr value $ \raw_value ->
    sendMsg inObject (mkSelector "setDisplayImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alternativeSpeakableMatches@
alternativeSpeakableMatches :: IsINObject inObject => inObject -> IO (Id NSArray)
alternativeSpeakableMatches inObject  =
  sendMsg inObject (mkSelector "alternativeSpeakableMatches") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlternativeSpeakableMatches:@
setAlternativeSpeakableMatches :: (IsINObject inObject, IsNSArray value) => inObject -> value -> IO ()
setAlternativeSpeakableMatches inObject  value =
withObjCPtr value $ \raw_value ->
    sendMsg inObject (mkSelector "setAlternativeSpeakableMatches:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:displayString:pronunciationHint:@
initWithIdentifier_displayString_pronunciationHintSelector :: Selector
initWithIdentifier_displayString_pronunciationHintSelector = mkSelector "initWithIdentifier:displayString:pronunciationHint:"

-- | @Selector@ for @initWithIdentifier:displayString:@
initWithIdentifier_displayStringSelector :: Selector
initWithIdentifier_displayStringSelector = mkSelector "initWithIdentifier:displayString:"

-- | @Selector@ for @initWithIdentifier:displayString:subtitleString:displayImage:@
initWithIdentifier_displayString_subtitleString_displayImageSelector :: Selector
initWithIdentifier_displayString_subtitleString_displayImageSelector = mkSelector "initWithIdentifier:displayString:subtitleString:displayImage:"

-- | @Selector@ for @initWithIdentifier:displayString:pronunciationHint:subtitleString:displayImage:@
initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImageSelector :: Selector
initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImageSelector = mkSelector "initWithIdentifier:displayString:pronunciationHint:subtitleString:displayImage:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @displayString@
displayStringSelector :: Selector
displayStringSelector = mkSelector "displayString"

-- | @Selector@ for @pronunciationHint@
pronunciationHintSelector :: Selector
pronunciationHintSelector = mkSelector "pronunciationHint"

-- | @Selector@ for @subtitleString@
subtitleStringSelector :: Selector
subtitleStringSelector = mkSelector "subtitleString"

-- | @Selector@ for @setSubtitleString:@
setSubtitleStringSelector :: Selector
setSubtitleStringSelector = mkSelector "setSubtitleString:"

-- | @Selector@ for @displayImage@
displayImageSelector :: Selector
displayImageSelector = mkSelector "displayImage"

-- | @Selector@ for @setDisplayImage:@
setDisplayImageSelector :: Selector
setDisplayImageSelector = mkSelector "setDisplayImage:"

-- | @Selector@ for @alternativeSpeakableMatches@
alternativeSpeakableMatchesSelector :: Selector
alternativeSpeakableMatchesSelector = mkSelector "alternativeSpeakableMatches"

-- | @Selector@ for @setAlternativeSpeakableMatches:@
setAlternativeSpeakableMatchesSelector :: Selector
setAlternativeSpeakableMatchesSelector = mkSelector "setAlternativeSpeakableMatches:"

