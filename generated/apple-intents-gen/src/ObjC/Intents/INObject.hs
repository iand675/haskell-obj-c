{-# LANGUAGE DataKinds #-}
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
  , alternativeSpeakableMatchesSelector
  , displayImageSelector
  , displayStringSelector
  , identifierSelector
  , initSelector
  , initWithIdentifier_displayStringSelector
  , initWithIdentifier_displayString_pronunciationHintSelector
  , initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImageSelector
  , initWithIdentifier_displayString_subtitleString_displayImageSelector
  , pronunciationHintSelector
  , setAlternativeSpeakableMatchesSelector
  , setDisplayImageSelector
  , setSubtitleStringSelector
  , subtitleStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINObject inObject => inObject -> IO (Id INObject)
init_ inObject =
  sendOwnedMessage inObject initSelector

-- | @- initWithIdentifier:displayString:pronunciationHint:@
initWithIdentifier_displayString_pronunciationHint :: (IsINObject inObject, IsNSString identifier, IsNSString displayString, IsNSString pronunciationHint) => inObject -> identifier -> displayString -> pronunciationHint -> IO (Id INObject)
initWithIdentifier_displayString_pronunciationHint inObject identifier displayString pronunciationHint =
  sendOwnedMessage inObject initWithIdentifier_displayString_pronunciationHintSelector (toNSString identifier) (toNSString displayString) (toNSString pronunciationHint)

-- | @- initWithIdentifier:displayString:@
initWithIdentifier_displayString :: (IsINObject inObject, IsNSString identifier, IsNSString displayString) => inObject -> identifier -> displayString -> IO (Id INObject)
initWithIdentifier_displayString inObject identifier displayString =
  sendOwnedMessage inObject initWithIdentifier_displayStringSelector (toNSString identifier) (toNSString displayString)

-- | @- initWithIdentifier:displayString:subtitleString:displayImage:@
initWithIdentifier_displayString_subtitleString_displayImage :: (IsINObject inObject, IsNSString identifier, IsNSString displayString, IsNSString subtitleString, IsINImage displayImage) => inObject -> identifier -> displayString -> subtitleString -> displayImage -> IO (Id INObject)
initWithIdentifier_displayString_subtitleString_displayImage inObject identifier displayString subtitleString displayImage =
  sendOwnedMessage inObject initWithIdentifier_displayString_subtitleString_displayImageSelector (toNSString identifier) (toNSString displayString) (toNSString subtitleString) (toINImage displayImage)

-- | @- initWithIdentifier:displayString:pronunciationHint:subtitleString:displayImage:@
initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImage :: (IsINObject inObject, IsNSString identifier, IsNSString displayString, IsNSString pronunciationHint, IsNSString subtitleString, IsINImage displayImage) => inObject -> identifier -> displayString -> pronunciationHint -> subtitleString -> displayImage -> IO (Id INObject)
initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImage inObject identifier displayString pronunciationHint subtitleString displayImage =
  sendOwnedMessage inObject initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImageSelector (toNSString identifier) (toNSString displayString) (toNSString pronunciationHint) (toNSString subtitleString) (toINImage displayImage)

-- | @- identifier@
identifier :: IsINObject inObject => inObject -> IO (Id NSString)
identifier inObject =
  sendMessage inObject identifierSelector

-- | @- displayString@
displayString :: IsINObject inObject => inObject -> IO (Id NSString)
displayString inObject =
  sendMessage inObject displayStringSelector

-- | @- pronunciationHint@
pronunciationHint :: IsINObject inObject => inObject -> IO (Id NSString)
pronunciationHint inObject =
  sendMessage inObject pronunciationHintSelector

-- | @- subtitleString@
subtitleString :: IsINObject inObject => inObject -> IO (Id NSString)
subtitleString inObject =
  sendMessage inObject subtitleStringSelector

-- | @- setSubtitleString:@
setSubtitleString :: (IsINObject inObject, IsNSString value) => inObject -> value -> IO ()
setSubtitleString inObject value =
  sendMessage inObject setSubtitleStringSelector (toNSString value)

-- | @- displayImage@
displayImage :: IsINObject inObject => inObject -> IO (Id INImage)
displayImage inObject =
  sendMessage inObject displayImageSelector

-- | @- setDisplayImage:@
setDisplayImage :: (IsINObject inObject, IsINImage value) => inObject -> value -> IO ()
setDisplayImage inObject value =
  sendMessage inObject setDisplayImageSelector (toINImage value)

-- | @- alternativeSpeakableMatches@
alternativeSpeakableMatches :: IsINObject inObject => inObject -> IO (Id NSArray)
alternativeSpeakableMatches inObject =
  sendMessage inObject alternativeSpeakableMatchesSelector

-- | @- setAlternativeSpeakableMatches:@
setAlternativeSpeakableMatches :: (IsINObject inObject, IsNSArray value) => inObject -> value -> IO ()
setAlternativeSpeakableMatches inObject value =
  sendMessage inObject setAlternativeSpeakableMatchesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INObject)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:displayString:pronunciationHint:@
initWithIdentifier_displayString_pronunciationHintSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id INObject)
initWithIdentifier_displayString_pronunciationHintSelector = mkSelector "initWithIdentifier:displayString:pronunciationHint:"

-- | @Selector@ for @initWithIdentifier:displayString:@
initWithIdentifier_displayStringSelector :: Selector '[Id NSString, Id NSString] (Id INObject)
initWithIdentifier_displayStringSelector = mkSelector "initWithIdentifier:displayString:"

-- | @Selector@ for @initWithIdentifier:displayString:subtitleString:displayImage:@
initWithIdentifier_displayString_subtitleString_displayImageSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id INImage] (Id INObject)
initWithIdentifier_displayString_subtitleString_displayImageSelector = mkSelector "initWithIdentifier:displayString:subtitleString:displayImage:"

-- | @Selector@ for @initWithIdentifier:displayString:pronunciationHint:subtitleString:displayImage:@
initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImageSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString, Id INImage] (Id INObject)
initWithIdentifier_displayString_pronunciationHint_subtitleString_displayImageSelector = mkSelector "initWithIdentifier:displayString:pronunciationHint:subtitleString:displayImage:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @displayString@
displayStringSelector :: Selector '[] (Id NSString)
displayStringSelector = mkSelector "displayString"

-- | @Selector@ for @pronunciationHint@
pronunciationHintSelector :: Selector '[] (Id NSString)
pronunciationHintSelector = mkSelector "pronunciationHint"

-- | @Selector@ for @subtitleString@
subtitleStringSelector :: Selector '[] (Id NSString)
subtitleStringSelector = mkSelector "subtitleString"

-- | @Selector@ for @setSubtitleString:@
setSubtitleStringSelector :: Selector '[Id NSString] ()
setSubtitleStringSelector = mkSelector "setSubtitleString:"

-- | @Selector@ for @displayImage@
displayImageSelector :: Selector '[] (Id INImage)
displayImageSelector = mkSelector "displayImage"

-- | @Selector@ for @setDisplayImage:@
setDisplayImageSelector :: Selector '[Id INImage] ()
setDisplayImageSelector = mkSelector "setDisplayImage:"

-- | @Selector@ for @alternativeSpeakableMatches@
alternativeSpeakableMatchesSelector :: Selector '[] (Id NSArray)
alternativeSpeakableMatchesSelector = mkSelector "alternativeSpeakableMatches"

-- | @Selector@ for @setAlternativeSpeakableMatches:@
setAlternativeSpeakableMatchesSelector :: Selector '[Id NSArray] ()
setAlternativeSpeakableMatchesSelector = mkSelector "setAlternativeSpeakableMatches:"

