{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMorphologyCustomPronoun@.
module ObjC.Foundation.NSMorphologyCustomPronoun
  ( NSMorphologyCustomPronoun
  , IsNSMorphologyCustomPronoun(..)
  , isSupportedForLanguage
  , requiredKeysForLanguage
  , subjectForm
  , setSubjectForm
  , objectForm
  , setObjectForm
  , possessiveForm
  , setPossessiveForm
  , possessiveAdjectiveForm
  , setPossessiveAdjectiveForm
  , reflexiveForm
  , setReflexiveForm
  , isSupportedForLanguageSelector
  , objectFormSelector
  , possessiveAdjectiveFormSelector
  , possessiveFormSelector
  , reflexiveFormSelector
  , requiredKeysForLanguageSelector
  , setObjectFormSelector
  , setPossessiveAdjectiveFormSelector
  , setPossessiveFormSelector
  , setReflexiveFormSelector
  , setSubjectFormSelector
  , subjectFormSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ isSupportedForLanguage:@
isSupportedForLanguage :: IsNSString language => language -> IO Bool
isSupportedForLanguage language =
  do
    cls' <- getRequiredClass "NSMorphologyCustomPronoun"
    sendClassMessage cls' isSupportedForLanguageSelector (toNSString language)

-- | @+ requiredKeysForLanguage:@
requiredKeysForLanguage :: IsNSString language => language -> IO (Id NSArray)
requiredKeysForLanguage language =
  do
    cls' <- getRequiredClass "NSMorphologyCustomPronoun"
    sendClassMessage cls' requiredKeysForLanguageSelector (toNSString language)

-- | @- subjectForm@
subjectForm :: IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun => nsMorphologyCustomPronoun -> IO (Id NSString)
subjectForm nsMorphologyCustomPronoun =
  sendMessage nsMorphologyCustomPronoun subjectFormSelector

-- | @- setSubjectForm:@
setSubjectForm :: (IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun, IsNSString value) => nsMorphologyCustomPronoun -> value -> IO ()
setSubjectForm nsMorphologyCustomPronoun value =
  sendMessage nsMorphologyCustomPronoun setSubjectFormSelector (toNSString value)

-- | @- objectForm@
objectForm :: IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun => nsMorphologyCustomPronoun -> IO (Id NSString)
objectForm nsMorphologyCustomPronoun =
  sendMessage nsMorphologyCustomPronoun objectFormSelector

-- | @- setObjectForm:@
setObjectForm :: (IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun, IsNSString value) => nsMorphologyCustomPronoun -> value -> IO ()
setObjectForm nsMorphologyCustomPronoun value =
  sendMessage nsMorphologyCustomPronoun setObjectFormSelector (toNSString value)

-- | @- possessiveForm@
possessiveForm :: IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun => nsMorphologyCustomPronoun -> IO (Id NSString)
possessiveForm nsMorphologyCustomPronoun =
  sendMessage nsMorphologyCustomPronoun possessiveFormSelector

-- | @- setPossessiveForm:@
setPossessiveForm :: (IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun, IsNSString value) => nsMorphologyCustomPronoun -> value -> IO ()
setPossessiveForm nsMorphologyCustomPronoun value =
  sendMessage nsMorphologyCustomPronoun setPossessiveFormSelector (toNSString value)

-- | @- possessiveAdjectiveForm@
possessiveAdjectiveForm :: IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun => nsMorphologyCustomPronoun -> IO (Id NSString)
possessiveAdjectiveForm nsMorphologyCustomPronoun =
  sendMessage nsMorphologyCustomPronoun possessiveAdjectiveFormSelector

-- | @- setPossessiveAdjectiveForm:@
setPossessiveAdjectiveForm :: (IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun, IsNSString value) => nsMorphologyCustomPronoun -> value -> IO ()
setPossessiveAdjectiveForm nsMorphologyCustomPronoun value =
  sendMessage nsMorphologyCustomPronoun setPossessiveAdjectiveFormSelector (toNSString value)

-- | @- reflexiveForm@
reflexiveForm :: IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun => nsMorphologyCustomPronoun -> IO (Id NSString)
reflexiveForm nsMorphologyCustomPronoun =
  sendMessage nsMorphologyCustomPronoun reflexiveFormSelector

-- | @- setReflexiveForm:@
setReflexiveForm :: (IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun, IsNSString value) => nsMorphologyCustomPronoun -> value -> IO ()
setReflexiveForm nsMorphologyCustomPronoun value =
  sendMessage nsMorphologyCustomPronoun setReflexiveFormSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isSupportedForLanguage:@
isSupportedForLanguageSelector :: Selector '[Id NSString] Bool
isSupportedForLanguageSelector = mkSelector "isSupportedForLanguage:"

-- | @Selector@ for @requiredKeysForLanguage:@
requiredKeysForLanguageSelector :: Selector '[Id NSString] (Id NSArray)
requiredKeysForLanguageSelector = mkSelector "requiredKeysForLanguage:"

-- | @Selector@ for @subjectForm@
subjectFormSelector :: Selector '[] (Id NSString)
subjectFormSelector = mkSelector "subjectForm"

-- | @Selector@ for @setSubjectForm:@
setSubjectFormSelector :: Selector '[Id NSString] ()
setSubjectFormSelector = mkSelector "setSubjectForm:"

-- | @Selector@ for @objectForm@
objectFormSelector :: Selector '[] (Id NSString)
objectFormSelector = mkSelector "objectForm"

-- | @Selector@ for @setObjectForm:@
setObjectFormSelector :: Selector '[Id NSString] ()
setObjectFormSelector = mkSelector "setObjectForm:"

-- | @Selector@ for @possessiveForm@
possessiveFormSelector :: Selector '[] (Id NSString)
possessiveFormSelector = mkSelector "possessiveForm"

-- | @Selector@ for @setPossessiveForm:@
setPossessiveFormSelector :: Selector '[Id NSString] ()
setPossessiveFormSelector = mkSelector "setPossessiveForm:"

-- | @Selector@ for @possessiveAdjectiveForm@
possessiveAdjectiveFormSelector :: Selector '[] (Id NSString)
possessiveAdjectiveFormSelector = mkSelector "possessiveAdjectiveForm"

-- | @Selector@ for @setPossessiveAdjectiveForm:@
setPossessiveAdjectiveFormSelector :: Selector '[Id NSString] ()
setPossessiveAdjectiveFormSelector = mkSelector "setPossessiveAdjectiveForm:"

-- | @Selector@ for @reflexiveForm@
reflexiveFormSelector :: Selector '[] (Id NSString)
reflexiveFormSelector = mkSelector "reflexiveForm"

-- | @Selector@ for @setReflexiveForm:@
setReflexiveFormSelector :: Selector '[Id NSString] ()
setReflexiveFormSelector = mkSelector "setReflexiveForm:"

