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
  , requiredKeysForLanguageSelector
  , subjectFormSelector
  , setSubjectFormSelector
  , objectFormSelector
  , setObjectFormSelector
  , possessiveFormSelector
  , setPossessiveFormSelector
  , possessiveAdjectiveFormSelector
  , setPossessiveAdjectiveFormSelector
  , reflexiveFormSelector
  , setReflexiveFormSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ isSupportedForLanguage:@
isSupportedForLanguage :: IsNSString language => language -> IO Bool
isSupportedForLanguage language =
  do
    cls' <- getRequiredClass "NSMorphologyCustomPronoun"
    withObjCPtr language $ \raw_language ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isSupportedForLanguage:") retCULong [argPtr (castPtr raw_language :: Ptr ())]

-- | @+ requiredKeysForLanguage:@
requiredKeysForLanguage :: IsNSString language => language -> IO (Id NSArray)
requiredKeysForLanguage language =
  do
    cls' <- getRequiredClass "NSMorphologyCustomPronoun"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "requiredKeysForLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @- subjectForm@
subjectForm :: IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun => nsMorphologyCustomPronoun -> IO (Id NSString)
subjectForm nsMorphologyCustomPronoun  =
  sendMsg nsMorphologyCustomPronoun (mkSelector "subjectForm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubjectForm:@
setSubjectForm :: (IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun, IsNSString value) => nsMorphologyCustomPronoun -> value -> IO ()
setSubjectForm nsMorphologyCustomPronoun  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMorphologyCustomPronoun (mkSelector "setSubjectForm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- objectForm@
objectForm :: IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun => nsMorphologyCustomPronoun -> IO (Id NSString)
objectForm nsMorphologyCustomPronoun  =
  sendMsg nsMorphologyCustomPronoun (mkSelector "objectForm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setObjectForm:@
setObjectForm :: (IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun, IsNSString value) => nsMorphologyCustomPronoun -> value -> IO ()
setObjectForm nsMorphologyCustomPronoun  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMorphologyCustomPronoun (mkSelector "setObjectForm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- possessiveForm@
possessiveForm :: IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun => nsMorphologyCustomPronoun -> IO (Id NSString)
possessiveForm nsMorphologyCustomPronoun  =
  sendMsg nsMorphologyCustomPronoun (mkSelector "possessiveForm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPossessiveForm:@
setPossessiveForm :: (IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun, IsNSString value) => nsMorphologyCustomPronoun -> value -> IO ()
setPossessiveForm nsMorphologyCustomPronoun  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMorphologyCustomPronoun (mkSelector "setPossessiveForm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- possessiveAdjectiveForm@
possessiveAdjectiveForm :: IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun => nsMorphologyCustomPronoun -> IO (Id NSString)
possessiveAdjectiveForm nsMorphologyCustomPronoun  =
  sendMsg nsMorphologyCustomPronoun (mkSelector "possessiveAdjectiveForm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPossessiveAdjectiveForm:@
setPossessiveAdjectiveForm :: (IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun, IsNSString value) => nsMorphologyCustomPronoun -> value -> IO ()
setPossessiveAdjectiveForm nsMorphologyCustomPronoun  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMorphologyCustomPronoun (mkSelector "setPossessiveAdjectiveForm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- reflexiveForm@
reflexiveForm :: IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun => nsMorphologyCustomPronoun -> IO (Id NSString)
reflexiveForm nsMorphologyCustomPronoun  =
  sendMsg nsMorphologyCustomPronoun (mkSelector "reflexiveForm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReflexiveForm:@
setReflexiveForm :: (IsNSMorphologyCustomPronoun nsMorphologyCustomPronoun, IsNSString value) => nsMorphologyCustomPronoun -> value -> IO ()
setReflexiveForm nsMorphologyCustomPronoun  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMorphologyCustomPronoun (mkSelector "setReflexiveForm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isSupportedForLanguage:@
isSupportedForLanguageSelector :: Selector
isSupportedForLanguageSelector = mkSelector "isSupportedForLanguage:"

-- | @Selector@ for @requiredKeysForLanguage:@
requiredKeysForLanguageSelector :: Selector
requiredKeysForLanguageSelector = mkSelector "requiredKeysForLanguage:"

-- | @Selector@ for @subjectForm@
subjectFormSelector :: Selector
subjectFormSelector = mkSelector "subjectForm"

-- | @Selector@ for @setSubjectForm:@
setSubjectFormSelector :: Selector
setSubjectFormSelector = mkSelector "setSubjectForm:"

-- | @Selector@ for @objectForm@
objectFormSelector :: Selector
objectFormSelector = mkSelector "objectForm"

-- | @Selector@ for @setObjectForm:@
setObjectFormSelector :: Selector
setObjectFormSelector = mkSelector "setObjectForm:"

-- | @Selector@ for @possessiveForm@
possessiveFormSelector :: Selector
possessiveFormSelector = mkSelector "possessiveForm"

-- | @Selector@ for @setPossessiveForm:@
setPossessiveFormSelector :: Selector
setPossessiveFormSelector = mkSelector "setPossessiveForm:"

-- | @Selector@ for @possessiveAdjectiveForm@
possessiveAdjectiveFormSelector :: Selector
possessiveAdjectiveFormSelector = mkSelector "possessiveAdjectiveForm"

-- | @Selector@ for @setPossessiveAdjectiveForm:@
setPossessiveAdjectiveFormSelector :: Selector
setPossessiveAdjectiveFormSelector = mkSelector "setPossessiveAdjectiveForm:"

-- | @Selector@ for @reflexiveForm@
reflexiveFormSelector :: Selector
reflexiveFormSelector = mkSelector "reflexiveForm"

-- | @Selector@ for @setReflexiveForm:@
setReflexiveFormSelector :: Selector
setReflexiveFormSelector = mkSelector "setReflexiveForm:"

